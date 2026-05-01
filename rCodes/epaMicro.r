
################################################
## ## Reompute data from microdata
################################################
usePackages(pkgs)
usePackages(c("ineapir", "dplyr", "tidyr", "data.table"))
#data[is.na(whx), .N]

# Build list of files to download from INE
# main url
epa <- "https://www.ine.es/ftp/microdatos/epa"
# Alternative cleaner approach using expand.grid
dtName <- expand.grid(q = 1:4, y = 05:25)
dtName <- sprintf("datos_%dt%02d", dtName$q, dtName$y)
# url list
epaList <- paste0(epa, "/", dtName, ".zip") 

# Local cache for INE EPA zip files. This avoids re-downloading files on reruns.
epa_cache_dir <- "./data/epa_zip_cache"
dir.create(epa_cache_dir, recursive = TRUE, showWarnings = FALSE)


# Get and process a single period file, returning a data.table with the quarterly estimates
################################################
getOneEAP <- function(url, cache_dir = "./data/epa_zip_cache") {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  # get file name from url
  file_name <- basename(url)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  local_zip <- file.path(cache_dir, file_name)
  
  tryCatch({
    # Download once and reuse cached zip files on reruns.
    if (!file.exists(local_zip)) {
      download.file(url, local_zip, mode = "wb", quiet = TRUE)
    }
    
    # Read the microdata file from the CSV folder. INE uses csv/tab names across years.
    zip_contents <- unzip(local_zip, list = TRUE)$Name
    csv_file <- grep("^CSV/.*\\.(csv|tab|txt)$", zip_contents, value = TRUE, ignore.case = TRUE)
    
    if (length(csv_file) == 0) return(NULL)
    unzip(local_zip, files = csv_file[1], exdir = tmp_dir)
    micro_file <- file.path(tmp_dir, csv_file[1])
    header <- names(fread(micro_file, nrows = 0, showProgress = FALSE))
    
    # EPA Microdata variable mapping:
    # FACTOREL: weight | EDAD1/EDAD5: age | AOI: labour nation
    # HORASH: effective weekly hours | PRONA1: province/country of birth
    age_var <- intersect(c("EDAD1", "EDAD5"), header)[1]
    needed <- c("FACTOREL", "AOI", "HORASH", "PRONA1", age_var)
    if (any(is.na(needed)) || !all(needed %in% header)) {
      stop("Missing required EPA variables")
    }
    dt <- fread(micro_file, select = needed, showProgress = FALSE)
    setnames(dt, age_var, "age")
    dt[, age := as.integer(age)]
    dt <- dt[age >= 16]
    
    dt[, w := as.numeric(as.character(FACTOREL))]
    dt[, r := sprintf("%02d", as.integer(trimws(as.character(AOI))))]
    dt[, h := as.numeric(as.character(HORASH)) / 100]
    dt[h <= 0 | h > 100, h := NA_real_]
    dt[, employed := r %in% c("03", "04")]
    dt[, active_status := r %in% c("03", "04", "05", "06")]
    dt[, birth_code := as.numeric(as.character(PRONA1))]
    dt[
      ,
      nation := fifelse(
        !is.na(birth_code) & birth_code >= 1 & birth_code <= 52,
        "Spanish",
        "Foreign"
      )
    ]
    
    # Process quarterly estimates
    q_data <- dt[
      ,
      .(
        pop = sum(w, na.rm = TRUE),
        whx = weighted.mean(h[employed], w[employed], na.rm = TRUE),
        wpx = weighted.mean(active_status, w, na.rm = TRUE)
      ),
      by = .(age, nation)
    ]
    
    # Store file name only; year is extracted after parallel processing.
    q_data[, file_name := file_name]
    
    # Clean up temp files for this iteration
    unlink(tmp_dir, recursive = TRUE)
    # Print progress as file_name: OK
    cat(file_name, ": OK\n")
    return(q_data)
    
  }, error = function(e) {
    warning(paste("Failed to process:", file_name, "-", conditionMessage(e)))
    unlink(tmp_dir, recursive = TRUE)
    return(NULL)
  })
}


# Testing
################################################
# # Test with the first URL to ensure it works before processing all
test <- getOneEAP(epaList[45]) 


# Full pipeline execution for all URLs
################################################
# 1. Pipeline Execution
usePackages("parallel")
res <- mclapply(epaList, getOneEAP, mc.cores = 4)
epa_4t <- rbindlist(res)
epa_4t[, year := as.integer(sub(".*t([0-9]{2})\\.zip$", "\\1", file_name))]
epa_4t[, year := fifelse(year > 50L, 1900L + year, 2000L + year)]
# add qid for quarterly number
epa_4t[, qid := as.integer(sub(".*datos_([0-9])t[0-9]{2}\\.zip$", "\\1", file_name))]
# setorder by year, qid, age, nation
setorder(epa_4t, year, qid, age, nation)
saveRDS(epa_4t, file.path("./data/epa_4t.rds"))

# 2. Summary Print
################################################
cat("\n--- EPA Annual Processing Summary ---\n")
cat("Rows:", nrow(epa_4t), "\n")
cat("Years covered:", paste(range(epa_4t$year), collapse = " to "), "\n")
cat("Columns:", paste(names(epa_4t), collapse = ", "), "\n")


# 3. Annual Aggregation (Weighted averaging quarterly estimates)
# read rds if missing
epa_4t <- readRDS(file.path("./data/epa_4t.rds"))
epa_yr <- epa_4t[, .(
  pop = mean(pop, na.rm = TRUE),
  whx = weighted.mean(whx, w = pop, na.rm = TRUE),
  wpx = weighted.mean(wpx, w = pop, na.rm = TRUE)
), by = .(year, age, nation)]

# 3 Group ages into ageg
epa_yr[age >= 16 & age <= 34, ageg := "Young(16-34)"]
epa_yr[age >= 35 & age <= 54, ageg := "Adult(35-54)"]
epa_yr[age >= 55, ageg := "Older(55+)"]
epa_yr[, ageg := factor(ageg, levels = c("Young(16-34)", "Adult(35-54)", "Older(55+)"))]
## convert wpx to annual
# assume 50 week per year
epa_yr[, whx := 50*whx]
# total labour
epa_yr[, lab := pop * wpx * whx]

# 4 Final Output
setorder(epa_yr, year, age, nation)
saveRDS(epa_yr, file.path("./data/epa_yr.rds"))



