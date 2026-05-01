


################################################
## Acitivity data
################################################
usePackages(pkgs)
usePackages(c("ineapir", "dplyr", "tidyr", "data.table"))


# download data from INE 65956
# wpt <- get_data_table(idTable = 65956, tip = "A")
# save for later use
# saveRDS(wpt, "./data/wpt.rds")

# load wpt previously downloaded and saved rds
wpt <- readRDS("./data/wpt.rds")
# reshape data to get year, age, nationality, value
activity_dt <- wpt %>%
  unnest(Data) %>%
  as.data.table()

activity_dt[
  ,
  c("measure", "geo", "sex", "age", "nationality") := tstrsplit(
    Nombre,
    "\\. ",
    keep = 1:5
  )
]

activity_dt <- activity_dt[
  geo == "Total Nacional" &
    sex == "Ambos sexos" &
    nationality %in% c("Española", "Extranjera: Total"),
  .(
    year = as.integer(Anyo),
    age = age,
    nation = nationality,
    prate = as.numeric(Valor)
  )
]

activity_dt[,
  nation := fcase(
    nation == "Española", "Spanish",
    nation == "Extranjera: Total", "Foreign",
    default = nation
  )
]

activity_dt[, age := gsub("^De\\s+", "", age, ignore.case = TRUE)]
activity_dt[, age := gsub("\\s+y\\s+más\\s+años$", "+", age, ignore.case = TRUE)]
activity_dt[, age := gsub("\\s+años$", "", age, ignore.case = TRUE)]
activity_dt[, age := gsub("\\s+a\\s+", "-", age, ignore.case = TRUE)]
activity_dt[, age := gsub("\\s+", " ", trimws(age))]


# final columns are year, age, nation, prate
activity_dt <- activity_dt[, .(year, age, nation, prate)]
setorder(activity_dt, year, nation, age)

################################################
## Population dta
################################################
# Get Population data from table 56936
# download data from INE 56936
# popt <- get_data_table(idTable = 56936, tip = "A")
# save for later use
# saveRDS(popt, "./data/popt.rds")


# final column are, year, age, nation, pop
# load data previously downloaded and saved rds
popt <- readRDS("./data/popt.rds")
pop_dt <- popt %>%
  unnest(Data) %>%
  as.data.table()

pop_dt[
  ,
  c("geo", "nationality", "age", "sex") := tstrsplit(
    Nombre,
    "\\. ",
    keep = 1:4
  )
]

pop_dt <- pop_dt[
  geo == "Total Nacional" &
    sex == "Total" &
    nationality %in% c("Española", "Extranjera") &
    age != "Todas las edades",
  .(
    year = as.integer(Anyo),
    age = age,
    nation = nationality,
    pop = as.numeric(Valor)
  )
]

pop_dt[
  ,
  nation := fcase(
    nation == "Española", "Spanish",
    nation == "Extranjera", "Foreign",
    default = nation
  )
]

pop_dt[, age := gsub("^De\\s+|\\s+años$", "", age, ignore.case = TRUE)]
pop_dt[, age := gsub("\\s+a\\s+", "-", age, ignore.case = TRUE)]
pop_dt[, age := gsub("\\s+y\\s+más$", "+", age, ignore.case = TRUE)]
pop_dt[, age := gsub("\\s+", " ", trimws(age))]


# Table 56936 is quarterly. Average quarters to obtain annual population.
pop_dt <- pop_dt[
  ,
  .(pop = mean(pop, na.rm = TRUE)),
  by = .(year, age, nation)
]

pop_dt <- pop_dt[, .(year, age, nation, pop)]
setorder(pop_dt, year, nation, age)



#### consistency check
# copy pop_dt as test
test <- as.data.table(pop_dt)
# remove line where age is 0-4, 10-15
test <- test[!age %in% c("0-4", "5-9", "10-15")]
# aggregate 5-year age groups into 10-year age groups for pop_dt
test[age %in% c("15-19", "20-24"), age := "16-24"]
test[age %in% c("25-29", "30-34"), age := "25-34"]
test[age %in% c("35-39", "40-44"), age := "35-44"]
test[age %in% c("45-49", "50-54"), age := "45-54"]
# make all other age into 55+
test[!age %in% c("16-24", "25-34", "35-44", "45-54"), age := "55+"]
# aggregate by year, age, nation
test <- test[, .(pop = sum(pop, na.rm = TRUE)), by = .(year, age, nation)]



# bring prate into activity_dt from pop_dt
test[activity_dt, on = .(year, age, nation), wpx := prate]
test <- test[!is.na(wpx)]
## recode ages into ageg
test[age %in% c("16-24", "25-34" ), ageg := factor( "Young(16-34)")]
test[age %in% c("35-44", "45-54" ), ageg := factor("Adult(35-54)")]
test[age  == "55+", ageg := factor("Older(55+)")]

wpxDta <- test[, .(wpx = wtd.mean(wpx, pop)), by = .(year, ageg, nation)]

# create plot
plot <- ggplot(wpxDta, mapping = aes(
    y = wpx, x = year, color = nation, linetype = nation,
    group = nation, shape = nation
    )) +
    mygthemep + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(ageg)) +
    scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 101),
    sec.axis = dup_axis()  # Add secondary axis as duplicate
    ) +
    scale_x_continuous(
    breaks = seq(2005, 2025, by = 5),
    limits = c(2005, 2025)
    ) +
    labs(y = "%") +
  ## separator
    patchwork::inset_element(sepline, left = -0.1, bottom = 0.1, right = 1.1, top = 0.9)
plot
