## this file will contain code to generage base data for analysis. Please do not include location settings.

################################################
## Initial Data processing
################################################

usePackages(pkgs)

data <- read_excel("./data/Data.xlsx" )
data <-  as.data.table(data)
names(data) <- c("year", "age", "sex", "img", "whx", "wpx", "pop")

## remove strings from age
data[, age := gsub("from", "", age)]
data[, age := gsub("more", "", age)]
data[, age := gsub("to", "-", age)]
data[, age := gsub("and", "+", age)]
data[, age := gsub(" ", "", age)]


## recode ages into ageg
data[age %in% c("16-24", "25-34" ), ageg := factor( "Young(16-34)")]
data[age %in% c("35-44", "45-54" ), ageg := factor("Adult(35-54)")]
data[age  == "55+", ageg := factor("Older(55+)")]

## convert pop and wpx
# assume 50 week per year
data[, whx := 50*whx]
# total labour
data[, lab := whx*wpx*pop]

# replace spain with spanish in img
data[img == "Spain", img := "Spanish"]

## save for later use
saveRDS(data, "./data/data.rds")




















