# base package
usePackages(pkgs)
## see local.r for folder settings
data <- readRDS("./data/data.rds")
## load config
source("./rCodes/config.r")
source("./rCodes/function.r")




########################################################################
## Descriptives Stats use in Paper
########################################################################

sepline <- ggplot()+ 
    geom_vline(xintercept = 1, linetype = "dashed",size = 0.1) + 
    geom_vline(xintercept = 1.925, linetype = "dashed",size = 0.1) +
    scale_x_continuous(limits = c(0, 3)) +
    theme_void()



########################
## Participation
########################
## compute data
library(Hmisc)
wpxDta <- data[, .(wpx = wtd.mean(wpx, pop)), by = .(year, ageg, img)]

# create plot
plot <- ggplot(wpxDta, mapping = aes(
    y = wpx, x = year, color = img, linetype = img,
    group = img, shape = img
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
    breaks = seq(2005, 2025, by = 3),
    limits = c(2005, 2025)
    ) +
    labs(y = "%") +
  ## separator
    inset_element(sepline, left = -0.1, bottom = 0.1, right = 1.1, top = 0.9)

# dev.off()
# plot
## save the plot
ggsave(file.path("./graph/wpx.png"), plot, width = 10, height = 4)


########################
## Hours worked
########################
## compute data
library(Hmisc)
whxDta <- data[, .(whx = wtd.mean(whx, pop)), by = .(year, ageg, img)]

# create plot
plot <- ggplot(whxDta, mapping = aes(
    y = whx, x = year, color = img, linetype = img,
    group = img, shape = img
    )) +
    mygthemep + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(ageg)) +
    scale_y_continuous(
        breaks=seq(1800,2600,by=200), 
        limits = c(1800,2600),
        name = "annual hours",
        sec.axis = sec_axis(~ ./50, breaks=seq(36,52,by=4), name = "weekly hours")) +
    scale_x_continuous(
        breaks = seq(2005, 2025, by = 3),
        limits = c(2005, 2025)
    ) +
  ## separator
    inset_element(sepline, left = -0.1, bottom = 0.1, right = 1.1, top = 0.9)

# dev.off()
# plot
## save the plot
ggsave(file.path("./graph/whx.png"), plot, width = 10, height = 4)



########################
## Age stx
########################
## compute total populaiton by year, img and sex
stxDta <-  data[, .(pop = sum(pop)), by = .(year, ageg, img)]
## total pop img + native
stxDta[, tPop := sum(pop), by = .(year, ageg)]
## compute population structure as proxy for aging
stxDta <-  stxDta[, .(stx = 100*pop/tPop), by = .(year, ageg, img)]

# create plot
# create plot
plot <- ggplot(stxDta, mapping = aes(
    y = stx, x = year, color = img, linetype = img,
    group = img, shape = img
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
    breaks = seq(2005, 2025, by = 3),
    limits = c(2005, 2025)
    ) +
    labs(y = "%") +
  ## separator
    inset_element(sepline, left = -0.1, bottom = 0.1, right = 1.1, top = 0.9)

# dev.off()
# plot
## save the plot
ggsave(file.path("./graph/stx.png"), plot, width = 10, height = 4)





########################
## Lab change
########################

usePackages("patchwork")

labchg <- data[, .(lab = sum(lab)), by = .(year, img)]
labchg <- rbind(labchg, labchg[, .(img = "Total", lab = sum(lab)), by = year])

# convert to million of person hours per year using 50 weeks and 40 hours per week
labchg[, lab := lab / (50 * 40 * 1e6)]

labchgPlot <- copy(labchg[img != "Total"])
labchgPlot[, img := factor(img, levels = c("Spanish", setdiff(unique(img), "Spanish")))]

plot <- ggplot(labchgPlot, aes(x = year, y = lab, fill = img)) +
    mygthemep +
    geom_area(color = "white", linewidth = 0.2, position = position_stack(reverse = TRUE)) +
    scale_y_continuous(
        breaks = seq(1000, 3000, by = 500)
    ) +
    scale_x_continuous(
        breaks = seq(2005, 2025, by = 3),
        limits = c(2006, 2023)
    ) +
    coord_cartesian(ylim = c(1000, 3000)) +
    labs(x = NULL, y = "Million person-hours per year") +
    scale_fill_brewer(palette = "Set2") 

plot



ggsave(file.path("./graph/labchg.png"), plot, width = 10, height = 4)


####################################################################
#  Labor growth by immig and age
####################################################################

lMat <- as.data.table(data)
pdta <- lMat[type == "lab" & immig != "Total", .(value = sum(value)),
  by = list(year, immig, ageg)
]
iValue <- pdta[year == 1981, value, by = list(immig, ageg)]
pdta[iValue, on = list(immig, ageg), value := -100 * (1 - value / i.value)]

# use french labels
levels(pdta$ageg) <- c("Jeune(15-34)", "Adulte(35-54)", "Agée(55+)")
pdta$immig <- factor(pdta$immig)
levels(pdta$immig) <- c("Natifs", "Immigrants")
# plot
plot <- ggplot(pdta, mapping = aes(
  y = value, x = year, color = ageg,
  group = ageg, shape = ageg
)) +
  mygthemep +
  theme(strip.text = element_text(size = 7)) +
  # geom_line(size = 1)+
  geom_point(size = 2) +
  geom_smooth(aes(y = value), formula = y ~ poly(x, 7), method = lm, alpha = 0.3, se = F) +
  facet_grid(cols = vars(immig)) +
  # scale_colour_manual(values = mygcolor[c(3,4,5)])+
  scale_colour_manual(values = mygcolor[c(3, 1, 6)]) +
  scale_linetype_manual(values = mygshape[c(1, 2, 4)]) +
  # scale_y_continuous(breaks=seq(0,40,by=8), limits = c(0,40))+
  scale_x_continuous(breaks = seq(1981, 2016, by = 5)) +
  scale_y_continuous(sec.axis = dup_axis()) +
  labs(x = "Année", y = "% par rapport à 1981")

# dev.off()
plot
ggsave(file.path(path, "Docs/labCanada/res/labRate.png"), plot, width = 10, height = 3.5)



########################################################################
## Decomposition
########################################################################

#####################
#  Generate effect mat
#####################

# build the labor matrix with required variables for decomposition
lMat <- data[, .(whx = wtd.mean(whx, pop), wpx = wtd.mean(wpx, pop), pop = sum(pop)), by = .(year, ageg, img)]
# add totoal population by year
lMat[, tpop := sum(pop), by = .(year)]
# add stx
lMat[, stx := pop/tpop, by = .(year, ageg)]

# build the effect table by calling getEffect()
etable <- getEffect(lMat)
# save as rds for later use
saveRDS(etable, file = file.path(rpath, "data/etable.rds"))



# reshape to long format
eMatL <- melt.data.table(etable, measure.vars = c("tpop", "stx", "wpx", "whx"), variable.name = "comp")




# extract and sum over years
fa <- eMatL[, .(value = sum(value)), by = c("img", "ageg", "comp")]
# add total img
fa <- rbind(
  fa,
  cbind(fa[, .(value = sum(value)), by = c("ageg", "comp")],
    img = factor("Total")
  )
)
# convert to percentage of total labor change
tLab <- fa[img == "Total", sum(value)]
fa <- fa[, value := 100 * value / tLab]


#####################
#  Plot English
#####################
plot <- ggplot(fa[value != 0, ], mapping = aes(
  y = value, x = comp, group = ageg,
  colour = ageg, fill = ageg, linetype = ageg
)) +
  theme(
    panel.spacing.x = unit(2, "lines"),
    aspect.ratio = 1.5 / 1,
  ) +
  geom_col(na.rm = TRUE, color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, size = 0.3, color = "gray80") +
  labs(x = NULL, y = "% contributed") +
  facet_grid(cols = vars(img)) 

# dev.off()
plot
ggsave(file.path(path, "Docs/labCanada/res/labDecompEN.png"), plot, width = 10, height = 6)





# load component  and prepare data
# May need to run ddecom()
load(file = file.path(rpath, "Article2/eLab.RData")) # Load ddecom before
eMatL <- copy(etable)
# extra and melt required columns
eMatL <- melt.data.table(eMatL, measure.vars = c("tPop", "pStr", "wpx", "whx"), variable.name = "comp")
levels(eMatL$comp) <- c("Pop", "Str", "Wpx", "Whx")


# Get total for immig
eMatL <- rbind(
  eMatL,
  cbind(eMatL[, .(value = sum(value)), by = c("year", "age", "comp")],
    immig = factor("Total")
  )
)

#####################
#  prcess eMat
#####################

# create and rename compg2
eMatL$compg2 <- factor(eMatL$comp, labels = c("pop" = "Demographic", "str" = "Demographic", "wpx" = "Behavioral", "whx" = "Behavioral"))
# create and rename compg3
# eMatL$compg3 <- factor(eMatL$comp, labels = c("pop"="Pop", "str"="Str","wpx"= "Participation","whx"="Workload"))
# Add age group
eMatL[age >= 1 & age <= 4, ageg := as.factor("Young(15-34)")]
eMatL[age >= 5 & age <= 8, ageg := as.factor("Adult(35-54)")]
eMatL[age >= 9, ageg := as.factor("Older(55+)")]
# sum  by ages
eMatL <- eMatL[, .(value = sum(value)), by = c("year", "immig", "ageg", "comp", "compg2")]
# sort immig
eMatL$immig <- factor(eMatL$immig)
# eMatL$immig <- factor(eMatL$immig, levels = c("Canadian Born", "Immigrant", "Total"))

# Use this isntead of rerning the above
saveRDS(eMatL, file = file.path(rpath, "Article2/eMatL.rds"))
# eMatL[immig!="Total",sum(value)]
# lMat[(year==1981 | year == 2016) & type=="Lab",sum(value),by=year]








