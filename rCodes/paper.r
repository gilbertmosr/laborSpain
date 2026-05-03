# base package
usePackages(pkgs)
usePackages(c("patchwork", "Hmisc", "ggpubr"))
## see local.r for folder settings
source("./rCodes/function.r")
# data <- readRDS("./data/data.rds")
# read epa_yr into data
data <- readRDS("./data/epa_yr.rds")


########################################################################
## Descriptives Stats use in Paper
########################################################################

########################
## Data summary
########################

# Keep only target years
dta <- data[year %in% c(2005, 2025)]

# Compute combined population and rbind
dta <- rbindlist(
  list(
    dta,
    dta[
      ,
      .(
        pop = sum(pop, na.rm = TRUE),
        wpx = weighted.mean(wpx, pop, na.rm = TRUE),
        whx = weighted.mean(whx, pop, na.rm = TRUE)
      ),
      by = .(year, age)
    ][, nation := "Combined"]
  ),
  fill = TRUE
)

# Create summary table
popSum <- dta[
  ,
  .(
    pop = sum(pop, na.rm = TRUE) / 1e6,
    wpx = 100 * weighted.mean(wpx, pop, na.rm = TRUE),
    whx = weighted.mean(whx, pop, na.rm = TRUE) / 50,
    age = sum(pop[age >= 55], na.rm = TRUE) / 1e6
  ),
  by = .(year, nation)
]

# Add % of 55+ in population
popSum[,
  age := 100 * age / pop
]

# Long format
popSum <- melt(
  popSum,
  id.vars = c("year", "nation"),
  measure.vars = c(
    "pop",
    "wpx",
    "whx",
    "age"
  ),
  variable.name = "ind",
  value.name = "value"
)
# round value to 2 decimal places
popSum[, value := round(value, 2)]
# Wide format: Spanish / Foreign
popSum <- dcast(
  popSum,
  ind + year ~ nation,
  value.var = "value"
)



# Order rows
indrk <- c("pop", "wpx", "whx", "age")

popSum[,
  ind := factor(ind, levels = indrk)
]

setorder(popSum, ind, year)

popSum

# Format and export summary table to TeX
popSumTex <- copy(popSum)
popSumTex[, ind := as.character(ind)]
ind_labels <- c(
  pop = "Population",
  wpx = "Activity rate",
  whx = "Weekly hours",
  age = "Share aged 55+"
)
popSumTex[, ind_label := ind_labels[ind]]
popSumTex <- rbindlist(
  lapply(
    levels(popSum$ind),
    function(ind_name) {
      x <- popSumTex[ind == ind_name]
      change <- x[year == 2025, .(Spanish, Foreign, Combined)] - x[year == 2005, .(Spanish, Foreign, Combined)]
      rbind(
        data.table(
          label = paste("\\textbf{", x$ind_label[1], "}", sep = ""),
          Spanish = "",
          Foreign = "",
          Combined = ""
        ),
        x[
          ,
          .(
            label = paste("\\quad", year),
            Spanish = sprintf("%.2f", Spanish),
            Foreign = sprintf("%.2f", Foreign),
            Combined = sprintf("%.2f", Combined)
          )
        ],
        data.table(
          label = "\\quad Change",
          Spanish = sprintf("%.2f", change$Spanish),
          Foreign = sprintf("%.2f", change$Foreign),
          Combined = sprintf("%.2f", change$Combined)
        )
      )
    }
  )
)
setnames(popSumTex, c("", "Spanish", "Foreign", "Combined"))

make_tabular_star <- function(file, from, to) {
  lines <- readLines(file)
  lines <- sub(from, to, lines)
  lines <- sub("\\\\end\\{tabular\\}", "\\\\end{tabular*}", lines)
  writeLines(lines, file)
}

out.tex <- latex(
  popSumTex,
  file = file.path("./resrc/popSum.tex"),
  booktabs = TRUE,
  collabel.just = c("l", "r", "r", "r"),
  col.just = c("l", "r", "r", "r"),
  center = "none",
  rowname = NULL,
  table.env = FALSE,
  longtable = FALSE,
  cgroup = c("", "Birthplace status"),
  n.cgroup = c(1, 3),
  dcolumn = FALSE
)
popSumFile <- file.path("./resrc/popSum.tex")
make_tabular_star(
  popSumFile,
  "\\\\begin\\{tabular\\}\\{lcrrr\\}",
  "\\\\begin{tabular*}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}lcrrr}"
)





########################
## Participation
########################
## compute data
wpxDta <- data[, .(wpx = 100 * wtd.mean(wpx, pop)), by = .(year, ageg, nation)]

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
    patchwork::inset_element(sepline, left = -0.05, bottom = 0.1, right = 1.05, top = 0.9)

# dev.off()
plot
## save the plot
ggsave(file.path("./resrc/wpx.png"), plot, width = 10, height = 4)


########################
## Hours worked
########################
## compute data

whxDta <- data[, .(whx = wtd.mean(whx, pop)), by = .(year, ageg, nation)]

# create plot
plot <- ggplot(whxDta, mapping = aes(
    y = whx, x = year, color = nation, linetype = nation,
    group = nation, shape = nation
    )) +
    mygthemep + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(ageg)) +
    scale_y_continuous(
        breaks=seq(1600,2600,by=200), 
        limits = c(1600,2600),
        name = "annual hours",
        sec.axis = sec_axis(~ ./50, breaks=seq(32,52,by=4), name = "weekly hours")) +
    scale_x_continuous(
        breaks = seq(2005, 2025, by = 5),
        limits = c(2005, 2025)
    ) +
  ## separator
    patchwork::inset_element(sepline, left = -0.05, bottom = 0.1, right = 1.05, top = 0.9)

# dev.off()
plot
## save the plot
ggsave(file.path("./resrc/whx.png"), plot, width = 10, height = 4)



########################
## Age stx
########################
## compute total populaiton by year, nation and sex
stxDta <-  data[, .(pop = sum(pop)), by = .(year, ageg, nation)]
## total pop nation + native
stxDta[, tPop := sum(pop), by = .(year, ageg)]
## compute population structure as proxy for aging
stxDta <-  stxDta[, .(stx = 100*pop/tPop), by = .(year, ageg, nation)]

# create plot
# create plot
plot <- ggplot(stxDta, mapping = aes(
    y = stx, x = year, color = nation, linetype = nation,
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
    patchwork::inset_element(sepline, left = -0.05, bottom = 0.1, right = 1.05, top = 0.9)

# dev.off()
plot
## save the plot
ggsave(file.path("./resrc/stx.png"), plot, width = 10, height = 4)


#################################################################
# Labour plot
#################################################################


########################
## Lab change
########################
labchg <- data[, .(lab = sum(lab)), by = .(year, nation)]

# convert to million of person hours per year using 50 weeks and 40 hours per week
# labchg[, lab := lab / (50 * 40 * 1e6)]
labchgPlot <- copy(labchg[nation %in% c("Spanish", "Foreign")])
labchgPlot[, nation := factor(nation, levels = c("Spanish", "Foreign"))]
labchgMax <- ceiling(max(labchgPlot[, sum(lab), by = year]$V1, na.rm = TRUE) / 5) * 5

plot <- ggplot(labchgPlot, aes(x = year, y = lab, fill = nation)) +
    mygthemep +
    geom_area(color = "white", linewidth = 0.2, position = "stack") +
    scale_y_continuous(
        breaks = seq(0, labchgMax, by = 5)
    ) +
    scale_x_continuous(
        breaks = seq(2005, 2025, by = 5)
    ) +
    coord_cartesian(xlim = range(labchgPlot$year), ylim = c(0, labchgMax)) +
    labs(x = NULL, y = "Million FTE workers") +
    scale_fill_brewer(palette = "Set2") 

plot



ggsave(file.path("./resrc/labchg.png"), plot, width = 10, height = 4)


#####################
#  Lab Donot plot 
#####################


pdta <- rbind(
  data[year %in% c(2005, 2025), .(total = sum(pop)), by = .(year, ageg)][
    ,
    .(year, type = "pop", ageg, total)
  ],
  data[year %in% c(2005, 2025), .(total = sum(lab)), by = .(year, ageg)][
    ,
    .(year, type = "Lab", ageg, total)
  ]
)
pdta[, value := 100 * total / sum(total), by = .(year, type)]
pdta[, value := round(value, 1)]
pdta[, ageg := factor(
  ageg,
  levels = unique(ageg),
  labels = c("Young (15-34)", "Adult (35-54)", "Older (55+)")
)]

pdta_change <- dcast(pdta, type + ageg ~ year, value.var = "value")
pdta_change[, value := `2025` - `2005`]
pdta_change[, year := 2025]




labName <- labeller(
  type = c(pop = "Population\nshare", Lab = "Labour supply\nshare"),
  year = c("2005" = "Share in 2005 (%)")
)

# plot A for 2005
plotA <- ggplot(pdta[year == 2005, ], aes(x = 2, y = value, fill = ageg)) +
  facet_grid(
    cols = vars(year), rows = vars(type), switch = "y",
    labeller = labName
  ) +
  geom_bar(stat = "identity", color = "gray", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = value),
    color = "white",
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_manual(values = mygcolor) +
  theme_void() +
  xlim(0.5, 2.5) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    strip.text.y.left = element_text(angle = 0, margin = margin(0, 2, 0, 2, "pt"))
  )


# plot B for 2025
labName <- labeller(
  type = c(pop = "", Lab = ""),
  year = c("2025" = "Share in 2025 (%)")
)
plotB <- ggplot(pdta[year == 2025, ], aes(x = 2, y = value, fill = ageg)) +
  facet_grid(
    cols = vars(year), rows = vars(type), switch = "y",
    labeller = labName
  ) +
  geom_bar(stat = "identity", color = "gray", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = value),
    color = "white",
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_manual(values = mygcolor) +
  theme_void() +
  xlim(0.5, 2.5) +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    strip.text.y.left = element_text(angle = 0)
  )

# plot for diff
labName <- labeller(
  type = c(pop = "", Lab = ""),
  year = c("2025" = "Change, 2025-2005 (pp)")
)
plotC <- ggplot(pdta_change, aes(x = ageg, y = value, fill = ageg)) +
  facet_grid(
    cols = vars(year), rows = vars(type),
    labeller = labName
  ) +
  geom_bar(stat = "identity", width = 0.7, color = "gray") +
  geom_hline(yintercept = 0, color = "gray") +
  geom_text(
    aes(label = value),
    color = "white",
    position = position_stack(vjust = 0.5), size = 3
  ) +
  scale_fill_manual(values = mygcolor) +
  theme_void() +
  theme(
    legend.position = "bottom", legend.title = element_blank(),
    aspect.ratio = 1 / 1
  )

usePackages("ggpubr")

cplot <- ggarrange(plotA, plotB, plotC, legend = "bottom", common.legend = TRUE, ncol = 3, widths = c(1.3, 1, 1))
# dev.off()
cplot

# export
ggsave(file.path("resrc/labPopChg.pdf"), cplot, width = 10, height = 6)



#####################
#  Labor growth plot
#####################
pdta <- data[, .(value = sum(lab)),
  by = list(year, nation, ageg)
]
iValue <- pdta[year == 2005, value, by = list(nation, ageg)]
pdta[iValue, on = list(nation, ageg), value := -100 * (1 - value / i.value)]


# plot
plot_spanish <- ggplot(pdta[nation == "Spanish"], mapping = aes(
  y = value, x = year, color = ageg,
  group = ageg, shape = ageg
)) +
  mygthemep +
  theme(plot.title = element_text(size = 7, hjust = 0.5)) +
  # geom_line(size = 1)+
  geom_point(size = 2) +
  geom_smooth(aes(y = value), formula = y ~ poly(x, 7), method = lm, alpha = 0.3, se = F) +
  # scale_colour_manual(values = mygcolor[c(3,4,5)])+
  scale_colour_manual(values = mygcolor) +
  # scale_y_continuous(breaks=seq(0,40,by=8), limits = c(0,40))+
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), limits = c(2005, 2025)) +
  scale_y_continuous(breaks = seq(-50, 100, by = 25)) +
  coord_cartesian(ylim = c(-50, 100)) +
  labs(title = "Spanish", x = NULL, y = "% relative to 2005")

plot_foreign <- ggplot(pdta[nation == "Foreign"], mapping = aes(
  y = value, x = year, color = ageg,
  group = ageg, shape = ageg
)) +
  mygthemep +
  theme(plot.title = element_text(size = 7, hjust = 0.5)) +
  # geom_line(size = 1)+
  geom_point(size = 2) +
  geom_smooth(aes(y = value), formula = y ~ poly(x, 7), method = lm, alpha = 0.3, se = F) +
  # scale_colour_manual(values = mygcolor[c(3,4,5)])+
  scale_colour_manual(values = mygcolor) +
  # scale_y_continuous(breaks=seq(0,40,by=8), limits = c(0,40))+
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), limits = c(2005, 2025)) +
  scale_y_continuous(position = "right", breaks = seq(-200, 1000, by = 200)) +
  coord_cartesian(ylim = c(-200, 1000)) +
  labs(title = "Foreign", x = NULL, y = "% relative to 2005")

plot <- ggarrange(
  plot_spanish, plot_foreign,
  legend = "top", common.legend = TRUE,
  ncol = 2, widths = c(1, 1)
)

# dev.off()
plot
ggsave(file.path("resrc/labRate.pdf"), plot, width = 10, height = 3.5)


#####################
#  Lab Summary table 
#####################
labSumBase <- data[
  year %in% c(2005, 2025),
  .(
    Population = sum(pop, na.rm = TRUE) / 1e6,
    `Labour supply` = sum(lab, na.rm = TRUE) / (50 * 40 * 1e6)
  ),
  by = .(year, nation)
]

labSumBase <- rbind(
  labSumBase,
  labSumBase[
    ,
    .(
      Population = sum(Population, na.rm = TRUE),
      `Labour supply` = sum(`Labour supply`, na.rm = TRUE)
    ),
    by = year
  ][, nation := "Combined"]
)

labSumLong <- melt(
  labSumBase,
  id.vars = c("year", "nation"),
  variable.name = "indicator",
  value.name = "value"
)

labSumLong <- rbind(
  labSumLong,
  labSumLong[
    ,
    .(value = value[year == 2025] - value[year == 2005]),
    by = .(nation, indicator)
  ][, year := "Change 2005--2025"]
)

labSumLong[year %in% c(2005, 2025), year := as.character(year)]
labSumLong[
  ,
  row := fifelse(
    year == "Change 2005--2025",
    "Change, 2005--2025",
    paste(indicator, "in", year)
  )
]
labSumLong[
  ,
  row_id := fifelse(
    year == "Change 2005--2025",
    paste(indicator, "change"),
    paste(indicator, year)
  )
]

row_order <- c(
  "Population 2005",
  "Population 2025",
  "Population change",
  "Labour supply 2005",
  "Labour supply 2025",
  "Labour supply change"
)

labSum <- dcast(
  labSumLong,
  row_id + row ~ nation,
  value.var = "value"
)

labSum[, row_id := factor(row_id, levels = row_order)]
setorder(labSum, row_id)
labSum[, row_id := NULL]
labSum[, c("Spanish", "Foreign", "Combined") :=
  lapply(.SD, round, 2),
  .SDcols = c("Spanish", "Foreign", "Combined")
]

setcolorder(labSum, c("row", "Spanish", "Foreign", "Combined"))


#  Export to latex
#####################
labSumTex <- copy(labSum)
labSumTex[
  ,
  c("Spanish", "Foreign", "Combined") :=
    lapply(.SD, function(x) sprintf("%.2f", x)),
  .SDcols = c("Spanish", "Foreign", "Combined")
]
setnames(labSumTex, "row", "")

out.tex <- latex(
  labSumTex,
  file = file.path("./resrc/labSum.tex"),
  booktabs = TRUE,
  collabel.just = c("l", "r", "r", "r"),
  col.just = c("l", "r", "r", "r"),
  center = "none",
  rowname = NULL,
  table.env = FALSE,
  longtable = FALSE,
  dcolumn = FALSE
)
labSumFile <- file.path("./resrc/labSum.tex")
make_tabular_star(
  labSumFile,
  "\\\\begin\\{tabular\\}\\{lrrr\\}",
  "\\\\begin{tabular*}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}lrrr}"
)




#################################################################
## Decomposition
#################################################################

etable[, sum(tpop + stx + whx + wpx) / (50 * 40 * 1e6), by = nation]

#####################
#  Generate effect
#####################

# build the labor matrix with required variables for decomposition
lMat <- data[, .(whx = wtd.mean(whx, pop), wpx = wtd.mean(wpx, pop), pop = sum(pop)), by = .(year, ageg, nation)]

lMat <- data[, .(year, pop, wpx, whx, age, nation)]
# add totoal population by year
lMat[, tpop := sum(pop), by = .(year, nation)]
# add stx
lMat[, stx := pop/tpop, by = .(year, nation)]
# build the effect table by calling getEffect()
etable <- getEffect(lMat)

# ageg back into etable
ageg <- unique(data[, .(age, ageg)])
etable[ageg, on = .(age), ageg := i.ageg]
# save to rds for later use
saveRDS(etable, file.path("data/etable.rds"))

# load existing
etable <- readRDS(file.path("data/etable.rds"))
# reshape to long format
eMatL <- melt.data.table(etable, measure.vars = c("tpop", "stx", "wpx", "whx"), variable.name = "comp")






#####################
#  Effect matrix
#####################

periods <- data.table(
  period = c("p", "p1", "p2", "p3", "p4"),
  start = c(2005, 2005, 2009, 2014, 2019),
  end = c(2024, 2008, 2013, 2018, 2024)
)


effectMat <- rbindlist(lapply(seq_len(nrow(periods)), function(i) {
  p <- periods[i]
  x <- eMatL[year %between% c(p$start, p$end)]
  x <- rbind(
    x[, .(value = sum(value)), by = .(nation, comp)],
    x[, .(value = sum(value), nation = "Combined"), by = comp],
    fill = TRUE
  )
  x <- rbind(
    x,
    x[, .(value = sum(value), comp = "total"), by = nation]
  )
  x[, period := p$period]
  x
}))

effectMat[, value := round(value / (50 * 40 * 1e6), 2)]
effectMat[, nation := factor(nation, levels = c("Spanish", "Foreign", "Combined"))]
effectMat[, comp := factor(comp, levels = c("stx", "wpx", "whx", "tpop", "total"))]
setorder(effectMat, nation, comp)

effectMat <- dcast(effectMat, nation + comp ~ period, value.var = "value")
effectMat <- effectMat[, .(nation, comp, `p`, `p1`, `p2`, `p3`, `p4`)]



#####################
#  Latex table
#####################

# Format and export summary table to TeX
effectMatTex <- copy(effectMat)
effectMatTex[, comp := as.character(comp)]
effectMatTex[, comp_label := fcase(
  comp == "tpop", "Population size",
  comp == "stx", "Age structure",
  comp == "wpx", "Activity rate",
  comp == "whx", "Hours worked",
  comp == "total", "Period total"
)]

period_cols <- c("p", "p1", "p2", "p3", "p4")
effectMatTex[
  ,
  (period_cols) := lapply(.SD, function(x) sprintf("%.2f", x)),
  .SDcols = period_cols
]

effectMatTex <- rbindlist(
  lapply(
    levels(effectMat$nation),
    function(nation_name) {
      x <- effectMatTex[nation == nation_name]
      rbind(
        data.table(
          label = paste("\\textbf{", nation_name, "}", sep = ""),
          `p` = "",
          `p1` = "",
          `p2` = "",
          `p3` = "",
          `p4` = ""
        ),
        x[
          ,
          .(
            label = paste("\\quad", comp_label),
            `p`,
            `p1`,
            `p2`,
            `p3`,
            `p4`
          )
        ]
      )
    }
  )
)

setnames(effectMatTex, c("", "2005--2025", "2005--2009", "2010--2014", "2015--2019", "2020--2025"))

out.tex <- latex(
  effectMatTex,
  file = file.path("./resrc/effectMat.tex"),
  booktabs = TRUE,
  collabel.just = c("l", rep("r", 5)),
  col.just = c("l", rep("r", 5)),
  center = "none",
  rowname = NULL,
  table.env = FALSE,
  longtable = FALSE,
  cgroup = c("", "Period"),
  n.cgroup = c(1, 5),
  dcolumn = FALSE
)
effectMatFile <- file.path("./resrc/effectMat.tex")
effectMatLines <- readLines(effectMatFile)
effectMatLines <- sub(
  "\\\\begin\\{tabular\\}\\{lcrrrrr\\}",
  "\\\\begin{tabular*}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}lcrrrrr}",
  effectMatLines
)
effectMatLines <- sub("\\\\end\\{tabular\\}", "\\\\end{tabular*}", effectMatLines)
writeLines(effectMatLines, effectMatFile)

#####################
#  Plot English
#####################

# extract plot data



# round(sum(eMatL$value) / (50 * 40 * 1e6), 2)

# extract and sum over years
fa <- eMatL[, .(value = sum(value)), by = c("nation", "ageg", "comp")]
# add total nation
fa <- rbind(
  fa,
  cbind(fa[, .(value = sum(value)), by = c("ageg", "comp")],
    nation = factor("Combined")
  )
)
# convert to percentage of total labor change
tLab <- fa[nation == "Combined", sum(value)]
fa[, value := 100 * value / tLab]
fa[, nation := factor(nation, levels = c("Spanish", "Foreign", "Combined"))]

# plot start here
plot <- ggplot(fa[value != 0, ], mapping = aes(
  y = value, x = comp, group = ageg,
  colour = ageg, fill = ageg, linetype = ageg)) + 
mygthemep + theme(
    panel.spacing.x = unit(2, "lines"),
    aspect.ratio = 1.5 / 1,
  ) +
  geom_col(na.rm = TRUE, color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, size = 0.3, color = "gray80") +
  labs(x = NULL, y = "% contributed") +
  facet_grid(cols = vars(nation))+
  scale_colour_manual(values = mygcolor[1:3]) +
  scale_fill_manual(values = mygcolor[1:3]) +
  patchwork::inset_element(sepline2, left = -0.025, bottom = 0.1, right = 1.025, top = 0.9)



# dev.off()
plot
# save the plot
ggsave(file.path("resrc/labDecomp.pdf"), plot, width = 10, height = 6)




#####################
#  LabCDQ 
#####################


test <-readRDS(file = file.path("/workData/workSpace/ptmDrive/phdWorks/myPhD/rCodes/Article2/eMatL.rds")) # Load ddecom before

# extract and sum over years
fa <- test[, .(value = sum(value)), by = c("immig", "ageg", "comp")]

# convert to percentage of total labor change
tLab <- fa[immig == "Total", sum(value)]
fa <- fa[, value := 100 * value / tLab]

ggplot(fa[value != 0, ], mapping = aes(
  y = value, x = comp, group = ageg,
  colour = ageg, fill = ageg, linetype = ageg)) + 
mygthemep + theme(
    panel.spacing.x = unit(2, "lines"),
    aspect.ratio = 1.5 / 1,
  ) +
  geom_col(na.rm = TRUE, color = "black", alpha = 0.7) +
  geom_hline(yintercept = 0, size = 0.3, color = "gray80") +
  labs(x = NULL, y = "% contributed") +
  facet_grid(cols = vars(immig))+
  scale_colour_manual(values = mygcolor[1:3])



#################################################################
## Test support
#################################################################

# Net effect of immigration vs population aging vs behavoir

eMatL



#####################
#  Net effects
#####################

# Total change in aggregate labour supply implied by the decomposition
tLab <- eMatL[, sum(value)]

# Net effect by birthplace status: Foreign captures the immigration contribution,
# while Spanish captures the contribution of the Spanish-born population.
netNation <- eMatL[
  ,
  .(value = sum(value)),
  by = .(effect = nation)
]
netNation <- rbind(
  netNation,
  data.table(effect = "Total", value = tLab)
)
netNation[, type := "Birthplace status"]

# Net effect by channel: age structure and behaviour.
netChannel <- eMatL[
  ,
  .(
    value = sum(value),
    comp = paste(unique(as.character(comp)), collapse = " + ")
  ),
  by = .(
    effect = fcase(
      comp == "stx", "Age structure",
      comp %in% c("wpx", "whx"), "Behaviour",
      comp == "tpop", "Population size"
    )
  )
]
netChannel[, type := "Component"]

# Combine and express each net effect as a percentage of total labour-supply change.
netEffect <- rbindlist(
  list(
    netNation[, .(type, effect, comp = NA_character_, value)],
    netChannel[, .(type, effect, comp, value)]
  )
)
netEffect[, pct_total_change := 100 * value / tLab]
netEffect[, `:=`(
  value = round(value, 2),
  pct_total_change = round(pct_total_change, 2)
)]

netEffect
fwrite(netEffect, file.path("resrc/netEffect.csv"))

# extract and sum over years
fa <- eMatL[, .(value = sum(value)), by = c("nation", "ageg", "comp")]
# add total nation
fa <- rbind(
  fa,
  cbind(fa[, .(value = sum(value)), by = c("ageg", "comp")],
    nation = factor("Total")
  )
)
# convert to percentage of total labor change
tLab <- fa[nation == "Total", sum(value)]
fa <- fa[, value := 100 * value / tLab]
fa[, nation := factor(nation, levels = c("Spanish", "Foreign", "Total"))]
