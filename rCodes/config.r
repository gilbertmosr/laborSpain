###################################################################
# GGPlot settings
# used aluf and ueCanada
###################################################################
mygthemep <- theme_bw() +  theme(
    legend.position = "top", 
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(linetype = "dotted"),
    panel.border = element_rect(colour = NA, fill = NA),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0),
    strip.text = element_text(size = 8)
    
  )
mygcolor <- c(`young`= "green", `adult`= "blue" , `older`= "yellow" )
mygshape <- c(15, 16, 17,18)


