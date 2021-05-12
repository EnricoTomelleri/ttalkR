


NTC1 <- Tref_1C
NTC1ref <- 29

if (site != "Mendola"){
  #calibration for spruce
  m <- -5E-6
  b <- 0.2
}else{
  #calibration for beech
  m <- -4E-5
  b <- 0.6
}
ECf_Tref <- mydata_4D$StWC-7.3*(NTC1-NTC1ref)
StWC <- m*ECf_Tref+b
df <- data.frame(HR_Timestamp_4D, StWC, id_col)
p <- ggplot(data=df, aes(HR_Timestamp_4D, StWC))
p + geom_point(aes(colour = id_col), size = 0.2)  +
  scale_color_gradientn(colours = hcl.colors(21, palette = "viridis")) +
  labs(x = "Timestamp", y = "StWC (g/cm3)") +
  labs(title = site) +
  scale_x_datetime(minor_breaks=("1 week")) +
  theme(legend.position = "none")
#plot(StWC)
#save the plot
ggsave(paste("../Figures/", site, "_StWC.png", sep=""),
       plot = last_plot(),
       width = 10,
       height = 7,
       units = c("in"),
       dpi = 300)
