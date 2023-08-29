library(tidyverse)
library(dplyr)

demdif_3 <- read.csv("C:/...", header = T, sep = ",")


result_demdif_3 <- demdif_3 %>%
  group_by(ID) %>%
  summarize(
    desv_pad = sd(DEM_DIFF),
    mean_dem_dif = mean(DEM_DIFF),
    max_dem_dif = max(DEM_DIFF),
    min_dem_dif = min(DEM_DIFF),
  ) %>%
  mutate(
    heightloss_mean_abs = abs(mean_dem_dif),
    min_abs_dem_dif = abs(max_dem_dif),
    max_abs_dem_dif = abs(min_dem_dif),    
    dif_abs_max_min = abs(max_dem_dif) - abs(min_dem_dif)
  )

dat2 <- dat %>%
  left_join(dem_dif_allgaps_clean, by = "ID")

plot(area_SR,heightloss_mean_abs, log = "x",
     ylab = "Height loss (m)",
     xlab = "Gap area (m?)",
     las = 1, cex.axis = 1.5, tcl = -0.2,
     cex.lab=1.5, cex.axis = 1.3,
     xlim = c(10, 800), ylim = c(0, 20),
     pch = 16, col = rgb(0.2,0.3,0.5,0.7), cex = 2)
reg=lm(heightloss_mean_abs~area_SR)
abline(reg, untf=T,col="black", lwd = 2, lty = 2)
minor.tick(nx = 10, ny = 2, tick.ratio = 0.5)
text(30, 16, labels = "r= 0.64; p= 0.0003", font = 2, cex = 1.5)