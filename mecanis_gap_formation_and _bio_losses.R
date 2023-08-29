library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(car)

##  GAP FORMATION MECHANISMS AND BIOMASS 

#### Load data
setwd("")
gap_bio <- read.table("../entry/GAPS_modeofgap_bio.csv",header=T, sep=",")

modeofgap <- as.factor(gap_bio$Mode_of_gap)

bio <- gap_bio$Biomass_ton_Marra2016
shapiro.test(bio) 


bio_log <- log(bio)
shapiro.test(bio_log)

par(mfrow=c(1,2))
hist(bio)
hist(bio_log)

testeLevene <- leveneTest(
  y = bio, 
  group = modeofgap_bio
)

testeLevene #It can be concluded that there is no evidence to reject H0,
#the groups have the same variance.

aov_modedead_biomass <- aov(bio_log ~ modeofgap, data = gap_bio)
aov_modedead_biomass

summary(aov_modedead_biomass)

TukeyHSD(aov_modedead_biomass)


plot2_bio <- ggplot(data = gap_bio,aes(x = modeofgap_bio, y = bio, fill = modeofgap_bio))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  theme_minimal()+
  ylab(  c("Biomass loss (Mg)")  )  +
  xlab(  c("Mechanisms of gap formation") )  +
  rremove("legend.title")+
  theme(legend.position = "none")+
  ylim(0,6) +
  scale_x_discrete(breaks = c("1","2","3","4"),
                   labels = c("Branch fall", "Standing dead", "Uprooting", "Snapping"))+
  annotate("text", x = 0.55, y = 6, label = "(b)", angle = 0, size = 8, fontface = "bold") +
  annotate("text", x = 1.1, y = 4.5, label = "*p-value = 0.018991", angle = 0, color = "red", size = 6) +
  annotate("text", x = 1, y = 3.5, label = "*", angle = 0, color = "red", size = 7) +
  annotate("text", x = 4, y = 6, label = "*", angle = 0, color = "red", size = 7) +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black", face="bold"),
    axis.ticks.length=unit(0.2,"cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.position = c(0.01, 0.85))+
  font("xylab",size=20)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = "none", color = "none") 


## GAP FORMATION MECHANISMS AND AREA 

area <- gap_bio$Area_SR_m2
shapiro.test(area) 

testeLevene <- leveneTest(
  y = area, 
  group = modeofgap_area
)

testeLevene #It can be concluded that there is no evidence to reject H0,
#the groups have the same variance.

log_area <- log(area)
shapiro.test(log_area) 

aov_modedead_area <- aov(log_area ~ modeofgap, data = gap_bio)
aov_modedead_area

summary(aov_modedead_area)

TukeyHSD(aov_modedead_area)

library(ggbeeswarm)
plot2_area<- ggplot(data = gap_area,aes(x = modeofgap_area, y = area, fill = modeofgap_area))+
  scale_fill_viridis_d( option = "D")+
  geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA) +
  # geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+
  ggbeeswarm::geom_quasirandom(shape = 21,size=2, dodge.width = .75, color="black",alpha=.5,show.legend = F)+
  scale_x_discrete(breaks = c("1","2","3","4"),
                   labels = c("Branch fall", "Standing dead", "Uprooting", "Snapping"))+
  theme_minimal()+
  ylab(  c("Gap area (m²)")  )  +
  xlab(  c("Mechanisms of gap formation")  )  +
  rremove("legend.title")+
  annotate("text", x = 0.55, y = 850, label = "(a)", angle = 0, size = 8, fontface = "bold") +
  annotate("text", x = 1.1, y = 650, label = "*p-value = 0.179", angle = 0, size = 6) +
  theme(#panel.border = element_rect(colour = "black", fill=NA, size=2),
    axis.line = element_line(colour = "black",size=1),
    axis.ticks = element_line(size=1,color="black"),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black", face="bold"),
    axis.ticks.length=unit(0.2,"cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    legend.position = c(0.95, 0.85))+
  font("xylab",size=20)+  
  font("xy",size=15)+ 
  font("xy.text", size = 15) +  
  font("legend.text",size = 15)+
  guides(fill = "none", color = "none")



