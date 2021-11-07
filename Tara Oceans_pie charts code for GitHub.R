#read the source data file "mixotroph_Tara_OTUs_surface"
otus <- read.csv("")
library(ggplot2)
library(maps)
library(ggthemes)
library(scales)
library(ggforce)
library(scatterpie)

#reorder isolate level
otus$isolate_f = factor(otus$isolate, levels=c("Florenciella parvula","Florenciellales_X sp.","Rhizochromulina marina","Dictyochales_X sp.","Bolidomonas mediterranea","Clade-H_X sp.","Chrysochromulina_X sp.","Chrysochromulina","Prymnesiophyceae_XXX sp.","Chlorarachnida_X","summed OTUs"))

otus.wide = pivot_wider(otus, names_from = isolate, values_from = abundance, id_cols = c(lat, lon, station))
otus.wide$dicty.sum = apply(otus.wide[,c("Florenciella parvula", "Florenciellales_X sp.","Rhizochromulina marina", "Dictyochales_X sp.")], 1, sum)
otus.wide$hapt.sum = apply(otus.wide[,c("Chrysochromulina_X sp.", "Chrysochromulina","Prymnesiophyceae_XXX sp.")], 1, sum)
otus.wide$other.sum = apply(otus.wide[,c("Bolidomonas mediterranea", "Clade-H_X sp.","Chlorarachnida_X")], 1, sum)

otus.med = subset(otus.wide, lon > -5 & lon < 40 & lat > 30 & lat < 50)
otus.notmed = subset(otus.wide, !(lon > -5 & lon < 40 & lat > 30 & lat < 50))
otus.med.ave = rbind(otus.notmed, otus.med[1,])
otus.med.ave = as.data.frame(otus.med.ave)
otus.med.ave$station = as.character(otus.med.ave$station)
otus.med.ave[nrow(otus.med.ave),1:2] = apply(otus.med[,1:2], 2, mean)
otus.med.ave[nrow(otus.med.ave),4:16] = apply(otus.med[,4:16], 2, mean)
otus.med.ave$station[nrow(otus.med.ave)] = 'Med'
otus.med.ave$station = as.factor(otus.med.ave$station)

datause = otus.med.ave
legx = 100
legy = -50

scaling = 4
world <- ggplot() +
  borders("world", colour = FALSE, fill = "gray80") +
  theme_map() + coord_quickmap(xlim = c(-150,120), ylim = c(-65,50))


#plotting panel a
map = world +
  geom_scatterpie(aes(x=lon, y=lat, group = station, r= sqrt(dicty.sum/pi)*scaling),
                           data = otus.med.ave, cols = c("Florenciella parvula", "Florenciellales_X sp.","Rhizochromulina marina", "Dictyochales_X sp."),alpha = 0.8,lwd=0.1) +
  scale_fill_manual(values=c("#00FF00", "#CCFF33", "#006633","#00FFFF")) +
  geom_scatterpie_legend(sqrt(c(1,10,30)/pi)*scaling, x=legx, y=legy, labeller = function(x) pi*((x/scaling)^2))

map + theme (legend.position = c(0.79,0.75), legend.key.size = unit(0.3, 'cm'), legend.text = element_text(size = 6.5), legend.title = element_text(size=0))

#plotting panel b
map = world +
  geom_scatterpie(aes(x=lon, y=lat, group = station, r= sqrt(hapt.sum/pi)*scaling),
                  data = otus.med.ave, cols = c("Chrysochromulina_X sp.", "Chrysochromulina","Prymnesiophyceae_XXX sp."),alpha = 0.8,lwd=0.1) +
  scale_fill_manual(values=c("#FFCC00", "#FF9900", "#993300")) +
  geom_scatterpie_legend(sqrt(c(1,10,30)/pi)*scaling, x=legx, y=legy, labeller = function(x) pi*((x/scaling)^2)) 

map + theme (legend.position = c(0.75,0.8), legend.key.size = unit(0.3, 'cm'), legend.text = element_text(size = 6.5), legend.title = element_text(size=0))

#plotting panel c
map = world +
  geom_scatterpie(aes(x=lon, y=lat, group = station, r= sqrt(other.sum/pi)*scaling),
                  data = otus.med.ave, cols = c("Bolidomonas mediterranea", "Clade-H_X sp.","Chlorarachnida_X"),alpha = 0.8,lwd=0.1) +
  scale_fill_manual(values=c("#0000FF", "#FF3333", "#9933FF")) +
  geom_scatterpie_legend(sqrt(c(1,10,30)/pi)*scaling, x=legx, y=legy, labeller = function(x) pi*((x/scaling)^2)) 

map + theme (legend.position = c(0.75,0.8), legend.key.size = unit(0.3, 'cm'), legend.text = element_text(size = 6.5), legend.title = element_text(size=0))

#plotting panel d
ggplot(otus, aes(x = isolate_f, y = abundance, fill = isolate_f)) + geom_boxplot(lwd=0.3) + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.y=element_text(colour="black",size = 8), axis.text.x = element_text(colour="black",size=8,angle=45,hjust=1,vjust=1), axis.title.y = element_text(colour="black",size=8,angle=90,face="plain")) + xlab('') + ylab("relative abundance (%)") + scale_y_continuous(breaks = c(0, 1, 5, 10, 15, 35), trans = 'sqrt') +
  theme(legend.position = "none") + scale_fill_manual (breaks = c("Florenciella parvula", "Florenciellales_X sp.","Rhizochromulina marina", "Dictyochales_X sp.", "Bolidomonas mediterranea", "Clade-H_X sp.", "Chrysochromulina_X sp.", "Chrysochromulina", "Prymnesiophyceae_XXX sp.", "Chlorarachnida_X", "summed OTUs"), values=c("#00FF00", "#CCFF33", "#006633","#00FFFF", "#0000FF", "#FF3333","#FFCC00", "#FF9900","#993300", "#9933FF", "#FFFF00"))

