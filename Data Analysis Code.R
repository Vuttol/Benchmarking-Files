library(tidyverse)
library(ggpubr)
library(agricolae)
library(Rmisc)
library(readxl)
ds <- read_excel("C:/Users/s213356/OneDrive - Cranfield University/Documents/R practice/Benchmarking_dataset.xlsx")
View(ds)

#Comparison of Irrigation application amounts by estate and irrigation type

TT1 <- aov(Irrigation_mm~Irrigation * Estate, data = ds)
anova(TT1)
Compare <-HSD.test(TT1,c("Irrigation"),console = TRUE)
Compare <-HSD.test(TT1,c("Estate"),console = TRUE)
Compare <-HSD.test(TT1,c("Irrigation","Estate"),console = TRUE)
SD1<-summarySE(measurevar = "Irrigation_mm",groupvars=c("Irrigation","Estate"),data = ds)
SD1
ds %>% 
  drop_na(Irrigation_mm) %>%
  ggplot(aes(Irrigation, Irrigation_mm, fill=Irrigation))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Irrigation (mm)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,2500),
                     breaks = seq(0,2500, by=500))+
  facet_grid(~Estate)+
  geom_text(data = SD1, aes(x=Irrigation, y=2250, label=c("a","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=750, label=c("","g","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1800, label=c("","","d","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=2150, label=c("","","","b","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1250, label=c("","","","","ef","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=2000, label=c("","","","","","c","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=900, label=c("","","","","","","g","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1600, label=c("","","","","","","","f","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=2275, label=c("","","","","","","","","b","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1250, label=c("","","","","","","","","","f","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1360, label=c("","","","","","","","","","","g","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1380, label=c("","","","","","","","","","","","g","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=2275, label=c("","","","","","","","","","","","","a","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Irrigation, y=1250, label=c("","","","","","","","","","","","","","e")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_2.png",width = 4.2,height = 3,dpi = 1000)

ds %>% 
  drop_na(Irrigation_mm) %>%
  ggplot(aes(Estate, Irrigation_mm, fill=Estate))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Irrigation (mm)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,2500),
                     breaks = seq(0,2500, by=500))+
  facet_grid(~Irrigation)+
  geom_text(data = SD1, aes(x=Estate, y=2250, label=c("a","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=750, label=c("","g","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1800, label=c("","","d","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=2150, label=c("","","","b","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1250, label=c("","","","","ef","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=2000, label=c("","","","","","c","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=900, label=c("","","","","","","g","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1600, label=c("","","","","","","","f","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=2275, label=c("","","","","","","","","b","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1250, label=c("","","","","","","","","","f","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1360, label=c("","","","","","","","","","","g","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1380, label=c("","","","","","","","","","","","g","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=2275, label=c("","","","","","","","","","","","","a","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD1, aes(x=Estate, y=1250, label=c("","","","","","","","","","","","","","e")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_3.png",width = 4.2,height = 3,dpi = 1000)

TT0 <- aov(Irrigation_mm~Estate, data = ds)
anova(TT0)
Compare <-HSD.test(TT0,c("Estate"),console = TRUE)
SD0<-summarySE(measurevar = "Irrigation_mm",groupvars=c("Estate"),data = ds)
SD0
ds %>% 
  drop_na(Irrigation_mm) %>%
  ggplot(aes(Estate, Irrigation_mm))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Irrigation (mm)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(500,2050),
                     breaks = seq(500,2050, by=500))+
  geom_text(data = SD0, aes(x=Estate, y=2000, label=c("b","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD0, aes(x=Estate, y=1230, label=c("","e","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD0, aes(x=Estate, y=1520, label=c("","","d","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD0, aes(x=Estate, y=2050, label=c("","","","a","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD0, aes(x=Estate, y=1220, label=c("","","","","c")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Irrigation0.png",width = 4.2,height = 3,dpi = 1000)

#Comparison of Cane yields (t/a) by estate and irrigation type
TT2 <- aov(TCH~Irrigation * Estate, data = ds)
anova(TT2)
Compare <-HSD.test(TT2,c("Irrigation"),console = TRUE)
Compare <-HSD.test(TT2,c("Estate"),console = TRUE)
Compare <-HSD.test(TT2,c("Irrigation","Estate"),console = TRUE)
SD2<-summarySE(measurevar = "TCH",groupvars=c("Irrigation","Estate"),data = ds)
SD2
ds %>% 
  drop_na(TCH) %>%
  ggplot(aes(Estate, TCH, fill=Estate))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Cane yield (t/ha)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,200),
                     breaks = seq(0,200, by=50))+
  facet_grid(~Irrigation)+
  geom_text(data = SD2, aes(x=Estate, y=155, label=c("ab","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=145, label=c("","ef","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=180, label=c("","","b","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=177, label=c("","","","ab","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=155, label=c("","","","","c","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=170, label=c("","","","","","b","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=143, label=c("","","","","","","f","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=180, label=c("","","","","","","","a","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=175, label=c("","","","","","","","","d","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=155, label=c("","","","","","","","","","e","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=150, label=c("","","","","","","","","","","f","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=150, label=c("","","","","","","","","","","","f","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=150, label=c("","","","","","","","","","","","","f","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD2, aes(x=Estate, y=145, label=c("","","","","","","","","","","","","","f")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_6.png",width = 4.2,height = 3,dpi = 1000)

TT3 <- aov(TCH~ Estate, data = ds)
anova(TT3)
Compare <-HSD.test(TT3,c("Irrigation"),console = TRUE)
Compare <-HSD.test(TT3,c("Estate"),console = TRUE)
Compare <-HSD.test(TT3,c("Irrigation","Estate"),console = TRUE)
SD3<-summarySE(measurevar = "TCH",groupvars=c("Estate","Irrigation"),data = ds)
SD3

ds %>% 
  drop_na(TCH) %>%
  ggplot(aes(Irrigation, TCH, fill=Irrigation))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Cane yield (t/ha)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,200),
                     breaks = seq(0,200, by=50))+
  facet_grid(~Estate)+
  geom_text(data = SD3, aes(x=Irrigation, y=155, label=c("ab","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=167, label=c("","b","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=145, label=c("","","ef","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=143, label=c("","","","f","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=148, label=c("","","","","f","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=180, label=c("","","","","","b","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=180, label=c("","","","","","","a","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=150, label=c("","","","","","","","f","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=175, label=c("","","","","","","","","ab","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=175, label=c("","","","","","","","","","d","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=150, label=c("","","","","","","","","","","f","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=155, label=c("","","","","","","","","","","","c","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=155, label=c("","","","","","","","","","","","","e","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD3, aes(x=Irrigation, y=145, label=c("","","","","","","","","","","","","","f")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_5.png",width = 4.2,height = 3,dpi = 1000)

ds %>% 
  drop_na(TCH) %>%
  ggplot(aes(Estate, TCH))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Cane yield (t/ha)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,250),
                     breaks = seq(0,250, by=50))+
  geom_text(data = SD, aes(x=Estate, y=170, label=c("a","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=150, label=c("","d","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=185, label=c("","","b","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=170, label=c("","","","c","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=155, label=c("","","","","c")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=3))
ggsave("Figure_4.png",width = 4.2,height = 3,dpi = 1000)


#Comparison of Water productivity between estates and systems
TT4 <- aov(`WP`~Irrigation * Estate, data = ds)
anova(TT4)
Compare <-HSD.test(TT4,c("Irrigation"),console = TRUE)
Compare <-HSD.test(TT4,c("Estate"),console = TRUE)
Compare <-HSD.test(TT4,c("Irrigation","Estate"),console = TRUE)
SD4<-summarySE(measurevar = "WP",groupvars=c("Estate","Irrigation"),data = ds)
SD4
ds %>% 
  drop_na(`WP`) %>%
  ggplot(aes(Irrigation, `WP`, fill=Irrigation))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Water productivity (kg/m3)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20, by=4))+
  facet_grid(~Estate)+
  geom_text(data = SD4, aes(x=Irrigation, y=6.5, label=c("g","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=8.8, label=c("","fg","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=7.5, label=c("","","fg","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=9.5, label=c("","","","ef","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=9.5, label=c("","","","","fg","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=10.5, label=c("","","","","","d","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=14.2, label=c("","","","","","","a","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=10, label=c("","","","","","","","f","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=10.5, label=c("","","","","","","","","e","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=9.5, label=c("","","","","","","","","","f","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=8, label=c("","","","","","","","","","","g","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=11.7, label=c("","","","","","","","","","","","b","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=11.5, label=c("","","","","","","","","","","","","c","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD4, aes(x=Irrigation, y=11.3, label=c("","","","","","","","","","","","","","d")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_8.png",width = 4.2,height = 3,dpi = 1000)

TT5 <- aov(`WP`~Irrigation * Estate, data = ds)
anova(TT5)
Compare <-HSD.test(TT5,c("Irrigation"),console = TRUE)
Compare <-HSD.test(TT5,c("Estate"),console = TRUE)
Compare <-HSD.test(TT5,c("Irrigation","Estate"),console = TRUE)
SD5<-summarySE(measurevar = "WP",groupvars=c("Estate","Irrigation"),data = ds)
SD5
ds %>% 
  drop_na(`WP`) %>%
  ggplot(aes(Estate, `WP`, fill=Estate))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Water productivity (kg/m3)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20, by=4))+
  facet_grid(~Irrigation)+
  geom_text(data = SD5, aes(x=Estate, y=6.5, label=c("g","","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=8.8, label=c("","c","","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=7.5, label=c("","","fg","","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=9.5, label=c("","","","c","","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=9.5, label=c("","","","","fg","","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=10.5, label=c("","","","","","d","","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=14.2, label=c("","","","","","","a","","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=10, label=c("","","","","","","","f","","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=10.5, label=c("","","","","","","","","e","","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=9.5, label=c("","","","","","","","","","c","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=8, label=c("","","","","","","","","","","g","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=11.7, label=c("","","","","","","","","","","","b","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=11.5, label=c("","","","","","","","","","","","","c","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=11.3, label=c("","","","","","","","","","","","","","d")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=2000, label=c("","","","","","","","","","","","","Estate***","")),hjust=3,vjust=2, angle=0,size=1.5, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=1850, label=c("","","","","","","","","","","","","System***","")),hjust=2.7,vjust=1, angle=0,size=1.5, family="aerial")+
  geom_text(data = SD5, aes(x=Estate, y=1750, label=c("","","","","","","","","","","","","Estate x System***","")),hjust=1.7,vjust=1, angle=0,size=1.5, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_9.png",width = 4.2,height = 3,dpi = 1000)

ds %>% 
  drop_na(WP) %>%
  ggplot(aes(Estate, WP))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Water productivity (kg/m3)')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20, by=5))+
  geom_text(data = SD, aes(x=Estate, y=8.6, label=c("c","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=9.5, label=c("","c","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=14.3, label=c("","","a","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=9.3, label=c("","","","c","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD, aes(x=Estate, y=11.7, label=c("","","","","b")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=3))
ggsave("Figure_7.png",width = 4.2,height = 3,dpi = 1000)

#Analysis of relative water and irrigation supply across estates

ds3 <- read_excel("C:/Users/s213356/OneDrive - Cranfield University/Documents/R practice/Water supply.xlsx")
threshold1 <- 1.0
View(ds3)
TT8 <- aov(RWS~Estate, data = ds3)
anova(TT8)
Compare <-HSD.test(TT8,c("Estate"),console = TRUE)
SD8<-summarySE(measurevar = "RWS",groupvars=c("Estate"),data = ds3)
SD8

ds3 %>% 
  drop_na(RWS) %>%
  drop_na(Estate) %>%
  ggplot(aes(x=Estate, y=RWS))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), linewidth = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), linewidth = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Relative Water Supply')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0,2),
                     breaks = seq(0.5,2, by=0.5))+
  geom_text(data = SD8, aes(x=Estate, y=1.57, label=c("a","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD8, aes(x=Estate, y=1.13, label=c("","d","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD8, aes(x=Estate, y=0.92, label=c("","","cd","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD8, aes(x=Estate, y=1.6, label=c("","","","b","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD8, aes(x=Estate, y=1.25, label=c("","","","","bc")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6),axis.title.x = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=3))
ggsave("Figure_10.png",width = 4.2,height = 3,dpi = 1000)

TT9 <- aov(RIS~Estate, data = ds3)
anova(TT9)
Compare <-HSD.test(TT9,c("Estate"),console = TRUE)
SD9<-summarySE(measurevar = "RIS",groupvars=c("Estate"),data = ds3)
SD9

ds3 %>% 
  drop_na(RIS) %>%
  drop_na(Estate) %>%
  ggplot(aes(x=Estate, y=RIS))+ 
  stat_boxplot(geom = 'errorbar', position = position_dodge(0.9), size = 0.15, width = 0.3)+
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.9), size = 0.01, width = 0.6) +
  labs(main= '', x = '', y='Relative Irrigation Supply')+
  guides(fill=guide_legend(nrow = 1))+
  stat_summary(fun=mean,geom='point', size=0.01, shape=20, colour='black', position=position_dodge(0.9))+
  scale_y_continuous(limits = c(0.5,2),
                     breaks = seq(0.5,2, by=0.5))+
  geom_text(data = SD9, aes(x=Estate, y=1.75, label=c("a","","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD9, aes(x=Estate, y=1.57, label=c("","d","","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD9, aes(x=Estate, y=0.82, label=c("","","cd","","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD9, aes(x=Estate, y=1.86, label=c("","","","b","")),vjust=0.3, angle=0,size=2, family="aerial")+
  geom_text(data = SD9, aes(x=Estate, y=1.52, label=c("","","","","bc")),vjust=0.3, angle=0,size=2, family="aerial")+
  theme(axis.title.y = element_text(size = 6),axis.title.x = element_text(size = 6), axis.ticks = element_line(size = 0.1), axis.text.y = element_text(size = 5), axis.text.x = element_text(angle = 90, vjust = 0.2, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1),
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=3))
ggsave("Figure_12.png",width = 4.2,height = 3,dpi = 1000)

ds3%>%
  drop_na(RWS, Season)%>%
  ggplot(aes(Season, RWS))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_hline(yintercept = threshold1, linetype = "dashed", linewidth=0.4, colour = "blue")+
  labs(main= '', x = '', y='Relative Water Supply')+
  facet_grid(~Estate)+
  #guides(fill=guide_legend(nrow = 1))+
  scale_x_discrete(limits = c(2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                   breaks = seq(2009,2019, by=2))+
  scale_y_continuous(limits = c(0,2),
                     breaks = seq(0,2, by=.5))+
  theme(axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6), axis.text.y = element_text(size = 5), axis.ticks = element_line(size = 0.1), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1), 
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_11.png",width = 4.2,height = 3,dpi = 1000)

ds3%>%
  drop_na(RIS, Season)%>%
  ggplot(aes(Season, RIS))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_hline(yintercept = threshold1, linetype = "dashed", linewidth=0.4, colour = "blue")+
  labs(main= '', x = '', y='Relative Irrigation Supply')+
  facet_grid(~Estate)+
  #guides(fill=guide_legend(nrow = 1))+
  scale_x_discrete(limits = c(2009,2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                   breaks = seq(2009,2019, by=2))+
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(0,4, by=.5))+
  theme(axis.title.x = element_text(size = 6), axis.title.y = element_text(size = 6), axis.text.y = element_text(size = 5), axis.ticks = element_line(size = 0.1), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 5),
        panel.background = element_blank(), panel.border = element_rect(fill = NA, size = 0.1), 
        strip.background = element_blank(), strip.text = element_text(size = 5), legend.key.size = unit(0.2, "cm"),
        legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size=5))
ggsave("Figure_13.png",width = 4.2,height = 3,dpi = 1000)
