library(readxl);library(ggplot2);library(dplyr);library(FSA);library(rcompanion);library(multcompView);library(knitr)

setwd("")
dd3_120<- read_excel("TB-D-22-00057_Research-data_Resubmitted_IVE-LMK-MOB_11-12-22.xlsx", sheet = 'Dataset')

dd3_120$POLYGON<- factor(dd3_120$POLYGON)
dd3_120$YEAR<- factor(dd3_120$YEAR)
dd3_120$CM<- factor(dd3_120$CM)
  str(dd3_120)

################################################################################
# VEGETATION COVER COMPARISONS AMONG POLYGONS AND BETWEEN YEARS #####################

# Data
kruskal_data<- read_excel("TB-D-22-00057_Research-data_Resubmitted_IVE-LMK-MOB_11-12-22.xlsx", sheet = 'NDVI.within.years')
# Analysis
wilcox.test(data=kruskal_data, NDVI~YEAR)
kruskal.test(data=kruskal_data, NDVI~POLYGON)
kruskal.test(data=kruskal_data, NDVI~TREATMENT)

# Post-hoc
dunnTest(data=kruskal_data, NDVI~factor(TREATMENT), method="bonferroni")

# Effect size
multiVDA(x= kruskal_data$NDVI, g=kruskal_data$TREATMENT)

# DIFFERENCE OF THE VEGETATION COVER BETWEEN YEARS AMONG POLYGONS ####################
# Data
dif.between.years<- read_excel("TB-D-22-00057_Research-data_Resubmitted_IVE-LMK-MOB_11-12-22.xlsx", sheet = 'NDVI.between.years')
# Analysis
aa<-aov(data=dif.between.years, NDVI_DIFFERENCE~POLYGON)

# Post-hoc
bb<-emmeans(aa, spec= pairwise ~ POLYGON, adjust='bonf')

# Effect size
kable(eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method='pairwise'))

# Plot of Figure 2 #############################################################
dd3_120$POLYGON = factor(dd3_120$POLYGON, levels=c('NAA','SAA','REBIOSH'))
ggplot(data= subset(dd3_120, CM==3 & SEASON=='RAINY'), aes(as.factor(YEAR), NDVI, fill=POLYGON))+ 
  geom_boxplot(color="black", size=0.8)+
  theme_classic()+
  labs(fill="Polygon")+
  scale_fill_manual(values= c("orange3","gray","green4"), labels= c("NAA","SAA","REBIOSH"))+
  ylab("Vegetation cover (%)")+ xlab("Year")+
  theme(axis.text = element_text(face = "bold", size=11, color = "black"),
        axis.title = element_text(face = "bold", size=14, color = "black"),
        legend.text = element_text(face = "bold", size=11, color = "black"),
        legend.title = element_text(face = "bold", size=14, color = "black"),
        axis.line = element_line(color="black",size=1))

################################################################################
# MICROCLIMATE TEMPERATURE

# 3 cm perch height ############################################################

# Analysis
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Microclimate.temperature_dCelcius~POLYGON*YEAR)
anova(aa)
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Microclimate.temperature_dCelcius~POLYGON+YEAR)

# Post-hoc
bb<-emmeans(aa, specs = pairwise ~ YEAR, type="response", adjust="tukey")
bb<- emmeans(aa, specs = pairwise ~ POLYGON, type="response", adjust="tukey")

# Effect size
eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method = 'pairwise')

# 120 cm perch height ##########################################################

# Analysis
aa<- aov(data=subset(dd3_120, CM==120 & SEASON=="DRY"), Microclimate.temperature_dCelcius~POLYGON*YEAR)
anova(aa)
aa<- aov(data=subset(dd3_120, CM==120 & SEASON=="DRY"), Microclimate.temperature_dCelcius~POLYGON+YEAR)

# Post-hoc
bb<-emmeans(aa, specs = pairwise ~ POLYGON, type="response", adjust="tukey")

# Effect size
eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method = 'pairwise')

# Plot of Figure 3 #############################################################
subset(dd3_120, SEASON=="DRY")%>%
  mutate(POLYGON= fct_relevel(POLYGON, c("NAA", "SAA", "REBIOSH")))%>%
  ggplot(aes(factor(YEAR),Microclimate.temperature_dCelcius,fill=POLYGON))+ 
  geom_boxplot(color="black", size=0.8)+
  facet_wrap(~CM)+
  labs(fill="Polygon")+
  scale_fill_manual(values= c("orange3","gray","green4"), labels= c("NAA","SAA","REBIOSH"))+
  theme_classic()+
  scale_y_continuous(limits=c(18,40),breaks = seq(18, 40, by = 4))+
  ylab("Microclimate temperature (°C)")+ xlab("Year")+
  annotate("rect", xmin=0.0,xmax=3,ymin=28.8,ymax=36.4,fill='black', alpha=0.3)+
  theme(strip.text = element_text(face="bold", size=11, color="black"),
    strip.background = element_rect(color="black"),
    axis.text = element_text(face = "bold", size=11, color = "black"),
    axis.title = element_text(face = "bold", size=14, color = "black"),
    legend.text = element_text(face = "bold", size=11, color = "black"),
    legend.title = element_text(face = "bold", size=14, color = "black"),
    axis.line = element_line(color="black",size=1),
    strip.text.x = element_text(size = 11, color = "black", face = "bold"))

################################################################################

# THERMAL SAFETY MARGIN 9-16h ##################################################

# Analysis
aa<- lm(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Thermal.safety.margin_dCelcius_9h.16h~POLYGON*YEAR)
anova(aa)
aa<- lm(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Thermal.safety.margin_dCelcius_9h.16h~POLYGON+YEAR)

# Post-hoc
bb<- emmeans(aa, specs= pairwise ~ YEAR, adjust='tukey')
bb<- emmeans(aa, specs= pairwise ~ POLYGON, adjust='tukey')

# Effect size
eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method='pairwise')

# Plot of Figure 4A
subset(dd3_120, SEASON=="DRY" & CM==3)%>%
  mutate(POLYGON= fct_relevel(POLYGON, c("NAA", "SAA", "REBIOSH")))%>%
  ggplot(aes(as.factor(YEAR),Thermal.safety.margin_dCelcius_9h.16h,fill=POLYGON))+ 
  geom_boxplot(color="black", size=0.8)+
  # facet_wrap(~CM)+
  labs(fill="Polygon")+
  scale_fill_manual(values= c("orange3","gray","green4"), labels= c("NAA","SAA","REBIOSH"))+
  theme_classic()+
  scale_y_continuous(limits=c(0,4),breaks = seq(0, 4, by = 1))+
  ylab("VTmax - Tb (°C; 9h – 16h)")+ xlab("Year")+#ylim(c(21,27))+
  # annotate("segment", x=0.8,xend=1.2,y=27.8,yend=27.8,size=1.5)+
  # annotate("segment", x=1.8,xend=2.2,y=27.8,yend=27.8,size=1.5)+
  # annotate("text", x=1,y=27.5,label="***",size=8)+
  # annotate("text", x=2,y=27.5,label="**",size=8)+
  theme(strip.text = element_text(face="bold", size=11, color="black"),
        strip.background = element_rect(color="black"),
        axis.text = element_text(face = "bold", size=11, color = "black"),
        axis.title = element_text(face = "bold", size=14, color = "black"),
        legend.text = element_text(face = "bold", size=11, color = "black"),
        legend.title = element_text(face = "bold", size=14, color = "black"),
        axis.line = element_line(color="black",size=1),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"))

# THERMAL SAFETY MARGIN 7-19h ##################################################

# Analysis
aa<- lm(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Thermal.safety.margin_dCelcius_7h.19h~POLYGON*YEAR)
anova(aa)
aa<- lm(data=subset(dd3_120, CM==3 & SEASON=="DRY"), Thermal.safety.margin_dCelcius_7h.19h~POLYGON+YEAR)

# Post-hoc
bb<- emmeans(aa, specs= pairwise ~ YEAR, adjust='tukey')
bb<- emmeans(aa, specs= pairwise ~ POLYGON, adjust='tukey')

# Effect size
eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method='pairwise')

################################################################################
# THERMAL SAFETY MARGIN~ NDVI RELATIONSHIP AMONG POLYGONS 

# With the outlier of thermal safety margin > 3.5 ##############################

# Analysis
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY' & YEAR==2020),
         Thermal.safety.margin_dCelcius_9h.16h~POLYGON*NDVI)
summary(aa)

# Post-hoc
bb<-emtrends(aa, spec= pairwise ~ POLYGON, var='NDVI', adjust='hochberg')

# Effect size
eff_size(bb, sigma = sigma(aa), edf= df.residual(aa), method='pairwise')

# Without the outlier of thermal safety margin > 3.5 ###########################

# Analysis
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY' & YEAR==2020 & Thermal.safety.margin_dCelcius_9h.16h<3.5),
         Thermal.safety.margin_dCelcius_9h.16h~POLYGON*NDVI)
summary(aa)

# Post-hoc
bb<-emtrends(aa, spec= pairwise ~ POLYGON, var='NDVI', adjust='hochberg')

# Effect size
eff_size(bb, sigma = sigma(aa), edf= df.residual(aa), method='pairwise')

# Plot of Figure S2
subset(dd3_120, CM==3 & SEASON=="DRY" & YEAR=="2020")%>%
  mutate(POLYGON= fct_relevel(POLYGON, c('NAA','SAA','REBIOSH')))%>%
  ggplot(aes(NDVI,Thermal.safety.margin_dCelcius_9h.16h,color=POLYGON))+
  geom_point()+
  scale_color_manual(values= c("orange3","grey","green4"), labels= c("NAA","SAA","REBIOSH"))+
  geom_smooth(method="lm", se=F, size=1.5)+
  # stat_ellipse()+
  theme_classic()+
  labs(color="Polygon")+
  scale_y_continuous(limits=c(0.5,4.3),breaks = seq(0.5, 4.3, by = 1))+
  ylab("VTmax - Tb (°C)")+ xlab("Vegetatation cover (%)")+#ylim(c(21,27))+
  theme(strip.text = element_text(face="bold", size=11, color="black"),
        strip.background = element_rect(color="black"),
        axis.text = element_text(face = "bold", size=11, color = "black"),
        axis.title = element_text(face = "bold", size=14, color = "black"),
        legend.text = element_text(face = "bold", size=11, color = "black"),
        legend.title = element_text(face = "bold", size=14, color = "black"),
        axis.line = element_line(color="black",size=1),
        strip.text.x = element_text(size = 11, color = "black", face = "bold"))

################################################################################
# FORAGING DURATION 

# Analysis
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY'), Daily.foraging.duration_h~POLYGON*YEAR)
anova(aa)
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY'), Daily.foraging.duration_h~POLYGON+YEAR)

# Post-hoc
bb<- emmeans(aa, specs= pairwise~ YEAR)

# Effect size
kable(eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method='pairwise'))

# Plot of Figure 4B
subset(dd3_120, CM==3 & SEASON=='DRY')%>%
  mutate(POLYGON = fct_relevel(POLYGON, "NAA", "SAA", "REBIOSH"))%>%
  ggplot(aes(factor(YEAR), Daily.foraging.duration_h, fill=POLYGON))+ 
  geom_boxplot(color="black", size=0.8)+
  theme_classic()+
  labs(fill="Polygon")+
  scale_fill_manual(values= c("orange3","gray","green4"), labels= c("NAA","SAA","REBIOSH"))+
  ylab("Duration of daily foraging (h / day)")+ xlab("Year")+
  theme(axis.text = element_text(face = "bold", size=11, color = "black"),
        axis.title = element_text(face = "bold", size=14, color = "black"),
        legend.text = element_text(face = "bold", size=11, color = "black"),
        legend.title = element_text(face = "bold", size=14, color = "black"),
        axis.line = element_line(color="black",size=1))

################################################################################
# BASAL METABOLIC RATE

# Analysis
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY'), Daily.basal.metabolic.rate_kJ.per.day~POLYGON*YEAR)
anova(aa)
aa<- aov(data=subset(dd3_120, CM==3 & SEASON=='DRY'), Daily.basal.metabolic.rate_kJ.per.day~POLYGON+YEAR)

# Post-hoc
bb<- emmeans(aa, specs= pairwise~ YEAR)
bb<- emmeans(aa, specs= pairwise~ POLYGON)

# Effect size
kable(eff_size(bb, sigma= sigma(aa), edf= df.residual(aa), method='pairwise'))

# Plot of Figure 4C
subset(dd3_120, CM==3 & SEASON=='DRY')%>%
  mutate(POLYGON = fct_relevel(POLYGON, "NAA", "SAA", "REBIOSH"))%>%
  ggplot(aes(factor(YEAR), Daily.basal.metabolic.rate_kJ.per.day, fill=POLYGON))+ 
  geom_boxplot(color="black", size=0.8)+
  # scale_fill_manual(values = c("green4","orange3","gray"))+
  theme_classic()+
  labs(fill="Polygon")+
  # scale_fill_grey(start = 0.3, end = 1, labels= c("Inside","North","South"))+
  scale_fill_manual(values= c("orange3","gray","green4"), labels= c("NAA","SAA","REBIOSH"))+
  ylab("Daily basal metabolic rate (kJ / day)")+ xlab("Year")+
  scale_y_continuous(limits=c(1.15,1.8), breaks=seq(1.0,1.8,0.2))+
  # annotate("segment", x=0.8,xend=1.2,y=87,yend=87,size=1.5)+
  # annotate("segment", x=1.8,xend=2.2,y=87,yend=87,size=1.5)+
  # annotate("text", x=0.7, y=85, label="a", size=5, color="black", facefold="bold")+
  # annotate("text", x=2,y=85,label="***",size=8)+
  theme(axis.text = element_text(face = "bold", size=11, color = "black"),
        axis.title = element_text(face = "bold", size=14, color = "black"),
        legend.text = element_text(face = "bold", size=11, color = "black"),
        legend.title = element_text(face = "bold", size=14, color = "black"),
        axis.line = element_line(color="black",size=1))

