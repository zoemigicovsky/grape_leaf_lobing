library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

dim(data)
#2632 leaves to start off with accessions having 1 to 6 leaves each for a total of 474 accessions
table(table(data$accession))
#1   2   3   4   5   6 
#5   5  47   7  12 398 
sum(table(table(data$accession)))
#474

#Only keep vines that occurred in both years with 3 leaves for each
data_filt <- data %>% na.omit() %>% group_by(accession) %>% filter(n()==6)
dim(data_filt)
# 2388  118
#2388/6
# 398 vines

data_filt$accession <- as.character(data_filt$accession)
data_filt$year <- as.character(data_filt$year)

data_filt_interannual <- data_filt %>% dplyr::select(accession, year, all_area, veins, blade)
data_filt_interannual <- data_filt_interannual %>% pivot_longer(cols=all_area:blade, values_to="value")

data_filt_interannual$year <- as.factor(data_filt_interannual$year)
data_filt_interannual$year <- fct_relevel(data_filt_interannual$year, "2019")
data_filt_interannual$name <- as.factor(data_filt_interannual$name)

cbPalette <- c("#009E73", "#56B4E9")

p1 <- data_filt_interannual %>% ggplot(aes(y=name, x=value, fill=year))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,
                                              dodge.width =0.8),
              pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.8,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Leaf area (cm2)",
       y="Leaf portion")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  scale_y_discrete(limits = rev(levels(data_filt_interannual$name)))+
  facet_zoom(x=name=="veins")

#How much does overall increase by? Median in 2018 vs 2019
data_filt_interannual %>% filter(name=="all_area") %>% group_by(year) %>% summarise_at(vars(value), list(median), na.rm=T) 
# year  value
#<fct> <dbl>
#1 2019   28.7
#2 2018   24.5
#28.7-24.5=4.2

#How much does vein increase by? Median in 2018 vs 2019
data_filt_interannual %>% filter(name=="veins") %>% group_by(year) %>% summarise_at(vars(value), list(median), na.rm=T) 
# year  value
# <fct> <dbl>
#   1 2019   1.62
# 2 2018   1.39
#1.62-1.39= 0.23 

#How much does blade increase by? Median in 2018 vs 2019
data_filt_interannual %>% filter(name=="blade") %>% group_by(year) %>% summarise_at(vars(value), list(median), na.rm=T) 
# year  value
# <fct> <dbl>
#   1 2019   27.1
# 2 2018   23.0
#27.1-23 = 4.1 

#Perform wilcoxon test to compare across years 
dat_blade_2018 <- data_filt_interannual %>% filter(name=="blade") %>% filter(year=="2018")
dat_blade_2019 <- data_filt_interannual %>% filter(name=="blade") %>% filter(year=="2019")

wilcox.test(dat_blade_2018$value, dat_blade_2019$value)
#W = 585202, p-value = 3.592e-14

dat_veins_2018 <- data_filt_interannual %>% filter(name=="veins") %>% filter(year=="2018")
dat_veins_2019 <- data_filt_interannual %>% filter(name=="veins") %>% filter(year=="2019")

wilcox.test(dat_veins_2018$value, dat_veins_2019$value)
#W = 571944, p-value < 2.2e-16

dat_all_area_2018 <- data_filt_interannual %>% filter(name=="all_area") %>% filter(year=="2018")
dat_all_area_2019 <- data_filt_interannual %>% filter(name=="all_area") %>% filter(year=="2019")

wilcox.test(dat_all_area_2018$value, dat_all_area_2019$value)
#W = 584071, p-value = 2.137e-14

data_filt_interannual <- data_filt %>% dplyr::select(accession, year, veins_to_blade)
data_filt_interannual <- data_filt_interannual %>% pivot_longer(cols=veins_to_blade, values_to="value")

data_filt_interannual$name <- as.factor(data_filt_interannual$name)
data_filt_interannual$year <- as.factor(data_filt_interannual$year)
data_filt_interannual$year <- fct_relevel(data_filt_interannual$year, "2018")

cbPalette <- c("#56B4E9", "#009E73")

p2 <- data_filt_interannual %>% filter(name=="veins_to_blade") %>% ggplot(aes(x=year, y=value, fill=year))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,  dodge.width =0.8), pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.6,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Year",
       y="Vein to blade ratio")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position = "none")

#How much does ratio change by? Median in 2018 vs 2019
data_filt_interannual %>% filter(name=="veins_to_blade") %>% group_by(year) %>% summarise_at(vars(value), list(median), na.rm=T) 
#year   value
#<fct>  <dbl>
# 1 2018  0.0607
#2 2019  0.0603

#Perform wilcoxon test to compare across years 

dat_vtb_2018 <- data_filt_interannual %>% filter(name=="veins_to_blade") %>% filter(year=="2018")
dat_vtb_2019 <- data_filt_interannual %>% filter(name=="veins_to_blade") %>% filter(year=="2019")

wilcox.test(dat_vtb_2018$value, dat_vtb_2019$value)
#W = 720044, p-value = 0.668

pdf("figures/figureS1.pdf", width=8, height=6)
ggarrange(p1, p2, ncol=2,labels="AUTO", widths=c(2,1))
dev.off()
