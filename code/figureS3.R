library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

#Load in all data except PCs
data <- read_csv("data/goali_all_data.csv")

#Load in PCA results
pca_results <- read_csv("data/goali_pcs_with_landmarks.csv")

#Only keep vines that occurred in both years with 3 leaves for each
data_filt <- data %>% na.omit() %>% group_by(accession) %>% filter(n()==6)
pca_results_filt <- pca_results %>% select(accession, year, PC1) %>% group_by(accession) %>% filter(n()==6)
  
#Add in total vein lengths
data_filt <- data_filt %>% mutate(vein_lengths=midvein_length+distal_length+proximal_length) 
data_filt <- data_filt %>% mutate(branch_lengths=petiolar_length+dist_branch_length+mid_branch_length) 

cbPalette <- c("#56B4E9", "#009E73")

#Divide data by year

data_filt_2018 <- data_filt %>% filter(year=="2018")
data_filt_2019 <- data_filt %>% filter(year=="2019")

pca_results_filt_2018 <- pca_results_filt %>% filter(year=="2018")
pca_results_filt_2019 <- pca_results_filt %>% filter(year=="2019")

#Do vines vary across PC1 by year?
wilcox.test(pca_results_filt_2018$PC1, pca_results_filt_2019$PC1)
#W = 704686, p-value = 0.6293
#alternative hypothesis: true location shift is not equal to 0

p1 <- pca_results_filt %>% ggplot(aes(x=as.factor(year), y=PC1, fill=as.factor(year)))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,  dodge.width =0.8), pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.6,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Year",
       y="PC1")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position = "none")

#Do vines vary in distal lobing by year?
wilcox.test(data_filt_2018$distal_lobing, data_filt_2019$distal_lobing)
#W = 717252, p-value = 0.7924
#alternative hypothesis: true location shift is not equal to 0

p2 <- data_filt %>% ggplot(aes(x=as.factor(year), y=distal_lobing, fill=as.factor(year)))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,  dodge.width =0.8), pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.6,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Year",
       y="Distal lobing")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position = "none")

#Do vines vary in major vein length by year?
wilcox.test(data_filt_2018$vein_lengths, data_filt_2019$vein_lengths)
#data:  data_filt_2018$vein_lengths and data_filt_2019$vein_lengths
#W = 590843, p-value = 4.482e-13

p3 <- data_filt %>% ggplot(aes(x=as.factor(year), y=vein_lengths, fill=as.factor(year)))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,  dodge.width =0.8), pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.6,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Year",
       y="Major vein lengths")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position = "none")

#Do vines vary in branching vein length by year?
wilcox.test(data_filt_2018$branch_lengths, data_filt_2019$branch_lengths)
#data:  data_filt_2018$branch_lengths and data_filt_2019$branch_lengths
#W = 566157, p-value < 2.2e-1

p4 <- data_filt %>% ggplot(aes(x=as.factor(year), y=branch_lengths, fill=as.factor(year)))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,  dodge.width =0.8), pch=21, stroke=0, size=1.8, alpha=0.5) +
  stat_summary(fun=median, geom = "crossbar",
               position = position_dodge(width=0.8),
               width=0.6,
               size=0.25,
               show.legend=FALSE)+
  labs(x="Year",
       y="Branching vein lengths")+
  theme_bw()+
  theme(axis.title = element_text(face="bold"))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position = "none")

pdf("figures/figureS3.pdf", width=7, height=9)
ggarrange(p1, p2,p3,p4, ncol=2,nrow=2,labels="AUTO")
dev.off()
