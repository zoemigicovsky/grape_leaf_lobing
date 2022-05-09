library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

#Add in total vein lengths
data <- data %>% mutate(vein_lengths=midvein_length+distal_length+proximal_length) 
data <- data %>% mutate(branch_lengths=petiolar_length+dist_branch_length+mid_branch_length) 

pdf("figures/figureS5.pdf", width=4, height=4.5)
data %>% ggplot(aes(x=vein_lengths, y=branch_lengths, colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  theme(axis.title = element_text(face="bold"))+
  labs(x="Total major vein length (cm)",
       y="Total branching vein length (cm)")  +
  theme(legend.position = "bottom")
dev.off()


#Correlation between branching vein length and major vein length

cor.test(data$vein_lengths, data$branch_lengths) 
# Pearson's product-moment correlation
# 
# data:  data$vein_lengths and data$branch_lengths
# t = 201.84, df = 2630, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9667987 0.9714389
# sample estimates:
#       cor 
# 0.9692047 
