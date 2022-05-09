library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

#PCA with mean leaves and correlation with proximal lobing

pca_results <- read_csv("data/goali_pcs_with_landmarks.csv")
#Recalculate distal and proximal lobing
data <- pca_results %>% mutate(distal_lobing=sqrt( (x19-x3)^2 + (y19-y3)^2 ) / sqrt( (x18-x3)^2 + (y18-y3)^2 ), proximal_lobing=sqrt( (x16-x2)^2 + (y16-y2)^2 ) / sqrt( (x15-x2)^2 + (y15-y2)^2 ))

cor.test(data$proximal_lobing, data$PC1)
# Pearson's product-moment correlation
# 
# data:  data$proximal_lobing and data$PC1
# t = 44.954, df = 2630, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6370087 0.6802474
# sample estimates:
#       cor 
# 0.6591726 

p1 <- data %>% ggplot(aes(y=proximal_lobing, x=PC1, colour=proximal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  theme(legend.position = "bottom")+
  xlab("PC1 (43.69%)")

pdf("figures/figureS2.pdf", width=6, height=5)
ggarrange(p1,labels="AUTO")
dev.off()
