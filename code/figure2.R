library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

#PCA with mean leaves and correlation with lobing

pca_results <- read_csv("data/goali_pcs_with_landmarks.csv")
#Recalculate distal and proximal lobing
data <- pca_results %>% mutate(distal_lobing=sqrt( (x19-x3)^2 + (y19-y3)^2 ) / sqrt( (x18-x3)^2 + (y18-y3)^2 ), proximal_lobing=sqrt( (x16-x2)^2 + (y16-y2)^2 ) / sqrt( (x15-x2)^2 + (y15-y2)^2 ))

#How many leaves are there?
dim(data)
#2632

cor.test(data$distal_lobing, data$PC1)

#Pearson's product-moment correlation
# data:  data$distal_lobing and data$PC1
# t = 297.66, df = 2630, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9843357 0.9865418
# sample estimates:
#       cor 
# 0.9854803 

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

data %>% ggplot(aes(y=distal_lobing, x=PC1, colour=distal_lobing))+
  geom_point()+
  theme_few()+
  viridis::scale_color_viridis()+
  theme(legend.position = "bottom")

#Divide into 4 groups, since I have 2632 samples that's 658 per group
#Top 658
data1 <- data %>% slice_max(PC1, n=658)
min(data1$PC1)
#0.8839757
mean(data1$PC1)
#1.214758

data2 <- data %>% filter(PC1<0.8839757) %>% slice_max(PC1, n=658)
min(data2$PC1)
#0.126528
mean(data2$PC1)
#0.498456

data3 <- data %>% filter(PC1<0.126528) %>% slice_max(PC1, n=658)
min(data3$PC1)
#-0.8295489
mean(data3$PC1)
#-0.3345257

data4 <- data %>% filter(PC1< -0.8295489) %>% slice_max(PC1, n=658)
min(data4$PC1)
#- -2.04983
mean(data4$PC1)
#-1.372349

#Plot average leaf for each of those datasets 
data1_avg <- data1 %>% dplyr::select(x1_adjusted:distal_lobing) %>% summarise(across(everything(), list(mean)))
data2_avg <- data2 %>% dplyr::select(x1_adjusted:distal_lobing) %>% summarise(across(everything(), list(mean)))
data3_avg <- data3 %>% dplyr::select(x1_adjusted:distal_lobing) %>% summarise(across(everything(), list(mean)))
data4_avg <- data4 %>% dplyr::select(x1_adjusted:distal_lobing) %>% summarise(across(everything(), list(mean)))

#How to plot shape with 21 landmarks?
size <- 1
alpha <- 0.6

#Let's visualize these average leaf shapes against each other with different colors for group along PC1

data4_col <- "#440154"
data3_col <- "#31688e"
data2_col <- "#35b779"
data1_col <- "#fde725"


p1 <- data %>% ggplot(aes(y=distal_lobing, x=PC1, colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  theme(legend.position = "bottom")+
  xlab("PC1 (43.69%)")


#Code to get 21 landmarks plotted

p2 <- ggplot(data1_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1))+ 
  geom_segment(colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha)+
  geom_segment(data=data2_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha)+geom_segment(data=data3_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha)+
  geom_segment(data=data4_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha)+
  theme_few() + coord_fixed() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank())


pdf("figures/figure2.pdf", width=6, height=5)
ggarrange(p1,p2, ncol=2, widths=c(3,1.5), labels="AUTO")
dev.off()
