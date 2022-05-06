library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

#Does lobing correlate with size?
pdf("figures/figure3.pdf", width=6, height=4.5)
data %>% ggplot(aes(x=distal_lobing, y=log(all_area), colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  stat_smooth(method="lm", colour="black", se=F)+
  theme(legend.position = "bottom")
dev.off()

model1 <- lm(log(all_area)~log(distal_lobing), data=data)
summary(model1)
# Call:
#   lm(formula = log(all_area) ~ log(distal_lobing), data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.3871 -0.3440  0.0663  0.4089  1.8099 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.14608    0.01723 182.593  < 2e-16 ***
#   log(distal_lobing) -0.03793    0.01257  -3.018  0.00257 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6019 on 2630 degrees of freedom
# Multiple R-squared:  0.003452,	Adjusted R-squared:  0.003073 
# F-statistic: 9.111 on 1 and 2630 DF,  p-value: 0.002565

#how many leaves?
dim(data)
#2632