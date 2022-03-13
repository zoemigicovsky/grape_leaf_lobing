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

#Relationship between area and vein length
p1 <- data %>% ggplot(aes(y=log(all_area), x=log(vein_lengths), colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  theme(axis.title = element_text(face="bold"))+
  labs(y="ln(Area (cm2))",
       x="ln(Total major vein length (cm))") +
  theme(legend.position = "bottom")

#Residuals from the model for vein length ~ area
model1 <- lm(log(all_area)~log(vein_lengths), data=data)
summary(model1)

#Call:
#   lm(formula = log(all_area) ~ log(vein_lengths), data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.51878 -0.08922  0.00929  0.09679  0.49547 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.946840   0.023268  -83.67   <2e-16 ***
#   log(vein_lengths)  1.829693   0.008244  221.95   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1357 on 2630 degrees of freedom
# Multiple R-squared:  0.9493,	Adjusted R-squared:  0.9493 
# F-statistic: 4.926e+04 on 1 and 2630 DF,  p-value: < 2.2e-16

#Add residuals
data <- data %>% add_residuals(model1)

#Plot the residuals against distal lobing
p2 <- data %>% ggplot(aes(x=distal_lobing, y=resid, colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  geom_smooth(method = "lm", se=FALSE, colour="black")+
  theme(axis.title = element_text(face="bold"))+
  labs(x="Distal lobing",
       y="Residuals ln(area)~ln(vein length)")  +
  theme(legend.position = "bottom")

model1 <- lm(resid~distal_lobing, data=data)
summary(model1)

# Call:
#   lm(formula = resid ~ distal_lobing, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.45745 -0.07114  0.00508  0.07650  0.37029 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.136695   0.004455  -30.69   <2e-16 ***
#   distal_lobing  0.278973   0.007927   35.20   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1119 on 2630 degrees of freedom
# Multiple R-squared:  0.3202,	Adjusted R-squared:  0.3199 
# F-statistic:  1239 on 1 and 2630 DF,  p-value: < 2.2e-16


pdf("figure3.pdf", width=8, height=4.5)
ggarrange(p1, p2, ncol=2, labels="AUTO")
dev.off()
