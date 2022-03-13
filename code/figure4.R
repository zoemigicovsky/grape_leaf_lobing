library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

#Relationship between area and vein to blade ratio
p1 <- data %>% ggplot(aes(y=log(all_area),x=log(veins_to_blade), colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  theme(axis.title = element_text(face="bold"))+
  labs(y="ln(Area (cm2))",
       x="ln(Vein to blade ratio)") +
  theme(legend.position = "bottom")

#Residuals from the model for vein length ~ area
model1 <- lm(log(all_area)~log(veins_to_blade), data=data)

summary(model1)
# Call:
#   lm(formula = log(all_area) ~ log(veins_to_blade), data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.69988 -0.33040  0.03444  0.36841  1.49452 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -0.90790    0.14134  -6.424 1.57e-10 ***
#   log(veins_to_blade) -1.46508    0.05047 -29.029  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5247 on 2630 degrees of freedom
# Multiple R-squared:  0.2427,	Adjusted R-squared:  0.2424 
# F-statistic: 842.7 on 1 and 2630 DF,  p-value: < 2.2e-16


#Add residuals
data <- data %>% add_residuals(model1)

model1 <- lm(resid~distal_lobing, data=data)

summary(model1)
# Call:
#   lm(formula = resid ~ distal_lobing, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.69817 -0.32375  0.02733  0.35927  1.46128 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.21898    0.02030   10.79   <2e-16 ***
#   distal_lobing -0.44691    0.03613  -12.37   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5101 on 2630 degrees of freedom
# Multiple R-squared:  0.05499,	Adjusted R-squared:  0.05463 
# F-statistic:   153 on 1 and 2630 DF,  p-value: < 2.2e-16

#Plot the residuals against distal lobing
p2 <- data %>% ggplot(aes(x=distal_lobing, y=resid, colour=distal_lobing))+
  geom_point(alpha=0.6, stroke=0, size=2)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  geom_smooth(method = "lm", se=FALSE, colour="black")+
  theme(axis.title = element_text(face="bold"))+
  labs(x="Distal lobing",
       y="Residuals ln(area)~ln(vein to blade ratio)")  +
  theme(legend.position = "bottom")

pdf("figures/figure4.pdf", width=8, height=4.5)
ggarrange(p1, p2, ncol=2, labels="AUTO")
dev.off()
