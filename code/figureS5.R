library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

data <- data %>% mutate(branch_lengths=petiolar_length+dist_branch_length+mid_branch_length) 

#Relationship between area and vein length
p1 <- data %>% ggplot(aes(x=log(branch_lengths), y=log(all_area), colour=distal_lobing))+
  geom_point(alpha=0.6, size=2, stroke=0)+
  theme_few()+
  scale_color_viridis(limits = c(0, 1.23))+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  theme(axis.title = element_text(face="bold"))+
  labs(y="ln(Area (cm2))",
       x="ln(Total branch vein length (cm))") +
  theme(legend.position = "bottom")

#Residuals from the model
model1 <- lm(log(all_area)~log(branch_lengths), data=data)

summary(model1)
# Call:
#   lm(formula = log(all_area) ~ log(branch_lengths), data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.68935 -0.06887  0.00979  0.08017  0.36464 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         -0.820248   0.016184  -50.68   <2e-16 ***
#   log(branch_lengths)  1.790117   0.007157  250.11   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1211 on 2630 degrees of freedom
# Multiple R-squared:  0.9597,	Adjusted R-squared:  0.9596 
# F-statistic: 6.256e+04 on 1 and 2630 DF,  p-value: < 2.2e-16

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
       y="Residuals ln(area)~ln(branching)")  +
  theme(legend.position = "bottom")

model1 <- lm(resid~distal_lobing, data=data)

summary(model1)
# Call:
#   lm(formula = resid ~ distal_lobing, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.63831 -0.06503  0.00482  0.07638  0.35775 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.081458   0.004463  -18.25   <2e-16 ***
#   distal_lobing  0.166244   0.007941   20.93   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1121 on 2630 degrees of freedom
# Multiple R-squared:  0.1428,	Adjusted R-squared:  0.1425 
# F-statistic: 438.2 on 1 and 2630 DF,  p-value: < 2.2e-16


pdf("figures/figureS4.pdf", width=8, height=4.5)
ggarrange(p1, p2, ncol=2, labels="AUTO")
dev.off()
