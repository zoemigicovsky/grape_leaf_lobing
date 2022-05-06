library(tidyverse)
library(ggthemes)
library(ggpubr)
library(modelr)
library(ggpmisc)
library(ggforce)
library(viridis)

data <- read_csv("data/goali_all_data.csv")

pdf("figures/figureS2.pdf", width=8, height=6)
data %>% ggplot(aes(x=distal_lobing))+
  geom_density(fill="lightgrey")+
  theme_few()+
  facet_wrap(~pop)
dev.off()
