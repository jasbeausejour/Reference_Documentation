library(tidyverse)
library(dslabs)

data("heights")

s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(average=mean(height), standard_deviation = sd(height))