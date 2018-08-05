library(tidyverse)
library(dslabs)

data("heights")


# Quick calculation fin datasets -----------------------
s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(average=mean(height), standard_deviation = sd(height)) #summarize is aware of the variables names
s

heights %>% 
  filter(sex=="Male") %>% 
  summarize(median=median(height),
            minimum=min(height),
            maximum=max(height))



