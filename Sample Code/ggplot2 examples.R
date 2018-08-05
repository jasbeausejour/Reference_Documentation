library(dslabs)
library(tidyverse)
library(ggthemes)
library(ggrepel)


# Creating a scaterplot ---------------------------------------------------
data(murders)
# Creating the log10 slope of the averge murder rate in the US
r <- murders %>% 
  summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate

# Creating a nice scatterplot
ggplot(data = murders, aes(x=population/10^6, y=total, label=abb)) +
  geom_abline(intercept = log10(r), lty=2, color="darkgrey")+
  geom_point(aes(color = region), size = 3)+
  geom_text_repel() + #Making sure the labels don't overlap
  scale_x_log10()+
  scale_y_log10() +
  labs(title= "US Gun Murders in US 2010", x="Populations in millions (log scale)", y="Total number of murders (log scale)", caption = "Created by Jas")+
  scale_color_discrete(name = "Region")+
  theme_economist() #Nice theme




# Creating summary plots --------------------------------------------------
data("heights")
head(heights)

heights %>% filter(sex=="Male") 

p <-  heights %>% 
  filter(sex=="Male") %>% 
  ggplot(aes(x=height))

# Histogram
p + geom_histogram(binwidth = 1, fill="blue", col="black") +
  xlab("Male heghts in inches") +
  ggtitle("Histogram") +
  theme_economist()

# Density plot
p + geom_density(fill="blue", col="black") +
  xlab("Male heghts in inches") +
  ggtitle("Density plot") +
  theme_economist()

# Q-Q Plot (option 1 calculation the mean and sd)
p <- heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=height))

params <- heights %>% 
  filter(sex=="Male") %>%
  summarize(mean=mean(height), sd= sd(height))

p + geom_qq(dparams = params) +
  geom_abline()

#Q-Q plor (Option 2, scaling the data in the function)

heights %>%  filter(sex=="Male") %>% 
  ggplot(aes(sample = scale(height))) +
  geom_qq()+
  geom_abline()


# Creating two smooth density plots on top of each other ------------------

data("heights")

heights %>% ggplot(aes(x=height, color=sex))+
  geom_density(sample=heights)

heights %>% ggplot(aes(x=height, fill=sex))+
  geom_density(sample=heights, alpha=0.2)

# Creating grid of plots --------------------------------------------------

plot <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

p1 <- plot+geom_histogram(binwidth = 1, fill="blue",col="black") + ggtitle("Histogram with binwith =1")
p2 <- plot+geom_histogram(binwidth = 2, fill="blue",col="black") + ggtitle("Histogram with binwith =2")
p3 <- plot+geom_histogram(binwidth = 3, fill="blue",col="black")+ ggtitle("Histogram with binwith =3")
p4 <- plot+geom_histogram(binwidth = 0.5, fill="blue",col="black") + ggtitle("Histogram with binwith =0.5")
p5 <- plot+geom_histogram(binwidth = 0.1, fill="blue",col="black") + ggtitle("Histogram with binwith =0.1")
p6 <- plot+geom_histogram(binwidth = 4, fill="blue",col="black")+ ggtitle("Histogram with binwith =4")


library(gridExtra)
grid.arrange(p1,p2,p3, ncol=3,newpage = p4,p5,p6)


