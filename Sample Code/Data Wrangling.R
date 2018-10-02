library(tidyverse)

# Let's first get a wide-format file
path <- system.file("extdata",package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# Using the GATHER() function to convert the wide data into the tidy data

tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015') #First argument is the name for the column that now contains the header, second is the name for whatever was in the cells, third is the columns we want to gather, by name

head(tidy_data)

# Sometimes it is easier to specify which column NOT to gather instead of specifying all the ones to gather
new_tidy_data <-  wide_data %>% 
  gather(year, fertility, -country, convert = TRUE)

# The SPREAD() function is basically the invert of the GTHER() function
new_wide_data <- new_tidy_data %>% 
  spread(year, fertility)
# Let's do a more complicated example
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read_csv(filename)
head(raw_dat)

dat <- raw_dat %>% gather(key, value, -country)
head(dat) #here we have a not yet tidy data, we would want to separate fertility and life-expectancy

tidy_data <- dat %>% separate(key, c("year", "variable_name"), sep="_", extra = "merge") %>% 
  spread(variable_name, value)


