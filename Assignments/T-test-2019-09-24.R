rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
countrydata <- read_csv("datasets/demos/countrydata.csv")
# names() tells you the names assigned to each column, generally variable
# names
names(countrydata)

# head() gives you the first six rows of a dataset
head(countrydata)

# dim() gives you the dimensions of your dataset
dim(countrydata)

# str() returns the structure of the dataset
str(countrydata)
# Plot height as a boxplot
ggplot(countrydata)+
  geom_boxplot(aes(x = "", y = Difference), notch = FALSE, varwidth = TRUE)
summary(data)
summary(countrydata)
