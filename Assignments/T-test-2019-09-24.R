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
rm(list = ls())
data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
data01 <- data01 %>% slice(-105)
library(readr)
chap12e3HornedLizards <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")
View(chap12e3HornedLizards)
# names() tells you the names assigned to each column, generally variable
# names
names(chap12e3HornedLizards)

# head() gives you the first six rows of a dataset
head(chap12e3HornedLizards)

# dim() gives you the dimensions of your dataset
dim(chap12e3HornedLizards)

# str() returns the structure of the dataset
str(chap12e3HornedLizards)
summary(chap12e3HornedLizards)
# <new_object_name> <- <data> %>%
# group_by(<grouping_variable>) %>%
# summarise(
# mean_resp = mean(<response_variable_name>),
# median_resp = median(<response_variable_name>),
# IQR_resp = IQR(<response_variable_name>),
# sd_resp = sd(<response_variable_name>),
# var_resp = var(<response_variable_name>)
# )

summ_lizards <- chap12e3HornedLizards %>%
  group_by(COLUMN) %>% 
  summarise(mean_HL = mean(squamosalHornLength),
            median_HL = median(squamosalHornLength),
            IQR_HL = IQR(squamosalHornLength),
            sd_HL = sd(squamosalHornLength),
            var_HL = var(squamosalHornLength))
