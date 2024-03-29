### Lab 5. t-tests and friends

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")
#Question 1
# Read in data file, generic version
#<name-you-assign><-read_csv("path-to-file", col_names = TRUE)
obliquity_data<-read_csv("datasets/demos/obliquity_data.csv")
names(obliquity_data)
head(obliquity_data)
dim(obliquity_data)
str(obliquity_data)
summary(obliquity_data)
summ_obliquity <- obliquity_data %>%
  summarise(mean_obliquity = mean(obliquity),
            sd_obliquity = sd(obliquity),
            n_obliquity = n())
sample_mean <- 	23.49878
sample_sd <- 0.019613
sample_n <- 5
df <- sample_n -1
null_mean <- 23.4722
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
two_tailed <- 2*(1-pt(abs(t_sample), df))

#Question 2
library(readr)
HeartAttack_short <- read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
               col_types = cols(
                 group = col_character() ))
names(HeartAttack_short)
head(HeartAttack_short)
dim(HeartAttack_short)
str(HeartAttack_short)
summary(HeartAttack_short)
summ_HA <-  %>%
  group_by(COLUMN) %>% 
  summarise(mean_heart = mean(HeartAttack_short),
            sd_heart = sd(HeartAttack_short),
            n_heart = n(HeartAttack_short))
ratio <-(max(summ_heart$sd_heart))/(min(summ_heart$sd_heart))
t.test(Heart ~ ZONE, data = ward, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#Question 3

### 7/10, Code breaks at line 48, COLUMN should be Group, Q3 incomplete ####
