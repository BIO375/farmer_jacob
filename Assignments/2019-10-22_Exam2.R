rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, load the package DescTools
library("DescTools")

# Question 9
library(readr)
feathers <- read_csv("datasets/exams/feathers.csv")
View(feathers)
names(feathers)
head(feathers)
dim(feathers)
str(feathers)

feathers <- mutate(feathers, diff = typical - odd)

ggplot(feathers) +
  geom_histogram(aes(diff), binwidth = .01)

ggplot(feathers) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(feathers)+
  geom_qq(aes(sample = diff))

SignTest(feathers$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#Question 10
library(readr)
baker <- read_csv("datasets/exams/baker.csv")
View(baker)
names(baker)
head(baker)
dim(baker)
str(baker)
baker <- mutate(baker, diff = Before - After)

ggplot(baker) +
  geom_histogram(aes(diff), binwidth = .1)

ggplot(baker) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(baker)+
  geom_qq(aes(sample = diff))

SignTest(baker$diff, alternative = "greater", mu = 0, conf.level = 0.95)

#Question 11
data <- read_csv("treatment, growthrate
              normal CO2, 2.31
              normal CO2, 1.95
              normal CO2, 1.86
              normal CO2, 1.59
             normal CO2, 1.55
             normal CO2, 1.30
             normal CO2, 1.07
             high CO2, 2.37
             high CO2, 1.89
             high CO2, 1.55
             high CO2, 1.49
             high CO2, 1.26
             high CO2, 1.20
             high CO2, 0.98")

summ_data <- data %>%
  group_by(treatment) %>% 
  summarise(mean_data = mean(growthrate),
            sd_data = sd(growthrate),
            n_data = n())

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_data$sd_data))/(min(summ_data$sd_data))

t.test(growthrate ~ treatment, data = data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)




#### ALL THE CODE WORKS, MISSING HALF OF Q11.  5/6 ####
