# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Problem 15-22 ####
# Complete parts a, b, c, d
library(readr)
stick <- read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", 
                                       col_types = cols(specimen = col_factor(levels = c("1", 
                                                                                         "2", "3", "4", "5", "6", "7", "8", 
                                                                                         "9", "10", "11", "12", "13", "14", 
                                                                                         "15", "16", "17", "18", "19", "20", 
                                                                                         "21", "22", "23", "24", "25"))))

head(stick)
summary(stick)

ggplot(stick, aes(x = specimen, y = headwidth))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(stick) +
  geom_histogram(aes(headwidth), binwidth = 0.1)+
  facet_wrap(~specimen)
ggplot(stick)+
  geom_qq(aes(sample = headwidth, color = specimen))

model01 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = stick)
model01_varcomp <- VarCorr(model01)
model01_varcomp
# To get repeatibility, tell R to do some math by extracting the first entry in the first
# column and calling it VarAmong

varAmong  <- as.numeric( model01_varcomp[1,1] )

# And then extracting the second entry in the first column and calling it VarWithin
varWithin <- as.numeric( model01_varcomp[2,1] )

#a. 0.00166
# And then doing the math
repeatability <- varAmong / (varAmong + varWithin)
repeatability
#b. 0.0002459167
#c. the repeatability of the headwidth measurements is 0.5970059
#d. the example has a higher repeatability while this problem has a higher measurement error

#### Problem 15-23 ####
# Complete parts a and c only
#a. Planned comparison
library(readr)
cone <- read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))
head(cone)
summary(cone)

ggplot(cone, aes(x = habitat, y = conemass))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(cone) +
  geom_histogram(aes(conemass), binwidth = 1)+
  facet_wrap(~habitat)
ggplot(cone)+
  geom_qq(aes(sample = conemass, color = habitat))

model02 <- lm(conemass~habitat, data = cone)

summ_conemass <- cone %>%
  group_by(habitat) %>% 
  summarise(mean_conemass = mean(conemass),
            sd_conemass = sd(conemass),
            n_conemass = n())
ratio <-(max(summ_conemass$sd_conemass))/(min(summ_conemass$sd_conemass))
autoplot(model02)
anova(model02)
summary(model02)

planned <- glht(model02, linfct = 
                  mcp(habitat = c("island.present - island.absent = 0")))
confint(planned)
summary(planned)
#c. ^

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.
library(readr)
Malaria <- read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))
# Look at the data
head(Malaria)
summary(Malaria)

# When you do a normal boxplot, the parasite species names overlap and are illegible
# so add a line that tells ggplot to flip the x and y axes, coord_flip()
ggplot(Malaria, aes(x = treatmentGroup, y = logSporozoiteNumbers))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Malaria) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = 0.15)+
  facet_wrap(~treatmentGroup)
ggplot(Malaria)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

# Similar to our two-sample t-test, you specify an equation (y~x) and the data

model03 <- lm(logSporozoiteNumbers~treatmentGroup, data = Malaria)

summ_logSporozoiteNumbers <- Malaria %>%
  group_by(logSporozoiteNumbers) %>% 
  summarise(mean_logSporozoiteNumbers = mean(logSporozoiteNumbers),
            sd_logSporozoiteNumbers = sd(logSporozoiteNumbers),
            n_logSporozoiteNumbers = n())
ratio <-(max(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))/(min(summ_logSporozoiteNumbers$sd_logSporozoiteNumbers))

autoplot(model03)

anova(model03)

summary(model03)

tukey <- glht(model03, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)


#### Problem 15-30
# Use the data to perform the correct test.  Please show code for all steps in your process.
library(readr)
crab <- read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))
head(crab)
summary(crab)

# When you do a normal boxplot, the parasite species names overlap and are illegible
# so add a line that tells ggplot to flip the x and y axes, coord_flip()
ggplot(crab, aes(x = crabType, y = bodyTemperature))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(crab) +
  geom_histogram(aes(bodyTemperature), binwidth = 0.15)+
  facet_wrap(~crabType)
ggplot(crab)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))
#a. based on the graph crab body temperature goes in the order from highest to lowest: Female, male major, intact male, male minor
crab <- crab %>%
  mutate(crabType = fct_recode(crabType, female = "female",
                               intact = "intact male",
                               minor = "male minor removed",
                               major = "male major removed"
  ))
model04 <- lm(crabType~bodyTemperature, data = crab)

summ_bodyTemperature <- crab %>%
  group_by(crabType) %>% 
  summarise(mean_bodyTemperature = mean(bodyTemperature),
            sd_bodyTemperature = sd(bodyTemperature),
            n_bodyTemperature = n())
ratio <-(max(summ_bodyTemperature$sd_bodyTemperature))/(min(summ_bodyTemperature$sd_bodyTemperature))

autoplot(model04)

anova(model04)

summary(model04)
