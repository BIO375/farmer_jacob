# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

library(readr)
Jaffe <- read_csv("datasets/demos/Jaffe.csv")

# It is important to read in the predictor as a factor
# In the case of this dataset, the parasite names have a space so I recoded
# the factor levels using the function fct_recode()
Jaffe <- Jaffe %>%
  mutate(Depth = fct_recode(Depth, Surface = "Surface",
                               Middepth = "Middepth",
                               Bottom = "Bottom"
  ))

# Look at the data
head(Jaffe)
summary(Jaffe)

ggplot(Jaffe, aes(x = Depth, y = HCB))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(HCB), binwidth = 0.1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = HCB, color = Depth))

model01 <- lm(HCB~Depth, data = Jaffe)

summ_HCB <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_HCB = mean(HCB),
            sd_HCB = sd(HCB),
            n_HCB = n())

ratio <-(max(summ_HCB$sd_HCB))/(min(summ_HCB$sd_HCB))

HCB01 <-lm(HCB~Depth, data = Jaffe)

autoplot(HCB01)

anova(HCB01)

summary(HCB01)




#Aldrin part

ggplot(Jaffe, aes(x = Depth, y = Aldrin))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(Jaffe) +
  geom_histogram(aes(Aldrin), binwidth = 0.1)+
  facet_wrap(~Depth)
ggplot(Jaffe)+
  geom_qq(aes(sample = Aldrin, color = Depth))

model02 <- lm(Aldrin~Depth, data = Jaffe)

summ_Aldrin <- Jaffe %>%
  group_by(Depth) %>% 
  summarise(mean_Aldrin = mean(Aldrin),
            sd_Aldrin = sd(Aldrin),
            n_Aldrin = n())

ratio <-(max(summ_Aldrin$sd_Aldrin))/(min(summ_Aldrin$sd_Aldrin))

Jaffe <- mutate(Jaffe,log_aldrin =log10(Aldrin))

Aldrin02 <-lm(log_aldrin~Depth, data = Jaffe)

autoplot(Aldrin02)

anova(Aldrin02)

summary(Aldrin02)

#Normal data

Aldrin01 <-lm(Aldrin~Depth, data = Jaffe)

autoplot(Aldrin01)

anova(Aldrin01)

summary(Aldrin01)

#Tukey
Jaffe <-read_csv("datasets/demos/Jaffe.csv", col_types = cols(
  Depth = col_factor() )

tukey <- glht(Aldrin02, linfct = mcp(Depth = "Tukey"))

summary(tukey)
