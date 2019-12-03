#####EXAM 3

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
# install.packages("broom")
library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
###Question 9####
library(readr)
bacteria <- read_csv("datasets/exams/bacteria.csv")



####Question 10####
library(readr)
aphids <- read_csv("datasets/exams/aphids.csv")


####Question 11#####
library(readr)
glucose <- read_csv("datasets/exams/glucose.csv")

model01 <- lm(blood_glucose ~ HbA1c, data = glucose)


autoplot(model01, smooth.colour = NA)


glucose_plus <- augment(model01)
ggplot(data = glucose_plus)+
  geom_point(aes(x = HbA1c, y= .resid))

ggplot(data = glucose)+
  geom_point(aes(x = HbA1c, y = resid(model01)))

glucose <- glucose %>%
  mutate(black_resid = resid(model01))
ggplot(data = glucose) +
  geom_point(aes(x = HbA1c, y = black_resid))

summary(model01)


#### Question 14####
library(readr)
vision <- read_csv("datasets/exams/DriverVision.csv")
model02 <- lm(Age ~ Distance, data = vision )


autoplot(model02, smooth.colour = NA)


vision_plus <- augment(model02)
ggplot(data = vision_plus)+
  geom_point(aes(x = Age, y= .resid))

ggplot(data = vision)+
  geom_point(aes(x = Age, y = resid(model02)))

vision <- vision %>%
  mutate(drive_resid = resid(model02))
ggplot(data = vision) +
  geom_point(aes(x = Age , y = drive_resid))

summary(model02)

#### Code runs perfectly 5/5 ####

