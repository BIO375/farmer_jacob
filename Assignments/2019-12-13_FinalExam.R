rm(list = ls())

library("DescTools")
library("ggfortify")
library("multcomp")
library("nlme")
library("broom")
library("ggmosaic")
library("epitools")
library("tidyverse")
tidyverse_update()

library(readr)
data <- read_csv("datasets/final/insulation.csv")

model01 <- lm(leanness ~ heat_loss, data = data )

autoplot(model01, smooth.colour = NA)

data_plus <- augment(model01)
ggplot(data = data_plus)+
  geom_point(aes(x = heat_loss, y= .resid))

ggplot(data = data)+
  geom_point(aes(x = heat_loss, y = resid(model01)))

data <- data %>%
  mutate(leanness = resid(model01))
ggplot(data = data) +
  geom_point(aes(x = heat_loss, y = leanness))

summary(model01)

ggplot(data = data, aes(x = heat_loss, y = leanness)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Heat Loss", y = "Leanness")

ggplot(data = data, aes(x = heat_loss, y = leanness)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "Heat Loss", y = "Leanness")
###Scenario 2####

rm(list = ls())
library(readr)
caf <- read_csv("datasets/final/caffeine.csv")

head(caf)
summary(caf)

ggplot(caf, aes(x = group, y = half_life))+
  geom_boxplot() +
  theme_bw() +
  coord_flip()
ggplot(caf) +
  geom_histogram(aes(half_life), binwidth = 1.5)+
  facet_wrap(~group)
ggplot(daphnia)+
  geom_qq(aes(sample = half_life, color = group))

model02 <- lm(half_life~group, data = caf)

summ_half_life <- caf %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())
ratio <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))

autoplot(model02)

anova(model02)

summary(model02)

####Scenario 3####

rm(list = ls())

library(readr)
library(readr)
davis <- read_csv("datasets/final/davis.csv")
