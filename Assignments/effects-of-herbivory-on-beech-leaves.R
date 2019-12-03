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

library(readr)
data <- read_csv("datasets/demos/dbhandherb.csv")

model01 <- lm(DBH ~ Herbivory, data = data)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model01, smooth.colour = NA)

# You will get the warning message:
# Removed 32 rows containing missing values (geom_path)
# That is because of the smooth.colour = NA argument, which I use because
# otherwise there is a distracting line

# I personally like to see residual by x plot.  To do that, we need
# to add columns to the original data, lion, for the residuals, etc.  To
# do this, we have multiple options.

# Option 1. Use the function augment.

dbh_plus <- augment(model01)
ggplot(data = dbh_plus)+
  geom_point(aes(x = DBH, y= .resid))

# Option 2.  Use the function resid() right in the plotting command
ggplot(data = data)+
  geom_point(aes(x = DBH, y = resid(model01)))

# Option 3.  Use mutate() to add a residuals column to the original data
data <- data %>%
  mutate(dbh_resid = resid(model01))
ggplot(data = data) +
  geom_point(aes(x = DBH, y = dbh_resid))

# So taking all the plots together, the normal Q-Q of the residuals looks
# like the residuals are normal, which is good
# Residuals vs. fitted (fitted is the same thing as predicted),
# Some fan shape, except for the highest fitted value.  Residuals vs.
# x?  Not good at all.  

# For the sake of learning, let's just see what happens if we proceed
# with the linear regression

# So what are the actual statistical results???
summary(model01)

# The p-value of interest is found in the row "ageInYears", the intercept
# and slope are found under the column header "Estimate".

# Older lions have significantly higher proportion of black on their noses
# (Linear regression: proportionBlack = 0.0697 + 0.0586(ageInYears);
# df = 1, 30, F=49.75, P<0.0001), and lion age explained more than 60%
# of the variabiity in nose blackness (R2 = 0.6238).

# For a linear regression, we usually want to add a regression line to 
# our plot and often we also want to give an idea about how confident we
# are in our estimate of that regression line.  To do this, we generate
# what are known as confidence bands.  The narrower the band, the more
# confident we are in our estimate of the line.  I

# We create confidence bands by adding in a layer 
# geom_smooth(method = "lm", level=0.95).

ggplot(data = data, aes(x = DBH, y = Herbivory)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "DBH", y = "Herbivory")

# What do you think will happen to the confidence band if we set level
# to 0.99?
ggplot(data = data, aes(x = DBH, y = Herbivory)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "DBH", y = "Herbivory")

rm(list = ls())

library(readr)
data2 <- read_csv("datasets/demos/herbivory.csv")

model02 <- lm(Herbivory ~ Health, data = data2)

# Autoplot gives you a residual by predicted plot in the upper left panel
autoplot(model02, smooth.colour = NA)

# You will get the warning message:
# Removed 32 rows containing missing values (geom_path)
# That is because of the smooth.colour = NA argument, which I use because
# otherwise there is a distracting line

# I personally like to see residual by x plot.  To do that, we need
# to add columns to the original data, lion, for the residuals, etc.  To
# do this, we have multiple options.

# Option 1. Use the function augment.

herb_plus <- augment(model02)
ggplot(data = herb_plus)+
  geom_point(aes(x = Herbivory, y= .resid))

# Option 2.  Use the function resid() right in the plotting command
ggplot(data = data2)+
  geom_point(aes(x = Herbivory, y = resid(model02)))

# Option 3.  Use mutate() to add a residuals column to the original data
data2 <- data2 %>%
  mutate(herb_resid = resid(model02))
ggplot(data = data2) +
  geom_point(aes(x = Herbivory, y = herb_resid))

# So taking all the plots together, the normal Q-Q of the residuals looks
# like the residuals are normal, which is good
# Residuals vs. fitted (fitted is the same thing as predicted),
# Some fan shape, except for the highest fitted value.  Residuals vs.
# x?  Not good at all.  

# For the sake of learning, let's just see what happens if we proceed
# with the linear regression

# So what are the actual statistical results???
summary(model02)

# The p-value of interest is found in the row "ageInYears", the intercept
# and slope are found under the column header "Estimate".

# Older lions have significantly higher proportion of black on their noses
# (Linear regression: proportionBlack = 0.0697 + 0.0586(ageInYears);
# df = 1, 30, F=49.75, P<0.0001), and lion age explained more than 60%
# of the variabiity in nose blackness (R2 = 0.6238).

# For a linear regression, we usually want to add a regression line to 
# our plot and often we also want to give an idea about how confident we
# are in our estimate of that regression line.  To do this, we generate
# what are known as confidence bands.  The narrower the band, the more
# confident we are in our estimate of the line.  I

# We create confidence bands by adding in a layer 
# geom_smooth(method = "lm", level=0.95).

ggplot(data = data2, aes(x = Herbivory, y = Health)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Percent Herbivory", y = "Tree Health (1-3)")

# What do you think will happen to the confidence band if we set level
# to 0.99?
ggplot(data = data2, aes(x = Herbivory, y = Health)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.99) +
  theme_bw()+
  labs( x = "Percent Herbivory", y = "Tree Health (1-3)")

