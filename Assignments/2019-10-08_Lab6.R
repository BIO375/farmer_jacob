# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
# install.packages("DescTools")
library("DescTools")

# To simplify summary statistics, install and load the package summarytools
install.packages("summarytools")
library("summarytools")

#Chapter 13
#Question 20
#a. 2-sample t-test, Mann-Whitney U test
#b. Null Hypothesis: there is no difference of mean between the Konkani and Sockeye skin color. 
#Alternate Hypothesis: there is difference of mean between the Konkani and Sockeye skin color. 
#There is a significant difference of mean between the two types of salmon based on the data.
salmon_data <- read_csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")
View(salmon_data)
names(salmon_data)
head(salmon_data)
dim(salmon_data)
str(salmon_data)

ggplot(salmon_data) +
  geom_histogram(mapping = aes(skinColor), binwidth = .1)+
  facet_wrap(~species)

ggplot(salmon_data) +
  geom_boxplot(aes(x = species, y = skinColor))

ggplot(salmon_data) +
  geom_qq(aes(sample = skinColor, color = species))
#Two sided
wilcox.test(skinColor ~ species, data = salmon_data, alternative = "two.sided", conf.level = 0.95)



#Question 25
#Null Hypothesis: the median difference between the two groups is zero
#Alternate Hypothesis: the median difference between the two groups is not zero
cuts_data <- read_csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")
View(cuts_data)
names(cuts_data)
head(cuts_data)
dim(cuts_data)
str(cuts_data)
null_mean <- 0
y<-cuts_data$biomassChange
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
negative_tail <- pt(t_sample, df)
positive_tail <- 1 - negative_tail

SignTest(cuts_data$biomassChange, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
#P-value is greater than significance 0.05 so we fail to reject null hypothesis.


#Question 26
#Null Hypothesis: mean = 0
#Alternate Hypothesis: mean not= 0
finch_data <- read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")
names(finch_data)
head(finch_data)
dim(finch_data)
str(finch_data)
null_mean <- 0
y<-finch_data$preference
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
negative_tail <- pt(t_sample, df)
positive_tail <- 1 - negative_tail
two_tailed <- 2*(1-pt(abs(t_sample), df))
SignTest(finch_data$preference, 
         alternative = "two.sided", mu = 0, conf.level = 0.95)
#P-value- 0.001953 < 0.05 so we fail to reject the null hypothesis.

#Review Problems 2 
#Question 15
#a. -0.5429
#b. 0.0189
#c. 9
trip_data <- read_csv("datasets/demos/trip_data1.csv")

names(trip_data)
head(trip_data)
dim(trip_data)
str(trip_data)
summary(trip_data)
null_mean <- 0
summ_trip <- trip_data %>%
  summarise(mean_score = mean(`Trip Score`),
            median_score = median(`Trip Score`),
            IQR_score = IQR(`Trip Score`),
            sd_score = sd(`Trip Score`),
            var_score = var(`Trip Score`))

View(summ_trip)
y<-trip_data$`Trip Score`
sample_mean <-mean(y)
sample_sd <- sd(y)
sample_n <- as.numeric(length(y))
df <- sample_n -1
t_sample <- (sample_mean - null_mean)/(sample_sd/sqrt(sample_n))
negative_tail <- pt(t_sample, df)
positive_tail <- 1 - negative_tail
two_tailed <- 2*(1-pt(abs(t_sample), df))




