#####################################
#### EPOG +
#### Advanced Econometrics
#### Final Assignment
#### Wage Gap in the US

####################################

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(dplyr)
library(plm)
library(readr)
library(panelr)
library(outliers)
library(MatchIt)

## Loading the data

cpsgen <- read_csv("cpsgen.csv")

# Data exploration --------------------------------------------------------

summary(cpsgen)

## It is clear that there are some outliers that need to be removed

## Distribution of wages
boxplot(cpsgen$hrwage, cpsgen$realhrwage) ## This shows the distribution WITH the outliers

## We can also formally test the presence of outliers

chisq.out.test(cpsgen$realhrwage)

## Remove Outliers

quartiles <- quantile(cpsgen$realhrwage, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(cpsgen$realhrwage)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

cpsgen_no <- subset(cpsgen, cpsgen$realhrwage > Lower & cpsgen$realhrwage < Upper)

## Now let's look at the distribution again
boxplot(cpsgen_no$hrwage, cpsgen_no$realhrwage)

## Now, to work with wage gap, we need to remove the independent workers and leave only the salaried

unique(cpsgen_no$classwkr)

cpsgen_no <- cpsgen_no %>% filter(
  classwkr == 28 | classwkr == 21 | classwkr == 27 | classwkr == 24 | classwkr == 25
)

## We need to recode education to create a dummy with +high school

cpsgen_no <- cpsgen_no %>% mutate(hs = ifelse(sch >= 12, 1, 0))

## And a variable of the age squared

cpsgen_no <- cpsgen_no %>% mutate(age_squared = age^2)



# Calculating the wage gap ------------------------------------------------

## Now we can calculate the wage gap in Illinois in 2000 and 2006 (This is just for descriptive purposes)

## First, I will recode the sex variable

cpsgen_no <- cpsgen_no %>% mutate(fem = (recode(sex, '2' = '1', '1' = '0')))

wagegap <- function(data, x, y, z) {
  wagegap <- data %>% {{filter}}(
    fem == x &
      year == y &
      statefip == z) %>% 
    {{summarise}}(mean(realhrwage, na.rm = TRUE)
    )
  return(wagegap)
}

wagegap(cpsgen_no, 0, 2000, 17) 
wagegap(cpsgen_no, 1, 2000, 17)
wagegap(cpsgen_no, 0, 2006, 17)
wagegap(cpsgen_no, 1, 2006, 17)

wagegap(cpsgen_no, 0, 2000, 17) - wagegap(cpsgen_no, 1, 2000, 17)
wagegap(cpsgen_no, 0, 2006, 17) - wagegap(cpsgen_no, 1, 2006, 17)

## Wagegap 2000 = 8.82 USD; 2006 = 3.88 USD

## We can also fit a regression:

gap_illi_2000 <- cpsgen_no %>% filter(
  year == 2000 &
    statefip == 17
)

gap_illi_2000_m1 <- lm(realhrwage ~ fem, data = gap_illi_2000)

summary(gap_illi_2000_m1)

gap_illi_2006 <- cpsgen_no %>% filter(
  year == 2006 &
    statefip == 17
)

gap_illi_2006_m1 <- lm(realhrwage ~ fem, data = gap_illi_2006)

stargazer(gap_illi_2000_m1, gap_illi_2006_m1, type = "text")

## Not surprisingly, the regression gives the same results!

## Now we can start preparing the necessary conditions for a fist exploratory model

## Create unique id

cpsgen_no <- cpsgen_no %>% mutate(id = paste(serial, statefip, pernum, sep = ""), .before = year)

## Treated: If Illinois == 1; Illinois == 17 in the state code classification

cpsgen_no <- cpsgen_no %>% mutate(treated = ifelse(statefip == 17, 1, 0))

#Post: The implementation of the law was in 2003

cpsgen_no <- cpsgen_no %>% mutate(post = ifelse(year > 2003, 1, 0))

##Treated + post

cpsgen_no <- cpsgen_no %>% mutate(post.treated = ifelse(treated == 1 & post == 1, 1, 0))

##Post + female

cpsgen_no <- cpsgen_no %>% mutate(post.fem = ifelse(post == 1 & fem == 1, 1, 0))

## Treated + female

cpsgen_no <- cpsgen_no %>% mutate(treated.fem = ifelse(treated == 1 & fem == 1, 1, 0))

##Treated + post + female

cpsgen_no <- cpsgen_no %>% mutate(post.treated.fem = ifelse(treated == 1 & post == 1 & fem == 1, 1, 0))

## Now, to start we can compare Illinois to it's neighboring states (Wisconsin, Indiana, 
## Missoury and Iowa), which does NOT have any policy on pay transparency. They are
## neighboring states and therefore might share some demographic characteristics. In any case, we can check that.

II <- cpsgen_no %>% filter(
  statefip == 17 | statefip == 18 | statefip == 19 | statefip == 29 | statefip == 55
)

## We can test some characteristics of the data:

t.test(II$age[II$treated == 1], II$age[II$treated == 0])

ggplot(data = II, aes(x = realhrwage, y = ..density..)) +
  geom_density(aes(color = as.factor(treated), fill = as.factor(treated)),
               position = "identity", bins = 30, alpha = 0.4)

ggplot(data = II, aes(x = age, y = ..density..)) +
  geom_density(aes(color = as.factor(treated), fill = as.factor(treated)),
               position = "identity", bins = 30, alpha = 0.4)

ggplot(data = II, aes(x = hrswork, y = ..density..)) +
  geom_density(aes(color = as.factor(treated), fill = as.factor(treated)),
               position = "identity", bins = 30, alpha = 0.4)

## And then try a first model comparing Illinois and Indiana

model1 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = II)

summary(model1)

model2 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + hrswork + ba + adv + ft,  data = II)

summary(model2)

# Matching Method ---------------------------------------------------------

## Now we can try to create a new control sample using the matching score method

## First we will check at the means of the independent variables that we are going to use

t.test(cpsgen_no$age[cpsgen_no$treated == 1], cpsgen_no$age[cpsgen_no$treated == 0])

t.test(cpsgen_no$race[cpsgen_no$treated == 1], cpsgen_no$race[cpsgen_no$treated == 0])

ggplot(data = cpsgen_no, aes(x = realhrwage, y = ..density..)) +
  geom_density(aes(color = as.factor(treated), fill = as.factor(treated)),
                 position = "identity", bins = 30, alpha = 0.4)

data_nomiss <- cpsgen_no %>% 
  na.omit()

data_nomiss$fem <- as.numeric(data_nomiss$fem)

m1 <- matchit(treated ~ year + age + fem + race + marst + sch + classwkr, method = "nearest", data = data_nomiss)
summary(m1)
# plot(m1, type = "jitter")
m1data <- match.data(m1)

wagegap2 <- function(data, x, y, z) {
  wagegap <- data %>% {{filter}}(
    sex == x &
      after == y &
      treated == z) %>% 
    {{summarise}}(mean(realhrwage, na.rm = TRUE)
    )
  return(wagegap)
}

wagegap2(m1data, 1, 1, 1)



#t.test(m1data$lnrwg[m1data$treated == 1], m1data$lnrwg[m1data$treated == 0], paired = T)

model3 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = m1data)

summary(model3)

model4 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + hrswork + ba + adv + ft,  data = m1data)

summary(model4)

stargazer(model1, model2, model3, model4, type = "text")
