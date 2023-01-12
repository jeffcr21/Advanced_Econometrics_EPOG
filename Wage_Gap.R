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

##Exploring the data

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

## Now we can calculate the wage gap in Illinois in 2000 and 2006 (This is just for descriptive purposes)

## First, I will recode the sex variable

cpsgen_no <- cpsgen_no %>% mutate(sex = (recode(sex, '2' = '0', '1' = '1')))

wagegap <- function(data, x, y, z) {
  wagegap <- data %>% {{filter}}(
    sex == x &
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

## Now we can start preparing the necessary conditions for a fist exploratory model

## Create unique id

cpsgen_no <- cpsgen_no %>% mutate(id = paste(serial, statefip, pernum, sep = ""), .before = year)

## Treated: If Illinois == 1; Illinois == 17 in the state code classification

cpsgen_no <- cpsgen_no %>% mutate(treated = ifelse(statefip == 17, 1, 0))

#After. The implementation of the law was in 2003

cpsgen_no <- cpsgen_no %>% mutate(after = ifelse(year > 2003, 1, 0))

##Treated + post

cpsgen_no <- cpsgen_no %>% mutate(post.treated = ifelse(treated == 1 & after == 1, 1, 0))

##Treated + post + women

cpsgen_no <- cpsgen_no %>% mutate(post.treated.fem = ifelse(treated == 1 & after == 1 & sex == 0, 1, 0))

## Now, to start we can compare Illinois to Indiana, which does NOT have any policy on pay transparency. They are
## neighboring states and therefore might share some demographic characteristics. In any case, we can check that.

II <- cpsgen_no %>% filter(
  statefip == 17 | statefip == 18
)

boxplot(II$age, II$realhrwage, II$lnrwg)

model1 <- lm(lnrwg ~ treated + after + sex + post.treated + post.treated.fem, data = II)

summary(model1)

## Question: Should I use lm or plm? **

## Now we can try to create a new control sample using the matching score method

## First we will check at the means of the independent variables that we are going to use

t.test(cpsgen_no$age[cpsgen_no$treated == 1], cpsgen_no$age[cpsgen_no$treated == 0])

t.test(cpsgen_no$race[cpsgen_no$treated == 1], cpsgen_no$race[cpsgen_no$treated == 0])

ggplot(data = cpsgen_no, aes(x = realhrwage, y = ..density..)) +
  geom_density(aes(color = as.factor(treated), fill = as.factor(treated)),
                 position = "identity", bins = 30, alpha = 0.4)

data_nomiss <- cpsgen_no %>% 
  na.omit()

data_nomiss$sex <- as.numeric(data_nomiss$sex)

m1 <- matchit(treated ~ age + sex + race + marst + sch + classwkr + union, method = "nearest", data = data_nomiss)
summary(m1)
plot(m1, type = "jitter")
m1data <- match.data(m1)

t.test(m1data$lnrwg[m1data$treated == 1], m1data$lnrwg[m1data$treated == 0], paired = T)

