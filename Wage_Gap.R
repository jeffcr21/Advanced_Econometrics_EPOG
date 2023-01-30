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
library(stargazer)


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

glimpse(cpsgen_no)

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

## Parallel trends

trend <- II %>% group_by(year, treated) %>% 
  summarise(wage = mean(realhrwage))

ggplot(data = trend, aes(x = year, y = wage)) +
  geom_line(aes(color = as.factor(treated)),  size = 2) +
  geom_vline(xintercept = 2003, linetype = "dotted", color = "blue", size = 1.5) +
  labs(title = "Trends", x= "Year", y = "Real hour wage") +
  scale_color_discrete(name = "Treated")

trend2 <- II %>% group_by(year, treated) %>% 
  summarise(wage = mean(lnrwg))

ggplot(data = trend2, aes(x = year, y = wage)) +
  geom_line(aes(color = as.factor(treated)),  size = 2) +
  geom_vline(xintercept = 2003, linetype = "dotted", color = "blue", size = 1.5) +
  labs(title = "Trends", x= "Year", y = "Real hour wage") +
  scale_color_discrete(name = "Treated")

## And then try a first model comparing Illinois and its neibouring states

model1.1 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = II)

summary(model1.1)

model1.2 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + hrswork + ba + adv + ft,  data = II)

summary(model1.2)

## We can also test whether the policy had an effect on the number of hours worked:

model1.3 <- lm(hrswork ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = II)

summary(model1.3)

model1.4 <- lm(hrswork ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + ba + adv + ft, data = II)

summary(model1.4)

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

model2.1 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = m1data)

summary(model2.1)

model2.2 <- lm(lnrwg ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + hrswork + ba + adv + ft,  data = m1data)

summary(model2.2)

stargazer(model1.1, model1.2, model2.1, model2.2, type = "text")

#Test the effect on the number of hours worked

model2.3 <- lm(hrswork ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem,  data = m1data)

summary(model2.3)

model2.4 <- lm(hrswork ~ treated + post + fem + post.treated + treated.fem + post.fem + post.treated.fem + age + race + ba + adv + ft,  data = m1data)

summary(model2.4)

stargazer(model1.3, model1.4, model2.3, model2.4, type = "text")


# Synthetic control -------------------------------------------------------

library(Synth)
library(tidysynth)

synt <- cpsgen_no %>% group_by(year, fem, statefip)

synt <- synt %>% summarise(
  age = mean(age),
  realwage = mean(realhrwage),
  edu = mean(sch),
  hrswork = mean(hrswork),
  lnrwg = mean(lnrwg)
)

synt1 <- synt %>% filter(fem == 0)
synt2 <- synt %>% filter(fem == 1)

wgap <- synt1$realwage - synt2$realwage

synt2$wgap <- wgap

synt2 <- synt2[-2]

synt2 <- synt2 %>% mutate(
  treated = ifelse(statefip == 17, 1, 0),
  post = ifelse(year > 2003, 1, 0)
)

synt2 <- synt2 %>% mutate(
  post.treated = ifelse(treated == 1 & post == 1, 1, 0)
)

synt2$statefip <- as.integer(synt2$statefip)

dataprep.out <- dataprep(foo = synt2,
         predictors = c("age", "edu", "lnrwg"),
         dependent = "wgap",
         unit.variable = "statefip",
         time.variable = "year",
         treatment.identifier = 17,
         controls.identifier = c(1:16, 18:56),
         time.predictors.prior = c(2000:2003),
         time.optimize.ssr = c(2000:2006),
         time.plot = c(2000-2006)
         )

synt.out <- synt2 %>% 
  synthetic_control(
    outcome = wgap,
    unit = statefip,
    time = year,
    i_unit = 17,
    i_time = 2003,
    generate_placebos = F
    ) %>% 
  generate_predictor(
    time_window = 2000:2003,
    age = mean(age, na.rm = T),
    edu = mean(edu, na.rm = T),
    lnrwg = mean(lnrwg, na.rm = T)
  ) %>% 
  generate_weights(optimization_window = 2000:2003,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
                   ) %>% 
  generate_control()

synt.out %>% plot_trends()

synt.out %>% plot_differences()

synt.out %>% grab_balance_table()

synt.out %>% grab_signficance()

synt.out %>% grab_outcome()


## This is the trend for the wage gap in Illinois versus neighbor states

synt2 <- synt2 %>% 
  mutate(treated = ifelse(statefip == 17, 1, 0))

trends2 <- synt2 %>% filter(statefip == 17 | statefip == 18 | statefip == 19 | statefip == 29 | statefip == 55) %>% 
  group_by(year, treated) %>% 
  summarise(realwage = mean(realwage),
            lnrwg = mean(lnrwg),
            wgap = mean(wgap))

ggplot(data = trends2, aes(x = year, y = wgap)) +
  geom_line(aes(color = as.factor(treated)),  size = 2) +
  geom_vline(xintercept = 2003, linetype = "dotted", color = "blue", size = 1.5) +
  labs(title = "Wage gap in Illinois and nighbour States", x= "Year", y = "Wage Gap (in US$)") +
  scale_color_discrete(name = "Treated")
