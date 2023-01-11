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

## Loading the data

cpsgen <- read_csv("Assignment/cpsgen.csv")