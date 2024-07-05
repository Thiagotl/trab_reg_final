
library(hnp) # pacote para envelope simulado
library(lmtest) # teste reset
library(car) # para teste de multicolinearidade (fatores de inflacao de variancia)
library(tseries)
library(tidyverse)

library(readr)
wine_data_red <- read_csv("winequality-red.csv")

View(wine_data_red)

glimpse(wine_data_red)
attach(wine_data_red)

str(wine_data_red)


summary(wine_data_red)

fit<- lm(alcohol ~ . -quality, data = wine_data_red)
summary(fit)

