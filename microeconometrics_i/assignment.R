library(tidyverse)
library(haven)
library(plm)
library(lmtest)

#################### Setup ####################

df <- haven::read_dta("/Users/gabrieldiasmp/Documents/pasta_gabriel/codigo/econometrics/microeconometrics I/assignment_1.dta")

df_final <- df %>% select(
  "county",
  "year",
  "lcrmrte", 
  "lprbconv", 
  "lprbarr", 
  "lavgsen", 
  "lpolpc",
  "ldensity",
  'taxpc', 
  'west', 
  'central', 
  'urban'
)

#################### Fixed effects model ####################

fixed <- plm(lcrmrte ~ lprbconv + lprbarr + lavgsen + 
               lpolpc + ldensity + taxpc + factor(county) + factor(year),
             data=df, index=c("county", "year"), model="within", effect="twoways")

summary(fixed)

## Breusch-Godfrey/Wooldridge test for serial correlation in panel models
pbgtest(fixed, order = 1)

## Result -> alternative hypothesis: serial correlation in idiosyncratic errors

## Testing for heteroskedasticity
test <- plmtest(fixed, effect = "individual")

## Robust standard errors for heteroskedasticity and serial correlation
coeftest(fixed, vcovHC(fixed, method = "arellano"))


#################### Random effects model ####################

random <- plm(lcrmrte ~ lprbconv + lprbarr + lavgsen + 
               lpolpc + ldensity + taxpc + west + central + urban + factor(year),
             data=df, index=c("county", "year"), model="random")

summary(random)

#################### Tests ####################

## Hausman test
phtest(fixed, random)

## Breusch-Godfrey/Wooldridge test for serial correlation in panel models
pbgtest(fixed, order = 1)

# Heteroskedasticity test
bptest(fixed, studentize=F)

