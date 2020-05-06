library(lme4)
library(lmerTest)
library(lsmeans)
library(reshape2)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(MASS)
library(tidyverse)
library(tidyr)


pop <- read.csv('Population.csv')
pop <- pop[1:9]
pop$Population <- as.factor(pop$Population)
# > str(pop)
# 'data.frame':	420 obs. of  9 variables:
#   $ Season           : Factor w/ 4 levels "2009-10","2010-11",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Locations        : Factor w/ 10 levels "Bouquet","Bustinza",..: 10 10 10 10 10 10 10 10 10 10 ...
# $ Variety          : Factor w/ 7 levels "DM 3810","DM 4210",..: 4 4 4 4 4 4 4 4 4 4 ...
# $ Population       : Factor w/ 4 levels "15","25","35",..: 1 2 3 4 1 2 3 4 1 2 ...
# $ RM               : Factor w/ 5 levels "Early 4","Early 5",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ Final.Plant.Acres: int  60000 100000 140000 180000 60000 100000 140000 180000 60000 100000 ...
# $ Repetition       : Factor w/ 3 levels "I","II","III": 1 1 1 1 2 2 2 2 3 3 ...
# $ REND             : int  4384 4438 4379 4047 4206 4438 4260 4166 4206 4497 ...
# $ Yield            : num  65.4 66.2 65.4 60.4 62.8 66.2 63.6 62.2 62.8 67.1 ...

# all variety names
# > levels(pop$Variety)
# [1] "DM 3810" "DM 4210" "DM 4612" "DM 4670" "DM 4712" "DM 4970" "DM 5.1" 



#1 best population per variety
var <- lm(Yield ~ Population*Variety, data = pop)
summary(var) 
## DM 3810: Population 45
## DM 4210: Population 25
## DM 4612: Population 15
## DM 4670: Population 25
## DM 4712: Population 45
## DM 4970: Population 25
## DM 5.1: Population 25


# plot best population per variety
ggplot(data = pop, aes(x = Population, y = Yield))+
  geom_boxplot()+
  facet_wrap(~ Variety)

pop %>%
  ggplot()+
  aes(x=Variety, color = Population, group = Population, y=Yield)+
  stat_summary(fun = mean, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')




#2 best population per RM
rm <- lm(Yield ~ Population*RM, data = pop)
summary(rm)
## Early 4: Population 25
## Early 5: Population 25
## Late 3: Population 35
## Late 4: Population 35
## Medium 4: Population 35

# plot best population per rm
ggplot(data = pop, aes(x = Population, y = Yield))+
  geom_boxplot()+
  facet_wrap(~ RM)

pop %>%
  ggplot()+
  aes(x=RM, color = Population, group = Population, y=Yield)+
  stat_summary(fun = mean, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')



#3 create environment variable
pop <- pop %>%
  mutate(enviornment = case_when(Yield > 75 ~ 'high',
                                 (Yield <= 75 & Yield >= 60) ~'medium',
                                 Yield < 60 ~ 'low'))

# best population per RM in high environment
high_rm <- lm(Yield ~ Population*RM, data = pop[which(pop$enviornment=='high'),])
summary(high_rm)
## Early 4: Population 45
## Early 5: Only Population 25 exceed 75 B/A
## Late 3: Population 35
## Late 4: Population 35
## Medium 4: Population 15

# plot best population per rm in high environment
ggplot(data = pop[which(pop$enviornment=='high'),], aes(x = Population, y = Yield))+
  geom_boxplot()+
  labs(title = 'Best population per RM in High environment')+
  facet_wrap(~ RM)

pop[which(pop$enviornment=='high'),] %>%
  ggplot()+
  aes(x=RM, color = Population, group = Population, y=Yield)+
  stat_summary(fun = mean, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')


# best population per RM in medium environment
med_rm <- lm(Yield ~ Population*RM, data = pop[which(pop$enviornment=='medium'),])
summary(med_rm)
## Early 4: Population 45
## Early 5: Populaation 15
## Late 3: Population 15
## Late 4: Population 35
## Medium 4: Population 15

# plot best population per rm in medium environment
ggplot(data = pop[which(pop$enviornment=='medium'),], aes(x = Population, y = Yield))+
  geom_boxplot()+
  labs(title = 'Best population per RM in Medium environment')+
  facet_wrap(~ RM)

pop[which(pop$enviornment=='medium'),] %>%
  ggplot()+
  aes(x=RM, color = Population, group = Population, y=Yield)+
  stat_summary(fun = mean, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')


# best population per RM in low environment
low_rm <- lm(Yield ~ Population*RM, data = pop[which(pop$enviornment=='low'),])
summary(low_rm)
## Early 4: N/A
## Early 5: N/A
## Late 3: Population 45
## Late 4: Population 25 or 35
## Medium 4: Population 25

# plot best population per rm in low environment
ggplot(data = pop[which(pop$enviornment=='low'),], aes(x = Population, y = Yield))+
  geom_boxplot()+
  labs(title = 'Best population per RM in Low environment')+
  facet_wrap(~ RM)

pop[which(pop$enviornment=='low'),] %>%
  ggplot()+
  aes(x=RM, color = Population, group = Population, y=Yield)+
  stat_summary(fun = mean, geom = 'point')+
  stat_summary(fun = mean, geom = 'line')








