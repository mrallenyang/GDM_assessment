Allen= read.csv("Allen_Population.csv", header=T, na.string=".")

Allen


attach(Allen)
names(Allen)
summary(Allen)


library(nlme)
require(lme4)


Allen_model <- lmer(Yield~Variety + (1|Env) + (1|Env:Variety), Allen)

Allen_model


