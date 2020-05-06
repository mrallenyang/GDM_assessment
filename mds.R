library(ggplot2)
library(tidyr)
library(xts)
library(forecast)
library(PerformanceAnalytics)

mds <- read.csv('MDS.csv')
mds_full <- mds
colnames(mds)[2] <- c('days')


mds_gather <- gather(mds, key = 'rm', value = 'yield', 3:6, factor_key = T)
mds_gather <- as.Date(m)
t_mds_gather<- t(mds_gather)

mds.mean <- colMeans(mds[,3:6])
summary(mds[,3:6])
plot(x=mds)

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_line()+
  geom_smooth(method='lm',se=F)
ggplot(data = mds_gather, aes(x=date,y=rm))+
  geom_bar()

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_boxplot()

xyplot(x=mds_gather$date,y=mds_gather$rm)

mds$date <- as.Date(mds$date)

mds <- mds %>%
  mutate(day_avg = mean(mds$RM.E4, mds$RM.E5, mds$RM.L4, mds$RM.M4))

# convert data into ts
mds_ts <- ts(mds)
mds_xts <- xts(mds[,c(1,3,4,5,6)], order.by = mds$date)

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_boxplot()
