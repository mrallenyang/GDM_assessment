library(ggplot2)
library(tidyr)


mds <- read.csv('MDS.csv')
mds_full <- mds
colnames(mds)[2] <- c('days')


mds_gather <- gather(mds, key = 'rm', value = 'yield', 3:6, factor_key = T)
mds_gather <- as.Date(m)

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_line()+
  geom_smooth(method='lm',se=F)

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_boxplot()


mds$date <- as.Date(mds$date)

mds <- mds %>%
  mutate(day_avg = mean(mds$RM.E4, mds$RM.E5, mds$RM.L4, mds$RM.M4))

# convert data into ts
mds_ts <- ts(mds)
mds_xts <- xts(mds[,c(1,3,4,5,6)], order.by = mds$date)

ggplot(data = mds_gather, aes(x=days, y=yield, color = rm))+
  geom_boxplot()
