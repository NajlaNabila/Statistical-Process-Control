#chap 6

#Ex 6.1

attach(HardBakeData)

library(dplyr)
library(qcc)

# xbarbar
a1=HardBakeData |>
  group_by(Sample.Number) |>
  summarise_at(vars(Flow.Width), list(mean = mean))

grand.mean=mean(a1$mean)

# R bar
a2=HardBakeData %>%
  group_by(Sample.Number) %>%
  summarise_at(vars(Flow.Width), list(max=max,min=min))

range.bake=a2$max-a2$min

R.mean=mean(range.bake)

#xbar chart model
A2=0.577
lcl = grand.mean-A2*R.mean
UCL=grand.mean+A2*R.mean

#xbar chart plot
qcc(data=a1$mean,stdev=0.32521,limits=c(lcl,UCL),plot=TRUE)

