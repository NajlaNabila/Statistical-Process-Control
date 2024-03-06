#chap 6

#Ex 6.1

attach(HardBakeData)

library(dplyr)

a1=HardBakeData %>%
  group_by(Sample.Number) %>%
  summarise_at(vars(Flow.Width), list(mean = mean))
 
grand.mean=mean(a1$mean)

a2=HardBakeData %>%
  group_by(Sample.Number) %>%
  summarise_at(vars(Flow.Width), list(max=max,min=min))

range.bake=aa$max-aa$min

R.mean=mean(range.bake)

