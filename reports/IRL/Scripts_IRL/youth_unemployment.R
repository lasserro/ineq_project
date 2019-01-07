library(eurostat)
library(dplyr)
library(ggplot2)
es_toc <- get_eurostat_toc()

# tipslm80 = unemployment rate of people aged 15 - 24 as a percentage of the 
# labour force of the same age. 
ur_IRL <- get_eurostat('tipslm80', time_format = 'raw', geo = 'IE' )
ur_IRL <- ur_IRL %>% filter(geo == 'IE' )

# tespm080 = percentage of unemployed young people (i.e. people aged 15-24) in 
# the total population of this age group.
yur_ratio <- get_eurostat('tespm080', time_format = 'raw')
yur_ratio <- yur_ratio %>% filter( geo == 'IE')

# ich glaube wir sollten mit tipslm80 arbeiten

yunempl <- ur_IRL %>% filter(unit == 'PC_ACT')
plot(yunempl$time, yunempl$values, type = 'l')

