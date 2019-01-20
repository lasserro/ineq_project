library(ggplot2)
library(reshape2)
library(eurostat)
library(dplyr)
install.packages("svglite")
library(svglite)


#### Gini Plot ####

gini_dispo <- get_eurostat("ilc_di12", time_format = "raw")
gini_factor <- get_eurostat("ilc_di12b", time_format = "raw")

gini_dispo <- gini_dispo %>% filter(geo == 'IE')
gini_factor <- gini_factor %>% filter(geo == 'IE')


gini_dispo[22,] <- 2002
gini_dispo <- gini_dispo[c(1:14,22,15:21),]

brks <- seq(1995, 2016, 2)

gini_dispo$grp <- 0
gini_dispo$grp[1:14] <- 2
gini_dispo$grp[15:21] <- 1

gini_dispo$grp <- as.factor(gini_dispo$grp)
gini_dispo$values[15] <- NA
gini_dispo$time <- as.numeric(gini_dispo$time)

gini <- ggplot(gini_dispo, aes(x = time, y = values)) + geom_line(aes(group = grp), colour = 'blue3', size = 1) +
  labs(color = '', x = "Jahr", y = "Gini-Koeffizient in %", 
       caption = '') +
  theme_linedraw() + ylim(28,35) +
  theme(legend.position="bottom") + scale_x_continuous(breaks = brks)
ggsave(file='./gini_ireland_1995-2016.svg',height=4,width=7)

#### P80/P20 Plot ####
p8020_dispo <- get_eurostat('tessi180', time_format = 'raw')
p8020_dispo <- p8020_dispo %>% filter(geo =='IE', sex == 'T')
p8020_dispo <- melt(p8020_dispo)
p80 <- ggplot(p8020_dispo, aes(x = time, y = value)) + geom_line(aes(group = variable), colour = 'blue3', size = 1) +
    labs(color = '', x = "Jahr", y = "P80/P20 ratio", 
       caption = '') +
  theme_linedraw() + ylim(3.5,5.5) +
  theme(legend.position="bottom")
ggsave(file='./p80p20_ireland_2005-2016.svg',height=4,width=7)

#### income qunatiles #####
meaninc <- get_eurostat('ilc_di04', time_format = 'raw')
meaninc <- meaninc %>% filter(geo == 'IE', indic_il == 'MEI_E', unit == 'EUR', 
                            hhtyp == 'TOTAL')
income_grps <- get_eurostat('ilc_di01', time_format = 'raw')
income_grps <- income_grps %>% filter(geo == 'IE', currency == 'EUR', 
                                      indic_il == 'TC', quantile == 'D1'| 
                                        quantile =='D9' |
                                        quantile == 'Q1' |
                                        quantile == 'Q2' |
                                        quantile == 'Q3' |
                                        quantile == 'Q4')

colnames(meaninc) <- colnames(income_grps)
meaninc$indic_il <- 'TC'
income_grps <- rbind(income_grps, meaninc)
income_grps[127:132,] <- NA
income_grps[127:132,1] <- c('D1', 'D9', 'Q1', 'Q2', 'Q3', 'TOTAL')
income_grps[127:132,2] <- 'TC'
income_grps[127:132,3] <- 'EUR'
income_grps[127:132,4] <- 'IE'
income_grps[127:132,5] <- 2002
# income_grps$time <- as.numeric(income_grps$time)
inc_grps <- melt(income_grps)
inc_grps$time <- as.numeric(inc_grps$time)

brks <- seq(1995,2016,2)
inc <- ggplot(inc_grps %>% filter(quantile!='TOTAL'), aes(x = time, y = value, colour = quantile)) + 
  geom_line(aes(group = quantile), size = 1) +
  geom_line(data=inc_grps %>% filter(quantile=='TOTAL'), 
            aes(x = time, y = value, group = quantile),
            linetype='dashed') +
  labs(x = 'Jahr', y = 'Einkommen ini EUR') + theme_linedraw() +
  scale_color_manual(name = '', 
                     values = c(D1 = 'red', D9 = 'yellow3', Q1 = 'blue1', Q2 = 'green',
                                Q3 = 'pink', TOTAL = 'black'),
                     labels=c("10. Perzentil", "90. Perzentil", "25. Perzentil",
                              'Median', '75. Perzentil', 'Mittelwert')) +
  theme(legend.position = 'bottom') +
  scale_x_continuous(breaks = brks)
ggsave(file='./income_qa_ireland_1995-2016.svg',height=4,width=7)

##### Income 
income <- get_eurostat('ilc_di04', time_format = 'raw')
income <- income %>% filter(geo == 'IE', indic_il == 'MEI_E', unit == 'EUR', 
                            hhtyp == 'TOTAL')

  

ggplot(inc_grps %>% filter(quantile!='TOTAL'), aes(x = time,y = value, colour = quantile)) + 
  geom_line(aes(group = quantile), size = 1) +
  geom_line(data=inc_grps %>% filter(quantile=='TOTAL'), 
            aes(y = value, group = quantile),
            linetype='dashed') +
  labs(x = 'Jahr', y = 'Einkommen ini EUR') + theme_linedraw() +
  scale_color_manual(name = '', 
                     values = c(D1 = 'red', D9 = 'yellow3', Q1 = 'blue1', Q2 = 'green',
                                Q3 = 'pink', TOTAL = 'black'),
                     labels=c("10. Perzentil", "90. Perzentil", "25. Perzentil",
                              'Median', '75. Perzentil', 'Mittelwert')) +
  theme(legend.position = 'bottom') 

###### poverty risk ####
poverty <- get_eurostat('ilc_li02', time = 'raw')
poverty <- poverty %>% filter(geo =='IE',  indic_il=='LI_R_MD60', sex =='T',
                              age=='TOTAL', unit=='PC')
poverty[22,] <- NA
poverty <- poverty[c(1:14,22,15:21),]
poverty[15,] <- poverty[14,]
poverty$time[15] <- 2002
poverty$values[15] <- NA
pov <- melt(poverty)
pov$time <- as.numeric(pov$time)
povrate <- ggplot(pov, aes(x=time, y=value)) + geom_line(aes(group = variable), colour='blue2', size=1) +
  scale_x_continuous(breaks = brks) +
  labs(color = '', x = "Jahr", y = "Armutsgefährdungsquote in %", 
       caption = '') + ylim(14.4,22) +
  theme_linedraw()
ggsave(file='./poverty_risk_ireland_1995-2016.svg',height=4,width=7)

gini
ggsave(file='./gini_ireland_1995-2016.svg',height=4,width=7)
p80povrate
ggsave(file='./P8020_ireland_2005-2016.svg',height=4,width=7)
povrate
ggsave(file='./poverty_risk_ireland_1995-2016.svg',height=4,width=7)
inc
ggsave(file='./income_quant_ireland_1995-2016.svg',height=4,width=7)
