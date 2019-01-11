quant_ch_plot_p23
quant_ch_plot_p21

figure <- ggarrange(quant_ch_plot_p21, quant_ch_plot_p23,
                                        ncol = 2, nrow = 1)
###############################################################################
library(ggplot2)
library(ggpubr)
library(reshape2)


rm(list= ls()[!(ls() %in% c('P1.svy', 'P2.svy', 'silc.rph', 'silc.rph2'))])
brks <- seq(2004, 2016, 2)
setwd("~/Dokumente/University/Volkswirtschaft/Master/WS2018/Verteilung/Ireland Project/ineq_project")

table_quant_p11 <- read.csv('./reports/IRL/tables/final/table_quantiles_p11.csv')
table_quant_p12 <- read.csv('./reports/IRL/tables/final/table_quantiles_p12.csv')
table_quant_p13 <- read.csv('./reports/IRL/tables/final/table_quantiles_p13.csv')

colnames(table_quant_p11) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p12) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p13) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')

table_quant_p21 <- read.csv('./reports/IRL/tables/final/table_quantiles_p21.csv')
table_quant_p22 <- read.csv('./reports/IRL/tables/final/table_quantiles_p22.csv')
table_quant_p23 <- read.csv('./reports/IRL/tables/final/table_quantiles_p23.csv')

colnames(table_quant_p21) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p22) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p23) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')

setwd('./reports/IRL/tables/final')
#### Quantile plot p21; percentage change ######

table_quant_p21$`10th_ch` <- table_quant_p21$`10th` / table_quant_p21$`10th`[1]
table_quant_p21$`25th_ch` <- table_quant_p21$`25th` / table_quant_p21$`25th`[1]
table_quant_p21$`Median_ch` <- table_quant_p21$`Median` / table_quant_p21$`Median`[1]
table_quant_p21$`75th_ch` <- table_quant_p21$`75th` / table_quant_p21$`75th`[1]
table_quant_p21$`90th_ch` <- table_quant_p21$`90th` / table_quant_p21$`90th`[1]
table_quant_p21$`Mean_ch` <- table_quant_p21$`Mean` / table_quant_p21$`Mean`[1]

quant_ch_plot_p21 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p21$'10th_ch', x = table_quant_p21$Year,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p21$'25th_ch',x = table_quant_p21$Year,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p21$Median_ch,x = table_quant_p21$Year,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p21$'75th_ch',x = table_quant_p21$Year,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p21$'90th_ch',x = table_quant_p21$Year,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p21$'Mean_ch',x = table_quant_p21$Year,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'blue', 
                         'Median' = 'green2',
                         '75th' = 'pink',
                         '90th' = 'yellow4',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "", 
       title = "Mean and Percentiles of pre-tax factor income",
       subtitle = "partial sharing of resources",
       caption = '') +
  theme_bw()  + scale_x_continuous(breaks = brks) + theme(legend.position = 'bottom')

quant_ch_plot_p21

#### Quantile plot p23; percentage change ######

table_quant_p23$`10th_ch` <- table_quant_p23$`10th` / table_quant_p23$`10th`[1]
table_quant_p23$`25th_ch` <- table_quant_p23$`25th` / table_quant_p23$`25th`[1]
table_quant_p23$`Median_ch` <- table_quant_p23$`Median` / table_quant_p23$`Median`[1]
table_quant_p23$`75th_ch` <- table_quant_p23$`75th` / table_quant_p23$`75th`[1]
table_quant_p23$`90th_ch` <- table_quant_p23$`90th` / table_quant_p23$`90th`[1]
table_quant_p23$`Mean_ch` <- table_quant_p23$`Mean` / table_quant_p23$`Mean`[1]
write.csv(table_quant_p23, './reports/IRL/tables/table_quant_p23.csv')


quant_ch_plot_p23 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p23$'10th_ch', x = table_quant_p23$Year,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p23$'25th_ch',x = table_quant_p23$Year,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$Median_ch,x = table_quant_p23$Year,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'75th_ch',x = table_quant_p23$Year,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'90th_ch',x = table_quant_p23$Year,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'Mean_ch',x = table_quant_p23$Year,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'blue', 
                         'Median' = 'green2',
                         '75th' = 'pink',
                         '90th' = 'yellow4',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "", 
       title = "Mean and Percentiles of post-tax disposable income",
       subtitle = "partial sharing of resources", 
       caption = 'Source: Authors\' analysis (EU-SILC)') +
  theme_bw()  + scale_x_continuous(breaks = brks) 

quant_ch_plot_p23
ggarrange(quant_ch_plot_p21, quant_ch_plot_p23, common.legend = T, legend = 'bottom')

#### Quantile plot p11; percentage change ######

table_quant_p11$`10th_ch` <- table_quant_p11$`10th` / table_quant_p11$`10th`[1]
table_quant_p11$`25th_ch` <- table_quant_p11$`25th` / table_quant_p11$`25th`[1]
table_quant_p11$`Median_ch` <- table_quant_p11$`Median` / table_quant_p11$`Median`[1]
table_quant_p11$`75th_ch` <- table_quant_p11$`75th` / table_quant_p11$`75th`[1]
table_quant_p11$`90th_ch` <- table_quant_p11$`90th` / table_quant_p11$`90th`[1]
table_quant_p11$`Mean_ch` <- table_quant_p11$`Mean` / table_quant_p11$`Mean`[1]
write.csv(table_quant_p11, './reports/IRL/tables/table_quant_p11.csv')


quant_ch_plot_p11 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p11$'10th_ch', x = table_quant_p11$Year,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p11$'25th_ch',x = table_quant_p11$Year,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$Median_ch,x = table_quant_p11$Year,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'75th_ch',x = table_quant_p11$Year,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'90th_ch',x = table_quant_p11$Year,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'Mean_ch',x = table_quant_p11$Year,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'blue', 
                         'Median' = 'green2',
                         '75th' = 'pink',
                         '90th' = 'yellow4',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "", 
       title = "Mean and Percentiles of pre-tax factor income",
       subtitle = "equal sharing of resources within household",
       caption = '') +
  theme_bw()  + scale_x_continuous(breaks = brks) + theme(legend.position = 'bottom')

quant_ch_plot_p11

#### Quantile plot p13; percentage change ######

table_quant_p13$`10th_ch` <- table_quant_p13$`10th` / table_quant_p13$`10th`[1]
table_quant_p13$`25th_ch` <- table_quant_p13$`25th` / table_quant_p13$`25th`[1]
table_quant_p13$`Median_ch` <- table_quant_p13$`Median` / table_quant_p13$`Median`[1]
table_quant_p13$`75th_ch` <- table_quant_p13$`75th` / table_quant_p13$`75th`[1]
table_quant_p13$`90th_ch` <- table_quant_p13$`90th` / table_quant_p13$`90th`[1]
table_quant_p13$`Mean_ch` <- table_quant_p13$`Mean` / table_quant_p13$`Mean`[1]
write.csv(table_quant_p13, './reports/IRL/tables/table_quant_p13.csv')


quant_ch_plot_p13 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p13$'10th_ch', x = table_quant_p13$Year,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p13$'25th_ch',x = table_quant_p13$Year,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$Median_ch,x = table_quant_p13$Year,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'75th_ch',x = table_quant_p13$Year,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'90th_ch',x = table_quant_p13$Year,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'Mean_ch',x = table_quant_p13$Year,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'blue', 
                         'Median' = 'green2',
                         '75th' = 'pink',
                         '90th' = 'yellow4',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "", 
       title = "Mean and Percentiles of post-tax disposable income",
       subtitle = "equal sharing of resources within household",
       caption = 'Source: Authors\' analysis (EU-SILC)') +
  theme_bw()  + scale_x_continuous(breaks = brks) 
quant_ch_plot_p13
ggarrange(quant_ch_plot_p11, quant_ch_plot_p13, common.legend = T, legend = 'bottom')


### Density plots ####
df04 <- silc.rph %>% filter(rb010=='2004') %>% 
  select(income_p11, income_p12, income_p13) %>% 
  rename('Pre-tax factor income' = income_p11, 'Pre-tax national income' = income_p12,
         'Post-tax disposable income' = income_p13)
df07 <- silc.rph %>% filter(rb010=='2007') %>% 
  select(income_p11, income_p12, income_p13) %>% 
  rename('Pre-tax factor income' = income_p11, 'Pre-tax national income' = income_p12,
         'Post-tax disposable income' = income_p13)
df12 <- silc.rph %>% filter(rb010=='2012') %>% 
  select(income_p11, income_p12, income_p13) %>% 
  rename('Pre-tax factor income' = income_p11, 'Pre-tax national income' = income_p12,
         'Post-tax disposable income' = income_p13)
df16 <- silc.rph %>% filter(rb010=='2016') %>% 
  select(income_p11, income_p12, income_p13) %>% 
  rename('Pre-tax factor income' = income_p11, 'Pre-tax national income' = income_p12,
         'Post-tax disposable income' = income_p13)

##### income density 2012 P1
df <- melt(df12[,c(1,3,4,5)])

plot2 <- ggplot(df, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2012",
       subtitle = "equal sharing of resources within household", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot2

##### income density 2004 P1
df2 <- melt(df04[,c(1,3,4,5)])

plot3 <- ggplot(df2, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2004",
       subtitle = "equal sharing of resources within household", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot3
##### income density 2016 P1
df3 <- melt(df16[,c(1,3,4,5)])

plot4 <- ggplot(df3, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2016",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: Authors\' analysis (EU-SILC)') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot4
##### income density 2007 P1
df4 <- melt(df07[,c(1,3,4,5)])

plot5 <- ggplot(df4, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2007",
       subtitle = "equal sharing of resources within household", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot5

ggarrange(plot3, plot5, plot2, plot4, nrow = 2, ncol = 2, common.legend = T,
          legend = 'bottom')

### Density plots p2 ####
df04 <- silc.rph2 %>% filter(rb010=='2004') %>% 
  select(income_p21, income_p22, income_p23) %>% 
  rename('Pre-tax factor income' = income_p21, 'Pre-tax national income' = income_p22,
         'Post-tax disposable income' = income_p23)
df07 <- silc.rph2 %>% filter(rb010=='2007') %>% 
  select(income_p21, income_p22, income_p23) %>% 
  rename('Pre-tax factor income' = income_p21, 'Pre-tax national income' = income_p22,
         'Post-tax disposable income' = income_p23)
df12 <- silc.rph2 %>% filter(rb010=='2012') %>% 
  select(income_p21, income_p22, income_p23) %>% 
  rename('Pre-tax factor income' = income_p21, 'Pre-tax national income' = income_p22,
         'Post-tax disposable income' = income_p23)
df16 <- silc.rph2 %>% filter(rb010=='2016') %>% 
  select(income_p21, income_p22, income_p23) %>% 
  rename('Pre-tax factor income' = income_p21, 'Pre-tax national income' = income_p22,
         'Post-tax disposable income' = income_p23)

##### income density 2012 P2
df <- melt(df12[,c(1,3,4,5)])

plot2 <- ggplot(df, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2012",
       subtitle = "partial sharing of resources", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot2

##### income density 2004 P2
df2 <- melt(df04[,c(1,3,4,5)])

plot3 <- ggplot(df2, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2004",
       subtitle = "partial sharing of resources", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot3
##### income density 2016 P2
df3 <- melt(df16[,c(1,3,4,5)])

plot4 <- ggplot(df3, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2016",
       subtitle = "partial sharing of resources", 
       caption = 'Source: Authors\' analysis (EU-SILC)') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot4
##### income density 2007 P2
df4 <- melt(df07[,c(1,3,4,5)])

plot5 <- ggplot(df4, aes(value, lty = variable)) + geom_density(show.legend = F) +
  stat_density(aes(value, color = variable), geom = 'line', position = 'identity') +
  xlim(0,150000) + theme_bw() +
  labs(x = "income", y = "density", 
       title = "Income density, 2007",
       subtitle = "partial sharing of resources", 
       caption = '') + 
  theme(legend.position = 'bottom', legend.title = element_blank())

plot5

ggarrange(plot3, plot5, plot2, plot4, nrow = 2, ncol = 2, common.legend = T,
          legend = 'bottom')
