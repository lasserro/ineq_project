##### quantile plot p13 ####


quant_p13_10 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                  quantile = 0.1, ci=T)
quant_p13_25 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.25, ci=T)
quant_p13_50 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.5, ci=T)
quant_p13_75 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.75, ci=T)
quant_p13_90 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.9, ci=T)
mean_p13 <- svyby(~income_p13, ~rb010, P1.svy, svymean)

table_quant_p13 <- data.frame(quant_p13_10$rb010, quant_p13_10$income_p13,
                              quant_p13_25$income_p13, quant_p13_50$income_p13,
                              quant_p13_75$income_p13, quant_p13_90$income_p13,
                              mean_p13$income_p13)
colnames(table_quant_p13) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
#write.csv(table_quant_p13, './reports/IRL/tables/table_quantiles_p13.csv')
#table_quant_p13 <- read.csv('./reports/IRL/tables/table_quantiles_p13.csv')

brks <- seq(2004, 2016, 1)
quant_plot_p13 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p13$'10%', x = table_quant_p13$Jahr,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p13$'25%',x = table_quant_p13$Jahr,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$Median,x = table_quant_p13$Jahr,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'75%',x = table_quant_p13$Jahr,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'90%',x = table_quant_p13$Jahr,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'Mean',x = table_quant_p13$Jahr,
                          color = "Mean"), 
            size = 1) +
 scale_color_manual(values = 
                                             c('10th' = 'red', 
                                               '25th' = 'darkblue', 
                                               'Median' = 'darkgreen',
                                               '75th' = 'black',
                                               '90th' = 'yellow',
                                               'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Income Quantiles",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
quant_plot_p13

###### Quantile Plot p23 #####

quant_p23_10 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                      quantile = 0.1, ci=T)
quant_p23_25 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                      quantile = 0.25, ci=T)
quant_p23_50 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                      quantile = 0.5, ci=T)
quant_p23_75 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                      quantile = 0.75, ci=T)
quant_p23_90 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                      quantile = 0.9, ci=T)
mean_p23 <- svyby(~income_p23, ~rb010, P2.svy, svymean)

table_quant_p23 <- data.frame(quant_p23_10$rb010, quant_p23_10$income_p23,
                              quant_p23_25$income_p23, quant_p23_50$income_p23,
                              quant_p23_75$income_p23, quant_p23_90$income_p23,
                              mean_p23$income_p23)
colnames(table_quant_p23) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
write.csv(table_quant_p23, './reports/IRL/tables/table_quantiles_p23.csv')
table_quant_p23 <- read.csv('./reports/IRL/tables/table_quantiles_p23.csv')





quant_plot_p23 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p23$'10%', x = table_quant_p23$Jahr,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p23$'25%',x = table_quant_p23$Jahr,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$Median,x = table_quant_p23$Jahr,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'75%',x = table_quant_p23$Jahr,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'90%',x = table_quant_p23$Jahr,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p23$'Mean',x = table_quant_p23$Jahr,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'darkblue', 
                         'Median' = 'darkgreen',
                         '75th' = 'black',
                         '90th' = 'yellow',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Income Quantiles",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
quant_plot_p23

###### Quantile Plot p11 #####


quant_p11_10 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.1, ci=T)
quant_p11_25 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.25, ci=T)
quant_p11_50 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.5, ci=T)
quant_p11_75 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.75, ci=T)
quant_p11_90 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                      quantile = 0.9, ci=T)
mean_p11 <- svyby(~income_p11, ~rb010, P1.svy, svymean)

table_quant_p11 <- data.frame(quant_p11_10$rb010, quant_p11_10$income_p11,
                              quant_p11_25$income_p11, quant_p11_50$income_p11,
                              quant_p11_75$income_p11, quant_p11_90$income_p11,
                              mean_p11$income_p11)
colnames(table_quant_p11) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
#write.csv(table_quant_p11, './reports/IRL/tables/table_quantiles_p11.csv')
#table_quant_p11 <- read.csv('./reports/IRL/tables/table_quantiles_p11.csv')

brks <- seq(2004, 2016, 1)
quant_plot_p11 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p11$'10%', x = table_quant_p11$Jahr,
                          color = "10th"),
            size = 1 ) +
  geom_line(mapping = aes(y = table_quant_p11$'25%',x = table_quant_p11$Jahr,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$Median,x = table_quant_p11$Jahr,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'75%',x = table_quant_p11$Jahr,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'90%',x = table_quant_p11$Jahr,
                          color = "90th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p11$'Mean',x = table_quant_p11$Jahr,
                          color = "Mean"), 
            size = 1) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'darkblue', 
                         'Median' = 'darkgreen',
                         '75th' = 'black',
                         '90th' = 'yellow',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Income Quantiles (factor)",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
quant_plot_p11

#### Quantile plot p13; percentage change ######

table_quant_p13$`10%_ch` <- table_quant_p13$`10%` / table_quant_p13$`10%`[1]*100
table_quant_p13$`25%_ch` <- table_quant_p13$`25%` / table_quant_p13$`25%`[1]*100
table_quant_p13$`Median_ch` <- table_quant_p13$`Median` / table_quant_p13$`Median`[1]*100
table_quant_p13$`75%_ch` <- table_quant_p13$`75%` / table_quant_p13$`75%`[1]*100
table_quant_p13$`90%_ch` <- table_quant_p13$`90%` / table_quant_p13$`90%`[1]*100
table_quant_p13$`Mean_ch` <- table_quant_p13$`Mean` / table_quant_p13$`Mean`[1]*100





brks <- seq(2004, 2016, 1)
quant_ch_plot_p13 <- ggplot() +
  geom_line(mapping = aes(y = table_quant_p13$'10%_ch', x = table_quant_p13$Jahr,
                          color = "10th"),
            size = 1.2 ) +
  geom_line(mapping = aes(y = table_quant_p13$'25%_ch',x = table_quant_p13$Jahr,
                          color = "25th"), size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$Median_ch,x = table_quant_p13$Jahr,
                          color = "Median"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'75%_ch',x = table_quant_p13$Jahr,
                          color = "75th"), 
            size = 1) +
  geom_line(mapping = aes(y = table_quant_p13$'90%_ch',x = table_quant_p13$Jahr,
                          color = "90th"), 
            size = 1.2) +
  geom_line(mapping = aes(y = table_quant_p13$'Mean_ch',x = table_quant_p13$Jahr,
                          color = "Mean"), 
            size = 1.2) +
  scale_color_manual(values = 
                       c('10th' = 'red', 
                         '25th' = 'blue', 
                         'Median' = 'green2',
                         '75th' = 'pink',
                         '90th' = 'yellow4',
                         'Mean' = 'black')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Income Quantiles; Percentage change",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw()  + scale_x_continuous(breaks = brks)
quant_ch_plot_p13



