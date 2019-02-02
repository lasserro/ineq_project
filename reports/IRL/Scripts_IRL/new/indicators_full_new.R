# ------------------------------------------------------------------------
#
# Indicators R-Script
# Autoren: Lasser & Reiter
# Datum: 
#
# -------------------------------------------------------------------------
# rm(list= ls()[!(ls() %in% c('P1.svy', 'P2.svy', 'silc.rph', 'silc.rph2', 'inflation'))])

################################################################################
################## 0. inflation ################################################
inflation$time <- as.integer(inflation$time)
silc.rph <- left_join(silc.rph, inflation, by = c('rb010' = 'time'))
silc.rph <- silc.rph %>% mutate(income_p11 = income_p11/values,
                                  income_p12 = income_p12/values,
                                  income_p13 = income_p13/values)

silc.rph2 <- left_join(silc.rph2, inflation, by = c('rb010' = 'time'))
silc.rph2 <- silc.rph2 %>% mutate(income_p21 = income_p21/values,
                                income_p22 = income_p22/values,
                                income_p23 = income_p23/values)
################################################################################
################## 1. Creating Survey Objects ##################################

P1.svy <- svydesign(ids =  ~ id_p,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.rph) %>% convey_prep()

P2.svy <- svydesign(ids = ~id_p,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.rph2) %>% convey_prep()


################################################################################
#################### 2. Eurostat Indicators ####################################

### 2.1 pre-tax factor income

mean_p11 <- svyby(~income_p11, ~rb010, P1.svy, svymean)
mean_p11$income_p11 <- mean_p11$income_p11 

median_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p11$income_p11 <-  median_p11$income_p11 

gini_p11 <- svyby(~income_p11, ~rb010, P1.svy, svygini)
# p8020_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyqsr)
# p9010_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyqsr, alpha1=0.1)


top10_svy_p11 <- subset(P1.svy, income_p11 >= as.numeric(
  svyquantile(~income_p11, P1.svy, quantile=c(0.9))))
top10num_p11 <- svyby(~income_p11, ~rb010, top10_svy_p11, svytotal)
top10den_p11 <- svyby(~income_p11, ~rb010, P1.svy, svytotal)
top10_p11 <- top10num_p11 / top10den_p11

table_p11 <- data.frame(mean_p11$rb010, mean_p11$income_p11, median_p11$income_p11,
                        gini_p11$income_p11, top10_p11$income_p11)
colnames(table_p11) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p11, './reports/IRL/tables/final/table_p11_new0.csv')
### 2.2 pre-tax national income

mean_p12 <- svyby(~income_p12, ~rb010, P1.svy, svymean)
mean_p12$income_p12 <- mean_p12$income_p12 

median_p12 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p12$income_p12 <-  median_p12$income_p12 

gini_p12 <- svyby(~income_p12, ~rb010, P1.svy, svygini)
# p8020_p12 <- svyby(~income_p12, ~rb010, P1.svy, svyqsr)
# p9010_p12 <- svyby(~income_p12, ~rb010, P1.svy, svyqsr, alpha1=0.1)

top10_svy_p12 <- subset(P1.svy, income_p12 >= as.numeric(
  svyquantile(~income_p12, P1.svy, quantile=c(0.9))))
top10num_p12 <- svyby(~income_p12, ~rb010, top10_svy_p12, svytotal)
top10den_p12 <- svyby(~income_p12, ~rb010, P1.svy, svytotal)
top10_p12 <- top10num_p12 / top10den_p12

table_p12 <- data.frame(mean_p12$rb010, mean_p12$income_p12, median_p12$income_p12,
                        gini_p12$income_p12, top10_p12$income_p12)
colnames(table_p12) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p12, './reports/IRL/tables/final/table_p12_new0.csv')

### 2.3 post-tax disposable income

mean_p13 <- svyby(~income_p13, ~rb010, P1.svy, svymean)
mean_p13$income_p13 <- mean_p13$income_p13 

median_p13 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p13$income_p13 <-  median_p13$income_p13 

gini_p13 <- svyby(~income_p13, ~rb010, P1.svy, svygini)
# p8020_p13 <- svyby(~income_p13, ~rb010, P1.svy, svyqsr)
# p9010_p13 <- svyby(~income_p13, ~rb010, P1.svy, svyqsr, alpha1=0.1)

top10_svy_p13 <- subset(P1.svy, income_p13 >= as.numeric(
  svyquantile(~income_p13, P1.svy, quantile=c(0.9))))
top10num_p13 <- svyby(~income_p13, ~rb010, top10_svy_p13, svytotal)
top10den_p13 <- svyby(~income_p13, ~rb010, P1.svy, svytotal)
top10_p13 <- top10num_p13 / top10den_p13

table_p13 <- data.frame(mean_p13$rb010, mean_p13$income_p13, median_p13$income_p13,
                        gini_p13$income_p13, top10_p13$income_p13)
colnames(table_p13) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p13, './reports/IRL/tables/final/table_p13_new0.csv')

################################################################################
########################## 3. wid Indicators####################################

### 3.1 pre-tax factor income

mean_p21 <- svyby(~income_p21, ~rb010, P2.svy, svymean)
mean_p21$income_p21 <- mean_p21$income_p21 

median_p21 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p21$income_p21 <-  median_p21$income_p21 

gini_p21 <- svyby(~income_p21, ~rb010, P2.svy, svygini)
# p8020_p21 <- svyby(~income_p21, ~rb010, P2.svy, svyqsr)
# p9010_p21 <- svyby(~income_p21, ~rb010, P2.svy, svyqsr, alpha1=0.1)

top10_svy_p21 <- subset(P2.svy, income_p21 >= as.numeric(
  svyquantile(~income_p21, P2.svy, quantile=c(0.9))))
top10num_p21 <- svyby(~income_p21, ~rb010, top10_svy_p21, svytotal)
top10den_p21 <- svyby(~income_p21, ~rb010, P2.svy, svytotal)
top10_p21 <- top10num_p21 / top10den_p21

table_p21 <- data.frame(mean_p21$rb010, mean_p21$income_p21, median_p21$income_p21,
                        gini_p21$income_p21, top10_p21$income_p21)
colnames(table_p21) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p21, './reports/IRL/tables/final/table_p21_new0.csv')

### 3.2 pre-tax national income

mean_p22 <- svyby(~income_p22, ~rb010, P2.svy, svymean)
mean_p22$income_p22 <- mean_p22$income_p22 

median_p22 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p22$income_p22 <-  median_p22$income_p22 

gini_p22 <- svyby(~income_p22, ~rb010, P2.svy, svygini)
# p8020_p22 <- svyby(~income_p22, ~rb010, P2.svy, svyqsr)
# p9010_p22 <- svyby(~income_p22, ~rb010, P2.svy, svyqsr, alpha1=0.1)


top10_svy_p22 <- subset(P2.svy, income_p22 >= as.numeric(
  svyquantile(~income_p22, P2.svy, quantile=c(0.9))))
top10num_p22 <- svyby(~income_p22, ~rb010, top10_svy_p22, svytotal)
top10den_p22 <- svyby(~income_p22, ~rb010, P2.svy, svytotal)
top10_p22 <- top10num_p22 / top10den_p22

table_p22 <- data.frame(mean_p22$rb010, mean_p22$income_p22, median_p22$income_p22,
                        gini_p22$income_p22, top10_p22$income_p22)
colnames(table_p22) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p22, './reports/IRL/tables/final/table_p22_new0.csv')

### 3.3 post-tax dispo income

mean_p23 <- svyby(~income_p23, ~rb010, P2.svy, svymean)
mean_p23$income_p23 <- mean_p23$income_p23 

median_p23 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p23$income_p23 <-  median_p23$income_p23 

gini_p23 <- svyby(~income_p23, ~rb010, P2.svy, svygini)
# p8020_p23 <- svyby(~income_p23, ~rb010, P2.svy, svyqsr)
# p9010_p23 <- svyby(~income_p23, ~rb010, P2.svy, svyqsr, alpha1=0.1)


top10_svy_p23 <- subset(P2.svy, income_p23 >= as.numeric(
  svyquantile(~income_p23, P2.svy, quantile=c(0.9))))
top10num_p23 <- svyby(~income_p23, ~rb010, top10_svy_p23, svytotal)
top10den_p23 <- svyby(~income_p23, ~rb010, P2.svy, svytotal)
top10_p23 <- top10num_p23 / top10den_p23

table_p23 <- data.frame(mean_p23$rb010, mean_p23$income_p23, median_p23$income_p23,
                        gini_p23$income_p23, top10_p23$income_p23)
colnames(table_p23) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'Top10%')
write.csv(table_p23, './reports/IRL/tables/final/table_p23_new0.csv')


# rm(list= ls()[!(ls() %in% c('P1.svy', 'P2.svy', 'silc.rph', 'silc.rph2',
#                            'table_p11', 'table_p12', 'table_p13', 'table_p21', 
#                           'table_p22', 'table_p23'))])

############### Quantile tables ################################
### P11
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
 write.csv(table_quant_p11, './reports/IRL/tables/final/table_quantiles_p11_new0.csv')

### p12
 quant_p12_10 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                       quantile = 0.1, ci=T)
 quant_p12_25 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                       quantile = 0.25, ci=T)
 quant_p12_50 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                       quantile = 0.5, ci=T)
 quant_p12_75 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                       quantile = 0.75, ci=T)
 quant_p12_90 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                       quantile = 0.9, ci=T)
 mean_p12 <- svyby(~income_p12, ~rb010, P1.svy, svymean)
 
 table_quant_p12 <- data.frame(quant_p12_10$rb010, quant_p12_10$income_p12,
                               quant_p12_25$income_p12, quant_p12_50$income_p12,
                               quant_p12_75$income_p12, quant_p12_90$income_p12,
                               mean_p12$income_p12)
 colnames(table_quant_p12) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
 write.csv(table_quant_p12, './reports/IRL/tables/final/table_quantiles_p12_new0.csv')
 ### P13
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
 write.csv(table_quant_p13, './reports/IRL/tables/final/table_quantiles_p13_new0.csv')
 
 ### P21
 quant_p21_10 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.1, ci=T)
 quant_p21_25 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.25, ci=T)
 quant_p21_50 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.5, ci=T)
 quant_p21_75 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.75, ci=T)
 quant_p21_90 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.9, ci=T)
 mean_p21 <- svyby(~income_p21, ~rb010, P2.svy, svymean)
 
 table_quant_p21 <- data.frame(quant_p21_10$rb010, quant_p21_10$income_p21,
                               quant_p21_25$income_p21, quant_p21_50$income_p21,
                               quant_p21_75$income_p21, quant_p21_90$income_p21,
                               mean_p21$income_p21)
 colnames(table_quant_p21) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
 write.csv(table_quant_p21, './reports/IRL/tables/final/table_quantiles_p21_new0.csv')
 ### P22
 quant_p22_10 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.1, ci=T)
 quant_p22_25 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.25, ci=T)
 quant_p22_50 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.5, ci=T)
 quant_p22_75 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.75, ci=T)
 quant_p22_90 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                       quantile = 0.9, ci=T)
 mean_p22 <- svyby(~income_p22, ~rb010, P2.svy, svymean)
 
 table_quant_p22 <- data.frame(quant_p22_10$rb010, quant_p22_10$income_p22,
                               quant_p22_25$income_p22, quant_p22_50$income_p22,
                               quant_p22_75$income_p22, quant_p22_90$income_p22,
                               mean_p22$income_p22)
 colnames(table_quant_p22) <- c('Jahr', '10%', '25%', 'Median', '75%', '90%', 'Mean')
 write.csv(table_quant_p22, './reports/IRL/tables/final/table_quantiles_p22_new0.csv')
 ### P23
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
 write.csv(table_quant_p23, './reports/IRL/tables/final/table_quantiles_p23_new0.csv')