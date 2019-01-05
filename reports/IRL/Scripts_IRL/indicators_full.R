# ------------------------------------------------------------------------
#
# Indicators R-Script
# Autoren: Lasser & Reiter
# Datum: 
#
# -------------------------------------------------------------------------
# rm(list= ls()[!(ls() %in% c('P1.svy', 'P2.svy', 'silc.rph', 'silc.rph2'))])

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
mean_p11$income_p11 <- mean_p11$income_p11 / inflation$values

median_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p11$income_p11 <-  median_p11$income_p11 / inflation$values

gini_p11 <- svyby(~income_p11, ~rb010, P1.svy, svygini)
p8020_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyqsr)

top10_svy_p11 <- subset(P1.svy, income_p11 >= as.numeric(
  svyquantile(~income_p11, P1.svy, quantile=c(0.9))))
top10num_p11 <- svyby(~income_p11, ~rb010, top10_svy_p11, svytotal)
top10den_p11 <- svyby(~income_p11, ~rb010, P1.svy, svytotal)
top10_p11 <- top10num_p11 / top10den_p11

table_p11 <- data.frame(mean_p11$rb010, mean_p11$income_p11, median_p11$income_p11,
                        gini_p11$income_p11, p8020_p11$income_p11, top10_p11$income_p11)
colnames(table_p11) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')

### 2.2 pre-tax national income

mean_p12 <- svyby(~income_p12, ~rb010, P1.svy, svymean)
mean_p12$income_p12 <- mean_p12$income_p12 / inflation$values

median_p12 <- svyby(~income_p12, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p12$income_p12 <-  median_p12$income_p12 / inflation$values

gini_p12 <- svyby(~income_p12, ~rb010, P1.svy, svygini)
p8020_p12 <- svyby(~income_p12, ~rb010, P1.svy, svyqsr)

top10_svy_p12 <- subset(P1.svy, income_p12 >= as.numeric(
  svyquantile(~income_p12, P1.svy, quantile=c(0.9))))
top10num_p12 <- svyby(~income_p12, ~rb010, top10_svy_p12, svytotal)
top10den_p12 <- svyby(~income_p12, ~rb010, P1.svy, svytotal)
top10_p12 <- top10num_p12 / top10den_p12

table_p12 <- data.frame(mean_p12$rb010, mean_p12$income_p12, median_p12$income_p12,
                        gini_p12$income_p12, p8020_p12$income_p12, top10_p12$income_p12)
colnames(table_p12) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')

### 2.3 post-tax disposable income

mean_p13 <- svyby(~income_p13, ~rb010, P1.svy, svymean)
mean_p13$income_p13 <- mean_p13$income_p13 / inflation$values

median_p13 <- svyby(~income_p13, ~rb010, P1.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p13$income_p13 <-  median_p13$income_p13 / inflation$values

gini_p13 <- svyby(~income_p13, ~rb010, P1.svy, svygini)
p8020_p13 <- svyby(~income_p13, ~rb010, P1.svy, svyqsr)

top10_svy_p13 <- subset(P1.svy, income_p13 >= as.numeric(
  svyquantile(~income_p13, P1.svy, quantile=c(0.9))))
top10num_p13 <- svyby(~income_p13, ~rb010, top10_svy_p13, svytotal)
top10den_p13 <- svyby(~income_p13, ~rb010, P1.svy, svytotal)
top10_p13 <- top10num_p13 / top10den_p13

table_p13 <- data.frame(mean_p13$rb010, mean_p13$income_p13, median_p13$income_p13,
                        gini_p13$income_p13, p8020_p13$income_p13, top10_p13$income_p13)
colnames(table_p13) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')

################################################################################
########################## 3. wid Indicators####################################

### 3.1 pre-tax factor income

mean_p21 <- svyby(~income_p21, ~rb010, P2.svy, svymean)
mean_p21$income_p21 <- mean_p21$income_p21 / inflation$values

median_p21 <- svyby(~income_p21, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p21$income_p21 <-  median_p21$income_p21 / inflation$values

gini_p21 <- svyby(~income_p21, ~rb010, P2.svy, svygini)
p8020_p21 <- svyby(~income_p21, ~rb010, P2.svy, svyqsr)

top10_svy_p21 <- subset(P2.svy, income_p21 >= as.numeric(
  svyquantile(~income_p21, P2.svy, quantile=c(0.9))))
top10num_p21 <- svyby(~income_p21, ~rb010, top10_svy_p21, svytotal)
top10den_p21 <- svyby(~income_p21, ~rb010, P2.svy, svytotal)
top10_p21 <- top10num_p21 / top10den_p21

table_p21 <- data.frame(mean_p21$rb010, mean_p21$income_p21, median_p21$income_p21,
                        gini_p21$income_p21, p8020_p21$income_p21, top10_p21$income_p21)
colnames(table_p21) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')

### 3.2 pre-tax national income

mean_p22 <- svyby(~income_p22, ~rb010, P2.svy, svymean)
mean_p22$income_p22 <- mean_p22$income_p22 / inflation$values

median_p22 <- svyby(~income_p22, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p22$income_p22 <-  median_p22$income_p22 / inflation$values

gini_p22 <- svyby(~income_p22, ~rb010, P2.svy, svygini)
p8020_p22 <- svyby(~income_p22, ~rb010, P2.svy, svyqsr)

top10_svy_p22 <- subset(P2.svy, income_p22 >= as.numeric(
  svyquantile(~income_p22, P2.svy, quantile=c(0.9))))
top10num_p22 <- svyby(~income_p22, ~rb010, top10_svy_p22, svytotal)
top10den_p22 <- svyby(~income_p22, ~rb010, P2.svy, svytotal)
top10_p22 <- top10num_p22 / top10den_p22

table_p22 <- data.frame(mean_p22$rb010, mean_p22$income_p22, median_p22$income_p22,
                        gini_p22$income_p22, p8020_p22$income_p22, top10_p22$income_p22)
colnames(table_p22) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')

### 3.3 post-tax dispo income

mean_p23 <- svyby(~income_p23, ~rb010, P2.svy, svymean)
mean_p23$income_p23 <- mean_p23$income_p23 / inflation$values

median_p23 <- svyby(~income_p23, ~rb010, P2.svy, svyquantile, 
                    quantiles = 0.5, ci = T)
median_p23$income_p23 <-  median_p23$income_p23 / inflation$values

gini_p23 <- svyby(~income_p23, ~rb010, P2.svy, svygini)
p8020_p23 <- svyby(~income_p23, ~rb010, P2.svy, svyqsr)

top10_svy_p23 <- subset(P2.svy, income_p23 >= as.numeric(
  svyquantile(~income_p23, P2.svy, quantile=c(0.9))))
top10num_p23 <- svyby(~income_p23, ~rb010, top10_svy_p23, svytotal)
top10den_p23 <- svyby(~income_p23, ~rb010, P2.svy, svytotal)
top10_p23 <- top10num_p23 / top10den_p23

table_p23 <- data.frame(mean_p23$rb010, mean_p23$income_p23, median_p23$income_p23,
                        gini_p23$income_p23, p8020_p23$income_p23, top10_p23$income_p23)
colnames(table_p23) <- c('Jahr', 'Mittelwert', 'Median', 'Gini', 'P80/P20', 'Top10%')


 rm(list= ls()[!(ls() %in% c('P1.svy', 'P2.svy', 'silc.rph', 'silc.rph2',
                            'table_p11', 'table_p12', 'table_p13', 'table_p21', 
                            'table_p22', 'table_p23'))])

