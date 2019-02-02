####################### Appendix #######################################

table_quant_p11 <- read.csv('./reports/IRL/tables/final/table_quantiles_p11_new0.csv')
table_quant_p12 <- read.csv('./reports/IRL/tables/final/table_quantiles_p12_new0.csv')
table_quant_p13 <- read.csv('./reports/IRL/tables/final/table_quantiles_p13_new0.csv')

table_quant_p11 <- round(table_quant_p11, digits = 0)
table_quant_p12 <- round(table_quant_p12, digits = 0)
table_quant_p13 <- round(table_quant_p13, digits = 0)

colnames(table_quant_p11) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p12) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p13) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')

table_quant_p21 <- read.csv('./reports/IRL/tables/final/table_quantiles_p21_new0.csv')
table_quant_p22 <- read.csv('./reports/IRL/tables/final/table_quantiles_p22_new0.csv')
table_quant_p23 <- read.csv('./reports/IRL/tables/final/table_quantiles_p23_new0.csv')

table_quant_p21 <- round(table_quant_p21, digits = 0)
table_quant_p22 <- round(table_quant_p22, digits = 0)
table_quant_p23 <- round(table_quant_p23, digits = 0)

colnames(table_quant_p21) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p22) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')
colnames(table_quant_p23) <- c('X', 'Year', '10th', '25th', 'Median', '75th', '90th', 'Mean')

indicators_p11 <- read.csv('./reports/IRL/tables/final/table_p11_new0.csv')
indicators_p12 <- read.csv('./reports/IRL/tables/final/table_p12_new0.csv')
indicators_p13 <- read.csv('./reports/IRL/tables/final/table_p13_new0.csv')

indicators_p11$Mittelwert <- round(indicators_p11$Mittelwert, digits = 0)
indicators_p12$Mittelwert <- round(indicators_p12$Mittelwert, digits = 0)
indicators_p13$Mittelwert <- round(indicators_p13$Mittelwert, digits = 0)

indicators_p11$Median<- round(indicators_p11$Median, digits = 0)
indicators_p12$Median <- round(indicators_p12$Median, digits = 0)
indicators_p13$Median <- round(indicators_p13$Median, digits = 0)

indicators_p11$Gini<- round(indicators_p11$Gini, digits = 2)
indicators_p12$Gini <- round(indicators_p12$Gini, digits = 2)
indicators_p13$Gini <- round(indicators_p13$Gini, digits = 2)

indicators_p11$Top10.<- round(indicators_p11$Top10., digits = 2)
indicators_p12$Top10. <- round(indicators_p12$Top10., digits = 2)
indicators_p13$Top10. <- round(indicators_p13$Top10., digits = 2)

indicators_p21 <- read.csv('./reports/IRL/tables/final/table_p21_new0.csv')
indicators_p22 <- read.csv('./reports/IRL/tables/final/table_p22_new0.csv')
indicators_p23 <- read.csv('./reports/IRL/tables/final/table_p23_new0.csv')

indicators_p21$Mittelwert <- round(indicators_p21$Mittelwert, digits = 0)
indicators_p22$Mittelwert <- round(indicators_p22$Mittelwert, digits = 0)
indicators_p23$Mittelwert <- round(indicators_p23$Mittelwert, digits = 0)

indicators_p21$Median <- round(indicators_p21$Median, digits = 0)
indicators_p22$Median <- round(indicators_p22$Median, digits = 0)
indicators_p23$Median <- round(indicators_p23$Median, digits = 0)

indicators_p21$Gini<- round(indicators_p21$Gini, digits = 2)
indicators_p22$Gini <- round(indicators_p22$Gini, digits = 2)
indicators_p23$Gini <- round(indicators_p23$Gini, digits = 2)

indicators_p21$Top10.<- round(indicators_p21$Top10., digits = 2)
indicators_p22$Top10. <- round(indicators_p22$Top10., digits = 2)
indicators_p23$Top10. <- round(indicators_p23$Top10., digits = 2)

library(xtable)

poverty_xtable <- xtable(poverty_p2,  caption = c('At-risk-of-poverty rate','Eurostat'), digits = 0)
align( poverty_xtable ) <- c( 'l', 'p{0.5in}', rep('p{0.65in}',3))
print(poverty_xtable, include.rownames = F)

poverty_xtable <- xtable(poverty_p1,  caption = c('At-risk-of-poverty rate','Eurostat'), digits = 0)
align( poverty_xtable ) <- c( 'l', 'p{0.5in}', rep('p{0.65in}',3))
print(poverty_xtable, include.rownames = F)

### Indicators P1 ####
### P11
p11_xtable <- xtable(indicators_p11[,2:6], caption = c('Indicators: Pre-tax factor income'), digits = c(0,0,0,0,2,2))
align( p11_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p11_xtable, include.rownames = F)

### P12
p12_xtable <- xtable(indicators_p12[,2:6], caption = c('Pre-tax national income'), digits = c(0,0,0,0,2,2))
align( p12_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p12_xtable, include.rownames = F)

### p13
p13_xtable <- xtable(indicators_p13[,2:6], caption = c('Post-tax disposable income'), digits = c(0,0,0,0,2,2))
align( p13_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p13_xtable, include.rownames = F)

### Percentiles P1 ####
### p11
p11_xtable <- xtable(table_quant_p11[,2:8], caption = c('Income Percentiles\\\\ Pre-tax factor income'), digits = 0)
align( p11_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p11_xtable, include.rownames = F)

### p12
p12_xtable <- xtable(table_quant_p12[,2:8], caption = c('Income Percentiles\\\\ Pre-tax national income'), digits = 0)
align( p12_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p12_xtable, include.rownames = F)

### p13
p13_xtable <- xtable(table_quant_p13[,2:8], caption = c('Income Percentiles\\\\ Post-tax disposable income'), digits = 0)
align( p13_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p13_xtable, include.rownames = F)

### Indicators p2 ####
### p21
p21_xtable <- xtable(indicators_p21[,2:6], caption = c('Pre-tax factor income'), digits = c(0,0,0,0,2,2))
align( p21_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p21_xtable, include.rownames = F)

### p22
p22_xtable <- xtable(indicators_p22[,2:6], caption = c('Pre-tax national income'), digits = c(0,0,0,0,2,2))
align( p22_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p22_xtable, include.rownames = F)

### p23
p23_xtable <- xtable(indicators_p23[,2:6], caption = c('Post-tax disposable income'), digits = c(0,0,0,0,2,2))
align( p23_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',4))
print(p23_xtable, include.rownames = F)

### Percentiles p2 ####
### p21
p21_xtable <- xtable(table_quant_p21[,2:8], caption = c('Income Percentiles\\\\ Pre-tax factor income'), digits = 0)
align( p21_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p21_xtable, include.rownames = F)

### p22
p22_xtable <- xtable(table_quant_p22[,2:8], caption = c('Income Percentiles\\\\ Pre-tax national income'), digits = 0)
align( p22_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p22_xtable, include.rownames = F)

### p23
p23_xtable <- xtable(table_quant_p23[,2:8], caption = c('Income Percentiles\\\\ Post-tax disposable income'), digits = 0)
align( p23_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p23_xtable, include.rownames = F)

