####################### Appendix #######################################

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

indicators_p11 <- read.csv('./reports/IRL/tables/final/table_p11.csv')
indicators_p12 <- read.csv('./reports/IRL/tables/final/table_p12.csv')
indicators_p13 <- read.csv('./reports/IRL/tables/final/table_p13.csv')

indicators_p21 <- read.csv('./reports/IRL/tables/final/table_p21.csv')
indicators_p22 <- read.csv('./reports/IRL/tables/final/table_p22.csv')
indicators_p23 <- read.csv('./reports/IRL/tables/final/table_p23.csv')

library(xtable)

poverty_xtable <- xtable(poverty_p2,  caption = c('At-risk-of-poverty rate','Eurostat'))
align( poverty_xtable ) <- c( 'l', 'p{0.5in}', rep('p{0.65in}',3))
print(poverty_xtable, include.rownames = F)

### Indicators P1 ####
### P11
p11_xtable <- xtable(indicators_p11[,2:8], caption = c('Indicators: Pre-tax factor income'))
align( p11_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p11_xtable, include.rownames = F)

### P12
p12_xtable <- xtable(indicators_p12[,2:8], caption = c('Pre-tax national income'))
align( p12_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p12_xtable, include.rownames = F)

### p13
p13_xtable <- xtable(indicators_p13[,2:8], caption = c('Post-tax disposable income'))
align( p13_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p13_xtable, include.rownames = F)

### Percentiles P1 ####
### p11
p11_xtable <- xtable(table_quant_p11[,2:8], caption = c('Income Percentiles\\\\ Pre-tax factor income'))
align( p11_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p11_xtable, include.rownames = F)

### p12
p12_xtable <- xtable(table_quant_p12[,2:8], caption = c('Income Percentiles\\\\ Pre-tax national income'))
align( p12_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p12_xtable, include.rownames = F)

### p13
p13_xtable <- xtable(table_quant_p13[,2:8], caption = c('Income Percentiles\\\\ Post-tax disposable income'))
align( p13_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p13_xtable, include.rownames = F)

### Indicators p2 ####
### p21
p21_xtable <- xtable(indicators_p21[,2:8], caption = c('Indicators: Pre-tax factor income'))
align( p21_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p21_xtable, include.rownames = F)

### p22
p22_xtable <- xtable(indicators_p22[,2:8], caption = c('Pre-tax national income'))
align( p22_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p22_xtable, include.rownames = F)

### p23
p23_xtable <- xtable(indicators_p23[,2:8], caption = c('Post-tax disposable income'))
align( p23_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p23_xtable, include.rownames = F)

### Percentiles p2 ####
### p21
p21_xtable <- xtable(table_quant_p21[,2:8], caption = c('Income Percentiles\\\\ Pre-tax factor income'))
align( p21_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p21_xtable, include.rownames = F)

### p22
p22_xtable <- xtable(table_quant_p22[,2:8], caption = c('Income Percentiles\\\\ Pre-tax national income'))
align( p22_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p22_xtable, include.rownames = F)

### p23
p23_xtable <- xtable(table_quant_p23[,2:8], caption = c('Income Percentiles\\\\ Post-tax disposable income'))
align( p23_xtable ) <- c( 'l', 'p{0.5in}', rep('p{1in}',6))
print(p23_xtable, include.rownames = F)

