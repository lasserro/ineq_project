############### Eigene Berechnung #############################################

poverty_p21 <- silc.rph2 %>% group_by(rb010) %>% 
  summarise(x = (length(income_p21[income_p21<0.6*median(income_p21)])/length(income_p21))*100)

poverty_p22 <- silc.rph2 %>% group_by(rb010) %>% 
  summarise(x = (length(income_p22[income_p22<0.6*median(income_p22)])/length(income_p22))*100)

poverty_p23 <- silc.rph2 %>% group_by(rb010) %>% 
  summarise(x = (length(income_p23[income_p23<0.6*median(income_p23)])/length(income_p23))*100)

poverty_p2 <- inner_join(poverty_p21, poverty_p22, by='rb010')
poverty_p2 <- inner_join(poverty_p2, poverty_p23, by = 'rb010')
colnames(poverty_p2) <- c('Year', 'Pre-tax factor income', 'Pre-tax national income',
                          'Post-tax disposable income')
write.csv(poverty_p2,'./reports/IRL/tables/final/poverty_rate_p2.csv')


poverty_p11 <- silc.rph %>% group_by(rb010) %>% 
  summarise(x = (length(income_p11[income_p11<0.6*median(income_p11)])/length(income_p11))*100)

poverty_p12 <- silc.rph %>% group_by(rb010) %>% 
  summarise(x = (length(income_p12[income_p12<0.6*median(income_p12)])/length(income_p12))*100)

poverty_p13 <- silc.rph %>% group_by(rb010) %>% 
  summarise(x = (length(income_p13[income_p13<0.6*median(income_p13)])/length(income_p13))*100)

poverty_p1 <- inner_join(poverty_p11, poverty_p12, by='rb010')
poverty_p1 <- inner_join(poverty_p1, poverty_p13, by = 'rb010')
colnames(poverty_p1) <- c('Year', 'Pre-tax factor income', 'Pre-tax national income',
                          'Post-tax disposable income')
write.csv(poverty_p1,'./reports/IRL/tables/final/poverty_rate_p1.csv')


############## Eurostat download #################################
library(xtable)
poverty_xtable <- xtable(poverty_p1,  caption = c('At-risk-of-poverty rate','Eurostat'))
align( poverty_xtable ) <- c( 'l', 'p{0.5in}', rep('p{0.65in}',3))
print(poverty_xtable, include.rownames = F)
