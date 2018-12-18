#############################################################################
####################    Erstelle Grundgesamtheiten      #####################
####################    P1 (Eurostat): Gesamte Bevölkerung 
####################       & equal sharing of resources within household. 
####################    P2 (wid.world):Nur Personen >= 20 Jahre 
####################       & partial sharing of resources 
#############################################################################
#options(digits = 0)

#############################################################################
###################### 1. P1 (Eurostat): ####################################

# 1.1 create full dataframe including p,r,h:

silc.rph <- left_join(silc.rp, silc.h, by = c('id_h.x' = 'id_h', 
                                              'rb010' = 'hb010' ))

# 1.2  check for NA's and if we can set them to 0:
# diff <- nrow(silc.r) - nrow(silc.p)
# summary(is.na(silc.rph))
# only Persons not in p but in r (age<16) are NA's. so its fine to set 0

silc.rph[is.na(silc.rph)] <- 0


#############################################################################
######### 2. Pre-tax factor income (Canberra: primary income)  ##############

### 2.1 Sum of personal income

### 2.1.1 summe Einkommen aus Arbeit (inkl Selbstständiger): pinc
silc.rph <- silc.rph %>% mutate(pinc = py010g + car + py050g + py080g) 

### 2.2 Haushaltseinkommen: hinc
silc.rph <- silc.rph %>% mutate(hinc = hy040g + hy090g + hy110g)

silc.rph <- silc.rph %>% group_by(id_h.x, rb010) %>% mutate(sum_pinc = sum(pinc))

### 2.3 Pre-tax factor income:
silc.rph <- silc.rph %>% mutate(income_p11 = (sum_pinc + hinc)/hx050)


#############################################################################
################## 3. Pre-tax national income  ##############################

### 3.1 Pensionen + Arbeitslosengeld
silc.rph <- silc.rph %>% mutate(benefits = py090g + py100g)
silc.rph <- silc.rph %>% group_by(id_h.x, rb010) %>% mutate(sum_benefits = sum(benefits))


### 3.2 Pre-tax national income
silc.rph <- silc.rph %>% mutate(income_p12 =( income_p11 +
                                sum_benefits/hx050))


#############################################################################
################# 4. Post-tax disposable income #############################

### 4.1 Alle anderen erhaltenen Transferzahlungen


### 4.1.1 sum(transfers)
silc.rph <- silc.rph %>% mutate(ptransfers = py110g + py120g + py130g + py140g)
silc.rph <- silc.rph %>% mutate(htransfers = hy050g + hy060g + hy070g + hy080g)

silc.rph <- silc.rph %>% group_by(id_h.x, rb010) %>% mutate(sum_ptransfers = sum(ptransfers))
### 4.1.2 sum(taxes)
silc.rph <- silc.rph %>% mutate(taxes = hy120g + hy130g + hy140g)

### 4.2 Post-tax disposable income
silc.rph <- silc.rph %>% mutate(income_p13 = income_p12 + (ptransfers + 
                                            htransfers - taxes)/hx050)

### 4.3 überprüfen ob income_p13 = hy020a

silc.rph$hy020a <- silc.rph$hy020/silc.rph$hx050

summary(silc.rph$hy020a==silc.rph$income_p13)

silc.rph$income_p13_round0 <- round(silc.rph$income_p13, digits = 0)
silc.rph$hy020a_round0 <- round(silc.rph$hy020a, digits = 0)
summary(silc.rph$hy020a_round0==silc.rph$income_p13_round0)

# it is almost. good enough for me... 3/4 der Werte stimmen exakt, der Rest wsl
# nur minimaler Unterschied, könnte man noch überprüfen mit Hilfe von Intervallen?


