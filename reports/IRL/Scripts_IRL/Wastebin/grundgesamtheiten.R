#############################################################################
####################    Erstelle Grundgesamtheiten      #####################
####################    P1 (Eurostat): Gesamte Bevölkerung 
####################       & equal sharing of resources within household. 
####################    P2 (wid.world):Nur Personen >= 20 Jahre 
####################       & partial sharing of resources 
#############################################################################


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
silc.rph <- silc.rph %>% mutate(hy110g_a = (hy110g/hx050)) #(HY110G) äquivalisiert

silc.rph <- silc.rph %>% mutate(pinc = py010g + car + py050g + hy110g_a)

### 2.2 Vermögenseinkommen: winc
silc.rph <- silc.rph %>% mutate(hy040g_a = (hy040g/hx050), 
                                hy090g_a = (hy090g/hx050)) 

silc.rph <- silc.rph %>% mutate(winc = hy040g_a + hy090g_a + py080g)

### 2.3 Pre-tax factor income:
silc.rph <- silc.rph %>% mutate(income_p11 = pinc + winc)


#############################################################################
################## 3. Pre-tax national income  ##############################

### 3.1 Pensionen + Arbeitslosengeld
silc.rph <- silc.rph %>% mutate(benefits = py090g + py100g)

### 3.2 Pre-tax national income
silc.rph <- silc.rph %>% mutate(income_p12 = pinc + winc + benefits)


#############################################################################
################# 4. Post-tax disposable income #############################

### 4.1 Alle anderen erhaltenen Transferzahlungen

### 4.1.1 äquivalisieren
silc.rph <- silc.rph %>% mutate(hy050g_a = (hy050g/hx050), 
                                hy060g_a = (hy060g/hx050), 
                                hy070g_a = (hy070g/hx050), 
                                hy080g_a = (hy080g/hx050), 
                                hy120g_a = (hy120g/hx050), 
                                hy130g_a = (hy130g/hx050), 
                                hy140g_a = (hy140g/hx050))

### 4.1.2 sum(transfers)
silc.rph <- silc.rph %>% mutate(transfers = py110g + py120g + py130g + py140g +
                                  hy050g_a + hy060g_a + hy070g_a + hy080g_a)
                                  
### 4.1.3 sum(taxes)
silc.rph <- silc.rph %>% mutate(taxes = hy120g_a + hy130g_a + hy140g_a)

### 4.2 Post-tax disposable income
silc.rph <- silc.rph %>% mutate(income_p13 = 
                                  pinc + winc + benefits + transfers - taxes)


summary(silc.rph$hy020a==silc.rph$income_p13)

silc.rph <- silc.rph %>%  group_by(id_h.x, rb010) %>% mutate(sum_income_p13 = sum(income_p13))
# ### überprüfung
# silc.rph$hy020a <- silc.rph$hy020/silc.rph$hx050
# 
# silc.rph <- silc.rph %>% mutate(hy020b = hy110g_a + hy040g_a + hy090g_a + 
#                                   hy050g_a + hy060g_a + hy070g_a + hy080g_a -
#                                   hy120g_a - hy130g_a - hy140g_a)




