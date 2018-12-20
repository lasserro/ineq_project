#############################################################################
####################    Erstelle Grundgesamtheiten      #####################
####################    P1 (Eurostat): Gesamte Bevölkerung 
####################       & equal sharing of resources within household. 
####################    P2 (wid.world):Nur Personen >= 20 Jahre 
####################       & partial sharing of resources 
#############################################################################


#############################################################################
######################### 1. P2 (wid.world): ################################

# 1.1 filter (rx010(age) >= 20):

silc.P2 <- silc.rph %>% filter(rx010 >= 20) %>% add_count(id_h.x) %>% 
                        rename(n_hh = n)

#############################################################################
######### 2. Pre-tax factor income (Canberra: primary income)  ##############

silc.P2 <- silc.P2 %>% mutate(income_p21 = pinc + hinc/n_hh)



#############################################################################
################## 3. Pre-tax national income  ##############################

silc.P2 <- silc.P2 %>% mutate(income_p22 = income_p21 + benefits)


#############################################################################
################# 4. Post-tax disposable income #############################

silc.P2 <- silc.P2 %>% mutate(income_p23 = income_p22 + ptransfers +
                                htransfers/n_hh - taxes/n_hh)


