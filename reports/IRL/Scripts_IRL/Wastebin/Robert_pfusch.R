load("./data/IRL_04-13.RData")

###### 1. Pre-Tax Factor Income #############

# 1.1 Einkommen aus Arbeit (inkl. Selbstständige)



# py020 2004 - 2006
# py021g 2007 - 2013


# 1.2 Vermögenseinkommen

silc.pd <- silc.pd %>% mutate(wincome = hy040g)

silc.pd04 <- silc.pd %>% filter(pb010 == '2004') 
silc.pd05 <- silc.pd %>% filter(pb010 == '2005') 
silc.pd06 <- silc.pd %>% filter(pb010 == '2006') 

rm(list=setdiff(ls(), "pg"))



