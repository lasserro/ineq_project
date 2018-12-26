# ------------------------------------------------------------------------
#
# Indicators R-Script
# Autoren: Lasser & Reiter
# Datum: 
#
# -------------------------------------------------------------------------
# 
# library(dplyr)
# library(survey)
# library(convey)
# 
# country <- "IE"
# year <- seq(2004, 2013, 1)
# 
# # Source the Setup scripts to provide merged household and personal data
# source("reports/IRL/Scripts_IRL/_connection.R")
# source("reports/IRL/Scripts_IRL/_setup_IRL.R")
# 
# 
# # Subsetting --------------------------------------------------------------
# 
# # To get useful results we may want to subset to only positive income
# silc.pd.inc <- silc.pd %>% filter(py010g > 0)
# silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

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


# # Indicators --------------------------------------------------------------
# 
# # Das sind nur aggregierte Werte, 
# ################################################################################
# ########################## 2. Canberra Indicators###############################
# 
# ### 2.1 pre-tax factor income
# mean_p11 <- svymean(~income_p11, P1.svy)
# median_p11 <- svyquantile(~income_p11, P1.svy, quantiles = c(0.5))
# gini_p11 <- svygini(~income_p11, P1.svy)
# P8020_p11 <- svyqsr(~income_p11, P1.svy, 0.2, 0.8)
# 
# top10_p11 <- svytotal(~income_p11, subset(P1.svy, income_p11 >= 
#                       as.numeric(svyquantile(~income_p11, P1.svy, 
#                       quantile = 0.9)))) / svytotal(~income_p11, P1.svy)
# 
# ### 2.2 pre-tax national income
# mean_p12 <- svymean(~income_p12, P1.svy)
# median_p12 <- svyquantile(~income_p12, P1.svy, quantiles = c(0.5))
# gini_p12 <- svygini(~income_p12, P1.svy)
# P8020_p12 <- svyqsr(~income_p12, P1.svy, 0.2, 0.8)
# 
# top10_p12 <- svytotal(~income_p12, subset(P1.svy, income_p12 >= 
#                       as.numeric(svyquantile(~income_p12, P1.svy, 
#                       quantile = 0.9)))) / svytotal(~income_p12, P1.svy)
# 
# ### 2.3 post-tax disposable income
# mean_p13 <- svymean(~income_p13, P1.svy)
# median_p13 <- svyquantile(~income_p13, P1.svy, quantiles = c(0.5))
# gini_p13 <- svygini(~income_p13, P1.svy)
# P8020_p13 <- svyqsr(~income_p13, P1.svy, 0.2, 0.8)
# 
# top10_p13 <- svytotal(~income_p13, subset(P1.svy, income_p12 >= 
#                       as.numeric(svyquantile(~income_p13, P1.svy, 
#                       quantile = 0.9)))) / svytotal(~income_p13, P1.svy)
# 
# 
# 
# ################################################################################
# ########################## 3. wid Indicators####################################
# 
# ### 3.1 pre-tax factor income
# mean_p21 <- svymean(~income_p21, P2.svy)
# median_p21 <- svyquantile(~income_p21, P2.svy, quantiles = c(0.5))
# gini_p21 <- svygini(~income_p21, P2.svy)
# P8020_p21 <- svyqsr(~income_p21, P2.svy, 0.2, 0.8)
# 
# top10_p21 <- svytotal(~income_p21, subset(P2.svy, income_p21 >= 
#                       as.numeric(svyquantile(~income_p21, P2.svy, 
#                       quantile = 0.9)))) / svytotal(~income_p21, P2.svy)
# 
# ### 3.2 pre-tax national income
# mean_p22 <- svymean(~income_p22, P2.svy)
# median_p22 <- svyquantile(~income_p22, P2.svy, quantiles = c(0.5))
# gini_p22 <- svygini(~income_p22, P2.svy)
# P8020_p22 <- svyqsr(~income_p22, P2.svy, 0.2, 0.8)
# 
# top10_p22 <- svytotal(~income_p22, subset(P2.svy, income_p22 >= 
#                                             as.numeric(svyquantile(~income_p22, P2.svy, 
#                                                                    quantile = 0.9)))) / svytotal(~income_p22, P2.svy)
# 
# ### 3.3 post-tax dispo income
# mean_p23 <- svymean(~income_p23, P2.svy)
# median_p23 <- svyquantile(~income_p23, P2.svy, quantiles = c(0.5))
# gini_p23 <- svygini(~income_p23, P2.svy)
# P8020_p23 <- svyqsr(~income_p23, P2.svy, 0.2, 0.8)
# 
# top10_p23 <- svytotal(~income_p23, subset(P2.svy, income_p23 >= 
#                                             as.numeric(svyquantile(~income_p23, P2.svy, 
#                                                                    quantile = 0.9)))) / svytotal(~income_p23, P2.svy)


#### grouped by years

mean_p11 <- svyby(~income_p11, ~rb010, P1.svy, svymean)
median_p11 <- svyby(~income_p11, ~rb010, P1.svy, svyquantile, quantile = 0.5)
gini_p11 <- svyby(~income_p11, ~rb010, P1.svy, svygini)


