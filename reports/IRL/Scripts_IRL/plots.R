###############################################################################
############################### PLOTS #########################################

### load tables of indicators
indicators_p11 <- read.csv('./reports/IRL/tables/table_p11.csv')
indicators_p12 <- read.csv('./reports/IRL/tables/table_p12.csv')
indicators_p13 <- read.csv('./reports/IRL/tables/table_p13.csv')

indicators_p21 <- read.csv('./reports/IRL/tables/table_p21.csv')
indicators_p22 <- read.csv('./reports/IRL/tables/table_p22.csv')
indicators_p23 <- read.csv('./reports/IRL/tables/table_p23.csv')

yunempl_0416 <- read.csv( './reports/IRL/tables/youth_unemployment_0416.csv')



######## Plot mean income Eurostat #################

brks <- seq(2004, 2016, 2)

mean_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p11$Mittelwert, x = indicators_p11$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p12$Mittelwert,x = indicators_p12$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p13$Mittelwert,x = indicators_p13$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Mean Income",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
mean_plot_p1


######## Plot median income Eurostat #################

median_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p11$Median, x = indicators_p11$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p12$Median,x = indicators_p12$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p13$Median,x = indicators_p13$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Median Income",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
median_plot_p1

######## Plot Gini Eurostat #################

gini_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p11$Gini, x = indicators_p11$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p12$Gini,x = indicators_p12$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p13$Gini,x = indicators_p13$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Gini Index", 
       title = "Gini Index",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
gini_plot_p1


######## Plot P80/P20 Eurostat #################

p8020_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p11$P80.P20, x = indicators_p11$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p12$P80.P20,x = indicators_p12$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p13$P80.P20,x = indicators_p13$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "P80/P20 ratio", 
       title = "P80/P20 ratio",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
p8020_plot_p1


######## Plot Top 10% share Eurostat #################

top10_plot_p1 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p11$Top10., x = indicators_p11$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p12$Top10.,x = indicators_p12$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p13$Top10.,x = indicators_p13$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "share of top decile", 
       title = "Income share of Top 10%",
       subtitle = "equal sharing of resources within household", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
top10_plot_p1



############################################################################
######## Plot mean income Wid ############################

mean_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p21$Mittelwert, x = indicators_p21$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p22$Mittelwert,x = indicators_p22$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p23$Mittelwert,x = indicators_p23$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Mean Income",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
mean_plot_p2


######## Plot median income Wid #################

median_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p21$Median, x = indicators_p21$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p22$Median,x = indicators_p22$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p23$Median,x = indicators_p23$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Income in EUR", 
       title = "Median Income",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
median_plot_p2

######## Plot Gini Wid #################

gini_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p21$Gini, x = indicators_p21$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p22$Gini,x = indicators_p22$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p23$Gini,x = indicators_p23$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "Gini Index", 
       title = "Gini Index",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
gini_plot_p2


######## Plot P80/P20 Wid #################

p8020_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p21$P80.P20, x = indicators_p21$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p22$P80.P20,x = indicators_p22$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p23$P80.P20,x = indicators_p23$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "P80/P20 ratio", 
       title = "P80/P20 ratio",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
p8020_plot_p2


######## Plot Top 10% share Wid #################

top10_plot_p2 <- ggplot() +
  geom_line(mapping = aes(y = indicators_p21$Top10., x = indicators_p21$Jahr,
                          color = "Pre-tax factor income"),
            size = 1 ) +
  geom_line(mapping = aes(y = indicators_p22$Top10.,x = indicators_p22$Jahr,
                          color = "Pre-tax national income"), size = 1) +
  geom_line(mapping = aes(y = indicators_p23$Top10.,x = indicators_p23$Jahr,
                          color = "Post-tax disposable income"), 
            size = 1) + scale_color_manual(values = 
                                             c('Pre-tax factor income' = 'red', 
                                               'Pre-tax national income' = 'darkblue', 
                                               'Post-tax disposable income' = 'darkgreen')) +
  labs(color = '', x = "Year", y = "share of top decile", 
       title = "Income share of Top 10%",
       subtitle = "partial sharing of resources, age >= 20 ", 
       caption = 'Source: EU-SILC') +
  theme_linedraw() + scale_x_continuous(breaks = brks) +
  theme(legend.position="bottom")
top10_plot_p2


################# Plot youth unemployment #####################################

yunemployment_plot <- ggplot() + 
  geom_line(mapping = aes(x = yunempl_0416$time, y = yunempl_0416$values, 
                          color = 'darkred'), size = 1) + theme_linedraw() +
  labs(color = '', x = 'Year', y = 'Youth unemployment in %',
       title = 'Youth-unemployment in %', 
       subtitle = 'aged 15 - 24 as a percentage of the labour force of the same age',
       caption = 'Source: Eurostat Database') +
  theme(legend.position = 'none')


############################## plot gini barplot ##########################
library(reshape2)
gini <- data.frame(seq(2004,2016,1),indicators_p11$Gini, indicators_p12$Gini, indicators_p13$Gini)
colnames(gini) <- c('Year', 'Pre-tax factor income', 'Pre-tax national income', 'Post-tax disposable income')
gini$Year <- as.factor(gini$Year)
gini <- melt(gini)

gini_barplot_p1 <- ggplot(gini, aes(x=Year, y=value, fill=variable)) + 
  geom_bar (stat="identity", position ='dodge', width = 0.8) +
  theme_linedraw() + coord_cartesian(ylim=c(0.2,0.48)) + 
  scale_fill_manual(values = 
                    c('Pre-tax factor income' = 'red4', 
                      'Pre-tax national income' = 'blue', 
                      'Post-tax disposable income' = 'green4')) +
  labs( x = 'Year', y = 'Gini-indizes',
       title = 'Gini index',
       subtitle = 'equal sharing of resources within household',
       caption = 'Source: EU-SILC') +
  theme(legend.position="bottom", legend.title = element_blank())
  


 


