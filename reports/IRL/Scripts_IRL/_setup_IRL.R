# Setup -------------------------------------------------------------------

library(dplyr)
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}


# Prepare Data ------------------------------------------------------------

# Download data
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country & pb010 %in% year) %>%
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country & hb010 %in% year) %>%
  select(hb010, hb020, hb030, hy010, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010) %>%
  collect(n = Inf)

silc.d <- tbl(pg, "dd") %>%
  filter(db020 %in% country & db010 %in% year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 %in% country & rb010 %in% year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

# Download c[YY]p tables 2007 - 2013:
c07p <- tbl(pg, "c07p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c08p <- tbl(pg, "c08p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c09p <- tbl(pg, "c09p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c10p <- tbl(pg, "c10p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c11p <- tbl(pg, "c11p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c12p <- tbl(pg, "c12p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

c13p <- tbl(pg, "c13p") %>% filter(pb020 %in% country) %>% select(pb010, pb030, 
        py021g) %>% collect(n = Inf)

cxxp <- bind_rows(c07p, c08p, c09p, c10p, c11p, c12p, c13p)
rm(c07p, c08p, c09p, c10p, c11p, c12p, c13p)

# merge cxxp with silc.p to get py021g variable for 2007-2013

silc.p <- left_join(silc.p, cxxp %>% select(py021g, pb010, pb030))
#rm(cxxp)

# include data for 2014-2017

c14p <- tbl(pg, "c14p") %>% filter(pb020 %in% country) %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

c15p <- tbl(pg, "c15p") %>% filter(pb020 %in% country) %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

c16p <- tbl(pg, "c16p") %>% filter(pb020 %in% country) %>% 
  select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
         py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
         px030, py021g) %>% collect(n = Inf)

cyyp <- bind_rows(c14p, c15p, c16p)
rm(c14p, c15p, c16p)

# c17p anscheinend nicht in der Datenbank
# c17p <- tbl(pg, "c17p") %>% filter(pb020 %in% country) %>% 
#   select(pb010, pb020, pb030, pb040, pb150, py010g, py020g, py050g, py050n, 
#          py080g, py090g, py100g, py110g, py120g, py130g, py140g, px010, 
#          px030, py021g) %>% collect(n = Inf)

# merge data to silc.p

silc.p <- bind_rows(silc.p, cyyp)

# Download c[YY]h tables 2007 - 2013:

c14h <- tbl(pg, "c14h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010) %>%
  collect(n = Inf)

c15h <- tbl(pg, "c15h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010) %>%
  collect(n = Inf)

c16h <- tbl(pg, "c16h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy040g, hy050g, hy060g, hy070g, hy080g, 
         hy090g, hy110g, hy120g, hy130g, hy140g, hx010) %>%
  collect(n = Inf)

cyyh <- bind_rows(c14h, c15h, c16h)
rm(c14h, c15h, c16h)

# merge data to silc.h
silc.h <- bind_rows(silc.h, cyyh)

# Download c[YY]d tables 2007 - 2013:

c14d <- tbl(pg, "c14d") %>%
  filter(db020 %in% country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c15d <- tbl(pg, "c15d") %>%
  filter(db020 %in% country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c16d <- tbl(pg, "c16d") %>%
  filter(db020 %in% country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

cyyd <- bind_rows(c14d, c15d, c16d)
rm(c14d, c15d, c16d)

# merge data to silc.d
silc.d <- bind_rows(silc.d, cyyd)


# Download c[YY]r tables 2007 - 2013:

c14r <- tbl(pg, "c14r") %>% 
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

c15r <- tbl(pg, "c15r") %>% 
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

c16r <- tbl(pg, "c16r") %>% 
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

cyyr <- bind_rows(c14r, c15r, c16r)
rm(c14r, c15r, c16r)

# merge data to silc.r
silc.r <- bind_rows(silc.r, cyyr)



# Create unique IDs for merging
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rx030))


# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db010, db020, db040, db090)
                     , by = c('id_h'='id_h', 'pb010'='db010'))

silc.hd <- left_join(silc.h, silc.d %>% select(id_h, db010, db020, db040, db090)
                     , by = c('id_h' = 'id_h', 'hb010'='db010'))

rm(cxxp, cyyp, cyyh, cyyd, cyyr)


# create car variable for later use: combine py020g & py021g 
int1 <- seq(2004,2006,1)
int2 <- seq(2007,2016,1)
df1 <- silc.pd %>% filter(pb010 %in% int1)
df2 <- silc.pd %>% filter(pb010 %in% int2)

df1$car <- df1$py020g
df2$car <- df2$py021g

silc.pd <- bind_rows(df1,df2)
rm(int1,int2,df1,df2)

# Create total personal income --------------------------------------------

# Find string "py" (i.e. income variables) for summing up total personal income. 
# silc.pd <- silc.pd %>% 
# mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], 
#                             na.rm = TRUE)) 

# Fin ---------------------------------------------------------------------

message("Prepared data for ", country, " in ", year, ".")
