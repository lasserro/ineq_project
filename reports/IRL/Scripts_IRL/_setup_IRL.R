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
rm(cxxp)

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


# Create total personal income --------------------------------------------

# Find string "py" (i.e. income variables) for summing up total personal income. 
# silc.pd <- silc.pd %>% 
# mutate(total.inc = rowSums(silc.pd[, grep("py", colnames(silc.pd))], 
#                             na.rm = TRUE)) 

# Fin ---------------------------------------------------------------------

message("Prepared data for ", country, " in ", year, ".")
