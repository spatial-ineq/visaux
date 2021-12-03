library(sf)
library(tidyverse)

# option setting
sf_use_s2(F)
options(tigris_use_cache = TRUE)

devtools::load_all()

rm(list=ls())

# get sample area to vis -------------------------------------------------------

# 2 counties in Baltimore
cos <- xwalks::co2cbsa %>%
  filter(grepl("Baltimore", cbsa_name))

cts <-
  tidycensus::get_acs(
  "tract",
  variables = 'B01001_001',
  state = '24',
  county = c('035', '025')
  , geometry = T
  )
colnames(cts) <- tolower(colnames(cts))

cts <- cts %>%
  select(geoid, pop = estimate,
         geometry)


# standalone queries -----------------------------------------------------------

.wtr <- water.wrapper(x = cts)
.plcs <- places.wrapper(x = st_union(cts))
.plcs



# visual tests -----------------------------------------------------------------

# county/plc/water layers
gglyrs <- visaux::add.map.layers(
  cts,
  add.places = '#20E0E0'
)

# cropped version
gglyrs_c <- visaux::add.map.layers(
  cts,
  add.places = 'white',
  spatial.trim = st_crop
)



# plots
ggplot(cts) +
  geom_sf(aes(fill = pop),
          color = NA) +
  gglyrs

pc <-
  ggplot(cts) +
  geom_sf(aes(fill = pop),
          color = NA) +
  gglyrs
pc

# add hwys
rm(hwys)
hwys <- visaux::get.NHPN(cts)
hwys <- hwys %>% st_intersection(st_union(cts))

hwy.lyrs <-
  list(
    geom_sf(data = filter(hwys,
                          signt1 == "I")
            , color = "black", size = 1.1)
    ,geom_sf(data = filter(hwys,
                           signt1 == "U")
             , color = "#882222", size = .9)
  )

pc +
  hwy.lyrs


# atlanta..? -------------------------------------------------------------------

devtools::load_all()

ctsf <- xwalks::ctx %>%
  filter(grepl('Atlanta', cbsa_name) )
tmprid <- divM::get.region.identifiers(cbsa= '12060')
ctsf <- divM::tracts.from.region(tmprid)
ctsf

atl.lyrs <- add.map.layers(ctsf,
                           add.counties = NULL
                           , lwd = .2)
pc <-
  ggplot(ctsf) +
  geom_sf(aes(fill = countyfp),
          size = .2) +
  atl.lyrs
pc



# stamen base layer ------------------------------------------------------------

library(ggmap)
cts <- cts %>% st_transform(4326)
stm <- visaux::get.stamen.bkg(cts)
stmw <- visaux::get.stamen.bkg(cts
                               ,maptype = 'watercolor')

ggmap(stm) +
  geom_sf(data = cts
          ,aes(fill = pop)
          ,color = NA
          ,inherit.aes = F
          ,alpha = .6) +
  scale_fill_viridis_c()

ggmap(stmw) +
  geom_sf(data = cts
          ,aes(fill = pop)
          ,color = NA
          ,inherit.aes = F
          ,alpha = .6) +
  scale_fill_viridis_c(option = 'C')


cts %>% mapview::mapview(zcol = 'pop')
