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



# visual tests -----------------------------------------------------------------

# get county/plc/water layers
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
  gglyrs_c
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
