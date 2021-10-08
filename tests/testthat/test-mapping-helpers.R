library(sf)
library(tidyverse)
library(visaux)

# ~~these are not finished ~~

# option setting
sf_use_s2(F)
options(tigris_use_cache = TRUE)

devtools::load_all()

rm(list=ls())

# get an area surrounding st louis
stl <- geox::rx %>%
  filter(grepl("St. Louis", cz_name))

library(mapview)
plc <-
  divM::largest.plc.in.cz %>%
  filter(cz.id %in% stl$cz)

# I think that shows me that plc centroid will actually be better.
buffer.dst <- units::set_units(20, 'miles')
buffered.plc <- plc[1] %>%
  divM::conic.transform() %>%
  st_centroid() %>%
  st_buffer(buffer.dst)
plc[1] %>% mapview::mapview()
buffered.plc[1] %>% mapview::mapview()


# get counties within st louis and in the surrounding area
?visaux::county.subset(plc)
visaux::county.subset(buffered.plc, subset.approach = 'crop')
ctsf <- stl$countyfp %>%
  map_dfr(
    ~tigris::tracts(state = substr(.x, 1,2)
                    ,county = substr(.x, 3,5)
                    ,year= 2019)
  )

ctsf

