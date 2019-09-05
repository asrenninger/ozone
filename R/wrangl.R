library(tidyverse)
library(sf)

##

zones <- st_read("data/oz.shp") %>% st_drop_geometry()
tract <- st_read("data/tracts.shp")

##

zones <- left_join(tract, zones)

##

zones <-
  zones %>%
  transmute(GEOID = GEOID10, 
            zone = if_else(QOZ == "DESIGNATEDQOZ", 1, 0)) %>%
  replace_na(list("zone" = 0)) %>%
  st_as_sf()

##

wealth <- 
  read_csv("data/tract_kir_top20_rP_gP_pall.csv") %>%
  set_names(c("GEOID", "name", "top_20")) %>% 
  mutate(GEOID = if_else(GEOID < 10000000000, paste("0", GEOID, sep = ""), paste(GEOID)))

glimpse(wealth)

##

library(glue)
library(fs)

##

files <- dir_ls("data/chetty")
length(files)

chetty <- 
  reduce(
  map(1:length(files), function(x) {
    files[x] %>%
      read_csv() %>%
      mutate(variable = names(.)[3]) %>%
      set_names(c("GEOID", "name", "value", "variable")) %>%
      mutate(GEOID = if_else(GEOID < 10000000000, paste("0", GEOID, sep = ""), paste(GEOID)))
  }), 
  bind_rows
)

##

distinct(chetty, variable)

##

comparisons <- 
  chetty %>%
  left_join(zones) %>%
  select(-geometry) %>%
  select(GEOID, name, zone, value, variable) %>%
  mutate(variable_name = str_replace_all(variable, pattern = "_", replacement = " "))

##

distinct(comparisons, variable_name)
