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

change <- 
  bind_cols(filter(chetty, variable == "Median_Hhold._Income_of_Residents_in_1990"),
            select(filter(chetty, variable == "Median_Hhold._Income_of_Residents_in_2012-16"), value)) %>%
  rename(pre = value,
         post = value1) %>%
  mutate(#value = ((post / pre) - 1) * 100,
         value = post - pre, 
         variable = "Median_Hhold._Income_of_Residents_Change") %>%
  select(GEOID, name, variable, value, pre, post)

comparisons <- 
  chetty %>%
  bind_rows(change) %>%
  left_join(zones) %>%
  select(-geometry) %>%
  select(GEOID, name, zone, value, variable) %>%
  mutate(variable_name = str_replace_all(variable, pattern = "_", replacement = " ")) %>%
  mutate(variable_name = str_remove_all(variable_name, pattern = " rP |gP |p25|pall"))

##

distinct(comparisons, variable_name)
distinct(comparisons, variable)

##

national_average <-
  comparisons %>%
  group_by(variable) %>%
  summarise(nat_avg = mean(value, na.rm = TRUE))

zonal_averages <-
  comparisons %>%
  group_by(variable, zone) %>%
  summarise(zone_avg = mean(value, na.rm = TRUE))

comparisons <-
  comparisons %>%
  left_join(national_average) %>%
  left_join(zonal_averages) %>%
  drop_na(zone)

##

library(RColorBrewer)

##

theme_rot <- function () {
  theme_light() +
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_rect(fill = 'transparent'),
          strip.text = element_text(colour = 'black', face = 'bold')
    )
}

##

g <- 
  ggplot(comparisons %>%
           mutate(zone = factor(zone)) %>%
           filter(variable == "Frac._in_Top_20%_Based_on_Indiv_Income_rP_gP_pall"),
         aes(x = zone, y = value, color = zone)) +
  labs(x = NULL, y = "percent in highest earnings quintile") +
  coord_flip() +
  scale_color_brewer(palette = 'Set1',
                     guide = 'none') +
  theme_rot()

g +
  geom_jitter(size = 0.25, alpha = 0.25, show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 3.3, colour = 'gray70') +
  geom_segment(aes(x = zone, 
                   xend = zone,
                   y = nat_avg, 
                   yend = zone_avg),
               size = 0.9, colour = 'gray70') +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  geom_segment(aes(x = zone, 
                   xend = zone,
                   y = nat_avg, 
                   yend = zone_avg),
               size = 0.8) +
  geom_hline(aes(yintercept = nat_avg), color = "gray70", size = 0.75) +
  annotate("text", x = factor(1), y = 0.4, size = 5, color = "gray20",
           label = "13% in highest quintile\nwithin opportunity zones") +
  annotate("text", x = factor(0), y = 0.4, size = 5, color = "gray20",
           label = "21% without") +
  ggsave("opportunity.png", height = 6, width = 8, dpi = 300)

## Triming outliers and cleaning up the data for plotting

toydata <-
  comparisons %>%
  filter(variable != "Frac._in_Top_20%_Based_on_Indiv_Income_rP_gP_pall") %>%
  filter(variable != "Individual_Income_Excluding_Spouse_rP_gP_p25") %>%
  mutate(variable_name = str_remove_all(variable_name, "Hhold.")) %>%
  group_by(variable) %>%
  mutate(range = ntile(value, 100)) %>%
  filter(range != 100 & range != 1 & !is.na(range))
  
ggplot(toydata %>%
         mutate(zone = factor(zone)), 
       aes(x = zone, y = value, color = zone)) +
  geom_jitter(size = 0.1, alpha = 0.1, show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 1.25, colour = 'gray70') +
  geom_segment(aes(x = zone, 
                   xend = zone,
                   y = nat_avg, 
                   yend = zone_avg),
               size = 0.5, colour = 'gray70') +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  geom_segment(aes(x = zone, 
                   xend = zone,
                   y = nat_avg, 
                   yend = zone_avg),
               size = 0.5) +
  geom_hline(aes(yintercept = nat_avg), color = "gray70", size = 0.75) +
  coord_flip() +
  scale_color_brewer(palette = 'Set1',
                     guide = 'none') +
  scale_y_continuous(labels = NULL) +
  facet_wrap(~ variable_name, scales = 'free') +
  theme_rot() +
  ggsave("aggregate.png", height = 8, width = 8, dpi = 300)


  
