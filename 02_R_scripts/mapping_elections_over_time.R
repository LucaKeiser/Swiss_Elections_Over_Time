
# define start time -------------------------------------------------------

start_time <- Sys.time()





# load packages and data --------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(scales)
library(gganimate)
library(glue)

# parallel processing
library(doParallel)

n_cores <- detectCores()
cl <- makeCluster(n_cores - 8L)
registerDoParallel(cl)

# set theme
theme_set(theme_minimal())


# data
elections_over_time <- read_rds(here::here("01_Data",
                                           "siwss_elections_over_time.rds")) %>% 
  mutate(parteistaerke_prozent = parteistaerke_prozent / 100)

shape_file <- read_sf(here::here("01_Data",
                                 "Shapefile_Switzerland",
                                 "swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp"))

# Sources:
# - https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen.html
# - https://www.swisstopo.admin.ch/de/geodata/landscape/boundaries3d.html#download





# merge data --------------------------------------------------------------

# check first
problematic_names <- shape_file %>% 
  anti_join(elections_over_time,
            by = c("NAME" = "gemeinde")) %>% 
  pull(NAME) %>% 
  unique()

elections_over_time <- elections_over_time %>% 
  mutate(gemeinde = ifelse(gemeinde == "Bagnes", "Val de Bagnes", gemeinde),
         gemeinde = ifelse(gemeinde == "Klosters-Serneus", "Klosters", gemeinde),
         gemeinde = ifelse(gemeinde == "Brione (Verzasca)", "Verzasca", gemeinde),
         gemeinde = ifelse(gemeinde == "Schwende", "Schwende-Rüte", gemeinde),
         gemeinde = ifelse(gemeinde == "La Punt-Chamues-ch", "La Punt Chamues-ch", gemeinde),
         gemeinde = ifelse(gemeinde == "Blonay", "Blonay - Saint-Légier", gemeinde),
         gemeinde = ifelse(gemeinde == "Bad Zurzach", "Zurzach", gemeinde),
         gemeinde = ifelse(gemeinde == "Welschenrohr", "Welschenrohr-Gänsbrunnen", gemeinde),
         gemeinde = ifelse(gemeinde == "Bözen", "Böztal", gemeinde),
         gemeinde = ifelse(gemeinde == "Prez-vers-Noréaz", "Prez", gemeinde),
         gemeinde = ifelse(gemeinde == "Villaz-Saint-Pierre", "Villaz", gemeinde),
         gemeinde = ifelse(gemeinde == "Herznach", "Herznach-Ueken", gemeinde),
         gemeinde = ifelse(gemeinde == "Ponte Tresa", "Tresa", gemeinde),
         gemeinde = ifelse(gemeinde == "Damphreux", "Damphreux-Lugne", gemeinde),
         gemeinde = ifelse(gemeinde == "Crans-près-Céligny", "Crans (VD)", gemeinde),
         gemeinde = ifelse(gemeinde == "Mühlethurnen", "Thurnen", gemeinde))


# check again
problematic_names <- shape_file %>% 
  anti_join(elections_over_time,
            by = c("NAME" = "gemeinde")) %>% 
  pull(NAME) %>% 
  unique()

# add problematic names as NAs to data frame (no white spots on the map...)
# a bit of a chunky approach... Not very sophisticated... But okay for the moment^^
elections_over_time <- elections_over_time %>% 
  add_row(tibble(
    partei = rep(c("SVP", "FDP", "Mitte", "GLP", "GPS", "SP"), 403),
    jahr = rep(seq(1971, 2019, 4), length(problematic_names) * 6),
    bezirk = NA_character_,
    gemeinde = sort(rep(problematic_names, 78)),
    parteistimmen = NA_real_,
    parteistaerke_prozent = NA_real_
  ))

# merge
mapping_data <- shape_file %>%
  left_join(elections_over_time %>% 
               select(jahr,
                      gemeinde, 
                      partei,
                      parteistaerke_prozent, 
                      parteistimmen),
             by = c("NAME" = "gemeinde"),
             relationship = "many-to-many")





# plot --------------------------------------------------------------------

### 1. SVP
mapping_data_SVP <- mapping_data %>% 
  filter(partei == "SVP")


p_SVP <- mapping_data_SVP %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") + 
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der SVP auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

save_animation(
  animation = animate(p_SVP, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("03_Output",
                    "map_SVP.gif")
)


### 2. FDP
mapping_data_FDP <- mapping_data %>% 
  filter(partei == "FDP")


p_FDP <- mapping_data_FDP %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") + 
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der FDP auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

save_animation(
  animation = animate(p_FDP, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("03_Output",
                    "map_FDP.gif")
)


### 3. Mitte
mapping_data_Mitte <- mapping_data %>% 
  filter(partei == "Mitte")


p_Mitte <- mapping_data_Mitte %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") + 
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der Mitte auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

animate(p_Mitte, 
        renderer = gifski_renderer(),
        height = 2000,
        width = 2750,
        nframes = 150)

save_animation(
  animation = animate(p_Mitte, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("Swiss_Elections_Over_Time",
                    "03_Output",
                    "map_Mitte.gif")
)


### 4. GLP
mapping_data_GLP <- mapping_data %>% 
  filter(partei == "GLP")


p_GLP <- mapping_data_GLP %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") + 
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der GLP auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

save_animation(
  animation = animate(p_GLP, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("03_Output",
                    "map_GLP.gif")
)


### 5. GPS
mapping_data_GPS <- mapping_data %>% 
  filter(partei == "GPS")


p_GLP <- mapping_data_GPS %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") +
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der Grünen auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

save_animation(
  animation = animate(p_GLP, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("03_Output",
                    "map_GPS.gif")
)


### 6. SP
mapping_data_SP <- mapping_data %>% 
  filter(partei == "SP")


p_SP <- mapping_data_SP %>% 
  ggplot() +
  geom_sf(aes(fill = parteistaerke_prozent),
          color = "grey50") + 
  scale_fill_viridis_c(option = "plasma",
                       labels = percent_format(),
                       na.value = "#19056B") +
  transition_manual(jahr) + 
  labs(title = "Wähler*innenanteil der SP auf Gemeindeebene | Jahr: { current_frame }",
       fill = "Partei-\nstärke",
       caption = "Aus Darstellungsgründen werden fehlende Werte (NAs) dunkelblau eingefärbt.\nDamit sind sie (beinahe aber eben nicht ganz) gleichbedeutend mit einer Parteistärke von 0%.") + 
  theme(text = element_text(size = 25))

save_animation(
  animation = animate(p_SP, 
                      renderer = gifski_renderer(),
                      height = 2000,
                      width = 2750,
                      nframes = 150),
  file = here::here("03_Output",
                    "map_SP.gif")
)





# end ---------------------------------------------------------------------

# stop parallel cluster
stopCluster(cl)

# define end time
end_time <- Sys.time()
glue("Run time: {as.numeric(round(end_time - start_time, 2))} minutes.")


