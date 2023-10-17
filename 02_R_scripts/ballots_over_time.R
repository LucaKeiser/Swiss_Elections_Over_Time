
# load packages and data --------------------------------------------------

### packages
library(tidyverse)
library(tidytext)
library(glue)
library(scales)

theme_set(theme_minimal())

### data
ballots_CH <- read_rds(here::here("01_Data",
                                  "ballots_over_time_CH.rds"))

ballots_canton <- read_rds(here::here("01_Data",
                                      "ballots_over_time_canton.rds"))

ballots_district <- read_rds(here::here("01_Data",
                                        "ballots_over_time_district.rds"))



# EDA ---------------------------------------------------------------------

ballots_CH %>% 
  ggplot(aes(x = jahr)) + 
  geom_line(aes(y = wahlberechtigte,
                color = "Anzahl wahlberechtigter Personen"),
            linewidth = 2) + 
  geom_point(aes(y = wahlberechtigte,
                 color = "Anzahl wahlberechtigter Personen"),
             size = 4) +
  geom_line(aes(y = wahlende_eingelegte_wahlzettel_wz,
                color = "Eingelegte Wahlzettel"),
            linewidth = 2) + 
  geom_point(aes(y = wahlende_eingelegte_wahlzettel_wz,
                 color = "Eingelegte Wahlzettel"),
             size = 4) +
  geom_line(aes(y = (wahlbeteiligung_percent * 54592.18),
                color = "Wahlbeteiligung (in %)"),
            linewidth = 2) + 
  geom_point(aes(y = (wahlbeteiligung_percent * 54592.18),
                 color = "Wahlbeteiligung (in %)"),
             size = 4) +
  expand_limits(y = 0) + 
  scale_x_continuous(breaks = seq(1971, 2019, 4)) + 
  scale_y_continuous(breaks = seq(0, 5500000, 
                                  length.out = 6),
                     labels = comma_format(big.mark = "'"),
                     name = "Anzahl Personen\n",
                     sec.axis = sec_axis(~.,
                                         breaks = seq(0, 5500000, 
                                                      length.out = 6),
                                         labels = glue("{c(0, 20, 40, 60, 80, 100)}%"),
                                         name = "Wahlbeteiligung (in %)\n")) +
  scale_color_discrete(breaks = c("Anzahl wahlberechtigter Personen",
                                  "Wahlbeteiligung (in %)",
                                  "Eingelegte Wahlzettel")) + 
  theme(legend.position = c(0.9, 0.15)) + 
  labs(title = "\nNationalratswahlen in der Schweiz (1971 - 2019)\n",
       x = "",
       color = "")


ballots_CH %>% 
  mutate(gultige_wz_percent = gultige_wz / wahlende_eingelegte_wahlzettel_wz,
         leere_wz_percent = leere_wz / wahlende_eingelegte_wahlzettel_wz,
         ungultige_wz_percent = ungultige_wz / wahlende_eingelegte_wahlzettel_wz) %>%
  select(jahr, gultige_wz_percent, leere_wz_percent, ungultige_wz_percent) %>% 
  pivot_longer(cols = -jahr,
               names_to = "variable",
               values_to = "value") %>% 
  mutate(variable = fct_reorder(variable, value)) %>% 
  ggplot(aes(jahr, value, fill = variable)) + 
  geom_col() +
  coord_flip()


ballots_district %>% 
  mutate(gultige_wz_percent = gultige_wz / wahlende_eingelegte_wahlzettel_wz,
         leere_wz_percent = leere_wz / wahlende_eingelegte_wahlzettel_wz,
         ungultige_wz_percent = ungultige_wz / wahlende_eingelegte_wahlzettel_wz) %>% 
  select(jahr, bezirk, gemeinde, gultige_wz_percent, leere_wz_percent, ungultige_wz_percent) %>% 
  pivot_longer(cols = -c(jahr, bezirk, gemeinde),
               names_to = "variable",
               values_to = "value") %>% 
  filter(variable == "ungultige_wz_percent") %>% 
  group_by(jahr) %>% 
  top_n(10) %>% 
  mutate(gemeinde = reorder_within(gemeinde, value, jahr)) %>% 
  ggplot(aes(gemeinde, value)) + 
  geom_col(aes(fill = factor(jahr)),
           show.legend = FALSE) + 
  facet_wrap(~ jahr,
             scales = "free",
             ncol = 3) + 
  coord_flip() +
  scale_x_reordered() + 
  scale_y_continuous(labels = percent_format()) +
  labs(title = "\nNationalratswahlen - Anteil ungültiger Wahlzettel (1971 - 2023)",
       subtitle = "Dargestellt sind die 10 Gemeinden mit den höchsten Werten pro Jahr\n",
       x = "",
       y = "")



