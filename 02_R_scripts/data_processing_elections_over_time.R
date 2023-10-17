
# load packages and data --------------------------------------------------

### packages
library(tidyverse)
library(glue)


### data
# 1) elections
elections_raw <- pxR::read.px(here::here("01_Data",
                                         "px-x-1702020000_105.px"))
# Source: https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen.html


# 2) ballots
ballots_raw <- pxR::read.px(here::here("01_Data",
                                       "px-x-1702020000_101.px"))
# Source: https://www.bfs.admin.ch/asset/de/12948000





# convert to data frame ---------------------------------------------------

elections <- elections_raw$DATA$value %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rowid_to_column()
# NOTE: here only the numbers for 'Gemeindeebene' are of interest!

ballots <- ballots_raw$DATA$value %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rowid_to_column()

# get Swiss Cantons
cantons <- ballots %>% 
  filter(str_detect(kanton_bezirk_gemeinde, "^[A-Z]")) %>% 
  pull(kanton_bezirk_gemeinde) %>% 
  unique()

cantons <- cantons %>% str_subset("Schweiz",
                                  negate = TRUE)





# 1) elections ------------------------------------------------------------

## 1. get beginning and end (Bezirk) --------------------------------------

districts_elections <- elections %>% 
  filter(str_detect(bezirk_gemeinde, ">>"))

# 170 districts
districts_elections %>% 
  count(bezirk_gemeinde,
        sort = TRUE)

districts_elections <- districts_elections %>% 
  pull(bezirk_gemeinde) %>% 
  unique()

# extract row-IDs
id_start_end_output_elections <- tibble(
  bezirk_gemeinde = vector(mode = "character"),
  start_id = vector(mode = "integer"),
  end_id = vector(mode = "integer")
)

for(i in seq_along(districts_elections)) {
  
  message(glue("{i}/{length(districts_elections)} ({str_remove_all(districts_elections, '>> ')[i]})..."))
  
  if(i == length(districts_elections)) {
    
    start_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts_elections[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- elections %>% 
      pull(rowid) %>% 
      max(.)
    
  } else{
    
    start_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts_elections[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts_elections[i + 1]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- end_id_temp - 1
    
  }
  
  # add to temporary output
  output_temp <- tibble(
    bezirk_gemeinde = districts_elections[i],
    start_id = start_id_temp,
    end_id = end_id_temp
  )
  
  
  # add to final output
  id_start_end_output_elections <- id_start_end_output_elections %>% 
    bind_rows(output_temp)
  
  
  # clean up
  rm(start_id_temp, end_id_temp, 
     output_temp)
  
}

# take a look
id_start_end_output_elections %>% 
  View()


## 2. reshape data --------------------------------------------------------

elections_output <- tibble(
  ergebnisse = vector(mode = "character"),
  partei = vector(mode = "character"),
  jahr = vector(mode = "character"),
  bezirk = vector(mode = "character"),
  gemeinde = vector(mode = "character")
)

for(i in 1:nrow(id_start_end_output_elections)) {
  
  message(glue("{i}/{nrow(id_start_end_output_elections)} ({id_start_end_output_elections[[1]][i]})..."))
  
  # get all observations for one district
  output_temp <- elections %>% 
    slice(id_start_end_output_elections[[2]][i]:
            id_start_end_output_elections[[3]][i])
  
  # get district names
  district_name_raw <- output_temp %>% 
    mutate(bezirk = str_extract(bezirk_gemeinde, ">> .*")) %>% 
    filter(!is.na(bezirk)) %>% 
    pull(bezirk) %>% 
    unique()
  
  district_name_clean <- district_name_raw %>% 
    str_remove(">>") %>% 
    str_squish()
  
  # finalize temporary output
  output_temp <- output_temp %>% 
    mutate(bezirk = district_name_clean) %>% 
    mutate(gemeinde = str_extract(bezirk_gemeinde, "[a-zA-Z].*")) %>% 
    filter(bezirk_gemeinde != district_name_raw) %>% 
    select(-c(rowid, bezirk_gemeinde))
  
  # finalize output
  elections_output <- bind_rows(elections_output,
                                output_temp)
  
  
  # clean up
  rm(district_name_raw, district_name_clean,
     output_temp)
  
}

# take a look
elections_output %>% 
  count(bezirk)


## final reshape (create a wider data frame) ------------------------------

elections_final <- elections_output %>% 
  pivot_wider(names_from = ergebnisse,
              values_from = value) %>% 
  rename("parteistimmen" = Parteistimmen,
         "parteistaerke_prozent" = `Parteistärke in %`) %>% 
  mutate(jahr = as.integer(jahr),
         partei = ifelse(partei == "CVP", "Mitte", partei),
         jahr = str_extract(jahr, "\\d{4}"),
         jahr = as.integer(jahr))






# 2) ballots --------------------------------------------------------------

# here it is a bit of a different story. Of interest are all levels
# (Switzerland as a whole, cantons and districts)
# => let's make different data sets first...


## 1. Switzerland as a whole ----------------------------------------------

ballots_CH <- ballots %>% 
  filter(kanton_bezirk_gemeinde == "Schweiz") %>%
  select(-c(rowid, kanton_bezirk_gemeinde)) %>% 
  pivot_wider(names_from = ergebnisse,
              values_from = value) %>% 
  janitor::clean_names() %>% 
  mutate(jahr = str_extract(jahr, "\\d{4}"),
         jahr = as.integer(jahr))


## 2. canton --------------------------------------------------------------

ballots_canton <- ballots %>% 
  filter(kanton_bezirk_gemeinde %in% cantons) %>%
  select(-rowid) %>% 
  pivot_wider(names_from = ergebnisse,
              values_from = value) %>% 
  rename("kanton" = kanton_bezirk_gemeinde) %>% 
  janitor::clean_names() %>% 
  mutate(jahr = str_extract(jahr, "\\d{4}"),
         jahr = as.integer(jahr))



## 3. district ------------------------------------------------------------

# NOTE: more or less the same procedure as above...

districts_ballots <- ballots %>% 
  filter(str_detect(kanton_bezirk_gemeinde, ">>"))

# 170 districts
districts_ballots %>% 
  count(kanton_bezirk_gemeinde,
        sort = TRUE)

districts_ballots <- districts_ballots %>% 
  pull(kanton_bezirk_gemeinde) %>% 
  unique()


### 1. extract row-ID -----------------------------------------------------

id_start_end_output_ballots <- tibble(
  kanton_bezirk_gemeinde = vector(mode = "character"),
  start_id = vector(mode = "integer"),
  end_id = vector(mode = "integer")
)

for(i in seq_along(districts_ballots)) {
  
  message(glue("{i}/{length(districts_ballots)} ({str_remove_all(districts_ballots, '>> ')[i]})..."))
  
  if(i == length(districts_ballots)) {
    
    start_id_temp <- ballots %>% 
      filter(kanton_bezirk_gemeinde == districts_ballots[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- ballots %>% 
      pull(rowid) %>% 
      max(.)
    
  } else{
    
    start_id_temp <- ballots %>% 
      filter(kanton_bezirk_gemeinde == districts_ballots[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- ballots %>% 
      filter(kanton_bezirk_gemeinde == districts_ballots[i + 1]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- end_id_temp - 1
    
  }
  
  # add to temporary output
  output_temp <- tibble(
    kanton_bezirk_gemeinde = districts_ballots[i],
    start_id = start_id_temp,
    end_id = end_id_temp
  )
  
  
  # add to final output
  id_start_end_output_ballots <- id_start_end_output_ballots %>% 
    bind_rows(output_temp)
  
  
  # clean up
  rm(start_id_temp, end_id_temp, 
     output_temp)
  
}

# take a look
id_start_end_output_ballots %>% 
  View()


### 2. reshape data -------------------------------------------------------

ballots_output <- tibble(
  ergebnisse = vector(mode = "character"),
  jahr = vector(mode = "character"),
  bezirk = vector(mode = "character"),
  gemeinde = vector(mode = "character")
)

for(i in 1:nrow(id_start_end_output_ballots)) {
  
  message(glue("{i}/{nrow(id_start_end_output_ballots)} ({id_start_end_output_ballots[[1]][i]})..."))
  
  # get all observations for one district
  output_temp <- ballots %>% 
    slice(id_start_end_output_ballots[[2]][i]:
            id_start_end_output_ballots[[3]][i])
  
  # get district names
  district_name_raw <- output_temp %>% 
    mutate(bezirk = str_extract(kanton_bezirk_gemeinde, ">> .*")) %>% 
    filter(!is.na(bezirk)) %>% 
    pull(bezirk) %>% 
    unique()
  
  district_name_clean <- district_name_raw %>% 
    str_remove(">>") %>% 
    str_squish()
  
  # finalize temporary output
  output_temp <- output_temp %>% 
    mutate(bezirk = district_name_clean) %>% 
    mutate(gemeinde = str_extract(kanton_bezirk_gemeinde, "[a-zA-Z].*")) %>% 
    filter(kanton_bezirk_gemeinde != district_name_raw) %>% 
    select(-c(rowid, kanton_bezirk_gemeinde))
  
  # finalize output
  ballots_output <- bind_rows(ballots_output,
                              output_temp)
  
  
  # clean up
  rm(district_name_raw, district_name_clean,
     output_temp)
  
}

# take a look
ballots_output %>% 
  count(bezirk)


### 3. final reshape ------------------------------------------------------

ballots_district <- ballots_output %>% 
  pivot_wider(names_from = ergebnisse,
              values_from = value) %>% 
  janitor::clean_names() %>% 
  mutate(jahr = str_extract(jahr, "\\d{4}"),
         jahr = as.integer(jahr))





# save --------------------------------------------------------------------

# elections
write_rds(x = elections_final,
          file = here::here("01_Data",
                            "siwss_elections_over_time.rds"))


# ballots
write_rds(x = ballots_CH,
          file = here::here("01_Data",
                            "ballots_over_time_CH.rds"))

write_rds(x = ballots_canton,
          file = here::here("01_Data",
                            "ballots_over_time_canton.rds"))

write_rds(x = ballots_district,
          file = here::here("01_Data",
                            "ballots_over_time_district.rds"))
