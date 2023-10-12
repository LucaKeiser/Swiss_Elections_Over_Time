
# load packages and data --------------------------------------------------

# packages
library(tidyverse)
library(glue)

theme_set(theme_minimal())


# data
elections_raw <- pxR::read.px(here::here("Swiss_Elections_Over_Time",
                                         "01_Data",
                                         "px-x-1702020000_105.px"))





# convert to data frame ---------------------------------------------------

elections <- elections_raw$DATA$value %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rowid_to_column()

# NOTE: the numbers for 'Gemeindeebene' are of interest!


## 1. get beginning and end (Bezirk) --------------------------------------

districts <- elections %>% 
  filter(str_detect(bezirk_gemeinde, ">>"))

# 170 districts
districts %>% 
  count(bezirk_gemeinde,
        sort = TRUE)

districts <- districts %>% 
  pull(bezirk_gemeinde) %>% 
  unique()

# extract rowids

id_start_end_output <- tibble(
  bezirk_gemeinde = vector(mode = "character"),
  start_id = vector(mode = "integer"),
  end_id = vector(mode = "integer")
)

for(i in seq_along(districts)) {
  
  message(glue("{i}/{length(districts)} ({str_remove_all(districts, '>> ')[i]})..."))
  
  if(i == 170) {
    
    start_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- elections %>% 
      pull(rowid) %>% 
      max(.)
    
  } else{
    
    start_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts[i]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- elections %>% 
      filter(bezirk_gemeinde == districts[i + 1]) %>% 
      pull(rowid) %>% 
      min(.)
    
    end_id_temp <- end_id_temp - 1
    
  }
  
  # add to temporary output
  output_temp <- tibble(
    bezirk_gemeinde = districts[i],
    start_id = start_id_temp,
    end_id = end_id_temp
  )
  
  
  # add to final output
  id_start_end_output <- id_start_end_output %>% 
    bind_rows(output_temp)
  
  
  # clean up
  rm(start_id_temp, end_id_temp, 
     output_temp)
  
}

# take a look
id_start_end_output %>% 
  View()


## 2. reshape data --------------------------------------------------------

data_frame_output <- tibble(
  ergebnisse = vector(mode = "character"),
  partei = vector(mode = "character"),
  jahr = vector(mode = "character"),
  bezirk = vector(mode = "character"),
  gemeinde = vector(mode = "character")
)

for(i in 1:nrow(id_start_end_output)) {
  
  message(glue("{i}/{nrow(id_start_end_output)} ({id_start_end_output[[1]][i]})..."))
  
  # get all observations for one district
  output_temp <- elections %>% 
    slice(id_start_end_output[[2]][i]:
            id_start_end_output[[3]][i])
  
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
  data_frame_output <- bind_rows(data_frame_output,
                                 output_temp)
  
  
  # clean up
  rm(district_name_raw, district_name_clean,
     output_temp)
  
}

# take a look
data_frame_output %>% 
  count(bezirk)


## final reshape (create a wider data frame) ------------------------------

elections_final <- data_frame_output %>% 
  pivot_wider(names_from = ergebnisse,
              values_from = value) %>% 
  rename("parteistimmen" = Parteistimmen,
         "parteistaerke_prozent" = `Parteistärke in %`) %>% 
  mutate(jahr = as.integer(jahr),
         partei = ifelse(partei == "CVP", "Mitte", partei))
  






# save --------------------------------------------------------------------

write_rds(x = elections_final,
          file = here::here("Swiss_Elections_Over_Time",
                            "01_Data",
                            "siwss_elections_over_time.rds"))
