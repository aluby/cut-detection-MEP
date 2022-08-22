library(tidyverse)

file_list = list.files("cut-detection-json")

for(ii in 1:length(file_list)){

  file_name = str_replace(file_list[ii], ".json", "")
  
  file_json = paste0("cut-detection-json/", file_name, ".json")
  
  raw_json = jsonlite::read_json(file_json)
  
  scene_cuts = pluck(raw_json, "items", 1, "items") %>%
    enframe() %>%
    unnest_wider(value) %>%
    unnest_longer(items) %>%
    select(-c(name, id)) %>%
    unnest_wider(items, names_repair = "minimal") %>%
    select(-c(type, type, generator, rights)) %>%
    rename(item_id = id) %>%
    unnest_wider(target) %>% 
    select(-type) %>%
    unnest_wider(creator) %>%
    select(-type) %>%
    rename(creator_nickname = nickname) %>%
    unnest_wider(selector) %>% 
    mutate(
      start_end = str_replace(value, "t=", "")
    ) %>%
    separate(start_end, into = c("start", "end"), sep = ",", convert = TRUE) %>%
    select(-value) 
  
  write_csv(scene_cuts, file = paste0("cut-detection-csv/", file_name, ".csv"))
}

