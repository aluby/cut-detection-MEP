library(tidyverse)

file_list = list.files("cut-detection-csv")

read_and_simplify = function(filename){
  full_data = read_csv(paste0("cut-detection-csv/", filename))
  
  selected_data = full_data %>%
    select(item_id, start, end) %>%
    mutate(film_name = str_replace(filename, ".csv", ""),
           scene_num = 1:nrow(full_data))
  
  return(selected_data)
}

all_films = map_dfr(
  file_list, 
  read_and_simplify
)

all_films_gg = all_films %>%
  mutate(
    film_num = as.numeric(factor(film_name)),
    name_clean = str_extract(film_name,  "[^\\[|-|1]+")
  )

all_films_gg %>% 
  ggplot(aes(x = start, y = film_num, xend = end, yend = film_num)) + 
  geom_segment(aes(color = scene_num), size = 5) +
  geom_text(aes(x = start - 10, y = film_num, label = film_name), 
            family = "Atkinson Hyperlegible",
            hjust = 1,
            size = 2, 
            data = all_films_gg %>% filter(scene_num == 1))+
  scale_color_viridis_c(end = .75) +
  scale_x_time(expand = expansion(mult = .5)) +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(
    x = "Timestamp",
    y = "", 
    col = "Shot Index",
    title = "Results from Auto Shot Detection",
    subtitle = "111-h films"
  )

ggsave("plots/autoshot-results-111-h.png", 
       width = 10, 
       height = 10,
       bg = "white")
