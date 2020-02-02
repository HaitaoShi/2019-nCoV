library(tidyverse)
library(here)
library(fs)
library(lubridate)
library(gganimate)
library(sf)



####################################################### 
china <- st_read("./chinamap_data/bou2_4p.shp") %>%
  st_set_crs(4326) %>%
  group_by(NAME) %>%
  summarize()


china_map <- china %>%
  mutate(NAME = iconv(NAME, "GBK", "UTF-8")) %>%
  mutate_at(vars(NAME), ~ str_remove_all(., "市|省|自治区|回族|维吾尔|壮族|特别行政区")) %>%
  mutate_at(vars(NAME), ~ str_trim(.))


china_map$NAME
china_map %>% 
  ggplot() +
  geom_sf()
####################################################### 






####################################################### 
read_plus <- function(flnm) {
  df <- read_csv(flnm) %>%
    janitor::clean_names("snake") %>%
    select(
      province_short_name,
      confirmed_count,
      suspected_count,
      cured_count,
      dead_count
    ) %>%
    distinct_all() %>%
    mutate(time = basename(flnm) %>% stringr::str_extract("\\d{4}-\\d{2}-\\d{2}T\\d{2}"))

  return(df)
}


d1 <- here::here("data") %>%
  dir_ls(regexp = "*.csv", recursive = FALSE) %>%
  map_dfr(purrr::possibly(read_plus, NA))
d1
####################################################### 









####################################################### 
mybreaks <- c(0, 1, 10, 50, 100, 500, 1000, 5000, 100000)
mylabels <- c(
  "0", "1-9", "10-49", "50-99", "100-499",
  "500-999", "1000-4999", ">=5000"
)

d2 <- d1 %>%
  mutate(conf2 = cut(confirmed_count,
    breaks = mybreaks,
    labels = mylabels, include.lowest = TRUE,
    right = FALSE, ordered_result = TRUE
  ))
####################################################### 







####################################################### 
d3 <- d2 %>% 
  group_by(time) %>%
  group_modify(
    ~ left_join(china_map, ., by = c("NAME" = "province_short_name"))
  ) %>% 
  #ungroup() %>% 
  st_as_sf(crs = 4326)


d3 %>% class()



anim <- d3 %>%
  # filter(time == "2020-01-26T07") %>%
  # filter(time %>% stringr::str_detect("2020-01-26")) %>%
  ggplot() +
  geom_sf(aes(fill = conf2)) +
  geom_sf_text(aes(label = NAME),
               size = 3
  ) +
  geom_sf_text(aes(label = confirmed_count),
               size = 3,
               nudge_y = c(-1, -1, -1)
  ) +
  coord_sf(crs = 4326) +
  #scale_fill_brewer(palette = "YlOrRd", na.value = "white", direction = 1) +
  scale_fill_manual(values = c('white','#ffffcc', '#feb24c','#fd8d3c',
                               '#fc4e2a', '#e31a1c', '#bd0026', '#800026'),
                    limits = mylabels,
                    labels = mylabels) +
  guides(fill = guide_legend(title = "确诊人数", reverse = T)) +
  transition_states(
    time,
    transition_length = 2,
    state_length = 1
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "2019-ncov疫情数据可视化 {closest_state}",
    caption = "数据来源：丁香园·丁香医生"
  ) +
  theme_bw()

# maybe need 16 minutes
#
# animate(anim, fps = 10, width = 800, height = 600)
# anim_save("2019-ncov疫情数据可视化.gif")
# or
anim_save(animation = anim, 
          width = 800, height = 600,
          fps = 2, 
          end_pause = 5,
          filename = "2019-ncov疫情数据可视化.gif")
####################################################### 



