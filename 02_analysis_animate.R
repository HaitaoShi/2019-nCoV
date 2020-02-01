library(tidyverse)
library(here)
library(fs)
library(lubridate)
library(gganimate)
library(sf)
library(magick)

####################################################### 33
# 地图
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
####################################################### 33



 
####################################################### 33
# 一小时，一个文件, 一个dataframe,  一张图
read_plus <- function(flnm) {
  
  time_stamp <- basename(flnm) %>% stringr::str_extract("\\d{4}-\\d{2}-\\d{2}T\\d{2}") 

  d1 <- read_csv(flnm) %>%
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
  
  d3 <- china_map %>% 
    left_join(d2, by = c("NAME" = "province_short_name")) %>% 
    fill(time)
  
  p <- d3 %>% 
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
    scale_fill_manual(values = c('white','#ffffcc', '#feb24c','#fd8d3c',
                                 '#fc4e2a', '#e31a1c', '#bd0026', '#800026'),
                      limits = mylabels,
                      labels = mylabels) +
    guides(fill = guide_legend(title = "确诊人数", reverse = T)) +
    labs(
      x =NULL, 
      y= NULL,
      title =  paste0("2019-ncov疫情数据可视化", time_stamp),
      caption = "数据来源：丁香园·丁香医生"
    ) +
    theme_bw()
  
  ggsave(
    filename = paste0("./img/", time_stamp, ".png"),
    plot = p,
    width = 8, 
    height = 6,
    dpi = 100,
    device = 'png'
  ) 
  
}
####################################################### 33






####################################################### 33
# 单文件测试
here::here("data", "city_level_2020-01-26T17.csv") %>% 
  read_plus
####################################################### 33





####################################################### 33
# 批量执行
here::here("data") %>%
  dir_ls(regexp = "*.csv", recursive = FALSE) %>%
  purrr::walk(purrr::possibly(read_plus, NA))

####################################################### 33







####################################################### 33
# 多图生成gif
# https://ropensci.org/tutorials/magick_tutorial/

anim <- here::here("img") %>% 
  dir_ls(regexp = "*.png") %>% 
  # stringr::str_subset(., "2020-01-26") %>% 
  
  purrr::map(~image_read(.)) %>% 
  image_join() %>% 
  image_animate(
    fps = 1,
    delay = NULL,
    loop = 0,
    dispose = "previous",
    optimize = TRUE
) 


 image_write(image = anim, path = "疫情地图可视化动态图.gif")
####################################################### 33


