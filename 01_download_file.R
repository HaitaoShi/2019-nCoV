library(tidyverse)
library(lubridate)
library(gganimate)


nowtime <- Sys.time() %>%
  with_tz(tz = "Asia/Shanghai")
nowtime


# define time range
d <- tibble(
  time = seq(
  	ymd_h("2020/1/25 21"),
    ymd_h(paste0(date(nowtime), " ", hour(nowtime))),
    by = "hour"
  )
) %>%
  mutate(
    date = floor_date(time, "day") %>% as.character(),
    hour = hour(time) %>% str_pad(width = 2, side = "left", pad = "0")
  ) %>%
  mutate(
    urls =
      glue::glue("http://69.171.70.18:5000/download/city_level_{date}T{hour}.csv")
  )

# get files
myfunc <- function(url) {
  download.file(url,
    destfile = paste0("./data/", basename(url)),
    method = "curl"
  )
}

# download data
d$urls %>% map(myfunc)
