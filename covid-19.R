library(sp)
library(sf)
library(tmap)
library(tmaptools)
library(rgdal)
library(dplyr)
library(tigris)

cv <- read.csv("us-counties.csv")
pop <- read.csv("co-est2019-alldata.csv")

### Wyoming County Map ###
# wy_counties <- counties(state = 56, resolution = "500k")
# rgdal::writeOGR(wy_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/wy_shp", "wy_map", driver = "ESRI Shapefile")

cv_wy <- filter(cv, state == "Wyoming", date == "2020-03-26")

wy_shp <- st_read("wy_shp\\wy_map.shp")

map_wy <- tbl_df(cv_wy) %>% group_by(county) %>% mutate(state = state)

map_dat_wy <- wy_shp %>% left_join(map_wy, by=c('NAME' = 'county'))

map_dat_wy$cases[is.na(map_dat_wy$cases)] <- 0

pop_wy <- filter(pop, STNAME == "Wyoming")

map_wy_pop <- tbl_df(pop_wy) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_wy <- map_dat_wy %>% left_join(map_wy_pop, by=c('NAME' = 'CTYNAME'))

map_dat_wy$wypop_10k <- map_dat_wy$POPESTIMATE2019 / 10000

map_dat_wy$incidence <- map_dat_wy$cases / map_dat_wy$wypop_10k

map1_wy <- tm_shape(map_dat_wy) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 8,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Wyoming COVID-19 Cases, 2020-03-26")

map2_wy <- tm_shape(map_dat_wy) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 8,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Wyoming COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_wy, map2_wy, ncol = 2)

### Oregon County Map ###
# or_counties <- counties(state = 41, resolution = "500k")
# rgdal::writeOGR(or_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/or_shp", "or_map", driver = "ESRI Shapefile")

cv_or <- filter(cv, state == "Oregon", date == "2020-03-26")

or_shp <- st_read("or_shp\\or_map.shp")

map_or <- tbl_df(cv_or) %>% group_by(county) %>% mutate(state = state)

map_dat_or <- or_shp %>% left_join(map_or, by=c('NAME' = 'county'))

map_dat_or$cases[is.na(map_dat_or$cases)] <- 0

pop_or <- filter(pop, STNAME == "Oregon")

map_or_pop <- tbl_df(pop_or) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_or <- map_dat_or %>% left_join(map_or_pop, by=c('NAME' = 'CTYNAME'))

map_dat_or$orpop_10k <- map_dat_or$POPESTIMATE2019 / 10000

map_dat_or$incidence <- map_dat_or$cases / map_dat_or$orpop_10k

map1_or <- tm_shape(map_dat_or) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 10,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Oregon COVID-19 Cases, 2020-03-26")

map2_or <- tm_shape(map_dat_or) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 10,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Oregon COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_or, map2_or, ncol = 2)

### Colorado County Map ###
# co_counties <- counties(state = 08, resolution = "500k")
# rgdal::writeOGR(co_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/co_shp", "co_map", driver = "ESRI Shapefile")

cv_co <- filter(cv, state == "Colorado", date == "2020-03-26")

co_shp <- st_read("co_shp\\co_map.shp")

map_co <- tbl_df(cv_co) %>% group_by(county) %>% mutate(state = state)

map_dat_co <- co_shp %>% left_join(map_co, by=c('NAME' = 'county'))

map_dat_co$cases[is.na(map_dat_co$cases)] <- 0

pop_co <- filter(pop, STNAME == "Colorado")

map_co_pop <- tbl_df(pop_co) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_co <- map_dat_co %>% left_join(map_co_pop, by=c('NAME' = 'CTYNAME'))

map_dat_co$copop_10k <- map_dat_co$POPESTIMATE2019 / 10000

map_dat_co$incidence <- map_dat_co$cases / map_dat_co$copop_10k
map1_co <- tm_shape(map_dat_co) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 6,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Colorado COVID-19 Cases, 2020-03-26")

map2_co <- tm_shape(map_dat_co) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 6,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Colorado COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_co, map2_co, ncol = 2)

### Minnesota County Map ###
# mn_counties <- counties(state = 27, resolution = "500k")
# rgdal::writeOGR(mn_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/mn_shp", "mn_map", driver = "ESRI Shapefile")

cv_mn <- filter(cv, state == "Minnesota", date == "2020-03-26")

mn_shp <- st_read("mn_shp\\mn_map.shp")

map_mn <- tbl_df(cv_mn) %>% group_by(county) %>% mutate(state = state)

map_dat_mn <- mn_shp %>% left_join(map_mn, by=c('NAME' = 'county'))

map_dat_mn$cases[is.na(map_dat_mn$cases)] <- 0

pop_mn <- filter(pop, STNAME == "Minnesota")

map_mn_pop <- tbl_df(pop_mn) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_mn <- map_dat_mn %>% left_join(map_mn_pop, by=c('NAME' = 'CTYNAME'))

map_dat_mn$mnpop_10k <- map_dat_mn$POPESTIMATE2019 / 10000

map_dat_mn$incidence <- map_dat_mn$cases / map_dat_mn$mnpop_10k

map1_mn <- tm_shape(map_dat_mn) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 7,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Minnesota COVID-19 Cases, 2020-03-26")

map2_mn <- tm_shape(map_dat_mn) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 7,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Minnesota COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_mn, map2_mn, ncol = 2)

### Pennsylvania County Map ###
# pa_counties <- counties(state = 42, resolution = "500k")
# rgdal::writeOGR(pa_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/pa_shp", "pa_map", driver = "ESRI Shapefile")

cv_pa <- filter(cv, state == "Pennsylvania", date == "2020-03-26")

pa_shp <- st_read("pa_shp\\pa_map.shp")

map_pa <- tbl_df(cv_pa) %>% group_by(county) %>% mutate(state = state)

map_dat_pa <- pa_shp %>% left_join(map_pa, by=c('NAME' = 'county'))

map_dat_pa$cases[is.na(map_dat_pa$cases)] <- 0

pop_pa <- filter(pop, STNAME == "Pennsylvania")

map_pa_pop <- tbl_df(pop_pa) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_pa <- map_dat_pa %>% left_join(map_pa_pop, by=c('NAME' = 'CTYNAME'))

map_dat_pa$papop_10k <- map_dat_pa$POPESTIMATE2019 / 10000

map_dat_pa$incidence <- map_dat_pa$cases / map_dat_pa$papop_10k

map1_pa <- tm_shape(map_dat_pa) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Pennsylvania COVID-19 Cases, 2020-03-26")

map2_pa <- tm_shape(map_dat_pa) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Pennsylvania COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_pa, map2_pa, ncol = 2)

### Washington County Map ###
# wa_counties <- counties(state = 53, resolution = "500k")
# rgdal::writeOGR(wa_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/wa_shp", "wa_map", driver = "ESRI Shapefile")

cv_wa <- filter(cv, state == "Washington", date == "2020-03-26")

wa_shp <- st_read("wa_shp\\wa_map.shp")

map_wa <- tbl_df(cv_wa) %>% group_by(county) %>% mutate(state = state)

map_dat_wa <- wa_shp %>% left_join(map_wa, by=c('NAME' = 'county'))

map_dat_wa$cases[is.na(map_dat_wa$cases)] <- 0

pop_wa <- filter(pop, STNAME == "Washington")

map_wa_pop <- tbl_df(pop_wa) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_wa <- map_dat_wa %>% left_join(map_wa_pop, by=c('NAME' = 'CTYNAME'))

map_dat_wa$wapop_10k <- map_dat_wa$POPESTIMATE2019 / 10000

map_dat_wa$incidence <- map_dat_wa$cases / map_dat_wa$wapop_10k

map1_wa <- tm_shape(map_dat_wa) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Washington COVID-19 Cases, 2020-03-26")

map2_wa <- tm_shape(map_dat_wa) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Washington COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_wa, map2_wa, ncol = 2)

### Georgia County Map ###
# ga_counties <- counties(state = 13, resolution = "500k")
# rgdal::writeOGR(ga_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/ga_shp", "ga_map", driver = "ESRI Shapefile")

cv_ga <- filter(cv, state == "Georgia", date == "2020-03-26")

ga_shp <- st_read("ga_shp\\ga_map.shp")

map_ga <- tbl_df(cv_ga) %>% group_by(county) %>% mutate(state = state)

map_dat_ga <- ga_shp %>% left_join(map_ga, by=c('NAME' = 'county'))

map_dat_ga$cases[is.na(map_dat_ga$cases)] <- 0

pop_ga <- filter(pop, STNAME == "Georgia")

map_ga_pop <- tbl_df(pop_ga) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_ga <- map_dat_ga %>% left_join(map_ga_pop, by=c('NAME' = 'CTYNAME'))

map_dat_ga$gapop_10k <- map_dat_ga$POPESTIMATE2019 / 10000

map_dat_ga$incidence <- map_dat_ga$cases / map_dat_ga$gapop_10k

map1_ga <- tm_shape(map_dat_ga) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Georgia COVID-19 Cases, 2020-03-26")

map2_ga <- tm_shape(map_dat_ga) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 9,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Georgia COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_ga, map2_ga, ncol = 2)

### Maine County Map ###
# me_counties <- counties(state = 23, resolution = "500k")
# rgdal::writeOGR(me_counties, "C:/Users/Joe Froelicher/OneDrive/corona/covid-19-data/me_shp", "me_map", driver = "ESRI Shapefile")

cv_me <- filter(cv, state == "Maine", date == "2020-03-26")

me_shp <- st_read("me_shp\\me_map.shp")

map_me <- tbl_df(cv_me) %>% group_by(county) %>% mutate(state = state)

map_dat_me <- me_shp %>% left_join(map_me, by=c('NAME' = 'county'))

map_dat_me$cases[is.na(map_dat_me$cases)] <- 0

pop_me <- filter(pop, STNAME == "Maine")

map_me_pop <- tbl_df(pop_me) %>% group_by(CTYNAME) %>% mutate(STNAME = STNAME)

map_dat_me <- map_dat_me %>% left_join(map_me_pop, by=c('NAME' = 'CTYNAME'))

map_dat_me$mepop_10k <- map_dat_me$POPESTIMATE2019 / 10000

map_dat_me$incidence <- map_dat_me$cases / map_dat_me$mepop_10k

map1_me <- tm_shape(map_dat_me) +
  tm_polygons(
    "cases",
    title = "Case Count",
    palette="YlOrBr",
    n = 5,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Maine COVID-19 Cases, 2020-03-26")

map2_me <- tm_shape(map_dat_me) +
  tm_polygons(
    "incidence",
    title = "Incidence per 10k",
    palette="YlOrBr",
    n = 5,
    legend.show=TRUE
  ) +
  tm_legend(position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, legend.frame = TRUE, panel.show = TRUE, panel.labels = "Maine COVID-19 Incidence, 2020-03-26")

tmap_arrange(map1_me, map2_me, ncol = 2)
