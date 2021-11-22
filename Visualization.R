library(tidyverse)
library(patchwork)
library(janitor)
library(ggrepel)
library(usethis)
library(lubridate)
library(colorspace)
library(scales)
library(kableExtra)
library(knitr)
library(sf)

# colors for plots
purple <- "#A244DA"
light_purple <- colorspace::lighten("#A244DA", 0.5)
green <- colorspace::desaturate("#2DE5D1", 0.2)
blue_gray <- "#464a62"
mid_gray <- "#ccd0dd"
light_gray <- "#f9f9fd"
orange <- "#ff9900"

# set some global theme defaults
theme_set(theme_minimal())
theme_update(text = element_text(family = "sans", color = "#464a62"))
theme_update(plot.title = element_text(hjust = 0.5, face = "bold"))
theme_update(plot.subtitle = element_text(hjust = 0.5))
# create a directory called â€œdataâ€
#dir.create("data")
#use_zip("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip", destdir = "data")

it_nuts_3 <- read_sf("ref-nuts-2021-01m.shp/NUTS_RG_01M_2021_3857_LEVL_3.shp") %>%
  filter(CNTR_CODE == "IT") %>%
  st_transform(4326) %>%
  clean_names() %>%
  mutate(urbn_desc = case_when( # add more descriptive labels for urban variable
    urbn_type == 1 ~ "Urban",
    urbn_type == 2 ~ "Intermediate",
    urbn_type == 3 ~ "Rural"
  ),
  urbn_desc = factor(urbn_desc, levels = c("Urban", "Intermediate", "Rural")))

# contextual city data
it_cities <- read_sf("https://opendata.arcgis.com/datasets/6996f03a1b364dbab4008d99380370ed_0.geojson") %>%
  clean_names() %>%
  filter(fips_cntry == "IT", pop_rank <= 5)

ggplot(it_nuts_3) +
  geom_sf(color = "#696969", fill = light_gray, lwd = 0.08) +
  labs(title = "Italy",
       subtitle = "NUTS 3 Areas") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave("Italy NUTS 3 Areas.jpeg", width = 9, height = 5)

it_bbox <- it_nuts_3 %>%
  st_union() %>% # otherwise would be calculating the bounding box of each individual area
  st_bbox()

# download the data with the following code:

#use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2020/quarter=1/2020-01-01_performance_fixed_tiles.zip", destdir = "data")
#use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2020/quarter=2/2020-04-01_performance_fixed_tiles.zip", destdir = "data")
#use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2020/quarter=3/2020-07-01_performance_fixed_tiles.zip", destdir = "data")
#use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2020/quarter=4/2020-10-01_performance_fixed_tiles.zip", destdir = "data")

# and then read in those downloaded files
# mobile_tiles_q1 <- read_sf("data/2020-01-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
#   st_crop(it_bbox)
# mobile_tiles_q2 <- read_sf("data/2020-04-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
#   st_crop(it_bbox)
# mobile_tiles_q3 <- read_sf("data/2020-07-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
#   st_crop(it_bbox)
# mobile_tiles_q4 <- read_sf("data/2020-10-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
#   st_crop(it_bbox)

# and then read in those downloaded files
#mobile_tiles_q1 <- read_sf("data/2020-01-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
#  st_crop(it_bbox)
#mobile_tiles_q2 <- read_sf("data/2020-04-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
#  st_crop(it_bbox)
#mobile_tiles_q3 <- read_sf("data/2020-07-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
#  st_crop(it_bbox)
#mobile_tiles_q4 <- read_sf("data/2020-10-01_performance_fixed_tiles/gps_fixed_tiles.shp") %>%
#  st_crop(it_bbox)

#################
# Iterative import for big files
#mobile_tiles_q4 <- read_sf("data/2020-10-01_performance_fixed_tiles/gps_fixed_tiles.shp", query = "SELECT * FROM gps_fixed_tiles LIMIT 1000000 OFFSET 6000000") %>%
#  st_crop(it_bbox)

#mobile_tiles_q2 <- read_sf("data/2020-04-01_performance_mobile_tiles/gps_mobile_tiles.shp", query = "SELECT * FROM gps_mobile_tiles LIMIT 1000000 OFFSET 4000000") %>%
  #st_crop(it_bbox)

#temp_mobile_tiles_q4 <- mobile_tiles_q4

#temp_mobile_tiles_q4 <- temp_mobile_tiles_q4 %>%
#  rbind(mobile_tiles_q4)

#mobile_tiles_q4 <- temp_mobile_tiles_q4
#################

#tiles_q1_nuts <- it_nuts_3 %>%
#  st_transform(7794) %>% 
#  st_join(mobile_tiles_q1 %>% st_transform(7794), left = FALSE) %>%
#  mutate(quarter_start = "2020-07-01")

#tiles_q1_nuts$geometry = NULL

#tiles_all <- tiles_q1_nuts %>%
#  mutate(quarter_start = ymd(quarter_start))

#tiles_all <- tiles_all %>%
#  rbind(tiles_q1_nuts) %>%
#  mutate(quarter_start = ymd(quarter_start)) #convert to date format

#tiles_q1_nuts <- read.csv("gps_fixed_tiles_q1.csv")
#tiles_q2_nuts <- read.csv("gps_fixed_tiles_q2.csv")
#tiles_q3_nuts <- read.csv("gps_fixed_tiles_q3.csv")
#tiles_q4_nuts <- read.csv("gps_fixed_tiles_q4.csv")

#tiles_all <- tiles_q1_nuts %>%
#  rbind(tiles_q2_nuts) %>%
#  rbind(tiles_q3_nuts) %>%
#  rbind(tiles_q4_nuts) 

# saveRDS(mobile_tiles_q1, "mobile_tiles_q1")
# saveRDS(mobile_tiles_q2, "mobile_tiles_q2")
# saveRDS(mobile_tiles_q3, "mobile_tiles_q3")
# saveRDS(mobile_tiles_q4, "mobile_tiles_q4")

# saveRDS(mobile_tiles_q1, "fixed_tiles_q1")
# saveRDS(mobile_tiles_q2, "fixed_tiles_q2")
# saveRDS(mobile_tiles_q3, "fixed_tiles_q3")
# saveRDS(mobile_tiles_q4, "fixed_tiles_q4")

mobile_tiles_q1 <- readRDS("mobile_tiles_q1")
mobile_tiles_q2 <- readRDS("mobile_tiles_q2")
mobile_tiles_q3 <- readRDS("mobile_tiles_q3")
mobile_tiles_q4 <- readRDS("mobile_tiles_q4")

# mobile_tiles_q1 <- readRDS("fixed_tiles_q1")
# mobile_tiles_q2 <- readRDS("fixed_tiles_q2")
# mobile_tiles_q3 <- readRDS("fixed_tiles_q3")
# mobile_tiles_q4 <- readRDS("fixed_tiles_q4")

tiles_q1_nuts <- it_nuts_3 %>%
  st_transform(7794) %>%
  st_join(mobile_tiles_q1 %>% st_transform(7794), left = FALSE) %>%
  mutate(quarter_start = "2020-01-01")

tiles_q2_nuts <- it_nuts_3 %>%
  st_transform(7794) %>%
  st_join(mobile_tiles_q2 %>% st_transform(7794), left = FALSE) %>%
  mutate(quarter_start = "2020-04-01")

tiles_q3_nuts <- it_nuts_3 %>%
  st_transform(7794) %>%
  st_join(mobile_tiles_q3 %>% st_transform(7794), left = FALSE) %>%
  mutate(quarter_start = "2020-07-01")

tiles_q4_nuts <- it_nuts_3 %>%
  st_transform(7794) %>%
  st_join(mobile_tiles_q4 %>% st_transform(7794), left = FALSE) %>%
  mutate(quarter_start = "2020-10-01")

tiles_all <- tiles_q1_nuts %>%
  rbind(tiles_q2_nuts) %>%
  rbind(tiles_q3_nuts) %>%
  rbind(tiles_q4_nuts) %>%
  mutate(quarter_start = ymd(quarter_start)) # convert to date format

#####

# mobile_q4 <- tiles_q4_nuts
# mobile_q4$geometry = NULL
# write.csv(mobile_q4, "fixed_tiles_q4_nuts.csv", row.names = FALSE)

# mobile_all <- tiles_all
# mobile_all$geometry = NULL
# write.csv(mobile_all, "mobile_tiles_all_nuts.csv", row.names = FALSE)

#####

# ggplot(it_nuts_3) +
#   geom_sf(color = mid_gray, fill = light_gray, lwd = 0.08) +
#   geom_sf(data = mobile_tiles_q4, fill = purple, color = NA) +
#   labs(title = "Italy",
#        subtitle = "Ookla® Open Data Fixed Tiles, NUTS 3 Areas") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank())

aggs_quarter <- tiles_all %>%
  st_set_geometry(NULL) %>%
  group_by(quarter_start) %>%
  summarise(tiles = n(),
            avg_d_mbps = weighted.mean(avg_d_kbps / 1000, tests), # I find Mbps easier to work with
            avg_u_mbps = weighted.mean(avg_u_kbps / 1000, tests),
            tests = sum(tests)) %>%
  ungroup()


knitr::kable(aggs_quarter) %>%
  kable_styling()

ggplot(aggs_quarter, aes(x = quarter_start)) +
  geom_point(aes(y = avg_d_mbps), color = purple, size = 3) +
  geom_line(aes(y = avg_d_mbps, color = purple), lwd = 1) +
  geom_text(aes(y = avg_d_mbps - 2, label = round(avg_d_mbps, 1)), color = purple, size = 3, family = "sans") +
  geom_point(aes(y = avg_u_mbps), color = green, size = 3) +
  geom_line(aes(y = avg_u_mbps, color = green), lwd = 1) +
  geom_text(aes(y = avg_u_mbps - 2, label = round(avg_u_mbps, 1)), color = green, size = 3, family = "sans") +
  labs(y = "", x = "Quarter start date",
       title = "Mobile Network Performance, IT",
       subtitle = "Ookla® Open Datasets | 2020") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        legend.key.size = unit(1.5, "cm"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10)) +
  scale_color_identity(name = "Network Speed",  breaks = c(purple, green), labels = c("Download Speed", "Upload Speed"), guide = "legend")+
  scale_y_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1)) +
  scale_x_date(date_labels = "%b %d")

#ggsave("M_Network Performance.jpeg", width = 9, height = 5)

ggplot(aggs_quarter, aes(x = quarter_start)) +
  geom_point(aes(y = tests), color = purple, size = 3) +
  geom_line(aes(y = tests), color = purple, lwd = 1) +
  geom_text(aes(y = c(tests[1] - 7500, tests[2] +7500, tests[3] - 7500, tests[4] +7500), label = comma(tests), x= quarter_start + 1), size = 3, color = purple) +
  labs(y = "", x = "Quarter start date",
       title = "Mobile Test Count, IT",
       subtitle = "Ookla® Open Datasets | 2020") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text = element_text(color = blue_gray)) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%b %d")

#ggsave("M_Test Count.jpeg", width = 9, height = 5)

ggplot(tiles_all) + 
  geom_histogram(aes(x = avg_d_kbps / 1000, group = quarter_start), size = 0.3, color = light_gray, fill = green) + 
  scale_x_continuous(labels = label_number(suffix = " Mbps", accuracy = 1), limits = c(NA,200)) +
  scale_y_continuous(labels = comma) +
  facet_grid(quarter_start ~ .) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        axis.title.x = element_text(hjust=1),
        axis.text = element_text(color = blue_gray),
        strip.text.y = element_text(angle = 0, color = blue_gray)) + 
  labs(y = "", x = "", title = "Mobile Download Speed Distribution by Tile, IT", 
       subtitle = "Ookla® Open Datasets | 2020")

ggsave("M_Download Speed Distribution by Tile.jpeg", width = 12, height = 7)

# generate aggregates table
nuts_3_aggs <- tiles_all %>%
  group_by(quarter_start, nuts_id, nuts_name, urbn_desc, urbn_type) %>%
  summarise(tiles = n(),
            avg_d_mbps = weighted.mean(avg_d_kbps / 1000, tests), # I find Mbps easier to work with
            avg_u_mbps = weighted.mean(avg_u_kbps / 1000, tests),
            tests = sum(tests)) %>%
  ungroup()

ggplot(nuts_3_aggs %>% filter(quarter_start == "2020-10-01")) +
  geom_sf(aes(fill = avg_d_mbps), color = blue_gray, lwd = 0.08) +
  scale_fill_stepsn(colors = RColorBrewer::brewer.pal(n = 5, name = "BuPu"), labels = label_number(suffix = " Mbps"), breaks=c(30, 40, 50, 60, 70, 90), limits=c(20,130), guide = guide_colorsteps(title = "")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        legend.text = element_text(color = blue_gray),
        axis.text = element_blank()) +
  labs(title = "Fixed Download Speed, IT", subtitle = "Ookla® Open Datasets | Q4 2020")

#ggsave("F_Q4_Download Speed.jpeg", width = 5, height = 5)

ggplot(it_nuts_3) +
  geom_sf(aes(fill = urbn_desc), color = light_gray, lwd = 0.08) +
  scale_fill_manual(values = c(purple, light_purple, green), name = "", guide = guide_legend(direction = "horizontal", label.position = "top", keywidth = 3, keyheight = 0.5)) +
  labs(title = "IT, NUTS 3 Areas") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "top")

#ggsave("IT Nuts 3 Areas.jpeg", width = 5, height = 5)

# generate aggregates table
rural_urban_aggs <- tiles_all %>%
  st_set_geometry(NULL) %>%
  group_by(quarter_start, urbn_desc, urbn_type) %>%
  summarise(tiles = n(),
            avg_d_mbps = weighted.mean(avg_d_kbps / 1000, tests), # I find Mbps easier to work with
            avg_u_mbps = weighted.mean(avg_u_kbps / 1000, tests),
            tests = sum(tests)) %>%
  ungroup()

ggplot(rural_urban_aggs %>% filter(quarter_start == "2020-10-01"), aes(x = avg_d_mbps, y = urbn_desc, fill = urbn_desc)) +
  geom_col(width = .3, show.legend = FALSE) +
  geom_jitter(data = nuts_3_aggs, aes(x = avg_d_mbps, y = urbn_desc, color = urbn_desc), size = 0.7) + 
  geom_text(aes(x = avg_d_mbps - 4, label = round(avg_d_mbps, 1)), family = "sans",  size = 3.5, color = blue_gray) +
  scale_fill_manual(values = c(orange, light_purple, green)) +
  scale_color_manual(values = darken(c(orange, light_purple, green))) +
  scale_x_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        legend.position = "none",
        axis.text = element_text(color = blue_gray)) +
  labs(y = "", x = "", 
       title = "Mobile Download Speed Distribution by NUTS 3 Area, IT", 
       subtitle = "Ookla® Open Datasets | Q4 2020")

#ggsave("M_Q4_Download Speed Distribution.jpeg", width= 9, height = 5)

ggplot(rural_urban_aggs) +
  geom_line(aes(x = quarter_start, y = avg_d_mbps, color = urbn_desc), lwd=1) +
  geom_point(aes(x = quarter_start, y = avg_d_mbps, color = urbn_desc), size=3) +
  geom_text(aes(x = quarter_start, y = avg_d_mbps - 2, label = round(avg_d_mbps, 1), color = urbn_desc), size = 3, family = "sans", show.legend = FALSE) +
  scale_color_manual(values = c(orange, light_purple, green)) +
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text = element_text(color = blue_gray),
        legend.key.size = unit(1.5, "cm"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10))+
  guides(colour = guide_legend(override.aes = list(shape = NA)))+
  labs(y = "", x = "Quarter start date", 
       title = "Mobile Download Speed by NUTS 3 Urban-Rural Type, IT", 
       subtitle = "Ookla® Open Datasets | 2020", color="NUTS 3 Type")

#ggsave("M_Download Speed by NUTS 3 Urban-Rural Type.jpeg", width= 9, height = 6)

ggplot(rural_urban_aggs) +
  geom_line(aes(x = quarter_start, y = avg_d_mbps, color = urbn_desc), lwd = 1) +
  geom_point(aes(x = quarter_start, y = avg_d_mbps, color = urbn_desc, size = tests)) +
  geom_text(aes(x = quarter_start, y = avg_d_mbps - 2, label = round(avg_d_mbps, 1), color = urbn_desc), size = 3, family = "sans", show.legend = FALSE) +
  geom_text(aes(x = quarter_start, y = avg_d_mbps + 2, label = formatC(tests, format = "e", digits = 2)), size = 3, family = "sans", show.legend = FALSE) +
  scale_color_manual(values = c(orange, light_purple, green)) +
  scale_x_date(date_labels = "%b %d") +
  scale_y_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text = element_text(color = blue_gray),
        legend.key.size = unit(1.5, "cm"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10)) +
  guides(colour = guide_legend(override.aes = list(shape = NA)))+
  labs(y = "", x = "Quarter start date", 
       title = ("Mobile Download Speed by NUTS 3 Urban-Rural Type, IT"), 
       subtitle = "Ookla® Open Datasets | 2020",
       caption = "Circle size indicates test count", size = "Test count", color="NUTS 3 Type")

#ggsave("M_Download Speed by NUTS 3 Urban-Rural Type (Test Count).jpeg", width= 9, height = 6)

bottom_20_q4 <- nuts_3_aggs%>%
  filter(quarter_start == "2020-10-01") %>% 
  top_n(n = -20, wt = avg_d_mbps) %>%
  mutate(nuts_name = fct_reorder(factor(nuts_name), -avg_d_mbps))

map <- ggplot() +
  geom_sf(data = it_nuts_3, fill = light_gray, color = mid_gray, lwd = 0.08) +
  geom_sf(data = bottom_20_q4, aes(fill = urbn_desc), color = mid_gray, lwd = 0.08, show.legend = FALSE) +
  scale_fill_manual(values = c(orange, light_purple, green), name = "", guide = guide_legend(direction = "horizontal", label.position = "top", keywidth = 3, keyheight = 0.5)) +
  labs(title = NULL,
       subtitle = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "top")

barplot <- ggplot(data = bottom_20_q4, aes(x = avg_d_mbps, y = nuts_name, fill = urbn_desc)) +
  geom_col(width = .5) +
  scale_fill_manual(values = c(orange, light_purple, green), guide = guide_legend(direction = "horizontal", label.position = "top", keywidth = 3, keyheight = 0.5, title = NULL)) +
  scale_x_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        legend.position = "top",
        axis.text = element_text(color = blue_gray)) +
  labs(y = "", x = "", 
       title = ("Slowest 20 NUTS 3 Areas by Fixed Download Speed, IT"), 
       subtitle = "Ookla® Open Datasets | Q4 2020")

# use patchwork to put it all together
barplot + map

#ggsave("F_Q4_Slowest 20 NUTS 3 Areas by Fixed Download Speed.jpeg", width= 11, height = 7)

top_20_q4 <- nuts_3_aggs %>% 
  filter(quarter_start == "2020-10-01") %>% 
  top_n(n = 20, wt = avg_d_mbps) %>%
  mutate(nuts_name = fct_reorder(factor(nuts_name), avg_d_mbps))

top_map <- ggplot() +
  geom_sf(data = it_nuts_3, fill = light_gray, color = mid_gray, lwd = 0.08) +
  geom_sf(data = top_20_q4, aes(fill = urbn_desc), color = mid_gray, lwd = 0.08, show.legend = FALSE) +
  scale_fill_manual(values = c(orange, light_purple, green), name = "", guide = guide_legend(direction = "horizontal", label.position = "top", keywidth = 3, keyheight = 0.5)) +
  labs(title = NULL,
       subtitle = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "top")

top_barplot <- ggplot(data = top_20_q4, aes(x = avg_d_mbps, y = nuts_name, fill = urbn_desc)) +
  geom_col(width = .5) +
  scale_fill_manual(values = c(orange, light_purple, green), guide = guide_legend(direction = "horizontal", label.position = "top", keywidth = 3, keyheight = 0.5, title = NULL)) +
  scale_x_continuous(labels = label_number(suffix = " Mbps", scale = 1, accuracy = 1), breaks = c(0, 25, 50, 75, 100)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1),
        legend.position = "top",
        axis.text = element_text(color = blue_gray)) +
  labs(y = "", x = "", 
       title = "Fastest 20 NUTS 3 Areas by Fixed Download Speed, IT", 
       subtitle = "Ookla® Open Datasets | Q4 2020")

top_map_comp <- top_map

top_barplot + top_map_comp

#ggsave("F_Q4_Fastest 20 NUTS 3 Areas by Fixed Download Speed.jpeg", width= 11, height = 7)

