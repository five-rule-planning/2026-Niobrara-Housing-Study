## ---------------------------
##
## Script name: viz_parcels.R
##
## Purpose of script: This script generates .png visuals for use in the development of planning documents.
##
## Author: Jordan Duffin Wong
##
## Date Created: 2026-04-23
##
## Copyright (c) Jordan Duffin Wong, 2026
## Email: jordan@fiveruleplanning.com
##
## ---------------------------
##
## Notes: Made with R 4.5.1
##
## ---------------------------
library(fixest) # Faster OLS
library(ggrepel) # Label repels
library(ggspatial) # For static map basemaps
library(kableExtra) # For knitting table formatting
library(knitr) # For knitting table outputs
library(mapview) # For HTML Map Widgets
library(readr) # To read in .csv files
library(sf) # For shapefile manipulation
library(tidyverse) # For a variety of R data manipulations

knitr::opts_chunk$set(
  out.width = "95%",
  fig.width = 15,
  fig.asp = 0.618,
  fig.align = "center"
)

ruby <- "#E0115F"
sapphire <- "#0F52BA"

color1 <- "#373B3E"
color2 <- "#BEC8D1"
color3 <- "#27C3CF"
color4 <- "#137A7F"
color5 <- "#E12885"
color6 <- "#EC79A0"

##### Housing Conditions -- All Parcels #####
housing_condition <- read_sf(
  dsn = "data/shapefiles/shp-parcel-inventory/shp-parcel-inventory.shp"
) %>%
  filter(
    sprbndr %in%
      c("Niobrara"),
    etj == 1,
    type == "Residential",
    !is.na(conditn)
  ) %>%
  select(Prcl_ID, conditn, geometry) %>%
  rename("Parcel_ID" = "Prcl_ID", "Condition" = "conditn") %>%
  mutate(
    Condition = factor(
      Condition,
      levels = c(
        "Worn-Out",
        "Worn-Out - Badly Worn",
        "Badly Worn",
        "Badly Worn - Average",
        "Average",
        "Average - Good",
        "Good",
        "Good - Very Good",
        "Very Good"
      )
    )
  ) %>%
  st_transform(crs = 4326)

ggplot(data = housing_condition) +
  annotation_map_tile(type = "osm", zoom = 15) +
  geom_sf(aes(fill = Condition), inherit.aes = FALSE) +
  scale_fill_manual(
    values = c(
      "#3D0C02",
      "#79443B",
      "#FF0800",
      "#FF7F50",
      "#FFBF00",
      "#C0FF00",
      "#32CD32",
      "#01796F",
      "#007FFF"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(
  filename = "figures/housing_conditions_all.png",
  plot = last_plot(),
  width = 15,
  height = 6,
  unit = "in"
)

sum_housing_condition <- housing_condition %>%
  mutate(area = as.numeric(st_area(.) * 0.002471054)) %>%
  as.data.frame(.) %>%
  select(area) %>%
  sum()

tab_housing_condition <- housing_condition %>%
  mutate(area = as.numeric(st_area(.) * 0.002471054)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(Condition) %>%
  summarise(
    "Parcels" = n(),
    "Area (Square Acres)" = sum(area, na.rm = TRUE) %>%
      round(x = ., digits = 2),
    "Percent of Total Area" = paste0(
      round((`Area (Square Acres)` / sum_housing_condition) * 100, digits = 2),
      "%"
    )
  )
save(tab_housing_condition, file = "tables/tab_housing_conditions_all.Rda")

##### Housing Age -- All Parcels #####
housing_age <- read_sf(
  dsn = "data/shapefiles/shp-parcel-inventory/shp-parcel-inventory.shp"
) %>%
  filter(
    sprbndr %in%
      c("Niobrara"),
    etj == 1,
    type == "Residential",
    !is.na(decade)
  ) %>%
  select(Prcl_ID, decade, geometry) %>%
  rename("Parcel_ID" = "Prcl_ID", "Decade Built" = "decade") %>%
  mutate(
    `Decade Built` = factor(
      `Decade Built`,
      levels = c(
        "Before 1900",
        "1900 - 1919",
        "1920 - 1939",
        "1940 - 1959",
        "1960 - 1979",
        "1980 - 1999",
        "2000 or later"
      )
    )
  ) %>%
  st_transform(crs = 4326)

ggplot(data = housing_age) +
  annotation_map_tile(type = "osm", zoom = 15) +
  geom_sf(aes(fill = `Decade Built`), inherit.aes = FALSE) +
  scale_fill_manual(
    values = c(
      "#3D0C02",
      "#FF0800",
      "#FFBF00",
      "#C0FF00",
      "#32CD32",
      "#01796F",
      "#007FFF"
    )
  ) +
  theme_void() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave(
  filename = "figures/housing_age_all.png",
  plot = last_plot(),
  width = 15,
  height = 6,
  unit = "in"
)

sum_housing_age <- housing_age %>%
  mutate(area = as.numeric(st_area(.) * 0.002471054)) %>%
  as.data.frame(.) %>%
  select(area) %>%
  sum()

tab_housing_age <- housing_age %>%
  mutate(area = as.numeric(st_area(.) * 0.002471054)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(`Decade Built`) %>%
  summarise(
    "Parcels" = n(),
    "Area (Square Acres)" = sum(area, na.rm = TRUE) %>%
      round(x = ., digits = 2),
    "Percent of Total Area" = paste0(
      round((`Area (Square Acres)` / sum_housing_age) * 100, digits = 2),
      "%"
    )
  )

save(tab_housing_age, file = "tables/tab_housing_age_all.Rda")

##### Relate Housing Age and Condition #####
housingdata <- read_sf(
  dsn = "data/shapefiles/shp-parcel-inventory/shp-parcel-inventory.shp"
) %>%
  filter(
    sprbndr %in%
      c("Niobrara"),
    etj == 1,
    type == "Residential",
    !is.na(decade)
  ) %>%
  select(Prcl_ID, decade, geometry, conditn, YearBlt) %>%
  rename(
    "Parcel_ID" = "Prcl_ID",
    "Condition" = "conditn",
    "Decade Built" = "decade"
  ) %>%
  mutate(
    `Decade Built` = factor(
      `Decade Built`,
      levels = c(
        "Before 1900",
        "1900 - 1919",
        "1920 - 1939",
        "1940 - 1959",
        "1960 - 1979",
        "1980 - 1999",
        "2000 or later"
      )
    ),
    Condition = factor(
      Condition,
      levels = c(
        "Worn-Out",
        "Worn-Out - Badly Worn",
        "Badly Worn",
        "Badly Worn - Average",
        "Average",
        "Average - Good",
        "Good",
        "Good - Very Good",
        "Very Good"
      )
    ),
    ConditionNumeric = case_when(
      Condition == "Worn-Out" ~ 0,
      Condition == "Worn-Out - Badly Worn" ~ 1,
      Condition == "Badly Worn" ~ 2,
      Condition == "Badly Worn - Average" ~ 3,
      Condition == "Average" ~ 4,
      Condition == "Average - Good" ~ 5,
      Condition == "Good" ~ 6,
      Condition == "Good - Very Good" ~ 7,
      Condition == "Very Good" ~ 8
    )
  ) %>%
  as.data.frame()

ggplot(
  data = housingdata %>% filter(!is.na(Condition)),
  aes(x = YearBlt, y = ConditionNumeric, color = Condition)
) +
  geom_jitter() +
  scale_color_manual(
    values = c(
      "#3D0C02",
      "#79443B",
      "#FF0800",
      "#FF7F50",
      "#FFBF00",
      "#C0FF00",
      "#32CD32",
      "#01796F",
      "#007FFF"
    )
  ) +
  xlab("Year Built") +
  ylab("Condition") +
  theme_minimal() +
  theme(
    text = element_text(size = 24),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
