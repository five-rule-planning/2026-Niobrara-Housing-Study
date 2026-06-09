## ---------------------------
##
## Script name: viz_census.R
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
##### Libraries #####
library(data.table)
library(readr)
library(readxl)
library(sf)
library(svMisc)
library(tidyverse)

##### Helper Functions #####
### Not-in
`%notin%` <- function(x, y) !(x %in% y)

### Color Palette
color1 <- "#373b3e"
color2 <- "#bec8d1"
color3 <- "#86CECB"
color4 <- "#137a7f"
color5 <- "#e12885"

ruby <- "#E0115F"
sapphire <- "#0F52BA"

### Fonts
library(extrafont)
# font_import()
loadfonts(device = "win")

### Gazetteer File Indices
target_geoids <- c(
  # 3108360, # Center
  # 3111230, # Creighton
  3134370 # Niobrara
  # 3150335, # Verdel
  # 3150370 # Verdigre
)

gaz <- read_csv(file = "data/census/gazetteer/gaz.csv") %>%
  filter(GEOID %in% target_geoids) %>%
  select(GEOID, NAME, USPS) %>%
  mutate(
    NAME2 = str_remove(string = NAME, pattern = " city") %>%
      str_remove(string = ., pattern = " village") %>%
      str_remove(string = ., pattern = " town")
  ) %>%
  select(-NAME) %>%
  rename(STATE = USPS, NAME = NAME2)

##### Generate Decennial Census Trends Post-2000 #####
decennial <- read_csv("data/census/decennial/decennial_pops.csv") %>%
  select(-NAME)

### 2000-2010-2020 Decennial Trend
make_decennial_trends <- function(index, dat) {
  df = left_join(index, dat, by = "GEOID") %>%
    mutate(tag = paste(GEOID, NAME, STATE, sep = "_"))

  start.time = Sys.time()

  pb = txtProgressBar(min = 0, max = length(unique(df$GEOID)), initial = 0)

  names = unique(df$NAME)

  for (i in unique(df$NAME)) {
    setTxtProgressBar(pb, i)

    id_tag = df %>% filter(NAME == i) %>% select(tag) %>% unique() %>% last()
    plot_path = paste0(
      "figures/decennial_population_trends/decennial_population_trends_",
      id_tag,
      ".png"
    )
    csv_path = paste0(
      "data/census/processed/decennial_population_trends/decennial_population_trends_",
      id_tag,
      ".csv"
    )

    dat = df %>%
      filter(NAME == i) %>%
      pivot_wider(id_cols = NAME, names_from = year, values_from = population)

    years = colnames(dat)[2:length(colnames(dat))]

    if ("2010" %notin% years) {
      next
    }
    if ("2020" %notin% years) {
      next
    }

    if ("2000" %notin% years) {
      plot_name <- df %>%
        filter(NAME == i) %>%
        select(tag) %>%
        unique() %>%
        as.character()

      # paste0(df$tag)

      dat_viz = dat %>%
        mutate(
          `Annual Growth Rate 2010-2020` = (`2020` - `2010`) / `2010` * 100
        ) %>%
        select(NAME, `2010`, `2020`, `Annual Growth Rate 2010-2020`) %>%
        select(NAME, `2010`, `2020`) %>%
        pivot_longer(
          cols = !NAME,
          names_to = "Year",
          values_to = "Population"
        ) %>%
        mutate(
          population_last = lag(Population),
          pct_change = (Population - population_last) / population_last * 100,
          `Percent Change` = case_when(
            is.na(pct_change) ~ NA,
            !is.na(pct_change) ~ paste(
              format(round(pct_change, 2), nsmall = 2),
              "%"
            )
          ),
          Year = as.numeric(Year),
          y_midpoint = (Population + population_last) / 2,
          Growth = case_when(
            pct_change > 0 ~ 1,
            pct_change < 0 ~ 0
          ) %>%
            as.factor()
        )

      write_csv(x = dat_viz, file = csv_path)

      dat_plot = ggplot(data = dat_viz, aes(x = Year, y = Population)) +
        geom_bar(
          data = dat_viz %>% filter(!is.na(pct_change)),
          aes(x = Year - 5, y = pct_change * 100, fill = Growth),
          alpha = 1.00,
          stat = "identity"
        ) +
        geom_line(
          aes(x = Year, y = scale(Population) * 1000),
          color = "black",
          size = 1.5
        ) +
        geom_label(
          aes(
            x = Year - 5,
            y = pct_change * 100,
            label = paste0(round(pct_change, digits = 2), "%"),
            color = Growth
          ),
          fill = "white",
          size = 8,
          show.legend = FALSE
        ) +
        scale_color_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
        scale_fill_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
        geom_point(
          aes(x = Year, y = scale(Population) * 1000),
          color = "black",
          size = 3
        ) +
        geom_label(
          aes(
            x = Year,
            y = scale(Population) * 1000,
            label = scales::comma(Population)
          ),
          size = 8,
          color = "black",
          fill = "white"
        ) +
        xlab("") +
        ylab("") +
        scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 10)) +
        # ylim(c(0, max(dat_viz$Population)))+
        theme_minimal() +
        labs(title = paste("Population Trends in", dat_viz$NAME)) +
        theme(
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.position = "none",
          text = element_text(family = "Roboto Condensed", size = 24),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, vjust = 10),
          panel.grid.major.x = element_line(),
          axis.text.x = element_text(vjust = 0),
          axis.text.y = element_blank()
        )

      ggsave(
        filename = plot_path,
        plot = get_last_plot(),
        width = 15,
        height = 6,
        unit = "in"
      )
    } else if ("2000" %in% years) {
      plot_name <- df %>%
        filter(NAME == i) %>%
        select(tag) %>%
        unique() %>%
        as.character()

      dat_viz = dat %>%
        mutate(
          `Annual Growth Rate 2000-2010` = (`2010` - `2000`) / `2000` * 100,
          `Annual Growth Rate 2010-2020` = (`2020` - `2010`) / `2010` * 100
        ) %>%
        select(
          NAME,
          `2000`,
          `2010`,
          `Annual Growth Rate 2000-2010`,
          `2020`,
          `Annual Growth Rate 2010-2020`
        ) %>%
        select(NAME, `2000`, `2010`, `2020`) %>%
        pivot_longer(
          cols = !NAME,
          names_to = "Year",
          values_to = "Population"
        ) %>%
        mutate(
          population_last = lag(Population),
          pct_change = (Population - population_last) / population_last * 100,
          `Percent Change` = case_when(
            is.na(pct_change) ~ NA,
            !is.na(pct_change) ~ paste(
              format(round(pct_change, 2), nsmall = 2),
              "%"
            )
          ),
          Year = as.numeric(Year),
          y_midpoint = (Population + population_last) / 2,
          Growth = case_when(
            pct_change > 0 ~ 1,
            pct_change < 0 ~ 0
          ) %>%
            as.factor()
        )

      write_csv(x = dat_viz, file = csv_path)

      dat_plot = ggplot(data = dat_viz, aes(x = Year, y = Population)) +
        geom_bar(
          data = dat_viz %>% filter(!is.na(pct_change)),
          aes(x = Year - 5, y = pct_change * 100, fill = Growth),
          alpha = 1.00,
          stat = "identity"
        ) +
        geom_line(
          aes(x = Year, y = scale(Population) * 1000),
          color = "black",
          size = 1.5
        ) +
        geom_label(
          aes(
            x = Year - 5,
            y = pct_change * 100,
            label = paste0(round(pct_change, digits = 2), "%"),
            color = Growth
          ),
          fill = "white",
          size = 8,
          show.legend = FALSE
        ) +
        scale_color_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
        scale_fill_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
        geom_point(
          aes(x = Year, y = scale(Population) * 1000),
          color = "black",
          size = 3
        ) +
        geom_label(
          aes(
            x = Year,
            y = scale(Population) * 1000,
            label = scales::comma(Population)
          ),
          size = 8,
          color = "black",
          fill = "white"
        ) +
        xlab("") +
        ylab("") +
        scale_x_continuous(breaks = seq(from = 2000, to = 2020, by = 10)) +
        # ylim(c(0, max(dat_viz$Population)))+
        theme_minimal() +
        labs(title = paste("Population Trends in", dat_viz$NAME)) +
        theme(
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.position = "none",
          text = element_text(family = "Verdana", size = 24),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, vjust = 10),
          panel.grid.major.x = element_line(),
          axis.text.x = element_text(vjust = 0),
          axis.text.y = element_blank()
        )

      ggsave(
        filename = plot_path,
        plot = get_last_plot(),
        width = 15,
        height = 6,
        unit = "in"
      )
    }
  }

  close(pb)

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

make_decennial_trends(index = gaz, dat = decennial)

##### Generate Household Size Figures #####
gc()
rm(csv_path, plot_path)

acs_pre20 <- fread(input = "data/census/acs/municipal/acs5_municipal_pre20.csv")
acs_post20 <- fread(
  input = "data/census/acs/municipal/acs5_municipal_post20.csv"
)

hh_size_vars <- c(
  "GEOID",
  "NAME",
  "year",
  "population",
  "average_household_size",
  "total_families"
)

hh_sizes <- rbind(
  acs_pre20 %>% select(hh_size_vars),
  acs_post20 %>% select(hh_size_vars)
) %>%
  mutate(
    GEOID = str_pad(string = GEOID, width = 7, side = "left", pad = "0")
  ) %>%
  filter(GEOID %in% target_geoids) %>%
  select(-NAME)

make_hh_sizes <- function(index, dat) {
  df = left_join(index, dat, by = "GEOID") %>%
    mutate(tag = paste(GEOID, NAME, STATE, sep = "_"))

  start.time = Sys.time()

  pb = txtProgressBar(min = 0, max = length(unique(df$NAME)), initial = 0)

  names = unique(df$tag)

  for (i in 1:length(names)) {
    setTxtProgressBar(pb, i)

    # progress(i, progress.bar = TRUE)

    id_tag = names[i] %>%
      gsub(., pattern = " ", replacement = "_") %>%
      gsub(., pattern = ",_", replacement = "_") %>%
      gsub(., pattern = "/", replacement = "_")
    # id_tag

    place_name = df %>%
      filter(tag == id_tag) %>%
      select(NAME) %>%
      unique() %>%
      last() %>%
      as.character()

    # id_tag = df %>% filter(NAME == i) %>% select(names_dat) %>% unique() %>% last()
    plot_path_hh_sizes = paste0("figures/hh_sizes/hh_sizes_", id_tag, ".png")
    plot_path_fam_sizes = paste0("figures/hh_sizes/fam_sizes_", id_tag, ".png")

    csv_path = paste0(
      "data/census/processed/hh_sizes/hh_sizes_",
      id_tag,
      ".csv"
    )

    dat_viz = df %>%
      filter(tag == id_tag) %>%
      rename(`Average Household Size` = average_household_size) %>%
      mutate(`Average Family Size` = population / total_families) %>%
      select(year, `Average Household Size`, `Average Family Size`) %>%
      pivot_longer(
        cols = !year,
        names_to = "Statistic",
        values_to = "Value"
      ) %>%
      mutate(Statistic = as.factor(Statistic)) %>%
      group_by(Statistic) %>%
      mutate(
        value_last = lag(Value, n = 1, order_by = Statistic),
        pct_change = (Value - value_last) / value_last * 100,
        `Percent Change` = case_when(
          is.na(pct_change) ~ NA,
          !is.na(pct_change) ~ paste(
            format(round(pct_change, 2), nsmall = 2),
            "%"
          )
        ),
        Year = as.numeric(year),
        y_midpoint = (Value + value_last) / 2,
        Growth = case_when(
          pct_change > 0 ~ 1,
          pct_change < 0 ~ 0
        ) %>%
          as.factor()
      )

    write_csv(x = dat_viz, file = csv_path)

    ### Family Sizes
    dat_plot = ggplot(
      data = dat_viz %>% filter(Statistic == "Average Family Size"),
      aes(x = Year, y = Value)
    ) +
      geom_bar(
        data = dat_viz %>%
          filter(!is.na(pct_change), Statistic == "Average Family Size"),
        aes(x = Year - 0.5, y = pct_change * 100, fill = Growth),
        alpha = 1.00,
        stat = "identity"
      ) +
      geom_line(
        aes(x = Year, y = scale(Value) * 1000),
        color = "black",
        size = 1.5
      ) +
      geom_label(
        aes(
          x = Year - 0.5,
          y = pct_change * 100,
          label = paste0(round(pct_change, digits = 2), "%"),
          color = Growth
        ),
        fill = "white",
        size = 4,
        show.legend = FALSE
      ) +
      scale_color_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      scale_fill_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      geom_point(
        aes(x = Year, y = scale(Value) * 1000),
        color = "black",
        size = 3
      ) +
      geom_label(
        aes(x = Year, y = scale(Value) * 1000, label = scales::comma(Value)),
        size = 8,
        color = "black",
        fill = "white"
      ) +
      xlab("") +
      ylab("") +
      scale_x_continuous(breaks = seq(from = 2013, to = 2024, by = 1)) +
      # ylim(c(0, max(dat_viz$Population)))+
      theme_minimal() +
      labs(title = paste("Average Family Size Trends in", place_name)) +
      theme(
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "none",
        text = element_text(family = "Verdana", size = 24),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 10),
        panel.grid.major.x = element_line(),
        axis.text.x = element_text(vjust = 0),
        axis.text.y = element_blank()
      )

    ggsave(
      filename = plot_path_fam_sizes,
      plot = get_last_plot(),
      width = 15,
      height = 6,
      unit = "in"
    )

    ### Household Sizes
    dat_plot = ggplot(
      data = dat_viz %>% filter(Statistic == "Average Household Size"),
      aes(x = Year, y = Value)
    ) +
      geom_bar(
        data = dat_viz %>%
          filter(!is.na(pct_change), Statistic == "Average Household Size"),
        aes(x = Year - 0.5, y = pct_change * 100, fill = Growth),
        alpha = 1.00,
        stat = "identity"
      ) +
      geom_line(
        aes(x = Year, y = scale(Value) * 1000),
        color = "black",
        size = 1.5
      ) +
      geom_label(
        aes(
          x = Year - 0.5,
          y = pct_change * 100,
          label = paste0(round(pct_change, digits = 2), "%"),
          color = Growth
        ),
        fill = "white",
        size = 4,
        show.legend = FALSE
      ) +
      scale_color_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      scale_fill_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      geom_point(
        aes(x = Year, y = scale(Value) * 1000),
        color = "black",
        size = 3
      ) +
      geom_label(
        aes(x = Year, y = scale(Value) * 1000, label = scales::comma(Value)),
        size = 8,
        color = "black",
        fill = "white"
      ) +
      xlab("") +
      ylab("") +
      scale_x_continuous(breaks = seq(from = 2013, to = 2024, by = 1)) +
      # ylim(c(0, max(dat_viz$Population)))+
      theme_minimal() +
      labs(title = paste("Average Household Size Trends in", place_name)) +
      theme(
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "none",
        text = element_text(family = "Verdana", size = 24),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 10),
        panel.grid.major.x = element_line(),
        axis.text.x = element_text(vjust = 0),
        axis.text.y = element_blank()
      )

    ggsave(
      filename = plot_path_hh_sizes,
      plot = get_last_plot(),
      width = 15,
      height = 6,
      unit = "in"
    )
  }

  close(pb)

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

make_hh_sizes(index = gaz, dat = hh_sizes)


##### Generate Population Pyramids #####
age_vars <- c("GEOID", "year", colnames(acs_post20)[63:108])
ages <- acs_post20 %>%
  filter(year == 2024, GEOID %in% target_geoids) %>%
  select(all_of(age_vars))

make_population_pyramids <- function(index, dat) {
  df = dat %>%
    mutate(
      GEOID = str_pad(string = GEOID, width = 7, side = "left", pad = "0")
    ) %>%
    left_join(index, ., by = "GEOID") %>%
    mutate(
      tag = paste(GEOID, NAME, STATE, sep = "_") %>%
        gsub(., pattern = ",_", replacement = "_") %>%
        gsub(., pattern = "/", replacement = "_") %>%
        gsub(., pattern = " ", replacement = "_")
    )

  start.time = Sys.time()

  pb = txtProgressBar(min = 0, max = length(unique(df$NAME)), initial = 0)

  names = unique(df$tag)

  for (i in 1:length(names)) {
    setTxtProgressBar(pb, i)

    id_tag = names[i] %>%
      gsub(., pattern = " ", replacement = "_") %>%
      gsub(., pattern = ",_", replacement = "_") %>%
      gsub(., pattern = "/", replacement = "_")

    place_name = df %>%
      filter(tag == id_tag) %>%
      select(NAME) %>%
      unique() %>%
      last() %>%
      as.character()

    plot_path = paste0("figures/pop_pyramids/", id_tag, ".png")

    csv_path = paste0("data/census/processed/pop_pyramids/", id_tag, ".csv")

    dat_viz = df %>%
      filter(tag == id_tag) %>%
      mutate(
        `Male 9 and Under` = male_under5 + male_5to9,
        `Male 10 to 19` = male_10to14 + male_15to17 + male_18to19,
        `Male 20 to 29` = male_20 + male_21 + male_22to24 + male_25to29,
        `Male 30 to 39` = male_30to34 + male_35to39,
        `Male 40 to 49` = male_40to44 + male_45to49,
        `Male 50 to 59` = male_50to54 + male_55to59,
        `Male 60 to 69` = male_60to61 + male_62to64 + male_65to66 + male_67to69,
        `Male 70 to 79` = male_70to74 + male_75to79,
        `Male 80 and Over` = male_80to84 + male_85up,

        `Female 9 and Under` = female_under5 + female_5to9,
        `Female 10 to 19` = female_10to14 + female_15to17 + female_18to19,
        `Female 20 to 29` = female_20 +
          female_21 +
          female_22to24 +
          female_25to29,
        `Female 30 to 39` = female_30to34 + female_35to39,
        `Female 40 to 49` = female_40to44 + female_45to49,
        `Female 50 to 59` = female_50to54 + female_55to59,
        `Female 60 to 69` = female_60to61 +
          female_62to64 +
          female_65to66 +
          female_67to69,
        `Female 70 to 79` = female_70to74 + female_75to79,
        `Female 80 and Over` = female_80to84 + female_85up
      ) %>%
      select(
        year,

        `Male 9 and Under`,
        `Male 10 to 19`,
        `Male 20 to 29`,
        `Male 30 to 39`,
        `Male 40 to 49`,
        `Male 50 to 59`,
        `Male 60 to 69`,
        `Male 70 to 79`,
        `Male 80 and Over`,

        `Female 9 and Under`,
        `Female 10 to 19`,
        `Female 20 to 29`,
        `Female 30 to 39`,
        `Female 40 to 49`,
        `Female 50 to 59`,
        `Female 60 to 69`,
        `Female 70 to 79`,
        `Female 80 and Over`
      ) %>%
      pivot_longer(
        cols = !year,
        names_to = "Statistic",
        values_to = "Value"
      ) %>%
      mutate(
        gender = case_when(
          grepl(pattern = "Female", Statistic) ~ "Women",
          !grepl(pattern = "Female", Statistic) ~ "Men"
        ) %>%
          as.factor(),
        Statistic2 = case_when(
          grepl(pattern = "9 and Under", Statistic) ~ "9 and Under",
          grepl(pattern = "10 to 19", Statistic) ~ "10 to 19",
          grepl(pattern = "20 to 29", Statistic) ~ "20 to 29",
          grepl(pattern = "30 to 39", Statistic) ~ "30 to 39",
          grepl(pattern = "40 to 49", Statistic) ~ "40 to 49",
          grepl(pattern = "50 to 59", Statistic) ~ "50 to 59",
          grepl(pattern = "60 to 69", Statistic) ~ "60 to 69",
          grepl(pattern = "70 to 79", Statistic) ~ "70 to 79",
          grepl(pattern = "80 and Over", Statistic) ~ "80 and Over",
        ),
        Value2 = case_when(
          gender == "Women" ~ Value,
          gender == "Men" ~ -Value
        )
      )

    write_csv(x = dat_viz, file = csv_path)

    dat_plot = ggplot(
      dat_viz,
      aes(
        x = Value2,
        y = factor(
          Statistic2,
          levels = c(
            "9 and Under",
            "10 to 19",
            "20 to 29",
            "30 to 39",
            "40 to 49",
            "50 to 59",
            "60 to 69",
            "70 to 79",
            "80 and Over"
          )
        ),
        group = gender,
        fill = gender
      )
    ) +
      geom_col() +
      geom_label(
        aes(label = Value, color = gender),
        fill = "white",
        size = 6,
        show.legend = FALSE
      ) +
      xlab("") +
      ylab("") +
      labs(title = paste("Population Distribution in", place_name)) +
      scale_fill_manual(
        labels = c("Men", "Women"),
        values = c(sapphire, ruby)
      ) +
      scale_color_manual(
        labels = c("Men", "Women"),
        values = c(sapphire, ruby)
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.key = element_rect(color = NA),
        text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 5),
        panel.grid.major.x = element_line(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank()
      )

    ggsave(
      filename = plot_path,
      plot = get_last_plot(),
      width = 15,
      height = 6,
      unit = "in"
    )
  }

  close(pb)

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

make_population_pyramids(index = gaz, dat = ages)

##### Generate Population Projections #####
hist_data_paths <- list.files(
  path = "data/census/processed/historical_population_trends/",
  pattern = ".csv",
)

hist_data <- map(
  paste0(
    "data/census/processed/historical_population_trends/",
    hist_data_paths
  ),
  readr::read_csv
)

historical_trends_plots <- list()

make_historical_trends <- function(my_list) {
  for (i in 1:length(my_list)) {
    dat_viz = my_list[[i]] %>%
      mutate(
        population_last = lag(Population),
        pct_change = (Population - population_last) / population_last * 100,
        `Percent Change` = case_when(
          is.na(pct_change) ~ NA,
          !is.na(pct_change) ~ paste(
            format(round(pct_change, 2), nsmall = 2),
            "%"
          )
        ),
        Year = as.numeric(Year),
        y_midpoint = (Population + population_last) / 2,
        Growth = case_when(
          pct_change > 0 ~ 1,
          pct_change < 0 ~ 0
        ) %>%
          as.factor()
      )

    plot_path = paste0("figures/historical_trends/", dat_viz$NAME, ".png")

    dat_plot = ggplot(data = dat_viz, aes(x = Year, y = Population)) +
      geom_bar(
        data = dat_viz %>% filter(!is.na(pct_change)),
        aes(x = Year - 5, y = pct_change * 100, fill = Growth),
        alpha = 1.00,
        stat = "identity"
      ) +
      geom_line(
        aes(x = Year, y = scale(Population) * 1000),
        color = "black",
        size = 1.0
      ) +
      geom_label(
        aes(
          x = Year - 5,
          y = pct_change * 100,
          label = paste0(round(pct_change, digits = 2), "%"),
          color = Growth
        ),
        fill = "white",
        size = 4,
        show.legend = FALSE
      ) +
      scale_color_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      scale_fill_manual(breaks = c("0", "1"), values = c(ruby, sapphire)) +
      geom_point(
        aes(x = Year, y = scale(Population) * 1000),
        color = "black",
        size = 3
      ) +
      geom_label(
        aes(
          x = Year,
          y = scale(Population) * 1000,
          label = scales::comma(Population)
        ),
        size = 4,
        color = "black",
        fill = "white"
      ) +
      xlab("") +
      ylab("") +
      scale_x_continuous(breaks = seq(from = 1910, to = 2020, by = 10)) +
      theme_minimal() +
      labs(title = paste("Population Trends in", dat_viz$NAME)) +
      theme(
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "none",
        text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 10),
        panel.grid.major.x = element_line(),
        axis.text.x = element_text(vjust = 0),
        axis.text.y = element_blank()
      )

    ggsave(
      filename = plot_path,
      plot = dat_plot,
      width = 15,
      height = 6,
      unit = "in"
    )
  }
}

make_historical_trends(my_list = hist_data)

population_projections_plots <- list()

make_population_projections <- function(my_list) {
  start.time = Sys.time()

  for (i in 1:length(my_list)) {
    names = unique(my_list[[i]]$NAME)

    id_tag = names %>%
      gsub(., pattern = " ", replacement = "_") %>%
      gsub(., pattern = ",_", replacement = "_") %>%
      gsub(., pattern = "/", replacement = "_")

    plot_path = paste0("figures/pop_projections/", id_tag, ".png")

    tab_path = paste0(
      "tables/tab_pop_increase/tab_pop_increase_",
      id_tag,
      ".Rda"
    )

    pred_1pct = my_list[[i]] %>%
      pivot_wider(
        id_cols = NAME,
        names_from = Year,
        values_from = Population
      ) %>%
      mutate(
        `2030` = round(`2020` * 1.1),
        `2040` = round(`2030` * 1.1),
        `2050` = round(`2040` * 1.1)
      ) %>%
      pivot_longer(
        cols = !NAME,
        names_to = "Year",
        values_to = "Population"
      ) %>%
      mutate(
        Year = as.numeric(Year),
        Population = as.numeric(Population),
        type = "1% Annual Growth Rate"
      ) %>%
      filter(Year >= 2020)

    pred_.25pct = my_list[[i]] %>%
      pivot_wider(
        id_cols = NAME,
        names_from = Year,
        values_from = Population
      ) %>%
      mutate(
        `2030` = round(`2020` * 1.025),
        `2040` = round(`2030` * 1.025),
        `2050` = round(`2040` * 1.025)
      ) %>%
      pivot_longer(
        cols = !NAME,
        names_to = "Year",
        values_to = "Population"
      ) %>%
      mutate(
        Year = as.numeric(Year),
        Population = as.numeric(Population),
        type = "0.25% Annual Growth Rate"
      ) %>%
      filter(Year >= 2020)

    pred_.25pct_decline = my_list[[i]] %>%
      pivot_wider(
        id_cols = NAME,
        names_from = Year,
        values_from = Population
      ) %>%
      mutate(
        `2030` = round(`2020` * 0.975),
        `2040` = round(`2030` * 0.975),
        `2050` = round(`2040` * 0.975)
      ) %>%
      pivot_longer(
        cols = !NAME,
        names_to = "Year",
        values_to = "Population"
      ) %>%
      mutate(
        Year = as.numeric(Year),
        Population = as.numeric(Population),
        type = "0.25% Annual Decline"
      ) %>%
      filter(Year >= 2020)

    pred_1pct_decline = my_list[[i]] %>%
      pivot_wider(
        id_cols = NAME,
        names_from = Year,
        values_from = Population
      ) %>%
      mutate(
        `2030` = round(`2020` * 0.90),
        `2040` = round(`2030` * 0.90),
        `2050` = round(`2040` * 0.90)
      ) %>%
      pivot_longer(
        cols = !NAME,
        names_to = "Year",
        values_to = "Population"
      ) %>%
      mutate(
        Year = as.numeric(Year),
        Population = as.numeric(Population),
        type = "1% Annual Decline"
      ) %>%
      filter(Year >= 2020)

    dat_viz = rbind(
      pred_1pct,
      pred_.25pct,
      pred_.25pct_decline,
      pred_1pct_decline
    ) %>%
      mutate(Year = as.numeric(Year), Population = as.numeric(Population))

    dat_plot = ggplot() +
      geom_line(
        data = dat_viz,
        aes(
          x = Year,
          y = Population,
          group = type,
          color = type,
          linetype = type
        )
      ) +
      geom_line(
        data = my_list[[i]] %>% filter(Year >= 1980),
        aes(x = Year, y = Population)
      ) +
      geom_label(
        data = my_list[[i]] %>% filter(Year >= 1980),
        size = 8,
        aes(x = Year, y = Population, label = round(Population)),
        nudge_y = 0
      ) +
      geom_label(
        data = dat_viz %>% filter(Year >= 2030),
        size = 8,
        aes(
          x = Year,
          y = Population,
          label = as.factor(round(Population)),
          color = type
        ),
        nudge_y = 0,
        check_overlap = TRUE,
        show.legend = FALSE
      ) +
      scale_x_continuous(breaks = seq(1980, 2050, 10)) +
      xlab("") +
      ylab("") +
      labs(title = paste("Population Projections for", my_list[[i]]$NAME)) +
      scale_color_manual(
        breaks = c(
          "1% Annual Growth Rate",
          "0.25% Annual Growth Rate",
          "0.25% Annual Decline",
          "1% Annual Decline"
        ),
        values = c(sapphire, color3, color2, ruby)
      ) +
      scale_linetype_manual(
        breaks = c(
          "Actual",
          "1% Annual Growth Rate",
          "0.25% Annual Growth Rate",
          "0.25% Annual Decline",
          "1% Annual Decline"
        ),
        values = c("solid", "dashed", "dotdash", "longdash", "twodash")
      ) +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(.20, .20),
        text = element_text(size = 24, family = "Verdana"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 10),
        panel.grid.major.x = element_line(),
        axis.text.x = element_text(vjust = 0),
        axis.text.y = element_blank()
      )

    ggsave(
      filename = plot_path,
      plot = dat_plot,
      width = 15,
      height = 6,
      unit = "in"
    )

    basepop = unique(dat_viz[dat_viz$Year == 2020, ]$Population)

    tab_growth = dat_viz %>%
      mutate(popchange = Population - basepop) %>%
      filter(
        Year %in% c(2050),
        type %in% c("1% Annual Growth Rate", "0.25% Annual Growth Rate")
      ) %>%
      pivot_wider(
        data = .,
        id_cols = Year,
        names_from = type,
        values_from = popchange
      ) %>%
      mutate(Year = "Population Increase by 2050")

    tab_viz = dat_viz %>%
      filter(
        Year %in% c(2040, 2050),
        type %in% c("1% Annual Growth Rate", "0.25% Annual Growth Rate")
      ) %>%
      mutate(Year = paste0("Projected Population in ", Year)) %>%
      pivot_wider(
        data = .,
        id_cols = Year,
        names_from = type,
        values_from = Population
      ) %>%
      rbind(., tab_growth) %>%
      rename("info" = "Year") %>%
      as.data.frame()

    save(tab_viz, file = tab_path)
  }

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

make_population_projections(my_list = hist_data)

##### Housing Cost Plots #####
gc()
rm(csv_path, plot_path)

acs_pre20 <- fread(input = "data/census/acs/municipal/acs5_municipal_pre20.csv")
acs_post20 <- fread(
  input = "data/census/acs/municipal/acs5_municipal_post20.csv"
)

hh_costs_vars <- c(
  "GEOID",
  "year",
  "median_household_income",
  "median_homevalue",
  "median_gross_rent"
)

hh_costs <- rbind(
  acs_pre20 %>% select(hh_costs_vars),
  acs_post20 %>% select(hh_costs_vars)
) %>%
  mutate(
    GEOID = str_pad(string = GEOID, width = 7, side = "left", pad = "0")
  ) %>%
  filter(GEOID %in% target_geoids)

make_hh_costs <- function(index, dat) {
  df = left_join(index, dat, by = "GEOID") %>%
    mutate(tag = paste(GEOID, NAME, STATE, sep = "_"))

  start.time = Sys.time()

  pb = txtProgressBar(min = 0, max = length(unique(df$NAME)), initial = 0)

  names = unique(df$tag)

  for (i in 1:length(names)) {
    setTxtProgressBar(pb, i)

    # progress(i, progress.bar = TRUE)

    id_tag = names[i] %>%
      gsub(., pattern = " ", replacement = "_") %>%
      gsub(., pattern = ",_", replacement = "_") %>%
      gsub(., pattern = "/", replacement = "_")
    # id_tag

    place_name = df %>%
      filter(tag == id_tag) %>%
      select(NAME) %>%
      unique() %>%
      last() %>%
      as.character()

    # id_tag = df %>% filter(NAME == i) %>% select(names_dat) %>% unique() %>% last()
    plot_path = paste0("figures/hh_costs/hh_costs_", id_tag, ".png")

    csv_path = paste0(
      "data/census/processed/hh_costs/hh_costs_",
      id_tag,
      ".csv"
    )

    dat_viz = df %>%
      filter(tag == id_tag) %>%
      select(-c(GEOID, STATE, NAME, tag)) %>%
      rename(
        "Median Gross Rent" = median_gross_rent,
        "Median Home Value" = median_homevalue,
        "Median Household Income" = median_household_income
      ) %>%
      pivot_longer(
        cols = !year,
        names_to = "Statistic",
        values_to = "Value"
      ) %>%
      mutate(
        Statistic = as.factor(Statistic),
        Value = case_when(
          year == 2024 ~ (449.3 / 462.30) * Value,
          year == 2023 ~ (449.3 / 449.3) * Value,
          year == 2022 ~ (449.3 / 431.5) * Value,
          year == 2021 ~ (449.3 / 399.2) * Value,
          year == 2020 ~ (449.3 / 380.8) * Value,
          year == 2019 ~ (449.3 / 375.8) * Value,
          year == 2018 ~ (449.3 / 369.1) * Value,
          year == 2017 ~ (449.3 / 360.3) * Value,
          year == 2016 ~ (449.3 / 352.8) * Value,
          year == 2015 ~ (449.3 / 348.3) * Value,
          year == 2014 ~ (449.3 / 347.7) * Value,
          year == 2013 ~ (449.3 / 342.0) * Value
        ) %>%
          round(digits = 0)
      ) %>%
      group_by(Statistic) %>%
      mutate(
        value_last = lag(Value, n = 1, order_by = Statistic),
        pct_change = (Value - value_last) / value_last * 100,
        `Percent Change` = case_when(
          is.na(pct_change) ~ NA,
          !is.na(pct_change) ~ paste(
            format(round(pct_change, 2), nsmall = 2),
            "%"
          )
        ),
        Year = as.numeric(year),
        y_midpoint = (Value + value_last) / 2,
        Growth = case_when(
          pct_change > 0 ~ 1,
          pct_change < 0 ~ 0
        ) %>%
          as.factor()
      )

    write_csv(x = dat_viz, file = csv_path)

    ### Plot HH Costs
    dat_plot = ggplot(
      data = dat_viz,
      aes(
        fill = Statistic,
        color = Statistic,
        group = Statistic,
        label = Statistic
      )
    ) +
      geom_point(
        data = dat_viz %>% filter(Statistic != "Median Gross Rent"),
        aes(x = year, y = Value, shape = Statistic),
        size = 4
      ) +
      geom_line(
        data = dat_viz %>% filter(Statistic != "Median Gross Rent"),
        aes(x = year, y = Value),
        size = 1,
        show.legend = TRUE
      ) +
      geom_bar(
        data = dat_viz %>% filter(Statistic == "Median Gross Rent"),
        aes(x = year, y = Value * 50),
        stat = "Identity"
      ) +
      geom_label(
        aes(x = year, y = Value, label = paste0("$", scales::comma(Value))),
        size = 4,
        vjust = -0.5,
        fill = "white",
        show.legend = FALSE
      ) +
      scale_color_manual(
        labels = c(
          "Median Home Value",
          "Median Household Income",
          "Median Gross Rent"
        ),
        values = c(color1, color2, color3)
      ) +
      scale_fill_manual(
        labels = c(
          "Median Home Value",
          "Median Household Income",
          "Median Gross Rent"
        ),
        values = c(color1, color2, color3)
      ) +
      scale_x_continuous(breaks = seq(from = 2013, to = 2024, by = 1)) +
      xlab("") +
      ylab("") +
      ylim(c(0, max(dat_viz$Value)) * 1.1) +
      theme_minimal() +
      labs(title = paste("Housing Costs in", place_name)) +
      theme(
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position = "none",
        text = element_text(family = "Verdana", size = 24),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = 10),
        panel.grid.major.x = element_line(),
        axis.text.x = element_text(vjust = 0),
        axis.text.y = element_blank()
      )

    ggsave(
      filename = plot_path,
      plot = get_last_plot(),
      width = 15,
      height = 6,
      unit = "in"
    )
  }

  close(pb)

  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

make_hh_costs(index = gaz, dat = hh_costs)
