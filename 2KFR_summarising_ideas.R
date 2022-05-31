## code below is an example of how the data can be split up to show areas burnt
## for different times since last burnt. Shows how a greater than or equal bin
## can be created and the wide format can also be exported to show park and 
## vegetype summaries.

## Bart Huntley 21 July 2021

library(tidyverse)

df <- read_csv("./output_stats/tslb_area_stats.csv")


## whole of drysdale example

# makes all possibilities of variables
df_exp <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP") %>%
  expand(year, tslb_yrs, vegetype)

df_04 <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP") %>%
  right_join(df_exp, by = c("year", "tslb_yrs", "vegetype")) %>%
  arrange(year) %>%
  filter(tslb_yrs < 5) %>%
  group_by(year, tslb_yrs) %>% ## combines euc and sandstone
  summarise(area = sum(burnt_area_ha))

# must be setup to compliment df_exp
df_all <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP" & tslb_yrs >= 5) %>%
  group_by(year) %>% ## combines euc and sandstone
  summarise(area = sum(burnt_area_ha)) %>%
  mutate(tslb_yrs = 5) %>%
  select(year, tslb_yrs,  area) %>%
  full_join(df_04, by = c("year", "tslb_yrs", "area")) %>%
  arrange(year) %>%
  pivot_wider(names_from = year, values_from = area)

## drysdale example

# makes all possibilities of variables
df_exp <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP") %>%
  expand(year, tslb_yrs, vegetype)

df_04 <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP") %>%
  right_join(df_exp, by = c("year", "tslb_yrs", "vegetype")) %>%
  arrange(year) %>%
  filter(tslb_yrs < 5) %>%
  group_by(year, tslb_yrs, vegetype) %>% 
  summarise(area = sum(burnt_area_ha))

# must be setup to compliment df_exp
df_all <- df %>%
  filter(!is.na(tslb_yrs) & park == "Drysdale River NP" & tslb_yrs >= 5) %>%
  group_by(year, vegetype) %>% ## combines euc and sandstone
  summarise(area = sum(burnt_area_ha)) %>%
  mutate(tslb_yrs = 5) %>%
  select(year, tslb_yrs, vegetype, area) %>%
  full_join(df_04, by = c("year", "tslb_yrs", "vegetype", "area")) %>%
  arrange(vegetype, year, tslb_yrs) %>%
  pivot_wider(names_from = year, values_from = area)

# makes all possibilities of variables
df_exp <- df %>%
  filter(!is.na(tslb_yrs)) %>%
  expand(park, year, tslb_yrs, vegetype)

df_04 <- df %>%
  filter(!is.na(tslb_yrs)) %>%
  right_join(df_exp, by = c("park", "year", "tslb_yrs", "vegetype")) %>%
  arrange(park, year) %>%
  filter(tslb_yrs < 5) %>%
  group_by(park, year, tslb_yrs, vegetype) %>% 
  summarise(area = sum(burnt_area_ha))

# must be setup to compliment df_exp
df_all <- df %>%
  filter(!is.na(tslb_yrs) & tslb_yrs >= 5) %>%
  group_by(park, year, vegetype) %>% ## combines euc and sandstone
  summarise(area = sum(burnt_area_ha)) %>%
  mutate(tslb_yrs = 5) %>%
  select(park, year, tslb_yrs, vegetype, area) %>%
  full_join(df_04, by = c("park", "year", "tslb_yrs", "vegetype", "area")) %>%
  arrange(park, vegetype, year, tslb_yrs) %>%
  pivot_wider(names_from = year, values_from = area) %>%
  write_csv(file = "./output_stats/tslb_area_stats_wide_summary.csv")

