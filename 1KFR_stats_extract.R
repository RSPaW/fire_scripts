## code below creates all of the rasters and stats extracts for the work shown
## to Ben and Ian as of the 21st July 2021. Add to this if further metrics are
## to be created.

## Bart Huntley 21 July 2021
library(sf)
library(tidyverse)
library(fasterize)
library(raster)
library(fs)
library(janitor)
library(here)
library(igraph)
library(exactextractr)


# Set up area of interest (aoi) -------------------------------------------

## define a Area of Interest by importing a shp file of the Kimberley and transforming it to a projected CRS
aoi <- st_read("../Masks/nafi_aoi.shp") %>% ##from GP notes, nafi_aoi.shp is simply kimbreg_ext raster converted to shp.  It includes the buffer area
  st_transform(3577)


# Set up raster template --------------------------------------------------

#250m resolution.  1 pixel is 6.25ha.  250*250 m 62500 msq.  0.0001ha in a msq.  
tmpltrast <- raster(aoi, res = 250, vals = 1)  


# Kimberley coast to use as mask ------------------------------------------

##land is 1 and offshore is NA
kimbcoast_alb <- st_read("../Masks/Kimberley_Coast_CLEAN.shp") %>% ##from GP notes, Kimberley_Coast_CLEAN.shp is used to produced kimbcoast
  st_transform(3577) %>%
  fasterize(tmpltrast, field = "GRIDCODE")


# Park and vegtype zones --------------------------------------------------

# vector
zones <- st_read("../Zones/Parks_veg_zonal_alb_testrun.shp") %>% 
  st_transform(3577)

# as a raster
zones_rst <- fasterize(zones, tmpltrast, field = "PK_VT_code")


season_codes <- c('cal_yr', 'eds', 'lds')
season <- c(1, 2, 3 )
season_df <- tibble(season = season, code = season_codes)

zones_df <- zones %>%
  st_drop_geometry()


# Bring in burnt stack and calculate area burnt ---------------------------

fs_bin_st <- stack("./stacks/fs_bin_st.tif")
fs_eds_st <- stack("./stacks/fs_eds_st.tif")
fs_lds_st <- stack("./stacks/fs_lds_st.tif")
st_names <- paste0("fs", str_pad(string = 0:(nlayers(fs_bin_st)-1), width = 2, 
                                 side = "left", pad = "0"))
names(fs_bin_st) <- st_names
names(fs_eds_st) <- st_names
names(fs_lds_st) <- st_names

burn_area_stats <- tibble()
for(i in seq_along(names(fs_bin_st))){
  # cal year
  yrly <- fs_bin_st[[i]]
  out_df_y <- zonal(yrly, zones_rst, fun ="sum", na.rm=TRUE) %>%
    as_tibble() %>%
    mutate(year = paste0("20", substr(names(fs_bin_st)[i], 3, 4)),
           season = "cal_yr") %>%
    relocate(year)
 
  # eds
  eds <- fs_eds_st[[i]]
  out_df_e <- zonal(eds, zones_rst, fun ="sum", na.rm=TRUE) %>%
    as_tibble() %>%
    mutate(year = paste0("20", substr(names(fs_eds_st)[i], 3, 4)),
           season = "eds") %>%
    relocate(year)
  
  # lds
  lds <- fs_lds_st[[i]]
  out_df_l <- zonal(lds, zones_rst, fun ="sum", na.rm=TRUE) %>%
    as_tibble() %>%
    mutate(year = paste0("20", substr(names(fs_lds_st)[i], 3, 4)),
           season = "lds") %>%
    relocate(year)
  
  burn_area_stats <- bind_rows(burn_area_stats, out_df_y, out_df_e, out_df_l)
}

# output to csv
burn_area_stats %>%
  rename(PK_VT_code = zone,
         pix_sum = sum) %>%
  full_join(zones_df, by = "PK_VT_code") %>%
  mutate(burnt_area_ha = pix_sum * 6.25,
         burnt_area_prop = burnt_area_ha/PK_VT_ha * 100) %>%
  dplyr::select(year, season, Park, VegeType, burnt_area_ha, PK_VT_ha, burnt_area_prop) %>%
  rename(park = Park,
         vegetype = VegeType,
         park_veg_area_ha = PK_VT_ha) %>%
  write_csv(file = "./output_stats/burnt_area_stats.csv")
  
  
  
  
  
  



# Bring in tslb stack calculate area of each tslb class by year -----------
files <- dir_ls("./tslb", glob = "*.tif$")
fs_tslb_st <- stack(files)
tslb_names <- paste0("fs", str_pad(string = 1:(nlayers(fs_tslb_st)), width = 2, 
                                 side = "left", pad = "0"))
names(fs_tslb_st) <- tslb_names

tslb_ext_df <- tibble()
for(i in 1:nrow(zones)){
  # per zone loop
  shp <- zones[i, ]
  fshp <- fasterize(shp, tmpltrast, field = "PK_VT_code")
  PK_VT_code <- zones[i, ] %>%
    st_drop_geometry() %>%
    pull(PK_VT_code)
  cat("doing...", PK_VT_code)
  for(j in seq_along(names(fs_tslb_st))){
    # per year layer of tslb loop
    yr <- fs_tslb_st[[j]] %>%
      mask(fshp)
    out_df <- freq(yr) %>%
      as_tibble() %>%
      mutate(year = paste0("20", substr(names(fs_tslb_st)[j], 3, 4)),
             PK_VT_code = PK_VT_code) %>%
      relocate(year)
    tslb_ext_df <- bind_rows(tslb_ext_df, out_df)
  }
  
}

tslb_ext_df <- tslb_ext_df %>%
  full_join(zones_df, by = "PK_VT_code") %>%
  mutate(burnt_area_ha = count * 6.25,
         burnt_area_prop = burnt_area_ha/PK_VT_ha * 100) %>%
  dplyr::select(year, value, Park, VegeType, burnt_area_ha, PK_VT_ha, burnt_area_prop) %>%
  rename(park = Park,
         vegetype = VegeType,
         park_veg_area_ha = PK_VT_ha,
         tslb_yrs = value) %>%
  write_csv(file = "./output_stats/tslb_area_stats.csv")


## same data but wide summary to match old excel - muck around with the joins 
# and group bys to exclude or lump variables (e.g. park or vegetype)

# makes all possibilities of variables
df_exp <- tslb_ext_df %>%
  filter(!is.na(tslb_yrs)) %>%
  expand(park, year, tslb_yrs, vegetype)

# here lumping all areas of tslb 5 or greater together
df_04 <- tslb_ext_df %>%
  filter(!is.na(tslb_yrs)) %>%
  right_join(df_exp, by = c("park", "year", "tslb_yrs", "vegetype")) %>%
  arrange(park, year) %>%
  filter(tslb_yrs < 5) %>%
  group_by(park, year, tslb_yrs, vegetype) %>% 
  summarise(area = sum(burnt_area_ha))

# must be setup to compliment df_04
df_all <- tslb_ext_df %>%
  filter(!is.na(tslb_yrs) & tslb_yrs >= 5) %>%
  group_by(park, year, vegetype) %>% ## combines euc and sandstone
  summarise(area = sum(burnt_area_ha)) %>%
  mutate(tslb_yrs = 5) %>% # makes an => tslb category
  select(park, year, tslb_yrs, vegetype, area) %>%
  full_join(df_04, by = c("park", "year", "tslb_yrs", "vegetype", "area")) %>%
  arrange(park, vegetype, year, tslb_yrs) %>%
  pivot_wider(names_from = year, values_from = area) %>%
  write_csv(file = "./output_stats/tslb_area_stats_wide_summary.csv")







## not quite working but promising

# tslb_ext_df <- tibble()
# for(i in seq_along(names(fs_tslb_st))){
#   yr <- fs_tslb_st[[i]]
#   out_df <- exact_extract(yr, zones, function(df) {
#     df %>%
#       mutate(value = ifelse(value == 0, 100 , value)) %>%
#       group_by(PK_VT_code, value) %>%
#       summarize(burn_area_ha = sum(value)) %>%
#       mutate(burn_area_ha = ifelse(value == 100, burn_area_ha/100, burn_area_ha),
#              value = ifelse(value == 100, 0, value),
#              burn_area_ha = burn_area_ha * 6.25,
#              year = paste0("20", substr(names(fs_tslb_st)[i], 3, 4))) %>%
#       arrange(PK_VT_code, value)
#   }, summarize_df = TRUE, include_cols = 'PK_VT_code', progress = FALSE)
#   tslb_ext_df <- bind_rows(tslb_ext_df, out_df)
# }