## Kimberley fire stats scripts


library(sf)
library(tidyverse)
library(fasterize)
library(raster)
library(fs)
library(janitor)
library(here)
library(igraph)

# Setup folders for layers for stats --------------------------------------

st <- here::here("stacks")
ff <- here::here("fireFreq")
ac <- here::here("ageClass")
unburnt <- here::here("unburnt")
acdist <- here::here("ageClass_dist")
tslb <- here::here("tslb")
outstats <- here::here("output_stats")

ifelse(!dir.exists(st),
       dir.create(st), FALSE)
ifelse(!dir.exists(ff),
       dir.create(ff), FALSE)
ifelse(!dir.exists(ac),
       dir.create(ac), FALSE)
ifelse(!dir.exists(unburnt),
       dir.create(unburnt), FALSE)
ifelse(!dir.exists(acdist),
       dir.create(acdist), FALSE)
ifelse(!dir.exists(tslb),
       dir.create(tslb), FALSE)
ifelse(!dir.exists(outstats),
       dir.create(outstats), FALSE)

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


# Set up classification matrices ------------------------------------------

## EDS - early dry season (Jan to June)  ###EDS definition may change (Jan - July)
eds <- c(-1,0,0, 0,7,1, 7,12,0)
edsmat <- matrix(eds, ncol=3, byrow=TRUE)

## LDS - late dry season (July to Dec)   ###LDS definition may change (Aug - Dec)
lds <- c(-1,0,0, 0,7,0, 7,12,1)
ldsmat <- matrix(lds, ncol=3, byrow=TRUE)

## Binary raster of burnt and not burnt
bin <- c(-1,0,0, 0,12,1)
binmat <- matrix(bin, ncol=3, byrow=TRUE)

##Unburnt   
ub <- c(-1,0,1, 0,12,0) 
ubmat <- matrix(ub, ncol=3, byrow=TRUE)


# Loop - creates stack of burn by month per year --------------------------


shps <- dir_ls("../../Source/NAFI_SHP", glob = "*.shp$")
yrs <- substr(sapply(str_split(names(shps), pattern = "/"), tail, 1), 1, 4)
fs_stack <- raster::stack()

for(i in seq_along(shps)){
  shp <- shps[i]
  f_mths <- st_read(shp) %>%
    clean_names() %>% # handles mix upper/lower case
    filter(month < 13) %>%
    st_transform(3577) %>%
    fasterize(tmpltrast, field = "month")
  f_mths[is.na(f_mths[])] <- 0
  f_mths <- mask(f_mths, kimbcoast_alb)
  fs_stack <- raster::stack(fs_stack, f_mths)
}
names(fs_stack) <- yrs


# Apply classification matrices to the stack ------------------------------

# classified stacks
fs_eds_st <- reclassify(fs_stack, edsmat)
fs_lds_st <- reclassify(fs_stack, ldsmat)
fs_bin_st <- reclassify(fs_stack, binmat)
fs_ub_st <- reclassify(fs_stack, ubmat)
# writeRaster(fs_eds_st, filename = "./stacks/fs_eds_st.tif", datatype = 'INT1U',
#             overwrite = TRUE)
# writeRaster(fs_lds_st, filename = "./stacks/fs_lds_st.tif", datatype = 'INT1U',
#             overwrite = TRUE)
# writeRaster(fs_bin_st, filename = "./stacks/fs_bin_st.tif", datatype = 'INT1U',
#             overwrite = TRUE)
# writeRaster(fs_ub_st, filename = "./stacks/fs_ub_st.tif", datatype = 'INT1U',
#             overwrite = TRUE)


# Create burn year stack --------------------------------------------------


fs_yr_st <- raster::stack()
for(i in seq_along(names(fs_bin_st))){
  yr <- as.numeric(substr(yrs[i], 3, 4)) + 2000
  r_layer <-fs_bin_st[[i]]
  r_layer[r_layer == 1] <- yr
  fs_yr_st <- raster::stack(fs_yr_st, r_layer)
}
names(fs_yr_st) <- yrs

# writeRaster(fs_ub_st, filename = "./stacks/fs_yr_st.tif", datatype = 'INT2U', 
#             overwrite = TRUE)


#  Create time since last burn layers -------------------------------------

l <- nlayers(fs_yr_st)
for(i in seq_along(names(fs_yr_st))){
  if(i + 1 < l){
    ind <- i + 1
    mstack <- fs_yr_st[[1:ind]]
    on1 <- paste0("./tslb/", gsub("fs", "tslb_", names(mstack)[ind]), ".tif")
    mval <- cellStats(mstack[[ind]], 'max', na.rm = TRUE)
    cat("doing...", names(mstack)[ind])
    beginCluster()
    ol <- clusterR(mstack, fun = calc,
                   args = list(fun = function(x){max(x, na.rm = TRUE)}))
    endCluster()
    tslb <- calc(ol, fun = function(x){ifelse(x == 0, ind, (mval - x))})# current yr - whatever is there
    writeRaster(tslb, filename = on1, datatype = 'INT1U', overwrite = TRUE)
  } else {
    ind <- l
    mstack <- fs_yr_st[[1:ind]]
    on1 <- paste0("./tslb/", gsub("fs", "tslb_", names(mstack)[ind]), ".tif")
    mval <- cellStats(mstack[[ind]], 'max', na.rm = TRUE)
    cat("doing...", names(mstack)[ind])
    beginCluster()
    ol <- clusterR(mstack, fun = calc,
                   args = list(fun = function(x){max(x, na.rm = TRUE)}))
    endCluster()
    tslb <- calc(ol, fun = function(x){ifelse(x == 0, ind, (mval - x))})# current yr - whatever is there
    writeRaster(tslb, filename = on1, datatype = 'INT1U', overwrite = TRUE)
  }
}


# Create fire frequency since 2000 layers ---------------------------------

## function to output fire frequency since 2000 written to /fireFreq
## fs_bin_st is the binary stack
## s is the initial layer in the stack to calculate back from

fireFreq <- function(fs_bin_st, s = 5){
   l <- nlayers(fs_bin_st)
   ffval <- c(-1,0,0, 0,1,1, 1,2,2, 2,3,3, 3,4,4, 4,l,5)
   ffmat <- matrix(ffval, ncol=3, byrow=TRUE)
   for(i in seq_along(names(fs_bin_st))){
     ind <- (s - 1) + i
     if(ind < l){
       mstack <- fs_bin_st[[1:ind]]
       on <- paste0("./fireFreq/", gsub("fs", "ff", names(mstack)[ind]), ".tif")
       ol <- calc(mstack, fun = sum, na.rm = TRUE)
       olc <- reclassify(ol, ffmat)
       writeRaster(olc, datatype = 'INT1U', filename = on, overwrite = TRUE)
     } else {
       mstack <- fs_bin_st[[1:l]]
       on <- paste0("./fireFreq/", gsub("fs", "ff", names(mstack)[l]), ".tif")
       ol <- calc(mstack, fun = sum, na.rm = TRUE)
       olc <- reclassify(ol, ffmat)
       writeRaster(olc, datatype = 'INT1U', filename = on, overwrite = TRUE)
     }
     
   }
 }
fireFreq(fs_bin_st, s = 5)
 
 
# Distance to unburnt -----------------------------------------------------

## loop to make cost distance to unburnt (no size limit) for each year, written
## to /unburnt and makes a stack for later use

# unb_dist_stack <- stack()

for(i in seq_along(names(fs_ub_st))){
  #first bit
  cat("doing...", names(fs_ub_st)[i])
  mlayer <- gridDistance(fs_ub_st[[i]], origin =1, omit = NA)
  names(mlayer) <- names(fs_ub_st[[i]])
  on <- paste0("./unburnt/", gsub("fs", "ubdist_", names(fs_ub_st)[i]), ".tif")
  # unb_dist_stack <- stack(unb_dist_stack, mlayer)
  writeRaster(mlayer, filename = on, datatype = 'INT2U', overwrite = TRUE)
  #second clumpy bit
  c <- clump(fs_ub_st[[i]], directions = 4, gaps = FALSE)
  c[is.na(c)] <- 0 #burnt areas to 0 (can't be NA)
  m_c <- mask(c, kimbcoast_alb) #remask water to NA leaving burnt as 0
  f <- freq(c, digits = 0, useNA = 'no', progress = 'text')# on orig clump as don't want burnt to be counted as clump in stats
  df <- as_tibble(f) %>%
    dplyr::mutate(ha = count * 6.25,
                  gr20ha = ifelse(ha > 20, 1, 0),
                  gr20ha = ifelse(value == 0, 0, gr20ha)) %>%
    dplyr::select(value, gr20ha)
  mat <- data.matrix(df, rownames.force = NA)
  g20 <- reclassify(m_c, mat)
  g20_out <- gridDistance(g20, origin = 1, omit = NA)
  on <- paste0("./unburnt/", gsub("fs", "ubdist20_", names(fs_ub_st)[i]), ".tif")
  writeRaster(g20_out, filename = on, datatype = 'INT2U', overwrite = TRUE)
}




# Years since last burn ---------------------------------------------------

## function to output ageclass raster layers for various stat metrics written to 
## /ageClass
## fs_yr_st is the year burnt stack
## is parallelised but is slow to run (many hours)


## Parked at this stage - created tslb layer which can be classified to whatever
## year ranges are desired
 
# ageClass <- function(fs_yr_st){
#   l <- nlayers(fs_yr_st)
#   ar <- c(-1,0,0, 0,1,1, 1,2,2, 2,3,3, 3,4,4, 4,l,5)
#   armat <- matrix(ar, ncol=3, byrow=TRUE)
#   y3 <- c(-1,2,0, 2,l,1)
#   y3mat <- matrix(y3, ncol=3, byrow=TRUE)
#   y5 <- c(-1,4,0, 4,l,1)
#   y5mat <- matrix(y5, ncol=3, byrow=TRUE)
#   
#   for(i in seq_along(names(fs_yr_st))){
#     if(i + 1 < l){
#       ind <- i + 1
#       mstack <- fs_yr_st[[1:ind]]
#       on1 <- paste0("./ageClass/", gsub("fs", "ac_annual_", names(mstack)[ind]), ".tif")
#       on3 <- gsub("ac_annual_", "ac3_", on1)
#       on5 <- gsub("ac_annual_", "ac5_", on1)
#       mval <- cellStats(mstack[[ind]], 'max', na.rm = TRUE)
#       cat("doing...", names(mstack)[ind])
#       beginCluster()
#       ol <- clusterR(mstack, fun = calc,
#                      args = list(fun = function(x){max(x, na.rm = TRUE)}))
#       endCluster()
#       ysb <- calc(ol, fun = function(x){ifelse(x == 0, ind, (mval - x))})# current yr - whatever is there
#       ysb_1 <- reclassify(ysb, armat)
#       writeRaster(ysb_1, filename = on1, datatype = 'INT1U', overwrite = TRUE)
#       ysb_3 <- reclassify(ysb, y3mat)
#       writeRaster(ysb_3, filename = on3, datatype = 'INT1U', overwrite = TRUE)
#       ysb_5 <- reclassify(ysb, y5mat)
#       writeRaster(ysb_5, filename = on5, datatype = 'INT1U', overwrite = TRUE)
#     } else {
#       ind <- l
#       mstack <- fs_yr_st[[1:ind]]
#       on1 <- paste0("./ageClass/", gsub("fs", "ac_annual_", names(mstack)[ind]), ".tif")
#       on3 <- gsub("ac_annual_", "ac3_", on1)
#       on5 <- gsub("ac_annual_", "ac5_", on1)
#       mval <- cellStats(mstack[[ind]], 'max', na.rm = TRUE)
#       cat("doing...", names(mstack)[ind])
#       beginCluster()
#       ol <- clusterR(mstack, fun = calc,
#                      args = list(fun = function(x){max(x, na.rm = TRUE)}))
#       endCluster()
#       ysb <- calc(ol, fun = function(x){ifelse(x == 0, ind, (mval - x))})
#       ysb_1 <- reclassify(ysb, armat)
#       writeRaster(ysb_1, filename = on1, datatype = 'INT1U', overwrite = TRUE)
#       ysb_3 <- reclassify(ysb, y3mat)
#       writeRaster(ysb_3, filename = on3, datatype = 'INT1U', overwrite = TRUE)
#       ysb_5 <- reclassify(ysb, y5mat)
#       writeRaster(ysb_5, filename = on5, datatype = 'INT1U', overwrite = TRUE)
#     }
#   }
# }
# ageClass(fs_yr_st) 



# Distance last burn by age class -----------------------------------------

## On hold not naming correctly at present

# yslb <- dir_ls("./ageClass")
# y3s <- yslb[str_detect(yslb, pattern = ".tif$") & str_detect(yslb, pattern = "ac3_")]
# y5s <- yslb[str_detect(yslb, pattern = ".tif$") & str_detect(yslb, pattern = "ac5_")]
# for(i in seq_along(names(fs_yr_st))){
#     on3 <- paste0("./ageClass_dist/", gsub("fs", "ac3_dist_", names(fs_yr_st)[i]), ".tif")
#     on5 <- paste0("./ageClass_dist/", gsub("fs", "ac5_dist_", names(fs_yr_st)[i]), ".tif")
#     
#     g3 <- gridDistance(raster(y3s[i]), origin = 1, omit = NA)
#     g5 <- gridDistance(raster(y5s[i]), origin = 1, omit = NA)
#     writeRaster(g3, filename = on3, datatype = 'INT2U', overwrite = TRUE)
#     writeRaster(g5, filename = on5, datatype = 'INT2U', overwrite = TRUE)
#   
# }

