#-------------------------------------------------------#####
#-------------------------------------------------------#####
# Labour force Thailand:
# Estimating hra by satellite imagery
# 28th. May 2019
# Yuzuru Utsunomiya
# Notice
# 1. Record whatever we consider to be important issue as comments
# 2. Save this code using Ctrl+S whenever we change the code
#
# Target: current labor force
# 1.1  กำลังแรงงานปัจจุบัน
#-------------------------------------------------------#####

#-------------------------------------------------------#####
#Referred sites:#####
# create many xlsx files with the same sheets
# https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
# https://www.karada-good.net/analyticsr/r-338
#END
#-------------------------------------------------------#####


#-------------------------------------------------------#####
# ---- read.libraries ----
## reading libaries
# tidyverse packages
library(tidyverse)
library(cowplot)
## plot figures
## treat map object
library(maptools)
library(future)
library(furrr)
library(GADMTools)
library(cowplot)
library(extrafont)
library(ggmap)
library(ggrepel)
library(ggsn)
library(grid)
library(GGally)
library(khroma)
library(papeR)
library(patchwork)
library(raster)
library(rgdal)
library(sf)
library(stargazer)
library(summarytools)
library(systemfonts)
library(viridis)
library(Cairo)
# # image processing
# library(imager)
# stan and itssettings
library(brms)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
theme_set(theme_get() + theme(aspect.ratio = 3/4))
library(shinystan)
# END###


# ---- settings ----
# # NOTE
# # 1.
# # When we install ggmap package, use the code below. CRAN version may not work.
# # devtools::install_github("dkahle/ggmap", ref = "tidyup")
# # 2.
# # Treat the key carefully
# # The api key is newer one.
# # To manage the key, access Google cloud platform.
# # Project name: Map with R
# # API key: For maps
source("key.r")
has_google_key()
vn_sf <- readRDS("gadm36_VNM_1_sf.rds")
#

# ---- google.satellite ----
## obtain RGB data from satellite image
## settings / fix parameters
## load some spatial data. Administrative Boundary
## set cell size of grid
## Smaller is better.
## When R returns error, adjust the cellsize.
## we cannot sample points correctly.
## Unit: Deg.
## 0.025 = 1/40 of 1.0 Deg.
cellsize <-  0.025
## set N. of sample per province
## We wish the number will be more than 10......
sample_size <-  50
## set fact number for aggregation
## Default: 1
## Adjust the number while considering the N. of sample above and memory capacity
#
# When we set the number "5", the raster will be aggregated 20% (1/5) of original in size.
# i.e.
# 640*640 raster -> 128*128 pixel
#
# Regarding the fact number, R help announce as follows:
# Aggregation of a x will result in a Raster* object with fewer cells. The number of cells is the number of cells of x divided by fact*fact (when fact is a single number) or prod(fact) (when fact consists of 2 or 3 numbers). If necessary this number is adjusted according to the value of expand. For example, fact=2 will result in a new Raster* object with 2*2=4 times fewer cells. If two numbers are supplied, e.g., fact=c(2,3), the first will be used for aggregating in the horizontal direction, and the second for aggregating in the vertical direction, and the returned object will have 2*3=6 times fewer cells. Likewise, fact=c(2,3,4) aggregates cells in groups of 2 (rows) by 3 (columns) and 4 (layers).
# References
#
# https://stackoverflow.com/questions/51685851/map-inside-map2-how-to-refer-properly-to-arguments-purrr
# https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
#
# Read an original spatial data
vn_sf <- readRDS("gadm36_VNM_1_sf.rds")
#


# ---- fix.sampling.points ----
## fix sampling points / location from sf data
vn_sf_sample <-
  vn_sf %>%
  group_by(NAME_1) %>%
  nest() %>%
  # Make grids on the map by province
  dplyr::mutate(
    province_grid_centers = furrr::future_map(
      data,
      ~
        # Make grids
        sf::st_make_grid(
          x = .,
          cellsize = cellsize,
          what = "centers"
        ) %>%
        # Obtain crossing point of the grids
        sf::st_intersection(.) %>%
        # Bind the coordinates of crossing points.
        base::do.call(rbind, .) %>%
        dplyr::as_tibble() %>%
        stats::setNames(c("lon","lat")
        )
    )
  ) %>%
  # Sample the coordinates of crossing points by province
  dplyr::mutate(
    sample_grid = furrr::future_map2(
      .x = province_grid_centers,
      .y = sample_size,
      .f =
        ~
        dplyr::sample_n(.x,
                        size = .y,
                        replace = FALSE
        )
    )
  ) %>%
  tidyr::unnest(sample_grid) %>%
  dplyr::bind_cols(
    individual = factor(
      rep(
        c(
          1:sample_size
        ),
        times = length(vn_sf$NAME_1)
      )
    )
  )
# ## save all the results
# Note
# Comment out when not in use. The procedure below takes long computation period.
# # This process needs extremely long periods.
saveRDS(vn_sf_sample, "vn_sf_sample.rds")
# gc()
# gc()
# gc()
#

# Obtain satellite imagery from Google maps
#

# ---- obtain.satellite.imagery ----
vn_sf_sample <- readRDS("vn_sf_sample.rds")
#
vn_sf_rgb <-
  # th_sf_sample$sample_grid %>%
  # # bind the grids' coordinates together
  # base::do.call(dplyr::bind_rows,.) %>%
  # obtain the satellite imagery
  vn_sf_sample %>%
  dplyr::mutate(
    ggmap_image_satellite = purrr::map2(
      lon,
      lat,
      function(lon, lat)
      {
        ggmap::get_map(
          location = c(
            lon = lon,
            lat = lat
          ),
          maptype = "satellite",
          size = c(320, 320),
          scale = 1,
          format = "png",
          zoom = 18 # <- BEWARE!!
        )
      }
    )
  ) %>%
  # Note on factor number and aggregate function.
  # The factor numbers depend on our previous experiments.
  # 2, 4, and 8 need too long computation period 
  dplyr::mutate(
    fact_number_10 = c(10), # dense
    fact_number_20 = c(20),  # modest
    fact_number_20 = c(40)  # rough
  ) %>%
  tidyr::gather(key = "fn",  value = "fact_number", starts_with("fact_number_")) %>%
  # Note on the aggregate function
  # The function, "mean", depends on our previous experiments.
  mutate(
    aggregate_fun_mean = c("mean")
  ) %>%
  tidyr::gather(
    key = "af",  
    value = "aggregate_fun", 
    starts_with("aggregate_fun_")
  ) %>%
  # omit unnecessary variables
  dplyr::select(
    -data, 
    -province_grid_centers, 
    - fn, 
    -af
  ) %>%
  # change position of the values
  dplyr::relocate(
    ggmap_image_satellite, 
    .after = last_col()
  )
#
# ## save all the results
# Note
# Comment out when not in use. The procedure below takes long computation period.
# # This process needs extremely long periods.
saveRDS(vn_sf_rgb, "vn_sf_rgb.rds")
gc()
gc()
gc()
#
##
### END ---
#

# ---- save.satellite.image.to.make.sure ----
# save the satellite image as .png files

# The images are for machine learning trial.
# WARNING
# This process takes a long period.
# Comment out when not in use.
vn_sf_rgb <- readRDS("vn_sf_rgb.rds")
vn_sf_rgb %>%
  # Limit the fact_number
  # This time, we choose 40.
  dplyr::filter(
    fact_number == 40
  ) %>%
  dplyr::group_by(
    NAME_1, individual,
    .add = TRUE
  ) %>%
  dplyr::group_split() %>%
  # save the images at a time using purrr::map()
  purrr::map(
    ~
      # save
      ggsave(
        # filename path
        # Example:
        # "Amnat Charoen1.png"
        filename = paste0(
          dplyr::first(.$NAME_1),
          "_",
          dplyr::first(.$individual),
          ".png"
        ),
        # target image
        plot = ggmap::ggmap(
          .$ggmap_image_satellite[[1]]
        ),
        # specify saving directory path
        path = "./google_sat_image"
      )
  )
#
# obtain file information
# normal expression picking strings ending with underscore
# https://stackoverflow.com/questions/40113963/how-to-extract-everything-until-first-occurrence-of-pattern
image_info <- 
  base::file.info(
    base::list.files(
      path = "./google_sat_image/", 
      pattern = "*.png", 
      full.names = TRUE
    )
  ) %>% 
  dplyr::bind_cols(
    dplyr::tibble(
      dir = rownames(.)
    )
  ) %>% 
  dplyr::as_tibble() %>%
  dplyr::arrange(mtime) %>% 
  dplyr::select(
    dir, 
    size, 
    mtime
  ) %>% 
  dplyr::mutate(
    NAME_1 = stringr::str_extract(
      string = stringr::str_sub(
        string = .$dir, 
        start = 21
      ), 
      pattern = ".+?(?=_)"),
    individual = stringr::str_extract(
      string = stringr::str_sub(
        string = .$dir, 
        start = 20
      ), 
      pattern = "[:digit:]{1,3}"
    )
  ) %>% 
  dplyr::select(
    NAME_1, 
    individual, 
    size
  )
# assign the results and select the available observation randomly
vn_sf_rgb_image <- 
  vn_sf_rgb %>% 
  # choose the fact_number
  # This time, we chose 40.
  dplyr::filter(
    fact_number == 40
  ) %>% 
  # join the objects
  dplyr::left_join(
    image_info, 
    by = c(
      "NAME_1", 
      "individual"
    )
  ) %>% 
  # NOTE
  # Appropriate threshold number differs by resolution.
  # This time we employed the 70,000.
  dplyr::filter(size > 1000000) 
#
# count the Min. N. of observation by province 
# WARNING
# For precise computaion, run this code with the part above
# (image_info)
n_individual <- 
  vn_sf_rgb_image %>% 
  dplyr::group_by(
    NAME_1
  ) %>% 
  dplyr::summarise(
    n_individual = n()
  ) %>% 
  dplyr::summarise(
    min = min(
      .$n_individual
    )
  ) %>% 
  as.numeric()
# sample observations by province in accordance with the Min. N.
vn_sf_rgb_image <- 
  vn_sf_rgb_image %>% 
  # sample 
  # To avoid malfunction, we omit 2 observations by province
  dplyr::sample_n(
    n_individual-2
  ) %>% # 
  group_by(NAME_1) %>% 
  dplyr::mutate(
    individual = order(individual)
  ) %>% 
  dplyr::arrange(NAME_1, individual) %>% 
  dplyr::select(-size)
#
# ## save all the results
# Note
# Comment out when not in use. The procedure below takes long computation period.
# # This process needs extremely long periods.
saveRDS(vn_sf_rgb_image, "vn_sf_rgb_image.rds")
gc()
gc()
gc()
#
##
### END ---

