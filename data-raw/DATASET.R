## code to prepare `DATASET` dataset goes here
source("~/mapbox.r")
df <- data.frame(id=c("LOC_A","LOC_B","LOC_C","LOC_D","LOC_E"),
                 x = c(-86.80904001819685,-77.03672814756133,-86.84944585580627,-87.06712684519992,-74.05017),
                 y = c(36.145976571141574,38.89988307243776,36.12416339993663,35.60692051481932, 40.71649))
library(hcmarket)
library(tidyverse)
library(sf)
library(ggthemes)
iso30 <-
    df %>%
    get_mapbox_isochrone(
        id = id,
        lat = y,
        long = x,
        contours_minutes = 30,
        mapbox_token = mapbox_token)
usethis::use_data(iso30, overwrite=TRUE)
