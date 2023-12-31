#############################################
## The following loads the needed packages ##
#############################################

# load the required packages --> qua mettere tutti i packages che usiamo
packages <- c(
  "here", # for the project's organization
  "tidyverse", # for wrangling
  "scales", # for plotting
  "knitr", "readxl", "dplyr", "tidyr", "stringr", "countrycode", "ggplot2", "plotly",
  "stargazer", "gtsummary", "corrplot", "gridExtra", "viridisLite", "htmltools",
  "RColorBrewer", "car", "DT", "htmlwidgets", "leaflet", "gganimate")
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 69,
  tibble.width = 69,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.asp = NULL,
  fig.show = "hold",
  message = FALSE,
  echo = TRUE,
  warning = FALSE,
  results= 'show'
)

#Import all the datasets
#Main dataset
attacks <- read.csv(here::here("data/attacks1.csv"))
attacks <- attacks[1:3406, ]

#Temperature dataset
temperature <- read_xlsx(here::here("data/Temperature.xlsx"))

#Sealevel dataset
sealevel <- read.csv(here::here("data/sealevel.csv"))

#CO2 dataset
co2 <- read.csv(here::here("data/CO2.csv"))

#map dataset
map <- read.csv(here::here("data/map.csv"))