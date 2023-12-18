#############################################
## The following loads the needed packages ##
#############################################

# load the required packages --> qua mettere tutti i packages che usiamo
packages <- c(
  "here", # for the project's organization
  "tidyverse", # for wrangling
  "ggrepel", "gghighlight", "patchwork", "maps", "scales", # for plotting
  "knitr", "read.xl", "dplyr", "tidyr", "stringr", "countrycode", "ggplot2", "plotly",
  "stargazer", "gtsummary", "corrplot", "gridExtra", "leaflet", "viridisLite", "htmltools",
  "RColorBrewer", "car"
)
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
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE,
  warning = FALSE
)

