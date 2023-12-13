library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(corrplot)
library(leaflet)
library(viridisLite)
library(htmltools)
library(RColorBrewer)
library(car)

#_______________________________________________________________________________________________________________________
#Interactive map

#Import the data concerning the map
map <- read.csv("map.csv")
map <- map[c('latitude', 'longitude', 'country')]

#Let's rename the columns of the dataset
colnames(map)[colnames(map) == 'latitude'] <- 'lat'
colnames(map)[colnames(map) == 'longitude'] <- 'lng'
colnames(map)[colnames(map) == 'country'] <- 'Country'
map$Country <- toupper(map$Country)

map$Country <- ifelse(map$Country == "UNITED STATES", "USA", map$Country)

merged_map <- merge(merged_data3,  map, by='Country', all=FALSE)

# Create a new variable representing the nb of attacks per country
results <- merged_map %>%
  group_by(Country) %>%
  summarise(Attackscountry = n())
print(results)

#Attach aggregated data to your original dataframe
merged_map <- left_join(merged_map, results, by = "Country")

# Definition of the thresholds for the categorization
seuils <- c(0, 50, 100, 500, Inf)

# Definition of the colors we want to have in the map
#couleurs <- c("#4DA6FF", "#0074CC", "#6C8EBF", "#001F3F80")
couleurs <- c("green", "yellow", "orange", "red")

# Add a new category column based on thresholds
merged_map$cat_attacks <- cut(merged_map$Attackscountry, breaks = seuils, labels = FALSE)

merged_map$echelle_taille <- merged_map$Attackscountry * 0.1

# Let's have fun with an interactive map
ma_carte <- leaflet(merged_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~sqrt(echelle_taille) * 2,
    color = ~factor(merged_map$cat_attacks, labels = couleurs),
    fillOpacity = 0.4,
    label = ~paste(Country, ":", Attackscountry),
    options = markerOptions(autoPopup = TRUE)
  ) %>%
  addLegend(
    position = "bottomleft",
    colors = couleurs,
    labels = c("Less than 50", "50 to 100", "100 to 500", "More than 500"),
    title = "Number of shark attacks"
  )

# Let's see the map
ma_carte












#_______________________________________________________________________________
#Interactive map

#Import the data concerning the map
map <- read.csv("map.csv")
map <- map[c('latitude', 'longitude', 'country')]

#Let's rename the columns of the dataset
colnames(map)[colnames(map) == 'latitude'] <- 'lat'
colnames(map)[colnames(map) == 'longitude'] <- 'lng'
colnames(map)[colnames(map) == 'country'] <- 'Country'
map$Country <- toupper(map$Country)

map$Country <- ifelse(map$Country == "UNITED STATES", "USA", map$Country)

merged_map <- merge(merged_data3,  map, by='Country', all=FALSE)

# Create a new variable representing the nb of attacks per country
results <- merged_map %>%
  group_by(Country) %>%
  summarise(Attackscountry = n())
print(results)

#Attach aggregated data to your original dataframe
merged_map <- left_join(merged_map, results, by = "Country")

# Definition of the thresholds for the categorization
seuils <- c(0, 50, 100, 500, Inf)

# Definition of the colors we want to have in the map
#couleurs <- c("#4DA6FF", "#0074CC", "#6C8EBF", "#001F3F80")
couleurs <- c("green", "yellow", "orange", "red")

# Add a new category column based on thresholds
merged_map$cat_attacks <- cut(merged_map$Attackscountry, breaks = seuils, labels = FALSE)

merged_map$echelle_taille <- merged_map$Attackscountry * 0.1

# Let's have fun with an interactive map
ma_carte <- leaflet(merged_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~sqrt(echelle_taille) * 2,
    color = ~factor(merged_map$cat_attacks, labels = couleurs),
    fillOpacity = 0.4,
    label = ~paste(Country, ":", Attackscountry),
    options = markerOptions(autoPopup = TRUE)
  ) %>%
  addLegend(
    position = "bottomleft",
    colors = couleurs,
    labels = c("Less than 50", "50 to 100", "100 to 500", "More than 500"),
    title = "Number of shark attacks"
  )

# Let's see the map
ma_carte
