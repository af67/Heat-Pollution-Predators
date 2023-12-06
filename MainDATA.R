
#________________________________________________________________________________________________________________________

#                           C L E A N I N G   OF    S H A R K      A T T A C K S

#________________________________________________________________________________________________________________________
install.packages("countrycode")
install.packages("corrplot")

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

attacks <- read.csv("attacks1.csv")
attacks <- attacks[1:3406, ]
#________________________________________________________________________________________________________________________
#we drop the columns that we do not need

attacks <- subset(attacks, select = setdiff(names(attacks), c('X', 'X.1', 'Area', 'Location', 'Injury', 'href', 'href.formula', 'Investigator.or.Source', 'pdf', 'Name', 'Case.Number.1', 'Case.Number.2', 'original.order')))


#________________________________________________________________________________________________________________________
#FIRSTLY WE WORK ON THE COUNTRY

#Firstly we put everything in CAPS in order to group countries (The problem was only Fiji which was written
#sometimes in caps and sometimes in lower cap. The rest was good). When we run a table, we can see that all
#countries are written in the same way, but we have 6 NA. We decided to investigate those trous. How? we used
#the function "which" to spot where the NA are. We found out that we could get the country of 2 of them because,
#the column with the precise area war filled. For row 2957 area says "English Channel", meaning that the attack happened in
#the UK, while for row 3388 the area is "Carribean Sea". As a consequence, we will fill this spot with Jamaica,
#since it is central to Carribean and has average temperatures similar to sourrounding countries.

#After that, we have investigated the other 4 NA: they don't have anything in common (activity, missing information etc.)
#nor we found any information about those attacks on internet, so we just deleted the rows.
#Now countries are clean :)

attacks$Country <- toupper(attacks$Country)
attacks$Country <- na_if(attacks$Country, "")

rows_with_NA <- which(is.na(attacks$Country))
rows_with_NA

attacks$Country <- gsub("COLUMBIA", "COLOMBIA", attacks$Country, ignore.case = TRUE)
attacks[2957, "Country"] <- "UNITED KINGDOM"
attacks[3388, "Country"] <- "JAMAICA"
attacks$Country <- gsub("UNITED ARAB EMIRATES \\(UAE\\)", "UNITED ARAB EMIRATES", attacks$Country)

attacks <- subset(attacks, !is.na(Country))


#________________________________________________________________________________________________________________________

#NOW WE WORK ON THE DATE

#For this column, we will drop both the year (which is already included in another column) and the day, 
#which we consider useless therefore, we will only keep the month


attacks$Date <- gsub("\\bReported\\b", "", attacks$Date, ignore.case = TRUE)
attacks$Date <-  gsub("\\bEarly\\b", "", attacks$Date, ignore.case = TRUE)
attacks$Date <- gsub("[[:digit:][:punct:]]", "", attacks$Date)

words_to_keep <- c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


# Remove words not in the specific list for each row
attacks$Date <- sapply(attacks$Date, function(sentence) {
  gsub(paste0("\\b(?!", paste(words_to_keep, collapse = "|"), "\\b)\\w+\\s*"), "", sentence, perl = TRUE)
})

attacks$Date <- trimws(attacks$Date)
table(attacks$Date)

#when we run the table, we see that there are stll some problems, that we will correct by hand (Jul Jul, Jula etc.).
#We run the function below to check in which row the mistake happened and we correct it. In the first case
#the resulting row is 2854, and we change Jul Jul with Jul


attacks$Date <- gsub("Jul Jul", "Jul", attacks$Date)
attacks$Date <- gsub("Jula", "Jul", attacks$Date)
attacks$Date <- gsub("Julb", "Jul", attacks$Date)
attacks$Date <- gsub("July", "Jul", attacks$Date)
attacks$Date <- gsub("June", "Jun", attacks$Date)
attacks$Date <- gsub("May  Nov", "Aug", attacks$Date)
attacks$Date <- gsub("November", "Nov", attacks$Date)

#Once the spelling mistakes are good, We replace all NA by blank spaces and investigate them. Some of them
#actually have their month on the column "Case Number". The others don't have anything in common.

rows_with_mistakes <- attacks[attacks$Date == "", ]
attacks$Date[is.na(attacks$Date)] <- ""

attacks <- mutate(attacks, Date = ifelse(Case.Number == "2017.06.05", "Jun", Date))
attacks <- mutate(attacks, Date = ifelse(Case.Number == "2008.01.30", "Jan", Date))
attacks <- mutate(attacks, Date = ifelse(Case.Number == "2001.04.02.b", "Apr", Date))
attacks <- mutate(attacks, Date = ifelse(Case.Number == "1980.12.30", "Dec", Date))
attacks <- mutate(attacks, Date = ifelse(Case.Number == "2012.12.00", "Dec", Date))


attacks$Date <- na_if(attacks$Date, "")
attacks <- subset(attacks, !is.na(Date))

#________________________________________________________________________________________________________________________
#NOW WE WORK ON AGE

#To clean the column of the age we took off all the letters, we computed by hand the means of all victims
# that had a range for their year and finally transformed all the ages expressed in months to years.

# Replace "18 months" with "1.5"

attacks$Age <- ifelse(attacks$Age == "18 months", "1.5", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "28 & 26", "27", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "18 or 20", "19", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "12 or 13", "12.5", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "46 & 34", "40", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "28, 23 & 30", "27", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "36 & 26", "31", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "8 or 10", "9", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "30 or 36", "33", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "6½", "6.5", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "21 & ?", "21", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "33 or 37", "35", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "23 & 20", "21.5", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "7      &    31", "19", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "32 & 30", "31", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "16 to 18", "27", attacks$Age)
attacks$Age <- ifelse(attacks$Age == "21 or 26", "23.5", attacks$Age)


attacks$Age <- str_replace_all(attacks$Age, "[A-Za-z]", "")

attacks$Age <- na_if(attacks$Age, "")

attacks$Age <- as.numeric(str_extract(attacks$Age, "\\d+\\.?\\d*"))


#CORRECTION


#USA#

ages_in_USA <- attacks %>%
  filter(Country == "USA") %>%
  select(Age)

ages_vector <- ages_in_USA$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)

hist(ages_without_na, 
     col = "skyblue",       
     xlab = "Ages",         
     ylab = "Frequency",     
     main = "Histogram of Ages in USA without NA"
) #We can see that the histogram is skewed, so we will replace the NAs by the first quartile of non-missing values

q.25 <- quantile(ages_without_na, 0.25)
attacks$Age[attacks$Country == "USA" & is.na(attacks$Age)] <- q.25 

#BAHAMAS#

ages_in_BAH <- attacks %>%
  filter(Country == "BAHAMAS") %>%
  select(Age)

ages_vector <- ages_in_BAH$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)

hist(ages_without_na, 
     col = "darkgreen",       
     xlab = "Ages",         
     ylab = "Frequency",     
     main = "Histogram of Ages in Bahamas without NA"
)
mean_BAH = mean(ages_without_na)

attacks$Age[attacks$Country == "BAHAMAS" & is.na(attacks$Age)] <- mean_BAH


#EGYPT#

ages_in_egy <- attacks %>%
  filter(Country == "EGYPT") %>%
  select(Age)

ages_vector <- ages_in_egy$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean_egy = mean(ages_without_na)

attacks$Age[attacks$Country == "EGYPT" & is.na(attacks$Age)] <- mean_egy


#ITALY#

# Extract all ages from individuals who come from Italy
ages_in_italy <- attacks %>%
  filter(Country == "ITALY") %>%
  select(Age)

# Convert the extracted ages to a vector
ages_vector <- ages_in_italy$Age


# Display or use the 'ages_vector' containing ages from individuals in Italy

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean_age = mean(ages_without_na)

# Replace all the NA on Italy with the mean of people attacked in Italy.

attacks$Age[attacks$Country == "ITALY" & is.na(attacks$Age)] <- 37.375

#AUSTRALIA#

ages_in_aus <- attacks %>%
  filter(Country == "AUSTRALIA") %>%
  select(Age)

ages_vector <- ages_in_aus$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean_aus = mean(ages_without_na)

attacks$Age[attacks$Country == "AUSTRALIA" & is.na(attacks$Age)] <- mean_aus



#SOUTH AFRICA#

ages_in_SA <- attacks %>%
  filter(Country == "SOUTH AFRICA") %>%
  select(Age)

ages_vector <- ages_in_SA$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean_SA = mean(ages_without_na)

attacks$Age[attacks$Country == "SOUTH AFRICA" & is.na(attacks$Age)] <- mean_SA


#BRAZIL#

ages_in_BRA <- attacks %>%
  filter(Country == "BRAZIL") %>%
  select(Age)

ages_vector <- ages_in_BRA$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean_BRA = mean(ages_without_na)

attacks$Age[attacks$Country == "BRAZIL" & is.na(attacks$Age)] <- mean_BRA


#FIJI#

ages <- attacks %>%
  filter(Country == "FIJI") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "FIJI" & is.na(attacks$Age)] <- mean

#FRENCH POLYNESIA#

ages <- attacks %>%
  filter(Country == "FRENCH POLYNESIA") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "FRENCH POLYNESIA" & is.na(attacks$Age)] <- mean

#HONG KONG#

ages <- attacks %>%
  filter(Country == "HONG KONG") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "HONG KONG" & is.na(attacks$Age)] <- mean

#JAPAN#

ages <- attacks %>%
  filter(Country == "JAPAN") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "JAPAN" & is.na(attacks$Age)] <- mean

#MEXICO#

ages <- attacks %>%
  filter(Country == "MEXICO") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "MEXICO" & is.na(attacks$Age)] <- mean


#MOZAMBIQUE#

ages <- attacks %>%
  filter(Country == "MOZAMBIQUE") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "MOZAMBIQUE" & is.na(attacks$Age)] <- mean


#NEW ZELAND#

ages <- attacks %>%
  filter(Country == "NEW ZEALAND") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "NEW ZEALAND" & is.na(attacks$Age)] <- mean

#PHILIPPINES#

ages <- attacks %>%
  filter(Country == "PHILIPPINES") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "PHILIPPINES" & is.na(attacks$Age)] <- mean

#REUNION#

ages <- attacks %>%
  filter(Country == "REUNION") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "REUNION" & is.na(attacks$Age)] <- mean

#NEW CALEDONIA#

ages <- attacks %>%
  filter(Country == "NEW CALEDONIA") %>%
  select(Age)

ages_vector <- ages$Age

ages_without_na = na.omit(ages_vector)
ages_without_na <- as.numeric(ages_without_na)
hist(ages_without_na)
mean = mean(ages_without_na)

attacks$Age[attacks$Country == "NEW CALEDONIA" & is.na(attacks$Age)] <- mean


#We fixed those countries that had lots of NA. Now, for the remaining NA we decided not to do anything for a specific reason. Not only we have not cound the real information on the internet, but we also believe that doing the mean for those remaining situations was useless, because these are the cases when NA are either the same amount of total shark attacks (see Antigua with 1 shark attack and 1 NA), or NA are more than half of the total attacks (see Malaysia with 4 total attacks but 3 of them are NA).

count_na_by_country <- attacks %>%
  group_by(Country) %>%
  summarise(NA_count = sum(is.na(Age)))


attacks <- subset(attacks, !is.na(Age))

#________________________________________________________________________________________________________________________


#NOW WORK ON TIME

#sometimes hours are written in a different format compared to most of the others, which follow the format
#13h30. Since they are not a lot we just replaced them manually

attacks<- mutate(attacks, Time= ifelse(Time=="Possibly same incident as 2000.08.21", "", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="14h00  -15h00", "14h30", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="14h30 / 15h30", "15h00", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="09h30 / 10h00", "9h45", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="10h45-11h15", "11h00", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="Sometime between 06h00 & 08hoo", "7h", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="18h15-18h30", "18h20", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="09h00 - 09h30", "9h15", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="10h00 -- 11h00", "10h30", Time))
attacks<- mutate(attacks, Time= ifelse(Time=="11h115", "11h15", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "Between 05h00 and 08h00", "6h30", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "17h00 or 17h40", "17h20", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "13h345", "13h45", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "<a0> ", "", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "09h00 -10h00", "9h30", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "2 hours after Opperman", "", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "11h00 / 11h30", "11h15", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "Between 06h00 & 07h20", "6h40", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "30 minutes after 1992.07.08.a", "", Time))
attacks <- mutate(attacks, Time = ifelse(Time == "Possibly same incident as 2000.08.21", "", Time))



#now, our objective is to classify hours in parts of the day. indeed, it is useless to keep hours as they are
#for a regression because we want times like 7h30 and 7h00 OR 15h00 and 16h00 to be read as the same thing.
# some of the rows already had the part of the day in it, but in order to work easily with all the column, we
#replaced "morning" with 8h00 etc. in this way, we're able to remove all the strings that are useless. finally, 
#void rows have been replaced by an NA and hours, which were there as CHAR have been replaced by numeric

attacks$Time <- str_replace_all(attacks$Time, "\\bmorning\\b", "8h00")
attacks$Time <- str_replace_all(attacks$Time, "\\bafternoon\\b", "15h00")
attacks$Time <- str_replace_all(attacks$Time, "\\bevening\\b", "20h00")
attacks$Time <- str_replace_all(attacks$Time, "\\bnight\\b", "23h00")


attacks$Time <- gsub("[^[:digit:]]", "", attacks$Time)

attacks$Time = na_if(attacks$Time, "")

attacks$Time <- as.numeric(attacks$Time)

# since we removed all the letters, hours now are not written as 8h00 or 15h30 but as 800 and 15h00. this is
# great for us, so that we can easily create a function that classifies those numbers as parts of the day. the 
#function works this way: if a value is included between 0 and 500 (i.e. midnight and 5a.m.), the value is replaced
#by the word "night" etc.

timeoftheday <- function(time) {
  if (!is.na(time)) {
    if (time >= 0 && time < 500) {
      return("night")
    } else if (time >= 500 && time < 1200) {
      return("morning")
    } else if (time >= 1200 && time < 1730) {
      return("afternoon")
    } else if (time >= 1730 && time < 2400) {
      return("evening")
    } else {
      return("")  # Handle any other cases (if necessary)
    }
  } else {
    return(NA)  # Retain NA values
  }
}

attacks$Time <- sapply(attacks$Time, timeoftheday)
attacks$Time <- tolower(attacks$Time)


sum(is.na(attacks$Time))
table(attacks$Time)


#________________________________________________________________________________________________________________________
#NOW WE WORK ON SEX


attacks$Sex <- na_if(attacks$Sex, "")
attacks$Sex <- ifelse(is.na(attacks$Sex), "Unknown", attacks$Sex)


#________________________________________________________________________________________________________________________
#NOW WE WORK ON SHARK SPECIES


#Creation of a variable species where we delete all the numbers that concern the size of the shark
species <- str_replace_all(attacks$Species, "\\d", "")

#If there is an empty cell, we will put NA
species = na_if(attacks$Species, "")

#gsub pour enlever la ponctuation
species <- str_remove_all(species, "[[:punct:]]")

#gsub pour enlever les chiffres accompagnés de cm ou m
species <- gsub("\\d+\\s*(cm|m)\\b", "", species)

#gsub pour supprimer les mots de une ou deux lettres
species <- gsub("\\b\\w{1,2}\\b", "", species)

#gsub pour supprimer tous les chiffres
species <- gsub("\\d", "", species)

# Supprimer les unités de mesure (kg, lb, b)
species <- gsub("\\b(kg|lb|b)\\b", "", species)

species <- gsub("\\bshark\\b(.*\\bshark\\b)?", "shark", species, ignore.case = TRUE)


# Utiliser gsub pour regrouper nos espèces en espèces communes 
species<- gsub("^.*White shark.*$", "White shark", species)
species<- gsub("^.*whitetip shark.*$", "White shark", species)
species<- gsub("^.*white shark.*$", "White shark", species)
species<- gsub("^.*Wobbegong shark.*$", "Wobbegong shark", species)
species<- gsub("^.*Wobbegong.*$", "Wobbegong shark", species)
species<- gsub("^.*Zambesi shark.*$", "Zambesi shark", species)
species<- gsub("^.*Zambezi shark.*$", "Zambesi shark", species)
species<- gsub("^.*whaler shark.*$", "Whale shark", species)
species<- gsub("^.*whale shark.*$", "Whale shark", species)
species<- gsub("^.*tiger shark.*$", "Tiger shark", species)
species<- gsub("^.*Tiger shark.*$", "Tiger shark", species)
species<- gsub("^.*thresher shark.*$", "Thresher shark", species)
species<- gsub("^.*spinner shark.*$", "Spinner shark", species)
species<- gsub("^.*Spinner shark feet.*$", "Spinner shark", species)
species<- gsub("^.*Spinner shark.*$", "Spinner shark", species)
species<- gsub("^.*spinner  blacktip shark.*$", "Spinner shark", species)
species<- gsub("^.*Tawny nurse shark.*$", "Nurse shark", species)
species<- gsub("^.*nurse shark.*$", "Nurse shark", species)
species<- gsub("^.*Nurse shark.*$", "Nurse shark", species)
species<- gsub("^.*Mako shark.*$", "Mako shark", species)
species<- gsub("^.*mako shark.*$", "Mako shark", species)
species<- gsub("^.*Lemon shark.*$", "Lemon shark", species)
species<- gsub(".*lemon shark.*", "Lemon shark", species, ignore.case = TRUE)
species<-gsub("^\\s*shark\\s*$", "Unidentified shark", species)

species<- gsub(".*bull.*", "Bull shark", species, ignore.case = TRUE)
species<- gsub(".*blue.*", "Blue shark", species, ignore.case = TRUE)
species<- gsub(".*reef.*", "Reef shark", species, ignore.case = TRUE)
species<- gsub(".*sand shark*", "Sand shark", species, ignore.case = TRUE)
species<- gsub("^.*Sand shark.*$", "Sand shark", species)
species<- gsub(".*Sand shark*", "Sand shark", species, ignore.case = TRUE)
species<- gsub(".*sandshark*", "Sand shark", species, ignore.case = TRUE)
species<- gsub(".*Sandbar shark*", "Sand shark", species, ignore.case = TRUE)
species<- gsub("juvenile\\s+\\w+", "Juvenile shark",species, ignore.case = TRUE)
species<- gsub("Juvenile shark shark", "Juvenile shark",species, ignore.case = TRUE)
species<- gsub("Juvenile shark  blacktip shark", "Juvenile shark",species, ignore.case = TRUE)
species<- gsub("blacktip\\s+\\w+", "Blacktip shark",species, ignore.case = TRUE)
species <- gsub("\\black\\w*\\b", "Blacktip shark", species, ignore.case = TRUE)

#remplacer toutes les occurrences du mot "blacktip" dans une colonne par "Blacktip Shark," même si le mot est accompagné par d'autres mots avant ou après

species <- gsub("\\bblacktip\\b", "Blacktip Shark", species, ignore.case = TRUE)
species<- gsub(".*copper shark*", "Copper shark", species, ignore.case = TRUE)
species <- gsub("\\bcow\\b", "Cow", species)
species <- gsub("\\bsilky\\b", "Silky", species)
species <- gsub("\\bsilvertip\\b", "Silvertip", species)


#conndition si "Hammerhead" suivi d'autre mots alors remplacé par Hammerhead shark
species <- ifelse(grepl("Hammerhead\\s+\\w+", species, ignore.case = TRUE), "Hammerhead shark", species)
species <- ifelse(grepl("Blacktip\\s+\\w+", species, ignore.case = TRUE), "Blacktip shark", species)
species <- ifelse(grepl("Raggedtooth\\s+\\w+", species, ignore.case = TRUE), "Raggedtooth shark", species)
species <- ifelse(grepl("Porbeagle\\s+\\w+", species, ignore.case = TRUE), "Porbeagle shark", species)


#si le mot shark apparaît pas alors NA
species <- ifelse(grepl("shark", species, ignore.case = TRUE), species, NA)

species <- ifelse(grepl("Juvenile\\s+\\w+", species, ignore.case = TRUE), "Juvenile shark", species)

species <- ifelse(grepl("grey\\s+\\w+", species, ignore.case = TRUE), "Grey shark", species)
species <- ifelse(grepl("greycolored\\s+\\w+", species, ignore.case = TRUE), "Grey shark", species)
species <- ifelse(grepl("gray\\s+\\w+", species, ignore.case = TRUE), "Grey shark", species)
species <- ifelse(grepl("Broadnose\\s+\\w+", species, ignore.case = TRUE), "Sevengill shark", species)
species <- ifelse(grepl("7gill\\s+\\w+", species, ignore.case = TRUE), "Sevengill shark", species)
species <- ifelse(grepl("sevengill\\s+\\w+", species, ignore.case = TRUE), "Sevengill shark", species)
species <- ifelse(grepl("black\\s+\\w+", species, ignore.case = TRUE), "Blacktip shark", species)
species <- ifelse(grepl("Sand\\s+\\w+", species, ignore.case = TRUE), "Sand shark", species)
species <- ifelse(grepl("Carpet\\s+\\w+", species, ignore.case = TRUE), "Carpet shark", species)
species <- ifelse(grepl("\\bbrown\\b", species, ignore.case = TRUE), "Brown shark", species)

species <- gsub("^frag\\w+", "Unrecognized shark", species)

species <- gsub("^shark$", "Unrecognized shark", species)

#Fonction pour vérifier si "small" est présent dans la cellule
contains_small <- function(text) {
  return(grepl("small", text))
}
# Remplacer la cellule par "unidentified shark" si "small" est présent
species <- ifelse(sapply(species, contains_small), "Unidentified shark", species)

#Fonction pour vérifier si "Small" est présent dans la cellule
contains_small <- function(text) {
  return(grepl("Small", text))
}
# Remplacer la cellule par "unidentified shark" si "Small" est présent
species <- ifelse(sapply(species, contains_small), "Unidentified shark", species)

#Fonction pour vérifier si "sharks" est présent dans la cellule
contains_sharks <- function(text) {
  return(grepl("sharks", text))
}
# Remplacer la cellule par "unidentified shark" si "sharks" est présent
species <- ifelse(sapply(species, contains_sharks), "Unidentified shark", species)

#Fonction pour vérifier si "cookie" est présent dans la cellule
contains_cookie <- function(text) {
  return(grepl("cookie", text))
}
# Remplacer la cellule par "Cookiecutter shark" si "cookie" est présent
species <- ifelse(sapply(species, contains_cookie), "Cookiecutter shark", species)


#Fonction pour vérifier si "involvement" est présent dans la cellule
contains_involvement <- function(text) {
  return(grepl("involvement", text))
}
# Remplacer la cellule par "No shark" si "involvement" est présent
species <- ifelse(sapply(species, contains_involvement), "No shark", species)

#Fonction pour vérifier si "invovlement" est présent dans la cellule
contains_invovlement <- function(text) {
  return(grepl("invovlement", text))
}
# Remplacer la cellule par "No shark" si "invovlement" est présent
species <- ifelse(sapply(species, contains_invovlement), "No shark", species)

#Fonction pour vérifier si "Not" est présent dans la cellule
contains_Not <- function(text) {
  return(grepl("Not", text))
}
# Remplacer la cellule par "No shark" si "Not" est présent
species <- ifelse(sapply(species, contains_Not), "No shark", species)

#Fonction pour vérifier si "not" est présent dans la cellule
contains_not <- function(text) {
  return(grepl("not", text))
}
# Remplacer la cellule par "No shark" si "not" est présent
species <- ifelse(sapply(species, contains_not), "No shark", species)

#Fonction pour vérifier si "Questionable" est présent dans la cellule
contains_Questionable <- function(text) {
  return(grepl("Questionable", text))
}
# Remplacer la cellule par "No shark" si "Questionable" est présent
species <- ifelse(sapply(species, contains_Questionable), "No shark", species)

#Fonction pour vérifier si "questionable" est présent dans la cellule
contains_questionable <- function(text) {
  return(grepl("questionable", text))
}
# Remplacer la cellule par "No shark" si "questionable" est présent
species <- ifelse(sapply(species, contains_questionable), "No shark", species)


#Fonction pour vérifier si "Salmon" est présent dans la cellule
contains_Salmon <- function(text) {
  return(grepl("Salmon", text))
}
# Remplacer la cellule par "Salmon shark" si "Salmon" est présent
species <- ifelse(sapply(species, contains_Salmon), "Salmon shark", species)


#Fonction pour vérifier si "whaler" est présent dans la cellule
contains_whaler <- function(text) {
  return(grepl("whaler", text))
}
# Remplacer la cellule par "Whale shark" si "involvement" est présent
species <- ifelse(sapply(species, contains_whaler), "Whale shark", species)


# Replace the cell with "Unidentified shark" if any of the specified words are found
species <- ifelse(grepl("(seen|observed|Tooth|tooth|large|killed|captive|female|metre|foot|followed|caused|Said|young|probably|gaffed)", species, ignore.case = TRUE), "Unidentified shark", species)

# Replace the cell with "No shark" if any of the specified words are found
species <- ifelse(grepl("(hoax|No Shark)", species, ignore.case = TRUE), "No shark", species)

# Replace the cell with "Copper shark" if any of the specified words are found
species <- ifelse(grepl("(Copper)", species, ignore.case = TRUE), "Copper shark", species)

# Replace the cell with "Dogfish shark" if any of the specified words are found
species <- ifelse(grepl("(Dog|dogfish)", species, ignore.case = TRUE), "Dogfish shark", species)

# Replace the cell with "Dusky shark" if any of the specified words are found
species <- ifelse(grepl("(Dusky)", species, ignore.case = TRUE), "Dusky shark", species)

# Replace the cell with "Sevengill shark" if any of the specified words are found
species <- ifelse(grepl("(gill)", species, ignore.case = TRUE), "Sevengill shark", species)

# Replace the cell with "Angel shark" if any of the specified words are found
species <- ifelse(grepl("(Angel)", species, ignore.case = TRUE), "Angel shark", species)

table(species)

# Compter le nombre de chaque catégorie
category_counts <- table(species)

# Créer un histogramme
histogram <- barplot(category_counts, col = "blue", main = "Histogramme de Catégories", xlab = "Catégories", ylab = "Fréquence")

# Afficher l'histogramme
print(histogram)

attacks$Species <- species

attacks$Species <- ifelse(is.na(attacks$Species), "Unknown", attacks$Species)


#________________________________________________________________________________________________________________________
#ACTIVITY

#when trying to put everything in lower cap, this was the result: Errore in tolower(attacks$Activity) : 
#stringa multibyte 921 non valida --> so converted everything in ASCII (=American Standard Code for Information Interchange)

attacks$Activity <- iconv(attacks$Activity, to = "ASCII", sub = " ")
attacks$Activity <- gsub("[^ -~]", "", attacks$Activity)

attacks$Activity <- tolower(attacks$Activity)

#when running a table of all activities, we can see that we can group them in some categories: 
#diving, race, windsurfing, walking, wading, wade fishing, touching, swimming, surfing, surf,
# standing, spearfishing, snorkeling,  scuba diving, playing, paddle, murder, kayaking, kayak, floating, fishing,

attacks$Activity <- gsub("[^A-Za-z ]", "", attacks$Activity)


kept_activities <- c("diving", "race", "windsurfing", "walking", "wading", "wade fishing", "touching", "swimming", "surfing", "surf",
                     "standing", "spearfishing", "snorkeling", "scuba diving", "playing", "paddle", "murder", "kayaking", "kayak", "floating", "fishing")

# Your column name
column_name <- "Activity"

# Create a regular expression pattern that matches any of the specific words
pattern <- paste(kept_activities, collapse = "|")

# Extract only the specific words from the column and replace the rest with an empty string
attacks$Activity <- str_extract(attacks$Activity, paste0("\\b(?:", pattern, ")\\b"))
attacks <- attacks %>%
  mutate(Activity = str_replace_all(Activity, "\\bkayaking\\b", "kayak")) %>%
  mutate(Activity = str_replace_all(Activity, "\\bsurfing\\b", "surf"))

attacks$Activity[is.na(attacks$Activity)] <- "other"

#________________________________________________________________________________________________________________________

#FATAL 

table(attacks$Fatal..Y.N.) #to check if categories have mistakes etc

attacks$Fatal..Y.N. <- gsub("2017", "", attacks$Fatal..Y.N.)
attacks$Fatal..Y.N. <- gsub("M", "", attacks$Fatal..Y.N.)

attacks$Fatal..Y.N. <-na_if(attacks$Fatal..Y.N., "")

attacks <- subset(attacks, !is.na(Fatal..Y.N.))

#Replace Yes by 1, 0 otherwise
attacks$Fatal..Y.N. <- ifelse(attacks$Fatal..Y.N. == "Y", 1, 0)

names(attacks)[names(attacks) == "Fatal..Y.N."] <- "Fatality"

#________________________________________________________________________________________________________________________
#final check up:

table(attacks$Date) #this one is fine
table(attacks$Year) #this one is fine
table(attacks$Type) #here we have boating and boat which mean the same thing. let us group them
attacks$Type <- gsub("Boating", "Boat", attacks$Type)
attacks$Type <- gsub("Boatomg", "Boat", attacks$Type)
table(attacks$Type) #this one is fine
table(attacks$Country)#this one is fine
table(attacks$Activity)#this one is fine
table(attacks$Age)#this one is fine
table(attacks$Fatality)#this one is fine
table(attacks$Time)#this one is fine
table(attacks$Species)#this one is fine

sum(is.na(attacks$Time)) #this is the only one that still presents 1237 NA. we dont want to delete
#them all coz we'd lose 40% of our data. 
table(attacks$Time)#table shows that most of them happen in the afternoon, while 2ns place is owned by
#morning. To confirm the higher frequency of attacks between 8am and 6pm is this artile (link JC found??)
#we explain this by the fact that, naturally, most of people swim during the day. therefore, what we do
#is replacing NA randomly by the proportion of afternoon, morning and evening.

attacks$Time <- na_if(attacks$Time, "")

898+228+589+9 
898/1724
228/1724
589/1724
#afternoon is 52%, evening is 12% and morning is 34%.

attacks$Time <- ifelse(is.na(attacks$Time),
                       sample(c("afternoon", "morning", "evening", "night"), 
                              size = sum(is.na(attacks$Time)), 
                              replace = TRUE, 
                              prob = c(0.52, 0.34, 0.12, 0.02)),
                       attacks$Time)


#________________________________________________________________________________________________________________________
#ADD ISO CODE TO MAKE COUNTRIES IN COMMON

# Get ISO country codes
library(countrycode)
iso_codes <- countrycode(attacks$Country, "country.name", "iso3c")
attacks$ISO_Code <- countrycode(attacks$Country, "country.name", "iso3c")

#We can see that there are 23 countries with no ISO Code. We will fix them by hand.
rows_with_na <- which(is.na(attacks$ISO_Code))
rows_with_na

attacks$ISO_Code[attacks$Country %in% c("ENGLAND", "SCOTLAND", "BRITISH ISLES") & is.na(attacks$ISO_Code)] <- "GB"
attacks$ISO_Code[attacks$Country %in% c("AZORES") & is.na(attacks$ISO_Code)] <- "PRT"
attacks$ISO_Code[attacks$Country %in% c("ST. MAARTIN", "ST. MARTIN") & is.na(attacks$ISO_Code)] <- "MAF"
attacks$ISO_Code[attacks$Country %in% c("OKINAWA") & is.na(attacks$ISO_Code)] <- "JPN"
attacks$ISO_Code[attacks$Country %in% c("MICRONESIA") & is.na(attacks$ISO_Code)] <- "FSM"

attacks <- subset(attacks, !is.na(ISO_Code))

#________________________________________________________________________________________________________________________

#                           C L E A N I N G   OF    T E M P E R A T U R E S

#________________________________________________________________________________________________________________________

library(dplyr)
temperature <- read_xlsx("Temperature.xlsx")

#it did not read the "°C" on the column Unit, so I'll change it manually

temperature <- temperature %>% mutate(Unit = "°C")

#I take off all the columns that we do not need 

temperature <- temperature %>% select(-'Area Code (M49)', -'Months Code', -'Element Code')

#eliminate all columns having an F at the end
columns_to_remove <- grep("F$", names(temperature), value = TRUE)
temperature <- temperature[, !(names(temperature) %in% columns_to_remove)]

# keep only meteorological year, we dont need to work with monthly stuff

target_name <- "Meteorological year"
temperature <- temperature[temperature$Months == target_name, ]

target_name2 <- "Temperature change"
temperature <- temperature[temperature$Element == target_name2, ]



#  take off cols that I dont need, take off NA and transforsm year cols so that i take off Y. in this
#way i can match with the main dataset

temperature <- temperature %>% select(-'Area Code', -'Months', -'Element', -'Unit')

new_colnames <- gsub("Y", "", colnames(temperature))
colnames(temperature) <- new_colnames
temperature <- na.omit(temperature)


# I need to change columns and rows so that the common variable (year) is on the left side

temperature <- t(temperature)

# I want the first row to be the header

colnames(temperature) <- temperature[1, ]
clean_temperature <- temperature[-1, ]

#col names in CAPS
colnames(clean_temperature) <- toupper(colnames(clean_temperature))


library(tidyr)

# Convert the matrix/array to a data frame
temperature <- as.data.frame(clean_temperature)

# Add 'Year' as a separate column
temperature$Year <- rownames(clean_temperature)

# Reshape the data from wide to long format
temperature <- tidyr::pivot_longer(temperature, 
                                   cols = -Year, 
                                   names_to = "Country", 
                                   values_to = "Temperature")

# Reorder columns as per your desired format
temperature <- temperature[c("Year", "Country", "Temperature")]


temperature <- temperature %>% filter(Year >= 1970)
temperature$Year <- as.numeric(temperature$Year)

temperature <- temperature %>% 
  mutate(Country = ifelse(Country == "UNITED STATES OF AMERICA", "USA", Country))

temperature$ISO_Code <- countrycode(temperature$Country, "country.name", "iso3c")

temperature$ISO_Code[temperature$Country %in% c("MICRONESIA") & is.na(temperature$ISO_Code)] <- "FSM"

#now that i put iso, i can remove the country one
temperature <- temperature %>% select(-'Country')

#________________________________________________________________________________________________________________________
#MERGE FIRST TWO DATA SETS

# Assuming 'Year' and 'Country' are the columns in both datasets for matching
merged_data <- left_join(attacks, temperature, by = c("Year", "ISO_Code"))

sum(is.na(merged_data$Temperature)) #155 NA --> SHOULD I TAKE THEM OFF???



#________________________________________________________________________________________________________________________

#                           C L E A N I N G   OF    S E A    L E V E L

#________________________________________________________________________________________________________________________


sealevel <- read.csv("sealevel.csv")

# keep the 2 columns we are interested to analyze because the other are irrelevant for our project
sealevel <- subset(sealevel, select = c(Year, GMSL_GIA))

#Show the year only the first time by creating a new column called Year2
sealevel$Year2 <- ifelse(duplicated(sealevel$Year), NA, sealevel$Year)

# delete column Year due to the creation of column Year 2
sealevel <- subset(sealevel, select = -Year)

# delete NA because it does not bring anything to our analysis
sealevel <- na.omit(sealevel)

#Change name of column Year
column <- gsub("2","",colnames(sealevel))
colnames(sealevel) <- column

#________________________________________________________________________________________________________________________
#MERGE PREVIOUS AND NEW

# Assuming 'Year' is the columns in both datasets for matching
merged_data2 <- left_join(merged_data, sealevel, by = c("Year"))



#________________________________________________________________________________________________________________________

#                           C L E A N I N G   OF    CO2      L E V E L

#_________________________________________________________________________________________________________________________
if (!require(dplyr)) {
  install.packages("dplyr")
}

# Charger le package dplyr
library(dplyr)

co2 <- read.csv("CO2.csv")

# Change names of columns in order to have the same columns that in the other datasets

names(co2)[names(co2) == "year"] <- "Year"
names(co2)[names(co2) == "Annual.CO..emissions"] <- "Annual CO2 Emissions"
names(co2)[names(co2) == "Entity"] <- "Country"
names(co2)[names(co2) == "Code"] <- "ISO_Code"



# keep the 4 columns we are interested to analyze

co2 <- subset(co2, select = c("ISO_Code", "Year", "Annual CO2 Emissions", "Country"))



# only keep information starting in 1970 because we want to look at the evolution of 
# the last 50 years
co2<- co2[co2$Year >= 1970, ]

#co2$ISO_Code <- na_if(co2$ISO_Code, "")

co2$ISO_Code <- ifelse(co2$ISO_Code == "", NA, co2$ISO_Code)


co2 <- subset(co2, !is.na(ISO_Code))

#________________________________________________________________________________________________________________________
#MERGE FIRST THREE DATA SETS

# Assuming 'Year' and 'Country' are the columns in both datasets for matching
merged_data3 <- left_join(merged_data2, co2, by = c("Year", "ISO_Code"))

#Change name of a column
colnames(merged_data3)[colnames(merged_data3) == 'Country.x'] <- 'Country'

#Delete a useless column
merged_data3 <- select(merged_data3, -Country.y)
merged_data3$Temperature <- as.numeric(as.character(merged_data3$Temperature))


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


map <- map %>%
  mutate(Country = ifelse(Country == "UNITED STATES", "USA", Country))

merged_map <- merge(merged_data3, map, by='Country', all=FALSE)

# Create a new variable representing the nb of attacks per country
results <- merged_map %>%
  group_by(Country) %>%
  summarise(Attackscountry = n())
print(results)

# Attach aggregated data to your original dataframe
merged_map <- left_join(merged_map, results, by = "Country")


# Definition of the thresholds for the categorization
seuils <- c(-Inf, 50, 100, 500, Inf)

# Definition of the colors we want to have in the map
couleurs <- c("#4DA6FF", "#0074CC", "#6C8EBF","#001F3F80")

# Add a new category column based on thresholds
merged_map$cat_attacks <- cut(merged_map$Attackscountry, breaks = seuils, labels = FALSE)

merged_map$echelle_taille <- merged_map$Attackscountry * 0.1

#Let's have fun with an interactive map
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

ma_carte



#________________________________________________________________________________________________________________________

#                           RESEARCH QUESTION ON CLIMATE CHANGE

#________________________________________________________________________________________________________________________


#Explain that we saw both increasing trend in frequency of shark attacks and on factors that explain
#climate change (co2 level etc) -> so, run regression  to see whats going on

#We only focus on 3 biggest countries (USA, ZAF, AUS), bacause others have very small frequencies of shark
#attacks. 

#The first thing we do is creating a copy of our dataset, so that we can take away some information
#that are not needed here. Indeed, with the na.omit function, we delete all the rows of the years
#after 1992. The reason for this is that we were not able to find a dataset on sea level information
#that contained information starting from 1970.

data_RQ2 <- merged_data3
data_RQ2 <- na.omit(data_RQ2) 


str(data_RQ2) #ok now im sure they all num/int and no chr

#Run correlation matrix to be sure that there is no multicollinearity. When we run it, we see that
#all vorrelations are far from being equal to 1 or -1, which is a positive sign.

subset_data <- data_RQ2[, c("Temperature", "Annual CO2 Emissions", "GMSL_GIA")]
correlation_matrix <- cor(subset_data, use = "complete.obs")
corrplot1 <- corrplot(correlation_matrix, method = "circle")

# Customized corrplot for the subset
corrplot2 <- corrplot(
  correlation_matrix,
  method = "ellipse",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "gray"
)


# Create a new variable 'shark_attacks' representing the frequency of shark attacks per year
count_shark_attacks <- data_RQ2 %>%
  group_by(Year, ISO_Code) %>%
  summarize(SharkAttacksCount = n())

# Merge the aggregated shark attacks data back to your original dataset based on the 'year' and 'country' variables
data_RQ2 <- merge(data_RQ2, count_shark_attacks, by = c("Year", "ISO_Code"), all.x = TRUE)

# FIRST MODEL
filtered_data <- data_RQ2 %>%
  filter(ISO_Code %in% c("USA", "ZAF", "AUS"))

model <- lm(SharkAttacksCount ~ Temperature + GMSL_GIA + `Annual CO2 Emissions`, data = filtered_data)

# With the summary function, we can see the new estimators. All positive and significant
summary(model)







