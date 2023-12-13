library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(corrplot)
library(viridisLite)
library(htmltools)
library(RColorBrewer)
library(car)
#________________________________________________________________________________________________________________________

#                           RESEARCH QUESTION ON SHARKS

#________________________________________________________________________________________________________________________
# REGRESSION

#Before doing the regression, we will do a correlation matrix of the numerical variables
#without fatality because is an irrelevant variable in this first regression 
#given that the fatality is something that we know once the accident happen and not before

data_RQ1 <- merged_map
data_RQ1 <- na.omit(data_RQ1) 

str(data_RQ1) #ok now im sure they all num/int and no chr

#Run correlation matrix to be sure that there is no multicollinearity. When we run it, we see that
#all correlations are far from being equal to 1 or -1, which is a positive sign.

subset_data1 <- data_RQ1[, c("Year", "Sex", "Type", "Time", "Age", "Species")]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data1)

# Show the correlation matrix
print(correlation_matrix)

# Let's do an overall multiple regression model
model <- lm(Attackscountry ~ Year + Sex + Type + Time + Age + Species, data = merged_map)

# Print the summary of the regression model
summary(model)

#________________________________________________________
# USA Regression

#This first regression will be based only on the USA
usa_data <- merged_map %>%
  filter(Country == "USA")

#Regression model of the USA
model1 <- lm(Attackscountry ~ Year + Sex + Age + Species + Type + Time, data = usa_data)

print("Regression for USA:")
summary(model1)

# Compute the VIF
vif_values <- car::vif(model1)

# Show me the results of the VIF
print(vif_values)


#___________________________________________________________
# Assuming 'usa_data' is your dataset for the USA
# Include the intercept in the initial model
current_model <- lm(Attackscountry ~ 1, data = usa_data)

# List to store models at each step
selected_models <- list()

# Variables to add
variables_to_add <- c("Year", "Sex", "Age", "Species", "Time", "Type")

# Forward selection loop
for (variable in variables_to_add) {
  # Add a variable to the formula
  formula <- as.formula(paste(". ~ . +", variable))
  current_model <- update(current_model, formula)
  
  # Store the current model in the list
  selected_models[[variable]] <- current_model
  
  # Calculate AIC
  aic_value <- AIC(current_model)
  
  # Print the summary of the current model and AIC value
  cat("Variable added:", variable, " | AIC:", aic_value, "\n")
  print(summary(current_model))
}

#___________________________________________________________
#This second regression will be based only on Australia
aus_data <- merged_map %>%
  filter(Country == "AUSTRALIA")

model2 <- lm(Attackscountry ~ Year + Sex + Age + Species + Type + Time, data = aus_data)

print("Regression for AUSTRALIA:")
summary(model2)

#___________________________________________________________
# Include the intercept in the initial model
current_model2 <- lm(Attackscountry ~ 1, data = aus_data)

# List to store models at each step
selected_models2 <- list()

# Variables to add
variables_to_add2 <- c("Year", "Sex", "Age", "Species", "Time", "Type")

# Forward selection loop
for (variable in variables_to_add2) {
  # Add a variable to the formula
  formula <- as.formula(paste(". ~ . +", variable))
  current_model2 <- update(current_model2, formula)
  
  # Store the current model in the list
  selected_models[[variable]] <- current_model2
  
  # Calculate AIC
  aic_value2 <- AIC(current_model2)
  
  # Print the summary of the current model and AIC value
  cat("Variable added:", variable, " | AIC:", aic_value, "\n")
  print(summary(current_model2))
}

# Compute the VIF
vif_values2 <- car::vif(model2)

# Show me the results of the VIF
print(vif_values2)


#_______________________________________________________________________________
# Let's focus on both countries
# This regression will be based on both USA and AUSTRALIA
combined_data <- merged_map %>%
  filter(Country %in% c("USA", "AUSTRALIA"))

# Run the regression based on both USA and AUSTRALIA
model_combined <- lm(Attackscountry ~ Year + Sex + Age + Species + Type + Time, data = combined_data)

# Print the results
print("Regression for USA and AUSTRALIA:")
print(summary(model_combined))

#______________________________________________________________________________
#Model combined, AIC 

# Include the intercept in the initial model
current_model3 <- lm(Attackscountry ~ 1, data = combined_data)

# List to store models at each step
selected_models3 <- list()

# Variables to add
variables_to_add3 <- c("Year", "Sex", "Age", "Species", "Time", "Type")

# Forward selection loop
for (variable in variables_to_add3) {
  # Add a variable to the formula
  formula <- as.formula(paste(". ~ . +", variable))
  current_model3 <- update(current_model3, formula)
  
  # Store the current model in the list
  selected_models[[variable]] <- current_model3
  
  # Calculate AIC
  aic_value2 <- AIC(current_model3)
  
  # Print the summary of the current model and AIC value
  cat("Variable added:", variable, " | AIC:", aic_value, "\n")
  print(summary(current_model3))
}

#________________________________________________________________________________________________________________________

#                           RESEARCH QUESTION ON FATALITY

#________________________________________________________________________________________________________________________

filtered_data4 <- merged_data3 %>%
  filter(ISO_Code %in% c("ZAF", "USA", "AUS"))

filtered_data4 <- merged_data3 %>%
  filter(ISO_Code %in% c("ZAF", "USA", "AUS"),
         Sex %in% c(0, 1))

model12 <- glm(Fatality ~ Date + Year + Age + Time, 
               data = filtered_data4, 
               family = "binomial")

summary(model12)

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
#all correlations are far from being equal to 1 or -1, which is a positive sign.

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


original_scipen <- options("scipen")
options(scipen = 1000)

stargazer(model,
          title = 'Regression for shark attacks against climate change factors',
          type = 'html',
          digits = 5)




