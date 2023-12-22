
#________________________________________________________________________________________________________________________

#EDA

#1. See the evolution of shark attacks throughout the years DONE

# Check the column names in your dataset
colnames(attacks)
# Check the structure of your data frame
str(attacks)

# Create a plot to show the trend of shark attacks throughout the years
ggplot(data = attacks, aes(x = Year)) +
  geom_smooth(stat = "count", aes(group = 1), color = "blue", size = 1) +
  geom_bar(stat = "count", fill = "red") +
  labs(title = "Shark Attacks Evolution Over Years", x = "Year", y = "Number of Attacks")


#2. See what are the countries with most shark attacks DONE

shark_attacks_by_country <- attacks %>%
  group_by(Country) %>%
  summarise(total_attacks = n())

shark_attacks_by_country <- shark_attacks_by_country %>%
  mutate(CountryCategory = ifelse(total_attacks >= 30, as.character(Country), "Other"))

# Order the countries by frequency in descending order
shark_attacks_by_country$CountryCategory <- reorder(shark_attacks_by_country$CountryCategory, -shark_attacks_by_country$total_attacks)

# Plot the data
plot2<- ggplot(data = shark_attacks_by_country, aes(x = total_attacks, y = CountryCategory)) +
  geom_col(fill = "skyblue") +
  labs(title = "Total Shark Attacks in Each Country", x = "Number of Attacks", y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  scale_y_discrete(labels = scales::label_wrap(10))

interactive_plot <- ggplotly(plot2)
interactive_plot


#3. WHAT TIME DONE

bar_colors <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

# Create a sorted table
sorted_table <- table(attacks$Time)
sorted_table <- sorted_table[order(-sorted_table)]

# Create a bar plot with the sorted data
barplot1 <- barplot(sorted_table,
                    col = bar_colors,
                    xlab = "Time of Day", ylab = "Frequency of Attacks",
                    main = "Frequency of Shark Attacks by Time of Day",
                    border = "white",
                    ylim = c(0, 1800),
                    space = 0.5,
                    cex.names = 0.8,
                    font.axis = 2,
                    beside = TRUE)


# Add text labels with frequencies on the bars
text_labels <- sorted_table
text(x = barplot1, y = sorted_table, labels = text_labels, pos = 3, cex = 0.8, col = "black")

#4. Fatality

# Get the top 10 shark species
top_10_sharks <- head(names(sort(table(final_attacks_cleaned$Species), decreasing = TRUE)), 10)

# Subset the data to include only the top 10 shark species
final_attacks_top_10 <- final_attacks_cleaned[final_attacks_cleaned$Species %in% top_10_sharks, ]

# Create the summary table for the top 10 shark species
summary_table_top_10 <- table(final_attacks_top_10$Species, final_attacks_top_10$Fatality)

# Convert the summary table to a data frame
summary_df_top_10 <- as.data.frame(summary_table_top_10)

# Create the ggplot with the top 10 shark species
plot_top_10 <- ggplot(summary_df_top_10, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Fatality by Top 10 Types of Shark",
    x = "Type of Shark",
    y = "Frequency",
    fill = "Legend"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    name = "Was the shark attack fatal?",
    values = c("blue", "pink", "yellow"),
    labels = c("Yes", "No", "Unknown")
  )

# Display the plot
print(plot_top_10)



#5. See the evolution of the level of co2 emission throughout the years DONE


# Create an interactive line plot with backticks to show the trend in co2 emissions throughout the years
interactive_co2_evolution <- ggplotly(
  ggplot(data = co2, aes(x = Year, y = `Annual CO2 Emissions`)) +
    geom_line(color = "blue") +
    labs(title = "Trend of CO2 Emissions Over the Years", x = "Year", y = "Annual CO2 Emissions")
)

# Display the interactive plot
print(interactive_co2_evolution)



#6A. See what are the countries with the most co2 emission

# Filter out 'OWID_WRL' from the data
co2_filtered_wrl <- final_ghg_cleaned %>% filter(ISO_Code != "OWID_WRL")

# Select the top 30 countries based on CO2 emissions
top30_countries <- co2_filtered_wrl %>%
  group_by(ISO_Code) %>%
  summarize(`Annual CO2 Emissions` = sum(`Annual CO2 Emissions`)) %>%
  arrange(desc(`Annual CO2 Emissions`)) %>%
  top_n(30)

# Create an interactive bar plot
interactive_top30_countries_co2 <- ggplotly(
  ggplot(data = top30_countries, aes(x = reorder(ISO_Code, -`Annual CO2 Emissions`), y = `Annual CO2 Emissions`)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Top 30 Countries with the Most CO2 Emissions", x = "Countries", y = "Annual CO2 Emissions") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# Display the interactive bar plot
print(interactive_top30_countries_co2)


#6B. Continents with most co2 emissions DONE

#The goal now is to regroup countries by continents and be able to have a better idea of where are most of the emissions coming from around the globe


# Get unique ISO codes to be able to put them in a group of continents
unique_iso_codes <- unique(co2$ISO_Code)

# Create a new column 'Region' based on the Continents and the countries 
co2_grouped <- co2 %>%
  mutate(Region = case_when(
    ISO_Code %in% c(NA) ~ "Unknown",
    ISO_Code %in% c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BOL", "BGR", "BFA", "BDI", "KHM", "CMR", "CPV", "CHN", "CXR", "CYP", "HKG", "IND", "IDN", "IRN", "IRQ", "ISR", "JOR", "JPN", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MAC", "MYS", "MDV", "MNG", "MMR", "NPL", "PRK", "OMN", "PAK", "PSE", "PHL", "QAT", "SAU", "SGP", "KOR", "LKA", "TUR", "SYR", "TWN", "TJK", "THA", "TKM", "ARE", "UZB", "YEM", "VNM") ~ "Asia",
    ISO_Code %in% c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BWA", "HRV", "CZE", "DNK", "EST", "FIN", "FRO", "FRA", "GEO", "DEU", "GRC", "GRL", "GLP", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LIE", "LUX", "MLT", "MDA", "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR") ~ "Europe",
    ISO_Code %in% c("DZA", "AGO", "BEN", "CAF", "TCD", "COM", "COG", "CIV", "COD", "DJI", "TLS", "EGY", "ERI", "SWZ", "ETH", "GNQ", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MTQ", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU", "RWA", "SHN", "SEN", "SYC", "SLE", "STP", "ZAF", "SOM", "SSD", "SDN", "SUR", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE") ~ "Africa",
    ISO_Code %in% c("AIA", "ATG", "ABW", "BHS", "BMU", "BRB", "BLZ", "VGB", "BRN", "CAN", "CRI", "CUB", "CUW", "DMA", "DOM", "SLV", "GRD", "GTM", "HTI", "HND", "JAM", "MSR", "MEX", "USA", "NIC", "PAN", "KNA", "LCA", "SPM", "VCT", "SXM", "TCA", "TTO") ~ "North America",
    ISO_Code %in% c("ARG", "COL", "CHL", "ECU", "GUF", "GUY", "PRY", "PER", "URY", "VEN", "BRA", "BES") ~ "South America",
    ISO_Code %in% c("AUS", "COK", "FJI", "PYF", "KIR", "MHL", "FSM", "NRU", "NCL", "NZL", "NIU", "PLW", "PNG", "WSM", "SLB", "TON", "TUV", "VUT", "WLF") ~ "Australia & Oceania",
    ISO_Code %in% c("ATA") ~ "Antarctica"
  ))

# Step 1: Calculate Mean Emissions by Year and Region
mean_emissions <- co2_grouped %>%
  group_by(Year, Region) %>%
  summarize(Mean_Emissions = mean(`Annual CO2 Emissions`, na.rm = TRUE))

# Step 2: Let's plot the Data
# Create an interactive line plot
interactive_co2_continent_wNA <- ggplotly(
  ggplot(mean_emissions, aes(x = Year, y = Mean_Emissions, color = Region)) +
    geom_line() +
    labs(title = "CO2 Emissions by Continent",
         x = "Year",
         y = "Annual CO2 Emissions") +
    theme_minimal()
)


# Now we get rid of that Unknown Region to have a better view of the other Regions

# Step 1: Calculate Mean Emissions by Year and Region and getting rid of the things we don't want
mean_emissions <- co2_grouped %>%
  filter(!is.na(Region) & !is.na(`ISO_Code`) & Region != "Unknown") %>%
  group_by(Year, Region) %>%
  summarize(Mean_Emissions = mean(`Annual CO2 Emissions`, na.rm = TRUE))

# Step 2: Let's plot the Data
interactive_co2_continent <- ggplotly(
  ggplot(mean_emissions, aes(x = Year, y = Mean_Emissions, color = Region)) +
    geom_line() +
    labs(title = "CO2 Emissions by Continent",
         x = "Year",
         y = "Annual CO2 Emissions") +
    theme_minimal()
)

# Display the interactive line plot
print(interactive_co2_continent)


#7. See the evolution of sea level throughout the years DONE

# Create a line plot
interactive_sealevel <- ggplotly(
  ggplot(data = sealevel, aes(x = Year, y = GMSL_GIA)) +
    geom_line(color = "green", size = 1.5) +
    labs(title = "Evolution of Sea Level Over the Years", x = "Year", y = "Sea Level")
)

# Display the interactive line plot
print(interactive_sealevel)



#8. See the evolution in temperature by countries throughout the years DONE

# Filter out NA values and non-numeric values in the Temperature column
temperature_data_filtered <- temperature %>%
  filter(!is.na(Temperature), !is.na(as.numeric(Temperature)))

# Convert the Temperature column to numeric
temperature_data_filtered$Temperature <- as.numeric(temperature_data_filtered$Temperature)

# Calculate the mean temperature for each year
world_temperature <- temperature_data_filtered %>%
  group_by(Year) %>%
  summarise(World_Temperature = mean(Temperature, na.rm = TRUE))

# Create a line plot for the world temperature
interactive_temperature <- ggplotly(
  ggplot(world_temperature, aes(x = Year, y = World_Temperature)) +
    geom_line() +
    labs(title = "World Temperature Change Over Years",
         x = "Year",
         y = "World Temperature Change") +
    theme_minimal()
)

# Display the interactive line plot
print(interactive_temperature)


#9. Shows the relationship between the attacks and the age of the victims DONE


# Create a summary table with the count of attacks for each age
attacks_summary <- attacks %>%
  group_by(Age) %>%
  summarise(NumAttacks = n())

# Scatter plot to show the relationship between the number of shark attacks and age
interactive_attacks_age <- ggplotly(
  ggplot(attacks_summary, aes(x = Age, y = NumAttacks)) +
    geom_point(color = "blue", size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear trend line
    labs(title = "Relationship between Shark Attacks and Age",
         x = "Age",
         y = "Number of Attacks")
)

# Display the interactive scatter plot
print(interactive_attacks_age)


#10. Better understanding of the attacks and sex of the victims DONE

# Create a summary table with the count of attacks for each sex
attacks_summary_sex <- attacks %>%
  group_by(Sex) %>%
  summarise(NumAttacks = n())

# Define the desired order of levels
sex_order <- c("M", "F", "Unknown")

# Convert the 'Sex' variable to a factor with the specified order
attacks_summary_sex$Sex <- factor(attacks_summary_sex$Sex, levels = sex_order)

# Bar plot to show the distribution of shark attacks by sex
interactive_bar_plot_sex_attacks <- ggplotly(
  ggplot(attacks_summary_sex, aes(x = Sex, y = NumAttacks, fill = Sex)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(title = "Distribution of Shark Attacks by Sex",
         x = "Sex",
         y = "Number of Attacks") +
    scale_fill_manual(values = c("blue", "pink", "gray"))
)

# Display the interactive bar plot
print(interactive_bar_plot_sex_attacks)



#11. Want to know where are the regions (continents) with the most attacks DONE

#Create a new column in attacks that represents the continents
attacks_grouped <- attacks %>%
  mutate(Region = case_when(
    ISO_Code %in% c(NA) ~ "Unknown",
    ISO_Code %in% c("AFG", "ARM", "AZE", "BHR", "BGD", "BTN", "BOL", "BGR", "BFA", "BDI", "KHM", "CMR", "CPV", "CHN", "CXR", "CYP", "HKG", "IND", "IDN", "IRN", "IRQ", "ISR", "JOR", "JPN", "KAZ", "KWT", "KGZ", "LAO", "LBN", "MAC", "MYS", "MDV", "MNG", "MMR", "NPL", "PRK", "OMN", "PAK", "PSE", "PHL", "QAT", "SAU", "SGP", "KOR", "LKA", "TUR", "SYR", "TWN", "TJK", "THA", "TKM", "ARE", "UZB", "YEM", "VNM") ~ "Asia",
    ISO_Code %in% c("ALB", "AND", "AUT", "BLR", "BEL", "BIH", "BWA", "HRV", "CZE", "DNK", "EST", "FIN", "FRO", "FRA", "GEO", "DEU", "GRC", "GRL", "GLP", "HUN", "ISL", "IRL", "ITA", "LVA", "LTU", "LIE", "LUX", "MLT", "MDA", "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR") ~ "Europe",
    ISO_Code %in% c("DZA", "AGO", "BEN", "CAF", "TCD", "COM", "COG", "CIV", "COD", "DJI", "TLS", "EGY", "ERI", "SWZ", "ETH", "GNQ", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MTQ", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU", "RWA", "SHN", "SEN", "SYC", "SLE", "STP", "ZAF", "SOM", "SSD", "SDN", "SUR", "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE") ~ "Africa",
    ISO_Code %in% c("AIA", "ATG", "ABW", "BHS", "BMU", "BRB", "BLZ", "VGB", "BRN", "CAN", "CRI", "CUB", "CUW", "DMA", "DOM", "SLV", "GRD", "GTM", "HTI", "HND", "JAM", "MSR", "MEX", "USA", "NIC", "PAN", "KNA", "LCA", "SPM", "VCT", "SXM", "TCA", "TTO") ~ "North America",
    ISO_Code %in% c("ARG", "COL", "CHL", "ECU", "GUF", "GUY", "PRY", "PER", "URY", "VEN", "BRA", "BES") ~ "South America",
    ISO_Code %in% c("AUS", "COK", "FJI", "PYF", "KIR", "MHL", "FSM", "NRU", "NCL", "NZL", "NIU", "PLW", "PNG", "WSM", "SLB", "TON", "TUV", "VUT", "WLF") ~ "Australia & Oceania",
    ISO_Code %in% c("ATA") ~ "Antarctica"
  ))


# Filter data to include only valid regions (excluding "Unknown")
filtered_attacks_region <- attacks_grouped %>% 
  filter(!is.na(Region) & Region != "Unknown")

# Bar plot to show the distribution of shark attacks by region
interactive_bar_plot_region_attacks <- ggplotly(
  ggplot(filtered_attacks_region, aes(x = Region, fill = Region)) +
    geom_bar() +
    labs(title = "Distribution of Shark Attacks by Region",
         x = "Region",
         y = "Number of Attacks") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
)


# Display the interactive bar plot
print(interactive_bar_plot_region_attacks)  



#12. Does sea level and the number of shark attacks have a relationship DONE

# Calculate the number of attacks per year
attacks_summary <- attacks %>%
  group_by(Year) %>%
  summarize(Number_of_Attacks = n())

# Assuming there's a common column named "Year" in both datasets
merged_data5 <- merge(attacks_summary, sealevel, by = "Year", all.x = TRUE)

correlation_coefficient <- cor(merged_data5$Number_of_Attacks, merged_data5$GMSL_GIA, use = "complete.obs")

# Scatter plot
interactive_scatter_plot_sea_level_attacks <- ggplotly(
  ggplot(merged_data5, aes(x = GMSL_GIA, y = Number_of_Attacks)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear trend line
    labs(title = "Relationship Between Sea Level and Shark Attacks",
         x = "Global Mean Sea Level Global Isostatic Adjustment (GMSL_GIA)",
         y = "Number of Attacks")
)

# Display the interactive scatter plot
print(interactive_scatter_plot_sea_level_attacks)


#13. Relationship between temperature change and the number of shark attacks DONE

# Identify non-numeric values in the Temperature column
non_numeric_temp <- temperature %>%
  filter(!is.numeric(Temperature)) %>%
  distinct(Temperature)

# Convert "Temperature" column to numeric
temperature$Temperature <- as.numeric(temperature$Temperature)

# Check for negative values
negative_values <- temperature %>%
  filter(Temperature < 0)

# Calculate the mean temperature
mean_temp_world <- temperature %>%
  group_by(Year) %>%
  summarize(Mean_Temperature = mean(Temperature, na.rm = TRUE))

# Filter out non-numeric values in the Temperature column
temperature2 <- temperature %>%
  filter(is.numeric(Temperature))

# Calculate mean temperature for the world per year
mean_temp_world <- temperature2 %>%
  group_by(Year) %>%
  summarize(Mean_Temperature = mean(Temperature, na.rm = TRUE))

# Merge datasets
merged_data6 <- merge(attacks_summary, mean_temp_world, by = "Year", all.x = TRUE)

# Plotting
interactive_scatter_plot_worldtemperature_attacks <- ggplotly(
  ggplot(merged_data6, aes(x = Mean_Temperature, y = Number_of_Attacks)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear trend line
    labs(title = "Relationship between Shark Attacks and World Mean Temperature Change",
         x = "World Mean Temperature Change",
         y = "Number of Shark Attacks") +
    theme_minimal()
)


print(interactive_scatter_plot_worldtemperature_attacks)


#14. What are the activities with the most attacks DONE

# Check the top activities with the most attacks
top_activities <- attacks %>%
  group_by(Activity, Sex) %>%
  summarize(Number_of_Attacks = n()) %>%
  arrange(desc(Number_of_Attacks)) %>%
  top_n(30)  # Adjust the number if you want more or fewer top activities

# Plot the results
top_activities_attacks <- ggplot(top_activities, aes(x = reorder(Activity, -Number_of_Attacks), y = Number_of_Attacks, fill = Sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Activities with the Most Attacks",
       x = "Activities",
       y = "Number of Attacks",
       fill = "Sex") +
  scale_fill_manual(values = c("pink", "blue", "orange"), name = "Sex") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


interactive_top_activities_attacks <- ggplotly(top_activities_attacks)
interactive_top_activities_attacks



#15. See the evolution of shark attacks throughout the years with sex DONE

# Create a plot to show the trend of shark attacks throughout the years, including victim's sex (interactive version)
attacks_evolution_sex <- ggplot(data = attacks, aes(x = Year, fill = Sex)) +
  geom_bar(position = "stack", color = "white") +
  labs(title = "Shark Attacks Evolution Over Years by Sex",
       x = "Year",
       y = "Number of Attacks",
       fill = "Sex") +
  scale_fill_manual(values = c("pink", "blue", "orange"), name = "Sex") +
  theme_minimal()


interactive_attacks_evolution_sex <- ggplotly(attacks_evolution_sex)
interactive_attacks_evolution_sex


#16. Shark attacks monthly repartition

#Here it is great but I tried to class the months in order but it has not worked yet

# Create a plot to show the repartition of shark attacks per month by sex
attacks_per_months <- ggplot(data = attacks, aes(x = factor(Date), fill = Sex)) +
  geom_bar(position = "stack", color = "white") +
  labs(title = "Shark Attacks Repartition by Month and Sex",
       x = "Month",
       y = "Number of Attacks",
       fill = "Sex") +
  scale_fill_manual(values = c("pink", "blue", "orange"), name = "Sex") +
  theme_minimal()


interactive_attacks_per_months <- ggplotly(attacks_per_months)
interactive_attacks_per_months


#17. Descriptive data analysis

#Attacks


#Age  

# Histogram for a numerical variable
hist(attacks$Age)

# Interactive Histogram
interactive_hist <- plot_ly(x = ~attacks$Age, type = "histogram")
print(interactive_hist)

# Bar chart for a categorical variable
barplot(table(attacks$Age))

# Interactive Bar Chart
interactive_bar <- plot_ly(x = names(table(attacks$Age)), y = table(attacks$Age), type = "bar")
print(interactive_bar)

# Boxplot for numerical variables
boxplot(attacks$Age)

# Interactive Boxplot
interactive_boxplot <- plot_ly(y = ~attacks$Age, type = "box")
print(interactive_boxplot)

# Mean and median for a numerical variable
mean(attacks$Age)
median(attacks$Age)

# Standard deviation and variance
sd(attacks$Age)
var(attacks$Age)

#CO2

# Summary statistics for numerical variables
summary(co2)

#Annual CO2 Emissions

# Histogram for a numerical variable
hist(co2$`Annual CO2 Emissions`)

# Interactive Histogram
interactive_hist_co2 <- plot_ly(x = ~co2$`Annual CO2 Emissions`, type = "histogram")
print(interactive_hist_co2)

# Bar chart for a categorical variable
barplot(table(co2$`Annual CO2 Emissions`))

# Interactive Bar Chart
interactive_bar_co2 <- plot_ly(x = names(table(co2$`Annual CO2 Emissions`)), y = table(co2$`Annual CO2 Emissions`), type = "bar")
print(interactive_bar_co2)

# Boxplot for numerical variables
boxplot(co2$`Annual CO2 Emissions`)

# Interactive Boxplot
interactive_boxplot_co2 <- plot_ly(y = ~co2$`Annual CO2 Emissions`, type = "box")
print(interactive_boxplot_co2)

# Mean and median for a numerical variable
mean(co2$`Annual CO2 Emissions`)
median(co2$`Annual CO2 Emissions`)

# Standard deviation and variance
sd(co2$`Annual CO2 Emissions`)
var(co2$`Annual CO2 Emissions`)


#Sea level

# Summary statistics for numerical variables
summary(sealevel)

#GMSL_GIA

# Histogram for a numerical variable
hist(sealevel$GMSL_GIA)

# Interactive Histogram
interactive_hist_sealevel <- plot_ly(x = ~sealevel$GMSL_GIA, type = "histogram")
print(interactive_hist_sealevel)

# Bar chart for a categorical variable
# Here result is that we only have one value for each number
barplot(table(sealevel$GMSL_GIA))

# Interactive Bar Chart
interactive_bar_sealevel <- plot_ly(x = names(table(sealevel$GMSL_GIA)), y = table(sealevel$GMSL_GIA), type = "bar")
print(interactive_bar_sealevel)

# Boxplot for numerical variables
boxplot(sealevel$GMSL_GIA)

# Interactive Boxplot
interactive_boxplot_sealevel <- plot_ly(y = ~sealevel$GMSL_GIA, type = "box")
print(interactive_boxplot_sealevel)

# Mean and median for a numerical variable
mean(sealevel$GMSL_GIA)
median(sealevel$GMSL_GIA)

# Standard deviation and variance
sd(sealevel$GMSL_GIA)
var(sealevel$GMSL_GIA)


#Temperature

# Summary statistics for numerical variables
summary(temperature)

#Temperature

# Histogram for a numerical variable
hist(temperature$Temperature)

# Interactive Histogram
interactive_hist_temperature <- plot_ly(x = ~temperature$Temperature, type = "histogram")
print(interactive_hist_temperature)

# Bar chart for a categorical variable
barplot(table(temperature$Temperature))

# Interactive Bar Chart
interactive_bar_temperature <- plot_ly(x = names(table(temperature$Temperature)), y = table(temperature$Temperature), type = "bar")
print(interactive_bar_temperature)

# Boxplot for numerical variables
boxplot(temperature$Temperature)

# Interactive Boxplot
interactive_boxplot_temperature <- plot_ly(y = ~temperature$Temperature, type = "box")
print(interactive_boxplot_temperature)

# Mean and median for a numerical variable
mean(temperature$Temperature)
median(temperature$Temperature)

# Standard deviation and variance
sd(temperature$Temperature)
var(temperature$Temperature)




#inter

map <- read.csv(here::here("data/map.csv"))
map <- map[c('latitude', 'longitude', 'country')]

#Let's rename the columns of the dataset
colnames(map)[colnames(map) == 'latitude'] <- 'lat'
colnames(map)[colnames(map) == 'longitude'] <- 'lng'
colnames(map)[colnames(map) == 'country'] <- 'Country'
map$Country <- toupper(map$Country)

map$Country <- ifelse(map$Country == "UNITED STATES", "USA", map$Country)
merged_map <- merge(merged_data3, map, by = "Country", all = FALSE)

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
unique_breaks <- c(-Inf, 50, 100, 500, max(merged_map$Attackscountry, na.rm = TRUE))
merged_map$cat_attacks <- cut(merged_map$Attackscountry, breaks = unique_breaks, labels = FALSE)

merged_map$echelle_taille <- merged_map$Attackscountry * 0.1

ma_carte <- leaflet(merged_map) %>%
  setView(lng = -95, lat = 37, zoom = 2) %>%
  addTiles() %>%
  addPolygons(
    data = merged_map,
    fillColor = ~colorQuantile("YlOrRd", merged_map$Attackscountry)(merged_map$Attackscountry),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste(Country, ":", Attackscountry),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    position = "bottomleft",
    colors = colorQuantile("YlOrRd", merged_map$Attackscountry)(merged_map$Attackscountry),
    labels = c("Less than 50", "50 to 100", "100 to 500", "More than 500"),
    title = "Number of shark attacks"
  )

# Customize the style CSS for the map
ma_carte$dependencies[[1]]$stylesheet <- "leaflet.css"

# Add custom style
ma_carte$dependencies[[1]]$inline <- TRUE
ma_carte$dependencies[[1]]$script <- "$('#map').css({'width': '80%', 'height': '300px', 'float': 'left'});"

# Let's see the map
ma_carte


# Assuming `merged_map` is your data frame
library(tmap)

# Merge with your data
merged_map <- left_join(merged_map, results, by = "Country")

# Create the map
shark_attacks_map <- tm_shape(merged_map) + 
  tm_fill(
    col = "cat_attacks",
    title = "Number of shark attacks",
    breaks = c(0, 50, 100, 500, Inf),
    palette = c("green", "yellow", "orange", "red")
  ) +
  tm_borders(col = "white", alpha = 0.5) +
  tm_layout(
    title = "Shark Attacks by Country",
    title.size = 1,
    legend.outside = TRUE,
    legend.outside.position = "right",
    inner.margins = 0.1,
    main.title = "Title of Your Map Here"
  )

# Display the map
tmap_mode("view")
tmap_leaflet(shark_attacks_map)
