# Data Collection OpenWeather APIs
install.packages(c('RSQLite'), repos = 'http://cran.rstudio.com',dependecies=TRUE)

install.packages("plotly")
library("tidymodels")
library("stringr")
library("plotly")
library(RSQLite)
require("httr")
library(httr)
library('glmnet')
library(tidyverse)
library(rvest)
library(rvest)
library(dplyr)
library(DBI)

# URL for Current Weather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'

# need to be replaced by your real API key
your_api_key <- "62d8a887a109f6a28e96dbe5f584e56e"
# Input `q` is the city name
# Input `appid` is your API KEY, 
# Input `units` are preferred units such as Metric or Imperial
current_query <- list(q = "Seoul", appid = your_api_key, units="metric")

# Get response 
response <- GET(current_weather_url, query=current_query)
http_type(response)
json_result <- content(response, as="parsed")
class(json_result)
json_result

# Create some empty vectors to hold data temporarily
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()

# $weather is also a list with one element, its $main element indicates the weather status such as clear or rain
weather <- c(weather, json_result$weather[[1]]$main)
# Get Visibility
visibility <- c(visibility, json_result$visibility)
# Get current temperature 
temp <- c(temp, json_result$main$temp)
# Get min temperature 
temp_min <- c(temp_min, json_result$main$temp_min)
# Get max temperature 
temp_max <- c(temp_max, json_result$main$temp_max)
# Get pressure
pressure <- c(pressure, json_result$main$pressure)
# Get humidity
humidity <- c(humidity, json_result$main$humidity)
# Get wind speed
wind_speed <- c(wind_speed, json_result$wind$speed)
# Get wind direction
wind_deg <- c(wind_deg, json_result$wind$deg)


# Combine all vectors
weather_data_frame <- data.frame(weather=weather, 
                                 visibility=visibility, 
                                 temp=temp, 
                                 temp_min=temp_min, 
                                 temp_max=temp_max, 
                                 pressure=pressure, 
                                 humidity=humidity, 
                                 wind_speed=wind_speed, 
                                 wind_deg=wind_deg)



# Check the generated data frame
print(weather_data_frame)



# TASK:  Get 5-day weather forecasts for a list of cities using the OpenWeather API

# Create some empty vectors to hold data temporarily

# City name column
city <- c()
# Weather column, rainy or cloudy, etc
weather <- c()
# Sky visibility column
visibility <- c()
# Current temperature column
temp <- c()
# Max temperature column
temp_min <- c()
# Min temperature column
temp_max <- c()
# Pressure column
pressure <- c()
# Humidity column
humidity <- c()
# Wind speed column
wind_speed <- c()
# Wind direction column
wind_deg <- c()
# Forecast timestamp
forecast_datetime <- c()
# Season column
# Note that for season, you can hard code a season value from levels Spring, Summer, Autumn, and Winter based on your current month.
season <- c()



# Get forecast data for a given city list
get_weather_forecaset_by_cities <- function(city_names){
  df <- data.frame()
  for (city_name in city_names){
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    # Create query parameters
    forecast_query <- list(q = city_name, appid = "{your_api_key}", units="metric")
    # Make HTTP GET call for the given city
    forecast_response <- GET(forecast_url, query = forecast_query)
    # Note that the 5-day forecast JSON result is a list of lists. You can print the reponse to check the results
    #results <- json_list$list
    forecast_json_list <- content(forecast_response, as = "parsed")
    results <- forecast_json_list$list
    result <- c(1:40)
    # Loop the json result
    for(result in results) {
      city <- c(city, city_name)
      
    }
    
    # Add the R Lists into a data frame
  }
  
  # Return a data frame
  return(df)
  
}


cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)

# Write cities_weather_df to `cities_weather_forecast.csv`
write.csv(cities_weather_df, "cities_weather_forecast.csv", row.names=FALSE)

# TASK: Download datasets as csv files from cloud storage
# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "raw_seoul_bike_sharing.csv")







# Data collection Web scrapping
# Check if need to install rvest` library
require("rvest")


url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
# Get the root HTML node by calling the `read_html()` method with URL
root_node <- read_html(url)
table_node <- html_nodes(root_node, "table")
table_content <- html_table(table_node, fill = TRUE)[[1]]


# Convert the bike-sharing system table into a dataframe
raw_bike_data <- as.data.frame(table_content)


# Summarize the dataframe
summary(raw_bike_data)

# Export the dataframe into a csv file
write.csv(raw_bike_data, "raw_bike_data")




##Data Wrangling with Regular Expressions</h1>

# Check whether you need to install the `tidyverse` library
require("tidyverse")
library(tidyverse)
# Download raw_bike_sharing_systems.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_bike_sharing_systems.csv"
download.file(url, destfile = "raw_bike_sharing_systems.csv")

# Download raw_cities_weather_forecast.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_cities_weather_forecast.csv"
download.file(url, destfile = "raw_cities_weather_forecast.csv")

# Download raw_worldcities.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
download.file(url, destfile = "raw_worldcities.csv")

# Download raw_seoul_bike_sharing.csv
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
download.file(url, destfile = "raw_seoul_bike_sharing.csv")



# List dataset
dataset_list <- c('raw_bike_sharing_systems.csv', 'raw_seoul_bike_sharing.csv', 'raw_cities_weather_forecast.csv', 'raw_worldcities.csv')


# for loop to convert names
for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read.csv(dataset_name)
  # Standardized its columns:
  
  # Convert all column names to uppercase
  colnames(dataset) <- toupper(colnames(dataset))
  # Replace any white space separators by underscores, using the str_replace_all function
  colnames(dataset) <- str_replace_all(colnames(dataset), " ", "_")
  # Save the dataset 
  write.csv(dataset, dataset_name, row.names=FALSE)
}

# read the result dataset 

for (dataset_name in dataset_list){
  # Print a summary for each data set to check whether the column names were correctly converted
  dataset <- read.csv(dataset_name)
  print(colnames(dataset))
}

#Process the web-scraped bike sharing system dataset 
# First load the dataset
bike_sharing_df <- read.csv("raw_bike_sharing_systems.csv")
# Print its head
head(bike_sharing_df)

#In this project, let's only focus on processing the following revelant columns (feel free to process the other columns for more practice):

#COUNTRY: Country name
#CITY: City name
#SYSTEM: Bike-sharing system name
#BICYCLES: Total number of bikes in the system
# Select the four columns
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)
sapply(sub_bike_sharing_df, typeof)
sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)
# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) {
  grepl("[^0-9]", strings)
}
#Let's try to find any elements in the `Bicycles` column containing non-numeric characters.
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)
# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)
# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)
#Hmm, looks like the `CITY` column has some reference links to be removed. Next, let's check the `SYSTEM` column.
# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)
# remove reference link
remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-z0-9]+\\]"
  result <- str_replace_all(strings, ref_pattern, " ")
  return(result)
  # Replace all matched substrings with a white space using str_replace_all()
  # Trim the reslt if you want
  # return(result)
}
# sub_bike_sharing_df %>% mutate(column1=remove_ref(column1), ... )
result <- sub_bike_sharing_df %>% 
  select(CITY, SYSTEM, BICYCLES, COUNTRY) %>%
  mutate(CITY = remove_ref(CITY)) %>%
  mutate(SYSTEM = remove_ref(SYSTEM)) %>%
  mutate(BICYCLES = remove_ref(BICYCLES))
result
result %>% 
  select(CITY, SYSTEM, BICYCLES) %>% 
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))
# TASK: Extract the numeric value using regular expressions

# Extract the first number
extract_num <- function(columns){
  # Define a digital pattern
  digitals_pattern <- "Define a pattern matching a digital substring"
  # Find the first match using str_extract
  # Convert the result to numeric using the as.numeric() function
}
# Extract the first number
extract_num <- function(columns){
  # Define a digital pattern
  digitals_pattern <- "[^0-9]"
  str_extract(columns, digitals_pattern)
  columns <- as.numeric(columns)
  # Find the first match using str_extract
  # Convert the result to numeric using the as.numeric() function
}
# Use the mutate() function on the BICYCLES column
result <- sub_bike_sharing_df %>% 
  select(CITY, SYSTEM, BICYCLES, COUNTRY) %>%
  mutate(CITY = remove_ref(CITY)) %>%
  mutate(SYSTEM = remove_ref(SYSTEM)) %>%
  mutate(BICYCLES = extract_num(BICYCLES))
result <- result[, -1]
summary(result$BICYCLES)
# Write dataset to `bike_sharing_systems.csv`
write.csv(result, "bike_sharing_systems.csv")





# Check if you need to install the `tidyverse` library

bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv", col_name =TRUE, cols(
  DATE = col_character(),
  RENTED_BIKE_COUNT = col_double(),
  HOUR = col_double(),
  TEMPERATURE = col_double(),
  HUMIDITY = col_double(),
  WIND_SPEED = col_double(),
  VISIBILITY = col_double(),
  DEW_POINT_TEMPERATURE = col_double(),
  SOLAR_RADIATION = col_double(),
  RAINFALL = col_double(),
  SNOWFALL = col_double(),
  SEASONS = col_character(),
  HOLIDAY = col_character(),
  FUNCTIONING_DAY = col_character()
))
# Or you may read it from he)

summary(bike_sharing_df)
dim(bike_sharing_df)
# Drop rows with `RENTED_BIKE_COUNT` column == NA
bike_sharing_df <- bike_sharing_df %>% drop_na(RENTED_BIKE_COUNT)
# Print the dataset dimension again after those rows are dropped
summary(bike_sharing_df)
dim(bike_sharing_df)
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))

# Calculate the summer average temperature
avg_temp <- bike_sharing_df %>%
  filter(SEASONS == "Summer") %>%
  summarise(avg_temp = mean(TEMPERATURE, na.rm = TRUE)) %>%
  pull(avg_temp)  # Extract the numeric value from the avg_tem object

# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df <- bike_sharing_df %>% 
  mutate(TEMPERATURE = ifelse(is.na(TEMPERATURE), avg_temp, TEMPERATURE))

head(bike_sharing_df)
head(bike_sharing_df, 10)
# Print the summary of the dataset again to make sure no missing values in all columns
summary(bike_sharing_df)
# Save the dataset as `seoul_bike_sharing.csv`
write_csv(bike_sharing_df, "seoul_bike_sharing.csv")


#TASK: Create indicator (dummy) variables for categorical variables
# Using mutate() function to convert HOUR column into character type
bike_sharing_df %>%
  select(HOUR) %>%
  mutate_if(is.numeric, as.character) %>%
  slice(1:5)

bike_sharing_df <- bike_sharing_df %>%
  mutate(quantile_rank = ntile(bike_sharing_df$HOUR, 4))

col <- c("SEASONS", "HOLIDAY", "quantile_rank")

feature <- function(x) {
  for (x in col) {
    bike_sharing_df <<- bike_sharing_df %>%
      mutate(dummy = 1) %>%
      spread(key = x, value = dummy, fill = 0)
  }
}
feature()
# Print the dataset summary again to make sure the indicator columns are created properly
summary(bike_sharing_df)
# Save the dataset as `seoul_bike_sharing_converted.csv`3

write_csv(bike_sharing_df, "seoul_bike_sharing_converted.csv")

# write_csv(dataframe, "seoul_bike_sharing_converted.csv")

# Normalize data
# Use the `mutate()` function to apply min-max normalization on columns 
# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`

bike_sharing_df <- bike_sharing_df %>%
  mutate(RENTED_BIKE_COUNT = (RENTED_BIKE_COUNT- min(RENTED_BIKE_COUNT))/ (max(RENTED_BIKE_COUNT- min(RENTED_BIKE_COUNT))))

head(bike_sharing_df)

# Print the summary of the dataset again to make sure the numeric columns range between 0 and 1
summary(bike_sharing_df)
# Save the dataset as `seoul_bike_sharing_converted_normalized.csv`
write_csv(bike_sharing_df, "seoul_bike_sharing_converted_normalized.csv")

# write_csv(dataframe, "seoul_bike_sharing_converted_normalized.csv")



# Standardize the column names again for the new datasetsDataset list
#Data List
dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}


# Define database connection
con <- dbConnect(RSQLite::SQLite(), "RDB.sqlite")

# Create tables if they do not exist
if (!dbExistsTable(con, "WORLD_CITIES")) {
  dbExecute(con, "
           CREATE TABLE WORLD_CITIES (
             CITY VARCHAR(50),
             CITY_ASCII VARCHAR(50),
             LAT DECIMAL(20,2),
             LNG DECIMAL(20,2),
             COUNTRY VARCHAR(50),
             ISO2 VARCHAR(5),
             ISO3 VARCHAR(5),
             ADMIN_NAME VARCHAR(100), 
             CAPITAL VARCHAR(50),
             POPULATION BIGINT,
             ID BIGINT NOT NULL
           );
           ")
}

if (!dbExistsTable(con, "BIKE_SHARING_SYSTEMS")) {
  dbExecute(con, "
           CREATE TABLE BIKE_SHARING_SYSTEMS (
             COUNTRY VARCHAR(20),
             CITY VARCHAR(87),
             SYSTEM VARCHAR(40),
             BICYCLES NUMERIC
           );
           ")
}

if (!dbExistsTable(con, "CITIES_WEATHER_FORECAST")) {
  dbExecute(con, "
           CREATE TABLE CITIES_WEATHER_FORECAST (
             CITY VARCHAR(16),
             WEATHER VARCHAR(6),
             VISIBILITY SMALLINT,
             TEMP DECIMAL(6,2),
             TEMP_MIN DECIMAL(6,2),
             TEMP_MAX DECIMAL(6,2),
             PRESSURE SMALLINT,
             HUMIDITY SMALLINT,
             WIND_SPEED DECIMAL(6,2),
             WIND_DEG SMALLINT,
             SEASON VARCHAR(6),
             FORECAST_DATETIME TIMESTAMP
           );
           ")
}

if (!dbExistsTable(con, "SEOUL_BIKE_SHARING")) {
  dbExecute(con, "
           CREATE TABLE SEOUL_BIKE_SHARING (
             DATE VARCHAR(30),
             RENTED_BIKE_COUNT SMALLINT,
             HOUR SMALLINT,
             TEMPERATURE DECIMAL(4,1),
             HUMIDITY SMALLINT,
             WIND_SPEED DECIMAL(3,1),
             VISIBILITY SMALLINT,
             DEW_POINT_TEMPERATURE DECIMAL(4,1),
             SOLAR_RADIATION DECIMAL(5,2),
             RAINFALL DECIMAL(3,1),
             SNOWFALL DECIMAL(3,1),
             SEASONS VARCHAR(10),
             HOLIDAY VARCHAR(20),
             FUNCTIONING_DAY VARCHAR(5)
           );
           ")
}

# Load data into tables
worldcities <- read.csv("raw_worldcities.csv")
bike_sharing_systems <- read.csv("raw_bike_sharing_systems.csv")
cities_weather_forecast <- read.csv("raw_cities_weather_forecast.csv")
seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")

dbWriteTable(con, "WORLD_CITIES", worldcities, overwrite = TRUE)
dbWriteTable(con, "bike_sharing_systems", bike_sharing_systems, overwrite = TRUE)
dbWriteTable(con, "CITIES_WEATHER_FORECAST", cities_weather_forecast, overwrite = TRUE)
dbWriteTable(con, "SEOUL_BIKE_SHARING", seoul_bike_sharing, overwrite = TRUE)


view(bike_sharing_systems)

# Close connection
dbDisconnect(con)

view(bike_sharing_systems)
library("RSQLite")
library(tidyverse)

# Connect to SQLite database
con <- dbConnect(RSQLite::SQLite(), "yzf09619.sqlite")
# Task 1 - Record Count
dbGetQuery(con, "SELECT count(*) as Count_of_Records FROM seoul_bike_sharing")

# Task 2 - Operational Hours
dbGetQuery(con, "SELECT count(HOUR) as Numer_of_hours FROM seoul_bike_sharing WHERE RENTED_BIKE_COUNT > 0")

# Task 3
dbGetQuery(con, "SELECT * FROM CITIES_WEATHER_FORECAST WHERE CITY = 'Seoul' LIMIT 1")

# Task 4
dbGetQuery(con, "SELECT DISTINCT(SEASONS) FROM seoul_bike_sharing")

# Task 5
dbGetQuery(con, "SELECT MIN(DATE) as Start_Date, MAX(DATE) as End_Date FROM seoul_bike_sharing")

# Task 6
dbGetQuery(con, "SELECT DATE, HOUR, RENTED_BIKE_COUNT as Maximum_COUNT FROM seoul_bike_sharing WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) FROM seoul_bike_sharing)")

# Task 7
dbGetQuery(con, "SELECT SEASONS, HOUR, AVG(RENTED_BIKE_COUNT), AVG(TEMPERATURE) FROM seoul_bike_sharing GROUP BY SEASONS, HOUR ORDER BY AVG(RENTED_BIKE_COUNT) DESC LIMIT 10")

# Task 8
dbGetQuery(con, "SELECT SEASONS, AVG(RENTED_BIKE_COUNT) as AVG_S_COUNT, MIN(RENTED_BIKE_COUNT) as MIN_S_COUNT, MAX(RENTED_BIKE_COUNT) as MAX_S_COUNT FROM seoul_bike_sharing GROUP BY SEASONS ORDER BY AVG_S_COUNT DESC")

# Task 9
dbGetQuery(con, "SELECT SEASONS, AVG(RENTED_BIKE_COUNT) as AVG_S_COUNT, AVG(TEMPERATURE) as AVG_S_TEMP, AVG(HUMIDITY) as AVG_S_HUMIDITY, AVG(WIND_SPEED) as AVG_WIND_SPEED, AVG(VISIBILITY) as AVG_VISIBILITY, AVG(DEW_POINT_TEMPERATURE) as AVG_DEW_POINT, AVG(SOLAR_RADIATION) as AVG_SOLAR_RADIATION, AVG(RAINFALL) as AVG_RAINFALL, AVG(SNOWFALL) as AVG_SNOWFALL FROM seoul_bike_sharing GROUP BY SEASONS ORDER BY AVG_S_COUNT DESC")

# Task 10
dbGetQuery(con, "SELECT B.BICYCLES, B.CITY, B.COUNTRY, W.LAT, W.LNG, W.POPULATION FROM bike_sharing_systems AS B LEFT JOIN WORLD_CITIES AS W ON B.CITY = W.CITY_ASCII WHERE B.CITY = 'Seoul'")

# Task 11
dbGetQuery(con, "SELECT B.BICYCLES, B.CITY, B.COUNTRY, W.LAT, W.LNG, W.POPULATION FROM BIKE_SHARING_SYSTEMS AS B LEFT JOIN WORLD_CITIES AS W ON B.CITY = W.CITY_ASCII WHERE B.CITY = 'Seoul' OR (B.BICYCLES BETWEEN 15000 AND 20000) ORDER BY B.BICYCLES DESC")

# Close database connection
dbDisconnect(con)

# Assignment: Exploratory Data Analysis with tidyverse and ggplot2
# provide your solution here
seoul_bike_sharing <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing.csv"
file <- download.file(seoul_bike_sharing, destfile = "seoul_bike_sharing.csv" )

seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")



# Assignment: Exploratory Data Analysis with tidyverse and ggplot2


# Download the dataset and read it into R
seoul_bike_sharing_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing.csv"
download.file(seoul_bike_sharing_url, destfile = "seoul_bike_sharing.csv")
seoul_bike_sharing <- read.csv("seoul_bike_sharing.csv")

# Task 2: Recast DATE as a date
seoul_bike_sharing$DATE <- as.Date(seoul_bike_sharing$DATE, format = "%d/%m/%Y")

# Task 3: Cast HOURS as a categorical variable
seoul_bike_sharing$HOUR <- as.factor(seoul_bike_sharing$HOUR)

# Task 4: Dataset Summary
summary(seoul_bike_sharing)

# Task 5: Calculate how many Holidays there are
holidays_count <- seoul_bike_sharing %>%
  filter(HOLIDAY == "Holiday") %>%
  nrow()
holidays_count

# Task 6: Calculate the percentage of records that fall on a holiday
total_records <- nrow(seoul_bike_sharing)
holidays_count_percentage <- (holidays_count / total_records) * 100
holidays_count_percentage

# Task 7: Determine how many records we expect to have
total_records

# Task 8: Determine the number of records based on 'FUNCTIONING_DAY'
seoul_bike_sharing %>%
  count(FUNCTIONING_DAY)

# Task 9: Calculate the seasonal total rainfall and snowfall
seoul_bike_sharing %>%
  group_by(SEASONS) %>%
  summarize(total_rainfall = sum(RAINFALL, na.rm = TRUE),
            total_snowfall = sum(SNOWFALL, na.rm = TRUE))






#Load the ggplot2 package so we can generate some data visualizations.
# provide your solution here
install.packages("ggplot2")
library(ggplot2)
#Create a scatter plot of RENTED_BIKE_COUNT vs DATE
# provide your solution here
ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT, alpha=0.5, color = I("orange"))) +
  geom_point() +
  ggtitle("Rented Bike Count According to Date") +
  theme(plot.title = element_text(face = "bold", color = "green"))
#reate the same plot of the RENTED_BIKE_COUNT time series,
#but now add HOURS as the colour
# provide your solution here
ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT, alpha=0.5, color = HOUR)) +
  geom_point() +
  ggtitle("Rented Bike Count According to Date") +
  theme(plot.title = element_text(face = "bold", color = "purple"))
#Histogram Rented bike density
# provide your solution here
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT, colour = I("green"), fill = I("white"))) +
  geom_histogram(aes(y = ..density..)
  ) +
  geom_density(aes(color = I("pink"), fill = I("Maroon"))) +
  ggtitle("Rented Bike Density") +
  theme(plot.title = element_text(face = "bold", color = "green"),
        legend.position = "none")
# Also we can use with alpha
# provide your solution here
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT), color = I("black"), fill = I("white")) +
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white", alpha = 0.5
                 
  ) +
  
  geom_density(aes(color = I("blue"))) +
  ggtitle("Rented Bike Density") +
  theme(plot.title = element_text(face = "bold", color = "black"),
        legend.position = "none")
#Correlation between two variables (scatter plot)
# provide your solution here
ggplot(seoul_bike_sharing, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT, colour=HOUR)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  ggtitle("Correlation between Rented Bike count and Temp.") +
  theme(plot.title = element_text(face = "bold", color = "black"))
# classify per season
ggplot(seoul_bike_sharing, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT, color = HOUR, alpha = 0.25)) +
  geom_point() +
  facet_wrap(~SEASONS) +
  geom_smooth(method = "lm", color = "brown") 
#Create boxplot
install.packages("plotly")
library(plotly)
# provide your solution here
ggplot(seoul_bike_sharing, aes(x= HOUR, y = RENTED_BIKE_COUNT)) +
  facet_wrap(~SEASONS) +
  geom_boxplot(fill = "bisque", alpha = 0.3)+
  theme_linedraw()+
  ggtitle(" Rented Bike count Per Hour Summary") +
  guides(color = FALSE) +
  geom_jitter(aes(color = I('purple')), alpha=0.05) +
  theme_linedraw()+
  theme(plot.title = element_text(face = "bold", color = "black"),
        plot.background = element_rect(color = "red"),
        axis.line = element_line(color = "red")
  )
# Rainfall and date scatter plot
seoul_bike_sharing %>%
  group_by(DATE) %>%
  summarise(daily_rainfall = sum(RAINFALL), daily_snowfall = sum(SNOWFALL)) %>%
  ggplot(aes(x = DATE)) +
  geom_point(aes(y = daily_rainfall, color = "RAINFALL")) +
  geom_point(aes(y = daily_snowfall, color = "SNOWFALL")) +
  theme_classic()+
  labs(y = "Daily Rainfall & snowfall") +
  ggtitle("Daily rainfall And snowfall According to Date:") +
  theme(plot.title = element_text(face = "bold", color = "brown"),
        axis.line = element_line(color = "red"))





# Uncomment to install packages if running locally
# install.packages("tidyverse")
# install.packages("tidymodels")
install.packages("plotly")

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)

# Print structure of dataframe
str(bike_sharing_df)

# Remove DATE and FUNCTIONING_DAY columns
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

# Use the `initial_split()`, `training()`, and `testing()` functions to split the dataset
# With seed 1234
set.seed(1234)
# prop = 3/4
bike_sharing_split <- initial_split(bike_sharing_df, prop = 0.75)

# train_data 
bike_sharing_training <- training(bike_sharing_split)

# test_data
bike_sharing_testing <- testing(bike_sharing_split)

# Use `linear_reg()` with engine `lm` and mode `regression`
lm_model_weather <- linear_reg(mode = "regression") %>%
  set_engine(engine = "lm")
summary(lm_model_weather)
training_fit <-lm_model_weather %>%
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL, data = bike_sharing_training)

# print(lm_model_weather$fit)
print(training_fit)

# build linear regression using all model
lm <- linear_reg(mode = "regression") %>%
  set_engine(engine = "lm")

# Fit the model called `lm_model_all`
lm_model_all <- lm %>%
  fit(RENTED_BIKE_COUNT ~ ., data = bike_sharing_training)
# `RENTED_BIKE_COUNT ~ .` means use all other variables except for the response variable

# summary(lm_model_all$fit)
summary(lm_model_all)

print(lm_model_all)


# Evalution Rmse R-squred
training_results <- 
  predict(training_fit, new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

head(training_results)

all_training_results  <- predict(lm_model_all, new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)
head(all_training_results)


# Use predict() function to generate test results for `lm_model_weather` and `lm_model_all`
# and generate two test results dataframe with a truth column:

test_results <- 
  predict(training_fit, new_data = bike_sharing_testing) %>%
  mutate(truth = bike_sharing_testing$RENTED_BIKE_COUNT)

head(test_results)

all_testing_results  <- predict(lm_model_all, new_data = bike_sharing_testing) %>%
  mutate(truth = bike_sharing_testing$RENTED_BIKE_COUNT)

head(all_testing_results)

# rsq_weather <- rsq(...)
# rsq_all <- rsq(...)
rsq_weather <- rsq(test_results,
                   truth = truth,
                   estimate = .pred
)
rsq_all <- rsq(all_testing_results,
               truth = truth,
               estimate = .pred
)

rmse_weather <- rmse(test_results,
                     truth = truth,
                     estimate = .pred
)
rmse_all <- rmse(all_testing_results,
                 truth = truth,
                 estimate = .pred
)
# rmse_weather <- rmse(...)
# rmse_all <- rmse(...)


rsq_weather
rsq_all
rmse_weather
rmse_all

lm_model_all$fit$coefficients

# Sort coefficient list
lm_model_all %>%
  tidy() %>%
  arrange(desc(abs(estimate)))

# Visualize the list using ggplot and geom_bar
lm_model_all %>%
  tidy() %>%
  filter(!is.na(estimate)) %>%
  ggplot(aes(x = fct_reorder(term, abs(estimate)), y = abs(estimate))) +
  geom_bar(stat = "identity", fill = "BLUE") +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 10, colour = "RED", size = 7)) +
  xlab("variable") +
  ggtitle("Ranked coefficients")


test_results %>%
  mutate(train = "testing") %>%
  bind_rows(training_results %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", size = 1.5) +
  geom_point(color = "#006EA1", alpha = 0.5) +
  facet_wrap(~train) +
  labs(x= "Truth", y = "Predict Count Of Rented Bike") +
  ggtitle("Evaluate of Model 1") +
  theme (plot.title = element_text(face = "bold", color = "green"))

all_testing_results %>%
  mutate(train = "testing") %>%
  bind_rows(all_training_results %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", size = 1.5) +
  geom_point(color = "#002ED1", alpha = 0.2) +
  facet_wrap(~train) +
  labs(x= "Truth", y = "Predict Count Of Rented Bike") +
  ggtitle("Evaluate of Model 2") +
  theme (plot.title = element_text(face = "bold", color = "orange"))



# Uncomment to install packages if running locally
# install.packages("tidyverse")
# install.packages("tidymodels")
install.packages("plotly")

# Uncomment to install packages if running locally


# Load the dataset
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)

# Remove unnecessary columns
bike_sharing_df <- bike_sharing_df %>%
  select(-DATE, -FUNCTIONING_DAY)

# Define a linear regression model specification
lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

# Split the data into training and testing datasets
set.seed(1234)
bike_sharing_split <- initial_split(bike_sharing_df, prop = 4/5)
bike_sharing_training <- training(bike_sharing_split)
bike_sharing_testing <- testing(bike_sharing_split)

# Add polynomial terms
# Plot the higher order polynomial fits
ggplot(data = bike_sharing_training, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color = "red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color = "blue")

# Fit a linear model with higher order polynomial on some important variables 
lmp <- linear_reg(mode = "regression") %>%
  set_engine(engine = "lm")

# Use poly function to build polynomial terms
lm_poly <- lmp %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + poly(HUMIDITY, 4) + poly(RAINFALL, 2), data = bike_sharing_training)






# Make predictions on test dataset using the lm_poly models
test_result_poly <- predict(lm_poly, new_data = bike_sharing_testing) %>%
  mutate(truth = bike_sharing_testing$RENTED_BIKE_COUNT)

# Replace negative prediction results with zero
test_result_poly[test_result_poly < 0] <- 0

# Calculate R-squared and RMSE for the test results generated by lm_poly model
rsq_lm_poly <- rsq(test_result_poly, truth = truth, estimate = .pred)
rmse_lm_poly <- rmse(test_result_poly, truth = truth, estimate = .pred)

# Add interaction terms
# Add interaction terms to the poly regression built in previous step
lm_poly_interact <- lmp %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + poly(HUMIDITY, 4) + poly(RAINFALL, 2) + HUMIDITY * TEMPERATURE, data = bike_sharing_training)

test_result_poly_interact <- predict(lm_poly_interact, new_data = bike_sharing_testing) %>%
  mutate(truth = bike_sharing_testing$RENTED_BIKE_COUNT)
test_result_poly_interact[test_result_poly < 0] <- 0

# Calculate R-squared and RMSE for the new model to see if performance has improved
rsq_lm_poly_interact <- rsq(test_result_poly_interact, truth = truth, estimate = .pred)
rmse_lm_poly_interact <- rmse(test_result_poly_interact, truth = truth, estimate = .pred)

# Add regularization
# Define a linear regression model specification glmnet_spec using glmnet engine

glmnet_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

# Fit a glmnet model called lm_glmnet using the fit() function
glmnet_recipe <- recipe(RENTED_BIKE_COUNT ~ . , data = bike_sharing_training)
lasso_wf <- workflow() %>%
  add_recipe(glmnet_recipe)

lm_glmnet <- lasso_wf %>%
  add_model(glmnet_spec) %>%
  fit(data = bike_sharing_training)

# Report rsq and rmse of the lm_glmnet model
lm_glmnet_train_results <- lm_glmnet %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

lm_glmnet_train_results$.pred <- replace(lm_glmnet_train_results$.pred, lm_glmnet_train_results$.pred < 0, 0)

rsq_lm_glmnet <- rsq(lm_glmnet_train_results, truth = truth, estimate = .pred)
rmse_lm_glmnet <- rmse(lm_glmnet_train_results, truth = truth, estimate = .pred)

# Experiment to search for improved models
# Build at least five different models using polynomial terms, interaction terms, and regularizations.
model2 <- linear_reg(penalty = 0.02, mixture = 1) %>%
  set_engine("glmnet")
model2_fit <- model2 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SUMMER * `18` + HUMIDITY, data = bike_sharing_training)

model2_train_results <- model2_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model2$.pred <- replace(model2_train_results$.pred, model2_train_results$.pred < 0, 0)

rsq_model2 <- rsq(model2_train_results, truth = truth, estimate = .pred)
rmse_model2 <- rmse(model2_train_results, truth = truth, estimate = .pred)

model3 <- linear_reg(penalty = 0.02, mixture = 0.2) %>%
  set_engine("glmnet")
model3_fit <- model3 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + SOLAR_RADIATION + SUMMER * `18` + TEMPERATURE * HUMIDITY, data = bike_sharing_training)

model3_train_results <- model3_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model3$.pred <- replace(model3_train_results$.pred, model3_train_results$.pred < 0, 0)

rsq_model3 <- rsq(model3_train_results, truth = truth, estimate = .pred)
rmse_model3 <- rmse(model3_train_results, truth = truth, estimate = .pred)

model4 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>%
  set_engine("glmnet")
model4_fit <- model4 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + SUMMER * `18` + TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6), data = bike_sharing_training)

model4_train_results <- model4_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model4$.pred <- replace(model4_train_results$.pred, model4_train_results$.pred < 0, 0)

rsq_model4 <- rsq(model4_train_results, truth = truth, estimate = .pred)
rmse_model4 <- rmse(model4_train_results, truth = truth, estimate = .pred)

model5 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>%
  set_engine("glmnet")
model5_fit <- model5 %>%
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + poly(VISIBILITY, 6) + SUMMER * `18` + TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6) + RAINFALL * TEMPERATURE + SNOWFALL * TEMPERATURE + RAINFALL * HUMIDITY + SNOWFALL * HUMIDITY, data = bike_sharing_training)

model5_train_results <- model5_fit %>%
  predict(new_data = bike_sharing_training) %>%
  mutate(truth = bike_sharing_training$RENTED_BIKE_COUNT)

model5_train_results$.pred <- replace(model5_train_results$.pred, model5_train_results$.pred < 0, 0)

rsq_model5 <- rsq(model5_train_results, truth = truth, estimate = .pred)
rmse_model5 <- rmse(model5_train_results, truth = truth, estimate = .pred)

# Save their rmse and rsq values
model_names <- c("lm_ploy", "model2", "model3", "model4", "model5")
rsq <- c("0.7295964", "0.7137937", "0.7392157", "0.7674439", "0.7817489")
rsme <- c("330.1608", "344.91", "329.24", "310.9102", "302.1648")
comparison_df <- data.frame(model_names, rsq, rsme)

# Report the best performed model in terms of rmse and rsq
rsq_lm_ploy <- 0.7295964
rmse_lm_ploy <- 330.1608

rsq_model2 <- 0.7137937
rmse_model2 <- 344.91

rsq_model3 <- 0.7392157
rmse_model3 <- 329.24

rsq_model4 <- 0.7674439
rmse_model4 <- 310.9102

rsq_model5 <- 0.7817489
rmse_model5 <- 302.1648

# Visualize the saved RMSE and R-squared values using a grouped barchart
comparison_df %>%
  pivot_longer(!model_names) %>%
  ggplot(aes(x = model_names, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparing RSME AND RSQ Across Models", fill = "Metric") +
  theme(plot.title = element_text(face = "bold", color = "blue"))

# Create a Q-Q plot by plotting the distribution difference between the predictions generated by your best model and the true values on the test dataset
ggplot(model5_train_results) +
  stat_qq(aes(sample = truth), color = "green") +
  stat_qq(aes(sample = .pred), color = "red")




























