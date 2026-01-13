
# Step 1: Load packages

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Step 2: Get London monitoring stations from OpenAQ API

url_locations <- paste0(
  "https://api.openaq.org/v3/locations?",
  "coordinates=51.5074,-0.1278",
  "&radius=25000",
  "&iso=GB",
  "&parameters_id=2",
  "&limit=200"
)

res_loc <- GET(
  url_locations, 
  add_headers("X-API-Key" = "YOUR_OPENAQ_API_KEY")
)
stop_for_status(res_loc)

loc_json <- fromJSON(content(res_loc, "text", encoding = "UTF-8"))

locs <- data.frame(
  location_id = loc_json$results$id,
  name = loc_json$results$name,
  locality = loc_json$results$locality,
  distance = loc_json$results$distance
)

head(locs)
View(locs)

# Step 3: Select 4 monitoring stations

stations <- data.frame(
  location_id = c(159, 141, 225801, 225797),
  station = c("Westminster", "Harlington", "Elizabeth Bridge", "Westhorne Avenue")
)


# Step 4: Fetch daily PM2.5 data

aq_all <- data.frame()

for (i in 1:4) { 
  loc_id <- stations$location_id[i]
  station_name <- stations$station[i]
  
  # Get station info
  url_loc <- paste0(
    "https://api.openaq.org/v3/locations/", 
    loc_id
  )
  
  res_loc <- GET(
    url_loc, 
    add_headers("X-API-Key" = "YOUR_OPENAQ_API_KEY")
  )
  stop_for_status(res_loc)
  
  loc_json <- fromJSON(content(res_loc, "text", encoding = "UTF-8"))
  
  # Find PM2.5 sensor ID
  sensors_df <- bind_rows(loc_json$results$sensors)
  pm25_sensor_id <- sensors_df$id[sensors_df$parameter$name == "pm25"][1]
  
  # Get daily PM2.5 data
  url_days <- paste0(
    "https://api.openaq.org/v3/sensors/", pm25_sensor_id, "/days?",
    "date_from=2025-01-01",
    "&date_to=2025-07-01",
    "&limit=1000"
  )
  
  res_days <- GET(
    url_days, 
    add_headers("X-API-Key" = "YOUR_OPENAQ_API_KEY")
  )
  stop_for_status(res_days)
  
  days_json <- fromJSON(content(res_days, "text", encoding = "UTF-8"))
  
  # Create dataframe
  station_data <- data.frame(
    date = as.Date(substr(days_json$results$period$datetimeFrom$utc, 1, 10)),
    pm25 = days_json$results$value,
    station = station_name
  )
  
  aq_all <- rbind(aq_all, station_data)
}


# Step 5: Check air quality data

head(aq_all)
summary(aq_all$pm25)
table(aq_all$station)

# Check data completeness by station
aq_all %>%
  group_by(station) %>%
  summarise(
    n_days = n(),
    n_missing = sum(is.na(pm25)),
    min_date = min(date),
    max_date = max(date)
  )


# Step 6: Calculate city-level PM2.5 (median)

aq_city <- aq_all %>%
  group_by(date) %>%
  summarise(
    pm25_median = median(pm25, na.rm = TRUE),
    n_stations = n()
  )


# Step 7: Define high pollution days 

threshold <- quantile(aq_city$pm25_median, 0.75, na.rm = TRUE)

aq_city <- aq_city %>%
  mutate(high_pollution = ifelse(pm25_median >= threshold, 1, 0))

table(aq_city$high_pollution)


# Step 8: Get weather data from Open-Meteo

url_weather <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=51.5074",
  "&longitude=-0.1278",
  "&start_date=2025-01-01",
  "&end_date=2025-07-01",
  "&daily=temperature_2m_mean,wind_speed_10m_mean,precipitation_sum",
  "&timezone=GMT"
)

res_w <- GET(url_weather)
stop_for_status(res_w)

w_json <- fromJSON(content(res_w, "text", encoding = "UTF-8"))

weather_daily <- data.frame(
  date = as.Date(w_json$daily$time),
  temp_mean = w_json$daily$temperature_2m_mean,
  wind_mean = w_json$daily$wind_speed_10m_mean,
  rain_sum = w_json$daily$precipitation_sum
)


# Step 9: Merge air quality and weather data

df_model <- aq_city %>%
  left_join(weather_daily, by = "date")

summary(df_model)


# Step 10: Data visualization

ggplot(df_model, aes(x = date, y = pm25_median)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Date",
    y = "PM2.5 (µg/m³)",
    title = "Daily PM2.5 in London (January - July 2025)"
  ) +
  theme_minimal()

ggplot(df_model, aes(x = factor(high_pollution), y = pm25_median)) +
  geom_boxplot() +
  labs(
    x = "High Pollution Day (0 = No, 1 = Yes)",
    y = "PM2.5 (µg/m³)",
    title = "PM2.5 Distribution by Pollution Level"
  ) +
  theme_minimal()

# Step 11: Build logistic regression model

set.seed(123)

# Remove missing values and split data (70/30)
df_use <- df_model %>%
  filter(!is.na(temp_mean), !is.na(wind_mean), !is.na(rain_sum))

train_idx <- sample(1:nrow(df_use), size = floor(0.7 * nrow(df_use)))
train <- df_use[train_idx, ]
test <- df_use[-train_idx, ]

# Train model
model <- glm(high_pollution ~ temp_mean + wind_mean + rain_sum,
             family = binomial,
             data = train)

summary(model)

# Step 12: Model evaluation

# Predict and evaluate
test$pred <- ifelse(predict(model, test, type = "response") >= 0.5, 1, 0)

# Confusion Matrix & Accuracy
print(table(Predicted = test$pred, Actual = test$high_pollution))
