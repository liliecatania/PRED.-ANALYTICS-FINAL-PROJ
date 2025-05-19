###Load libraries 
library(rpart.plot)
library(dplyr)
library(readr)
library(forcats)
library(ggplot2)
library(scales)
library(sysfonts)
library(showtext)
library(jsonlite)
library(curl)

###Load the data 
flights_train<- read_csv("/Users/svc-olin130lounge/Downloads/flights_train.csv")
flights_test<- read_csv("/Users/svc-olin130lounge/Downloads/flights_test.csv")
flights_val<- read_csv("/Users/svc-olin130lounge/Downloads/flights_val.csv")
flights_raw<- read_csv("/Users/svc-olin130lounge/Downloads/flights_raw.csv")

###Defining custom theme for visuals 
theme_airplane <- function(base_size = 14, base_family = "poppins",
                                 title_color = "#D9542C", axis_color = "#333333",
                                 grid_color = "grey85", axis_line_color = "#999999") {
  theme_minimal(base_family = base_family, base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 6, color = title_color),
      axis.title.x = element_text(size = base_size + 2, margin = margin(t = 10)),
      axis.title.y = element_text(size = base_size + 2, margin = margin(r = 10)),
      axis.text = element_text(size = base_size - 1, color = axis_color),
      panel.grid.major.y = element_line(color = grid_color),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(color = axis_line_color),
      plot.margin = margin(10, 20, 10, 20)
    )
}

###Convert target variable to factor
flights_raw$DEPARTURE_DELAY <- as.factor(flights_raw$DEPARTURE_DELAY)

###One-hot encode scheduled departure time into time-of-day bins
bin_departure_time <- function(df) {
  df %>%
    mutate(
      dep_hour = as.numeric(SCHEDULED_DEPARTURE) %/% 100,
      SCHEDULED_GRAVEYARD_DEPARTURE = ifelse(dep_hour >= 0  & dep_hour < 6, 1, 0),
      SCHEDULED_MORNING_DEPARTURE   = ifelse(dep_hour >= 6  & dep_hour < 12, 1, 0),
      SCHEDULED_AFTERNOON_DEPARTURE = ifelse(dep_hour >= 12 & dep_hour < 18, 1, 0),
      SCHEDULED_NIGHT_DEPARTURE     = ifelse(dep_hour >= 18 & dep_hour < 24, 1, 0)
    ) %>%
    select(-dep_hour)
}

###Apply one-hot encoding function
flights_raw <- bin_departure_time(flights_raw)

###Load font and define brand colors
font_add_google("Poppins", "poppins")
showtext_auto()
main_blue <- "#3366CC"
main_orange <- "#D9542C"

###Create binary delay indicator (1 = delayed, 0 = on time)
flights_raw <- flights_raw %>%
  mutate(DELAYED = ifelse(DEPARTURE_TIME > SCHEDULED_DEPARTURE, 1, 0))

###Summarize number of delayed flights by time of day
time_of_day_delays <- flights_raw %>%
  filter(DELAYED == 1) %>%
  summarise(
    Graveyard = sum(SCHEDULED_GRAVEYARD_DEPARTURE),
    Morning   = sum(SCHEDULED_MORNING_DEPARTURE),
    Afternoon = sum(SCHEDULED_AFTERNOON_DEPARTURE),
    Night     = sum(SCHEDULED_NIGHT_DEPARTURE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "TimeOfDay", values_to = "DelayCount")

###Plot: Delays by Time of Day
ggplot(time_of_day_delays, aes(x = TimeOfDay, y = DelayCount)) +
  geom_col(fill = main_blue, width = 0.6, alpha = 0.9) +
  labs(
    title = "Number of Delayed Flights by Time of Day",
    x = "Time of Day at Departure",
    y = "Number of Delayed Flights"
  ) +
  scale_y_continuous(labels = comma) +
  theme_airplane()

###Plot: Delayed Flights by Day of Week (no scientific notation)
flights_delayed <- flights_raw %>%
  filter(DEPARTURE_TIME > SCHEDULED_DEPARTURE) %>%
  mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK,
                              levels = 1:7,
                              labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")))
ggplot(flights_delayed, aes(x = DAY_OF_WEEK)) +
  geom_bar(fill = "#3366CC", width = 0.6, alpha = 0.9) +
  scale_y_continuous(labels = scales::comma) +  # <- This fixes it
  labs(title = "Number of Delayed Flights by Day of Week",
       x = "Day of Week",
       y = "Number of Delayed Flights") +
  theme_airplane()

###Rename the airline column
airlines_key <- airlines_key %>%
  rename(AIRLINE_NAME = AIRLINE)

###Join full name to flights_raw
flights_raw <- flights_raw %>%
  left_join(airlines_key, by = c("AIRLINE" = "IATA_CODE"))

###Remove suffixes like "Inc.", "Co.", "Air Lines", etc.
flights_raw <- flights_raw %>%
  mutate(AIRLINE_NAME_CLEAN = gsub(" Airlines Inc\\.| Airways Inc\\.| Air Lines Inc\\.| Airlines Co\\.| Air Lines Co\\.| Inc\\.| Co\\.", "", AIRLINE_NAME))

###Plot: Proportion of Delayed Flights by Airline 
flights_raw <- flights_raw %>%
  mutate(DELAYED_STATUS = ifelse(DELAYED == 1, "Delayed", "On-Time"))

ggplot(flights_raw, aes(x = AIRLINE_NAME_CLEAN, fill = DELAYED_STATUS)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Delayed Flights by Airline",
       x = "Airline",
       y = "Proportion of Flights",
       fill = "Status") +
  theme_airplane() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 20))  # Moves x-axis title up
  )

###Create DELAYED flag
flights_raw <- flights_raw %>%
  mutate(DELAYED = ifelse(DEPARTURE_TIME > SCHEDULED_DEPARTURE, 1, 0))

####Summarize Delay Rate by OD Pair
route_delays <- flights_raw %>%
  group_by(ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>%
  summarise(
    Total_Flights = n(),
    Delayed_Flights = sum(DELAYED, na.rm = TRUE),
    Delay_Rate = Delayed_Flights / Total_Flights,
    .groups = "drop"
  )

###Get Top 20 routes by flight count
top_routes <- route_delays %>%
  slice_max(order_by = Total_Flights, n = 20)

###Create a "Route" label (DEST → ORIGIN)
top_routes <- top_routes %>%
  mutate(Route_Label = paste(DESTINATION_AIRPORT, "→", ORIGIN_AIRPORT))

###Plot: Delay Rate by Top 20 Busiest Routes 
ggplot(route_delays_top, aes(x = ORIGIN_AIRPORT, y = DESTINATION_AIRPORT, fill = Delay_Rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#D3E5FF", high = "#3366CC", labels = scales::percent) +
  labs(title = "Delay Rate by Top 20 Busiest Routes",
       x = "Origin Airport",
       y = "Destination Airport",
       fill = "Delay Rate") +
  theme_airplane() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  )

# Summarize delay rate by month
monthly_delays <- flights_raw %>%
  group_by(MONTH) %>%
  summarise(
    Total_Flights = n(),
    Delayed_Flights = sum(DELAYED, na.rm = TRUE),
    Delay_Rate = Delayed_Flights / Total_Flights
  ) %>%
  ungroup() %>%
  mutate(Month_Label = factor(MONTH, levels = 1:12,
                              labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

###Plot: Monthly Delay Patterns 
ggplot(monthly_delays, aes(x = Month_Label, y = Delay_Rate, group = 1)) +
  geom_line(color = "#3366CC", linewidth = 1.2) +
  geom_point(color = "#3366CC", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monthly Delay Patterns",
       x = "Month",
       y = "Proportion of Flights Delayed") +
  theme_airplane()

###Load Airport Location Data
airports <- read_csv("/Users/svc-olin130lounge/Downloads/airports.csv")  

###Calculate Delay Rate per Origin Airport
flights_raw <- flights_raw %>%
  mutate(DELAYED = ifelse(DEPARTURE_TIME > SCHEDULED_DEPARTURE, 1, 0))
airport_delays <- flights_raw %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(
    Total_Flights = n(),
    Delayed_Flights = sum(DELAYED, na.rm = TRUE),
    Delay_Rate = Delayed_Flights / Total_Flights
  )

### Merge Airport Coordinates
airport_delays <- airport_delays %>%
  left_join(airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE"))

###Plot U.S. Map with Airports Delay Rate
us_map <- map_data("state")

###Plot: Average Departure Delay Rate by U.s. Airport 
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "grey98", color = "grey90", linewidth = 0.2) +
  geom_point(data = airport_delays,
             aes(x = LONGITUDE, y = LATITUDE, fill = Delay_Rate),
             shape = 21, stroke = 0.3, color = "black", size = 4, alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent) +
  coord_fixed(1.3, xlim = c(-130, -65), ylim = c(23, 50)) +
  labs(
    title = "Average Departure Delay Rate by U.S. Airport",
    fill = "Delay Rate"
  ) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 20,
      color = "#D9542C"
    ),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

###For correlation matrix: select numeric columns 
numeric_data <- flights_train %>%
  dplyr::select(where(is.numeric))
numeric_data <- numeric_data %>%
  dplyr::select(-(YEAR:FLIGHT_NUMBER), -CANCELLED)

###Compute Correlation Matrix 
cor_matrix <- cor(numeric_data)

###View Correlation Matrix 
print(cor_matrix)

###Plot Correlation Heat Map 
library(reshape2)

correlation_melt <- melt(cor_matrix)

colnames(correlation_melt) <- c("Variable1", "Variable2", "Correlation")

num_correl_hm<- ggplot(correlation_melt, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), 
            color = "black", size = 3) +  # Add Correlation Coefficients
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_classic() +
  labs(title = "Correlation Heatmap of Numeric Predictors",
       x = "Predictors", y = "Predictors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
num_correl_hm


