# Loading necessary packages
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(gifski)

# Loading in the data
dataset <- read.csv(url("https://covid19site.s3.amazonaws.com/datafiles/jhu_current_data_.csv"))

# Reformatting the variables
dataset$State <- as.character(dataset$State)
dataset$Country <- as.character(dataset$Country)
dataset$Date <- as.Date(dataset$Date)
dataset$last_update_ts <- NULL
dataset$last_update_tz <- NULL

# Removing data with missing observations, and removing Princess cruise ship data
dataset$delete[dataset$Country == "Canada" & is.na(dataset$Lat) & is.na(dataset$Long)] <- 1
dataset$delete[dataset$Country == "Canada" & dataset$State == "Diamond Princess"] <- 1
dataset$delete[dataset$Country == "Canada" & dataset$State == "Grand Princess"] <- 1
dataset$delete[dataset$Country == "Canada" & dataset$State == "Recovered"] <- 1
dataset$delete[dataset$Country == "Diamond Princess"] <- 1
dataset <- subset(dataset, is.na(delete))
dataset$delete <- NULL

# Loading in the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Keeping only world map data I need
world <- world[c("sovereignt", "name", "pop_est", "gdp_md_est")]

# Editing the dataset country and state names so they match those in the world map data
# This includes shortening/translating some names and collapsing the state data down to country level
dataset$Country[dataset$Country == "Antigua and Barbuda"] <- "Antigua and Barb."
dataset_australia <- subset(dataset, Country == "Australia")
dataset <- subset(dataset, Country != "Australia")
dataset_australia <- dataset_australia %>%
  group_by(Country, Date) %>%
  summarize(State = "", Lat = -25.7329, Long = 134.4910, 
            Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths))
dataset_australia <- as.data.frame(dataset_australia)
dataset <- rbind(dataset, dataset_australia)
dataset$Country[dataset$Country == "Bosnia and Herzegovina"] <- "Bosnia and Herz."
dataset$Country[dataset$Country == "Cabo Verde"] <- "Cape Verde"
dataset_canada <- subset(dataset, Country == "Canada")
dataset <- subset(dataset, Country != "Canada")
dataset_canada <- dataset_canada %>%
  group_by(Country, Date) %>%
  summarize(State = "", Lat = 61.3621, Long = -98.3078, 
            Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths))
dataset_canada <- as.data.frame(dataset_canada)
dataset <- rbind(dataset, dataset_canada)
dataset$Country[dataset$Country == "Central African Republic"] <- "Central African Rep."
dataset$Country[dataset$State == "Hong Kong"] <- "Hong Kong"
dataset_china <- subset(dataset, Country == "China")
dataset <- subset(dataset, Country != "China")
dataset_china <- dataset_china %>%
  group_by(Country, Date) %>%
  summarize(State = "", Lat = 36.5618, Long = 103.8191, 
            Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deaths = sum(Deaths))
dataset_china <- as.data.frame(dataset_china)
dataset <- rbind(dataset, dataset_china)
dataset$Country[dataset$Country == "Congo (Brazzaville)"] <- "Congo"
dataset$Country[dataset$Country == "Congo (Kinshasa)"] <- "Dem. Rep. Congo"
dataset$Country[dataset$Country == "Cote d'Ivoire"] <- "Côte d'Ivoire"
dataset$Country[dataset$Country == "Czechia"] <- "Czech Rep."
dataset$Country[dataset$State == "Faroe Islands"] <- "Faeroe Is."
dataset$Country[dataset$State == "Greenland"] <- "Greenland"
dataset$Country[dataset$Country == "Dominican Republic"] <- "Dominican Rep."
dataset$Country[dataset$Country == "Equatorial Guinea"] <- "Eq. Guinea"
dataset$Country[dataset$Country == "Eswatini"] <- "Swaziland"
dataset$Country[dataset$Country == "Holy See"] <- "Vatican"
dataset$Country[dataset$Country == "Korea, South"] <- "Korea"
dataset$Country[dataset$Country == "Laos"] <- "Lao PDR"
dataset$Country[dataset$State == "Aruba"] <- "Aruba"
dataset$Country[dataset$State == "Curacao"] <- "Curaçao"
dataset$Country[dataset$State == "Sint Maarten"] <- "Sint Maarten"
dataset$Country[dataset$Country == "North Macedonia"] <- "Macedonia"
dataset$Country[dataset$Country == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
dataset$Country[dataset$Country == "Saint Vincent and the Grenadines"] <- "St. Vin. and Gren."
dataset$Country[dataset$Country == "Taiwan*"] <- "Taiwan"
dataset$Country[dataset$State == "Bermuda"] <- "Bermuda"
dataset$Country[dataset$State == "Cayman Islands"] <- "Cayman Is."
dataset$Country[dataset$State == "Channel Islands"] <- "Jersey"
dataset$Country[dataset$State == "Isle of Man"] <- "Isle of Man"
dataset$Country[dataset$State == "Montserrat"] <- "Montserrat"
dataset$Country[dataset$Country == "US"] <- "United States"
dataset$Country[dataset$State == "French Polynesia"] <- "Fr. Polynesia"
dataset$Country[dataset$State == "New Caledonia"] <- "New Caledonia"
dataset$Country[dataset$State == "Saint Barthelemy"] <- "St-Barthélemy"
dataset$Country[dataset$State == "St Martin"] <- "St-Martin"
dataset$Country[dataset$State == "Mayotte"] <- "Comoros"
dataset$Country[dataset$Country == "West Bank and Gaza"] <- "Palestine"
dataset$delete[dataset$State == "French Guiana"] <- 1
dataset$delete[dataset$State == "Guadeloupe"] <- 1
dataset$delete[dataset$State == "Martinique"] <- 1
dataset$delete[dataset$State == "Reunion"] <- 1
dataset$delete[dataset$State == "Gibraltar"] <- 1
dataset <- subset(dataset, is.na(delete))
dataset$delete <- NULL
dataset$State <- NULL
colnames(dataset)[which(names(dataset) == "Country")] <- "name"

# Sorting the COVID-19 data by country and date
dataset <- dataset[order(dataset$name, dataset$Date) ,]

# Merging in the COVID-19 data with the world map data
dataset <- merge(world, dataset, by = "name", all.x = TRUE)

# Subsetting the countries that do not have any COVID-19 data
no_data <- subset(dataset, is.na(Date))
dataset <- subset(dataset, !is.na(Date))

# Adding date observations for the countries that do not have any COVID-19 data
dates <- as.data.frame(unique(dataset$Date))
dates$null <- 1
colnames(dates) <- c("fill_Date", "null")
no_data$null <- 1
no_data <- merge(no_data, dates, by = "null")
no_data$Date <- no_data$fill_Date
no_data$null <- NULL
no_data$fill_Date <- NULL

# Merging the countries that do not have any COVID-19 data back into the main dataset
dataset <- rbind(dataset, no_data)

# Sorting the merged by country and date
dataset <- dataset[order(dataset$name, dataset$Date) ,]

# Plotting the merged data
dates <- unique(dataset$Date)

# Confirmed Cases
for (i in seq_along(dates)){
  ggplot(data = dataset[dataset$Date == dates[i] ,]) +
    geom_sf(aes(fill = Confirmed)) +
    scale_fill_gradient(low = "blue", high = "red",
                        limits = c(min(dataset$Confirmed, na.rm = TRUE), 
                                   max(dataset$Confirmed, na.rm = TRUE)), na.value = "gray") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("COVID-19 Confirmed Cases", subtitle = paste0("Date: ", dates[i]))
  ggsave(paste0("covid_map_cases_", i, ".png"), width = 15, height = 9)
}

png_files <- list.files(getwd(), pattern = ".png$")
png_files <- gtools::mixedsort(png_files)
gif_file <- "covid_map_cases.gif"
gifski(png_files, gif_file)
unlink(png_files)


# Deaths
for (i in seq_along(dates)){
  ggplot(data = dataset[dataset$Date == dates[i] ,]) +
    geom_sf(aes(fill = Deaths)) +
    scale_fill_gradient(low = "blue", high = "red",
                        limits = c(min(dataset$Deaths, na.rm = TRUE), 
                                   max(dataset$Deaths, na.rm = TRUE)), na.value = "gray") +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle("COVID-19 Deaths", subtitle = paste0("Date: ", dates[i]))
  ggsave(paste0("covid_map_deaths_", i, ".png"), width = 15, height = 9)
}

png_files <- list.files(getwd(), pattern = ".png$")
png_files <- gtools::mixedsort(png_files)
gif_file <- "covid_map_deaths.gif"
gifski(png_files, gif_file)
unlink(png_files)
