# Set the working directory and verify it
setwd('C://Users//prami//Desktop//SCMA 632//data')
getwd()
#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for TN
df <- data %>%
  filter(state_1 == "TN")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
tnnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
tnnew$Meals_At_Home <- impute_with_mean(tnnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  tnnew <- remove_outliers(tnnew, col)
}

# Summarize consumption
tnnew$total_consumption <- rowSums(tnnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- tnnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Thiruvallur","2" = "Chennai","3" = "Kancheepuram","4" = "Vellore","5" = "Dharmapuri","6" = "Tiruvannamalai","7" = "Viluppuram","8" = "Salem","9" = "Namakkal","10" = "Erode","11" = "The Nilgiris","12" = "Coimbatore","13" = "Dindigul","14" = "Karur","15" = "Tiruchirappalli","16" = "Perambalur","17" = "Ariyalur","18" = "Cuddalore","19" = "Nagapattinam","20" = "Thiruvarur","21" = "Thanjavur","22" = "Pudukkottai","23" = "Sivaganga","24" = "Madurai","25" = "Theni","26" = "Virudhunagar","27" = "Ramanathapuram","28" = "Thoothukkudi","29" = "Tirunelveli","30" = "Kanniyakumari","31" = "Krishnagiri")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

tnnew$District <- as.character(tnnew$District)
tnnew$Sector <- as.character(tnnew$Sector)
tnnew$District <- ifelse(tnnew$District %in% names(district_mapping), district_mapping[tnnew$District], tnnew$District)
tnnew$Sector <- ifelse(tnnew$Sector %in% names(sector_mapping), sector_mapping[tnnew$Sector], tnnew$Sector)

View(tnnew)

hist(tnnew$total_consumption, breaks = 10, col = 'green', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Tamilnadu State")

TN_consumption <- aggregate(total_consumption ~ District, data = tnnew, sum) 
View(TN_consumption)
??barplot
barplot(TN_consumption$total_consumption, 
        names.arg = TN_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'green', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed


# b) Plot {'any variable of your choice'} on the Karnataka state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C://Users//prami//Desktop//SCMA 632//SCMA 632//assignments//A5//TAMIL NADU_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(TN_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")