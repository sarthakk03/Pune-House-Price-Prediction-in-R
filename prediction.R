#DS CP R CODE
library(ggplot2)
library(tidyverse)


#Data Preprocessing
#Loading the data set
df <- read.csv('E:/DATA SCIENCE/House-Prices-Prediction-in-Pune-master/Pune_House_Data.csv')
head(df)
#Exploring the data set
dim(df)
#Exploring the dataset
df %>% group_by(area_type) %>% summarize(count=n())
#Exploring the dataset
df %>% group_by(availability) %>% summarize(count=n())
#Exploring the dataset
df %>% group_by(size) %>% summarize(count=n())
#Exploring the dataset
df %>% group_by(site_location) %>% summarize(count=n())
#Removing the columns of society
df <- df %>% select(-society)
head(df)



#Data Cleaning
#Checking the null values in the dataset
sum(is.na(df))
#Applying median to the balcony and bath column
balcony_median <- floor(median(df$balcony, na.rm=TRUE))
bath_median <- floor(median(df$bath, na.rm=TRUE))
df$balcony[is.na(df$balcony)] <- balcony_median
df$bath[is.na(df$bath)] <- bath_median
#Checking the null values in the dataset again
sum(is.na(df))
#Dropping the rows with null values because the dataset is huge as compared to null values.
df <- df %>% drop_na()
sum(is.na(df))
#Converting the size column to bhk
df$bhk <- as.integer(strsplit(df$size, " ")[[1]][1])
df <- df %>% select(-size)
df %>% group_by(bhk) %>% summarize(count=n())
#Exploring the total_sqft column
unique(df$total_sqft)
#Since the total_sqft contains range values such as 1133-1384, lets filter out these values
isFloat <- function(x) {
  !is.na(as.numeric(x))
}
#Displaying all the rows that are not integers
df[!sapply(df$total_sqft, isFloat),]
#Converting the range values to integer values and removing other types of error
convert_sqft_to_num <- function(x) {
  tokens <- strsplit(x, "-")[[1]]
  if (length(tokens) == 2) {
    return((as.numeric(tokens[1]) + as.numeric(tokens[2]))/2)
  }
  else {
    return(as.numeric(x))
  }
}
df$new_total_sqft <- sapply(df$total_sqft, convert_sqft_to_num)
df <- df %>% select(-total_sqft)
head(df)
#Removing the rows in new_total_sqft column that have None values
sum(is.na(df$new_total_sqft))
#Removing the rows in new_total_sqft column that have None values
df <- df %>% drop_na()
sum(is.na(df$new_total_sqft))



#Adding a new column of price_per_sqft
df1 <- df
#In our dataset the price column is in Lakhs
df1$price_per_sqft <- (df1$price * 100000) / df1$new_total_sqft
head(df1)

#Checking unique values of 'location' column
locations <- unique(df$site_location)
length(locations)


# Removing the extra spaces at the end
df1$site_location <- trimws(df1$site_location)

# Calulating all the unqiue values in 'site_location' column
location_stats <- df1 %>% group_by(site_location) %>% summarise(count = n()) %>% arrange(desc(count))
location_stats

# Checking locations with less than 10 values
location_stats <- df1 %>%
  group_by(site_location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




# Labelling the dates into Not Ready
dates <- table(df1$availability)

dates_not_ready <- names(dates[dates<10000])
df1$availability <- ifelse(df1$availability %in% dates_not_ready, "Not Ready", df1$availability)

length(unique(df1$availability))



df2 <- subset(df1, !(new_total_sqft/bhk < 300))
cat(sprintf("Length of df2: %d, Length of df1: %d\n", nrow(df2), nrow(df1)))


# Load required libraries
library(dplyr)

# Define function to remove outliers using standard deviation
remove_pps_outliers <- function(df) {
  
  # Create an empty data frame to store the reduced data
  df_out <- data.frame()
  
  # Group the data by site_location and calculate mean and standard deviation for price_per_sqft
  df_grouped <- df %>%
    group_by(site_location) %>%
    mutate(m = mean(price_per_sqft), sd = sd(price_per_sqft)) %>%
    ungroup()
  
  # Filter the data within one standard deviation from the mean for each site_location
  for (key in unique(df$site_location)) {
    sub_df <- df_grouped %>% filter(site_location == key)
    reduce_df <- sub_df %>% filter(price_per_sqft > (mean(sub_df$price_per_sqft) - sd(sub_df$price_per_sqft)) &
                                     price_per_sqft < (mean(sub_df$price_per_sqft) + sd(sub_df$price_per_sqft)))
    df_out <- rbind(df_out, reduce_df)
  }
  
  return(df_out)
}

# Apply the function to df2 and save the result as df3
df3 <- remove_pps_outliers(df2)
cat(paste("Original data length: ", nrow(df2), "\nReduced data length: ", nrow(df3), "\n"))


#EDA(Exploiratory Data Analysis)
# Load required libraries
library(ggplot2)

# Define function to plot scatter chart
plot_scatter_chart <- function(df, site_location) {
  
  # Filter the data by site_location and bhk
  bhk2 <- df %>% filter(site_location == site_location & bhk == 2)
  bhk3 <- df %>% filter(site_location == site_location & bhk == 3)
  
  # Set figure size
  options(repr.plot.width = 15, repr.plot.height = 10)
  
  # Create scatter plot
  ggplot() +
    geom_point(data = bhk2, aes(x = new_total_sqft, y = price), color = 'blue', size = 5, shape = 1, show.legend = TRUE) +
    geom_point(data = bhk3, aes(x = new_total_sqft, y = price), color = 'red', size = 5, shape = 4, show.legend = TRUE) +
    labs(x = "Total Square Feet Area", y = "Price (in Lakhs)", title = site_location) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 14)) +
    scale_shape_manual(values = c(1, 4))
}

# Call the function to plot scatter chart for 'Hadapsar' site location
plot_scatter_chart(df3, 'Hadapsar')



# Define function to remove outliers based on bhk
remove_bhk_outliers <- function(df) {
  
  exclude_indices <- c()
  
  # Group by site_location
  site_location_groups <- df %>% group_by(site_location)
  
  for (site_location in unique(df$site_location)) {
    
    site_location_df <- site_location_groups %>% filter(site_location == !!site_location)
    
    bhk_stats <- site_location_df %>% group_by(bhk) %>%
      summarise(mean_pps = mean(price_per_sqft),
                std_pps = sd(price_per_sqft),
                count = n()) %>%
      dplyr::rename(bhk_count = count)
    
    for (bhk in unique(site_location_df$bhk)) {
      stats <- bhk_stats %>% filter(bhk == !!bhk-1)
      if (nrow(stats) != 0 && stats$bhk_count > 5) {
        exclude_indices <- c(exclude_indices, site_location_df %>% filter(bhk == !!bhk & price_per_sqft < stats$mean_pps) %>% pull(index))
      }
    }
  }
  
  # Check if exclude_indices is empty before subsetting
  if (length(exclude_indices) > 0) {
    df <- df[-exclude_indices, ]
  }
  
  return(df)
}

df4 <- remove_bhk_outliers(df3)
cat("Before removing outliers based on bhk:", nrow(df3), "\n")
cat("After removing outliers based on bhk:", nrow(df4), "\n")


plot_scatter_chart <- function(df, site_location) {
  
  bhk2 <- df %>% filter(site_location == !!site_location & bhk == 2)
  bhk3 <- df %>% filter(site_location == !!site_location & bhk == 3)
  
  ggplot() +
    geom_point(data = bhk2, aes(x = new_total_sqft, y = price, color = "2 BHK"), size = 2) +
    geom_point(data = bhk3, aes(x = new_total_sqft, y = price, color = "3 BHK"), shape = 4, size = 3) +
    scale_color_manual(values = c("blue", "green")) +
    labs(x = "Total Square Feet Area", y = "Price (in Lakhs)", title = site_location) +
    theme_minimal()
}

plot_scatter_chart(df4, 'Hadapsar')
plot_scatter_chart(df4, 'Alandi Road')


ggplot(df4, aes(x = price_per_sqft)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  labs(x = "Price Per Square Feet", y = "Count") +
  theme_minimal()



ggplot(df4, aes(x = bath)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Number of Bathrooms", y = "Count") +
  theme_minimal()

df5 <- df4[df4$bath < (df4$bhk + 2), ]
cat(paste("Length of df4: ", nrow(df4), ", Length of df5: ", nrow(df5)))

# Removing the unnecessary columns (columns that were added only for removing the outliers)
df6 <- df5
df6 <- subset(df6, select=-c(price_per_sqft))

head(df6)
library(fastDummies)

# Converting the categorical_value into numerical_values using get_dummies method
dummy_cols <- data.frame(model.matrix(~ df6$site_location + 0))[,-1]
df6 <- cbind(df6,dummy_cols)

# Converting the categorical_value into numerical_values using get_dummies method
dummy_cols <- data.frame(model.matrix(~ df6$availability + 0))[,-1]

df6 <- cbind(df6,dummy_cols)

# Converting the categorical_value into numerical_values using get_dummies method
dummy_cols <- data.frame(model.matrix(~ df6$area_type + 0))[,-1]
df6 <- cbind(df6,dummy_cols)

df6 <- df6[, !(names(df6) %in% c("area_type", "availability", "site_location"))]
head(df6, 10)









X <- df6[, -which(names(df6) == "price")]
y <- df6$price

library(caret)
set.seed(20)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]


# Create a linear regression model
model <- lm(y_train ~ ., data = X_train)

# Print the model summary
summary(model)

# Make predictions on the test set
y_pred <- predict(model, newdata = X_test)

# Compute the R-squared value
R2 <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_train))^2)
R2


#print("Accuracy of LR Model on Test data ->")