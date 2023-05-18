## to set up the directory 
setwd("C:/eda")

##Install and load necessary packages:
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

##Load the dataset:
data <- read.csv("nyc_bike_sharing.csv")

##Display the structure of the dataset:
str(data)

##Display the summary statistics of the dataset:
summary(data)

##View the first few rows of the dataset:
head(data)

##View the last few rows of the dataset:
tail(data)

##Count the number of rows in the dataset:
nrow(data)

##Count the number of columns in the dataset:
ncol(data)

##Select specific columns from the dataset:

selected_data <- data[, c("humidity", "temp")]
selected_data
##Filter rows based on a condition
filtered_data <- filter(data, count > 100)
filtered_data
##Arrange the dataset based on a column:
arranged_data <- arrange(data, datetime)
arranged_data

##Rename columns in the dataset:
renamed_data <- rename(data, date_time = datetime, temperature = temp)
renamed_data

##Calculate the mean of a column:
mean_value <- mean(data$registered)
mean_value

##Calculate the median of a column:
median_value <- median(data$registered)
median_value

##Calculate the standard deviation of a column:
sd_value <- sd(data$registered)
sd_value

##Calculate the minimum value in a column:
min_value <- min(data$count)
min_value

##Calculate the maximium value in a column:
max_value <- max(data$count)
max_value

##Create a new variable based on existing variables:
data$new_variable <- data$holiday + data$workingday
data$new_variable

##Group the dataset by a column:
grouped_data <- group_by(data, count)
grouped_data

##Summarize grouped data:
summarized_data <- summarize(grouped_data, mean_holiday = mean(holiday), max_workingday = max(workingday))
summarized_data

##Plot a histogram:
ggplot(data, aes(x = count)) +
  geom_histogram()

##Plot a scatter plot:
ggplot(data, aes(x = weather, y = count)) +
  geom_point()

##Plot a bar chart:
ggplot(data, aes(x = weather, y = count)) +
  geom_bar(stat = "identity")

##Plot a line chart:
ggplot(data, aes(x = registered, y = count)) +
  geom_line()
ggplot(data, aes(x = count, y = weather)) +
  geom_line()

##Add labels and titles to a plot:
ggplot(data, aes(x = weather, y = count)) +
  geom_point() +
  xlab("weather") +
  ylab("count") +
  ggtitle("weather Vs count")

##Calculate correlation between two columns:
correlation_value <- cor(data$weather, data$count)
correlation_value

##Create a box plot:
ggplot(data, aes(x = weather, y = humidity)) +
  geom_boxplot()

##Perform a t-test:
t_test_result <- t.test(data$weather, data$humidity)
t_test_result

##Contingency_table
contingency_table <- table(data$holiday, data$workingday)
contingency_table

##Perform chi-squared test:
chi_squared_test <- chisq.test(contingency_table)
chi_squared_test

##Perform linear regression
linear_model <- lm(weather ~ count, data = data)
linear_model

##Display the coefficients of the linear regression model:
summary(linear_model)

##Convert a column to a different data type:
data$holidays <- as.numeric(data$holiday)
data$holidays

##Subset rows based on multiple conditions:
subset_data <- data[data$count > 100 & data$registered == "100", ]
subset_data

##Calculate the sum of a column:
sum_value <- sum(data$count)
sum_value

##Calculate the cumulative sum of a column:
cumulative_sum <- cumsum(data$count)
cumulative_sum

##Perform a one-way ANOVA:
anova_result <- aov(holiday ~ workingday, data = data)
anova_result

##Display the ANOVA table:
summary(anova_result)

##Perform a Mann-Whitney U test:
mwu_test_result <- wilcox.test(holiday ~ workingday, data = data)
mwu_test_result

##Perform a Kruskal-Wallis test:
kw_test_result <- kruskal.test(holiday ~ workingday, data = data)
kw_test_result

##Calculate the mode of a column:
mode_value <- names(table(data$count))[which.max(table(data$count))]
mode_value

##Remove missing values from the dataset:
clean_data <- na.omit(data)
clean_data

##Replace missing values in a column with a specific value:
data$temp[is.na(data$temp)] <- 0
data$temp

##Create a new dataset with random sampling:
sampled_data <- data[sample(nrow(data), size = 100, replace = FALSE), ]
sampled_data

##Create a new dataset with random sampling:
sampled_data <- data[sample(nrow(data), size = 100, replace = FALSE), ]
sampled_data

##Calculate the factorial value
factorial_value <- factorial(5)
factorial_value

##Calculate the weighted mean of a column:
weighted_mean <- weighted.mean(data$weather, data$count)
weighted_mean

##Create a violin plot:
ggplot(data, aes(x = weather, y = count)) +
  geom_violin()

##Calculate the interquartile range of a column:
iqr_value <- IQR(data$holiday)
iqr_value

##Generate random numbers from a normal distribution:
random_numbers <- rnorm(100, mean = 0, sd = 1)
random_numbers

##Remove duplicate rows from the dataset:
unique_data <- unique(data)
unique_data

##Replace specific values in a column with a new value:
data$count[data$count == "old_value"] <- "new_value"
data$count

##Calculate the cumulative product of a column:
cumulative_product <- cumprod(data$count)
cumulative_product

##Create a table of frequencies:
frequency_table <- table(data$count)
frequency_table

##Convert a character column to a factor:
data$weather <- as.factor(data$weather)
data$weather

##Create a scatter plot with a linear regression line:

ggplot(data, aes(x = count, y = weather)) +
  geom_point() +
  geom_smooth(method = "lm")

##Calculate the cumulative distribution function (CDF) of a column:
cdf_values <- ecdf(data$count)
cdf_values

## plotting a graph
plot(data$count,data$weather)

## create table
table(data$count)

##subset
subset(data)

#scale
scale(data$weather)

## list lists all present in the dataset
ls()

ls(pattern="ind")

ls(pattern="rw")

## length() finds the length
length(data)

#dimension of dataset
dim(data)

# rep() finds the repetion
rep(data)

##seq() creates the sequence
seq(data)

## finds the duplicate in the datasets
duplicated(data)

##to find whether it is in data frame
is.data.frame(data)

## to find the log in the datasets
log(data$count)

##to find the expo in the datasets
exp(data$count)

##to find the sum of columns in the datasets
sum(data$count)

# helps us to find the r commands
help(data)

#helps us round of datasets
pretty(data$weather)

# helps us to find the attributes in the datasets
attributes(data)

#accessing the values in the datasets
data$weather

#to find the class of the data sets
class(data)

## to find the type of datasets
typeof(data)

## to find the lagged values
lagged_values <- lag(data$count)
lagged_values

##to find the difference in the data
differences <- diff(data$registered)
differences
 
##Generate a random permutation of a dataset:
random_permutation <- sample(data)
random_permutation

## to find the z-scores 
z_scores <- scale(data$count)
outliers <- data$count[abs(z_scores) > 400]
outliers

##Perform data imputation using mean imputation:
data$count[is.na(data$count)] <- mean(data$count, na.rm = TRUE)
data$count

##Perform data imputation using median imputation:
data$count[is.na(data$count)] <- median(data$count, na.rm = TRUE)
data$count

##Perform data normalization using mean normalization:
normalized_data <- (data$count - mean(data$count)) / (max(data$count) - min(data$count))
normalized_data

##Perform data normalization using z-score normalization:
normalized_data <- scale(data$count)

##Perform data normalization using min-max normalization:
normalized_data <- scale(data$weather, center = min(data$weather), scale = max(data$weather) - min(data$weather))
normalized_data

##Perform data normalization using decimal scaling:
decimal_scaling <- floor(log10(max(abs(data$count))))
normalized_data <- data$count / (10^decimal_scaling)
normalized_data

##Perform outlier detection using the boxplot method:
boxplot(data$count, coef = 1.5)

##Calculate the relative frequency of a variable/column:
relative_frequency <- prop.table(table(data$count))
relative_frequency

##Calculate the variance of a variable/column
var_value <- var(data$holiday)
var_value

##Calculate the correlation between two variables/columns:
correlation <- cor(data$weather, data$workingday)
correlation

##Convert a variable/column to a different data type:
data$season <- as.numeric(data$season)
data$season

data$datetime
data$season
data$holiday
data$workingday
data$weather
data$temp
data$atemp

##Sort the dataset by a column:
sorted_data <- data[order(data$windspeed), ]
sorted_data

#Convert a character variable/column to a date format:
data$temp<- as.Date(data$temp, format = "yyyy-mm-dd")
data$temp

## barplot of windspeed
barplot(data$windspeed)

##Perform a one-sample t-test:
t.test(data$count, mu = 0)

##Calculate the quantiles of a distribution:
quantiles <- quantile(data$count, probs = c(0.25, 0.5, 0.75))
quantiles

##Perform a polynomial regression:
lm_model <- lm(windspeed ~ poly(count, degree = 2), data = data)
summary(lm_model)

##checking column names
names(data)



