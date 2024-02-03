df <- read.csv("C:/Users/mehre/test.r/HW2/Wholesale customers data.csv")
View(df)

options(scipen=999)

f1=boxplot(df$Milk, df$Fresh, df$Delicassen, 
           names = c('Milk', 'Fresh', 'Delicassen'),
           main = 'Boxplot of Milk, Fresh, and Delicassen',
           ylab = 'Value',
           xlab = 'Attribute'
)


get_outliers <- function(data) {
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  IQR <- Q3 - Q1
  outliers <- data[(data < (Q1 - 1.5 * IQR)) | (data > (Q3 + 1.5 * IQR))]
  return(outliers)
}

# Extract outliers for each of the specified columns
milk_outliers <- get_outliers(df$Milk)
fresh_outliers <- get_outliers(df$Fresh)
delicassen_outliers <- get_outliers(df$Delicassen)

milk_outliers_count <- length(milk_outliers)
fresh_outliers_count <- length(fresh_outliers)
delicassen_outliers_count <- length(delicassen_outliers)

# Display the count and the outlier values for each column
cat("Number of outliers in Milk:", milk_outliers_count, "\n")
cat("Outlier values for Milk:", milk_outliers, "\n\n")

cat("Number of outliers in Fresh:", fresh_outliers_count, "\n")
cat("Outlier values for Fresh:", fresh_outliers, "\n\n")

cat("Number of outliers in Delicassen:", delicassen_outliers_count, "\n")
cat("Outlier values for Delicassen:", delicassen_outliers, "\n")

# Rows with outliers for each column
milk_outlier_rows <- df$Milk %in% milk_outliers
fresh_outlier_rows <- df$Fresh %in% fresh_outliers
delicassen_outlier_rows <- df$Delicassen %in% delicassen_outliers

# Combine the outlier rows for all columns
all_outlier_rows <- milk_outlier_rows | fresh_outlier_rows | delicassen_outlier_rows

# Count the number of rows with outliers
sum(all_outlier_rows)

df_no_outliers <- df[!(df$Milk %in% milk_outliers | 
                         df$Fresh %in% fresh_outliers | 
                         df$Delicassen %in% delicassen_outliers), ]

View(df_no_outliers)



# Display the dimensions of the original and cleaned dataframes
cat("Original dataframe dimensions:", dim(df), "\n")
cat("Dataframe dimensions after outlier removal:", dim(df_no_outliers), "\n")

View(df_no_outliers)


df_no_outliers <- df

repeat {
  # Identify outliers for each column
  milk_outliers <- boxplot.stats(df_no_outliers$Milk)$out
  fresh_outliers <- boxplot.stats(df_no_outliers$Fresh)$out
  delicassen_outliers <- boxplot.stats(df_no_outliers$Delicassen)$out
  
  # Create a logical index for rows with outliers
  outlier_index <- df_no_outliers$Milk %in% milk_outliers | 
    df_no_outliers$Fresh %in% fresh_outliers | 
    df_no_outliers$Delicassen %in% delicassen_outliers
  
  # If no outliers are found, break the loop
  if (sum(outlier_index) == 0) {
    break
  }
  
  # Otherwise, remove the outliers and continue
  df_no_outliers <- df_no_outliers[!outlier_index, ]
}



f2=boxplot(df_no_outliers$Milk, df_no_outliers$Fresh, df_no_outliers$Delicassen, 
           names = c('Milk', 'Fresh', 'Delicassen'),
           main = 'Boxplot of Milk, Fresh, and Delicassen',
           ylab = 'Value',
           xlab = 'Attribute'
)


# Manual oversampling and undersampling
set.seed(123) # for reproducibility

# Extract samples for each region
region1_samples <- df_no_outliers[df_no_outliers$Region == 1, ]
region2_samples <- df_no_outliers[df_no_outliers$Region == 2, ]
region3_samples <- df_no_outliers[df_no_outliers$Region == 3, ]

# Oversample Region 1 and Region 2
oversampled_region1 <- region1_samples[sample(1:nrow(region1_samples), 246, replace = TRUE), ]
oversampled_region2 <- region2_samples[sample(1:nrow(region2_samples), 246, replace = TRUE), ]

# Combine all samples and rename to df_no_outliers
df_no_outliers <- rbind(oversampled_region1, oversampled_region2, region3_samples)

# Check the distribution
table(df_no_outliers$Region)
# Load required libraries
library(caret)
library(C50)

df2 <- df_no_outliers[order(runif(nrow(df_no_outliers))),]
tail(df2,25)

df2$Region <- factor(df2$Region)
# Split your data into training and testing sets
set.seed(123)  # For reproducibility
split_idx <- sample(1:nrow(df2), 0.7 * nrow(df2))  # 70% training, 30% testing
df2_train <- df2[split_idx, ]
df2_test <- df2[-split_idx, ]

# Train the C5.0 model with the correct formula
C50_model <- C5.0(Region ~ ., data = df2_train)

# Print the summary of the model
summary(C50_model)
##plot model
plot(C50_model)

#C5.0 Predict

C50_predict<- predict(C50_model,df2_test)

#Compare
#table(df2_test[,2],C50_predict)

#C5.0 Train Improve performance
C50_model<- C5.0(df2_train[,-2],df2_train[,2],trials=10)
summary(C50_model)

#C5.0 Predict Class

C50_predict_class<- predict(C50_model,df2_test, method = "class")
C50_predict_class


#Compare
table(df2_test[,2],C50_predict_class)

#C5.0 Predict Prob

C50_predict_prob<- predict(C50_model,df2_test, method = "prob")
C50_predict_prob

#Compare
table(df2_test[,2],C50_predict_prob)




# Set seed for reproducibility
#set.seed(123)

# Define the number of folds (k)
k <- 3

# Create a training control object for k-fold cross-validation
train_control <- trainControl(method = "cv", number = k)

# Initialize a vector to store error rates for C5.0
error_rates_C50 <- numeric(k)

# Perform k-fold cross-validation
for (fold in 1:k) {
  # Create training and testing subsets for the current fold
  set_index <- createFolds(df2$Region, k = k, list = TRUE)
  df2_train <- df2[-set_index[[fold]], ]
  df2_test <- df2[set_index[[fold]], ]
  
  # Train the C5.0 model
  C50_model <- C5.0(Region ~ ., data = df2_train)
  
  # Make predictions on the test data
  C50_predict <- predict(C50_model, newdata = df2_test)
  
  # Create a confusion matrix
  confusion_matrix <- table(df2_test$Region, C50_predict)
  
  # Print the confusion matrix for the current fold
  cat("Confusion Matrix (Fold", fold, "):\n")
  print(confusion_matrix)
  
  # Calculate the error rate for the current fold
  error_rate_C50 <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Print and store the error rate
  cat("Error Rate (Fold", fold, "):", error_rate_C50, "\n")
  error_rates_C50[fold] <- error_rate_C50
}

# Calculate and print the average error rate across all folds
avg_error_rate_C50 <- mean(error_rates_C50)
cat("Average Error Rate:", avg_error_rate_C50, "\n")

cat("Length of error_rates_C50:", length(error_rates_C50), "\n")


#########################################################CART#####################


library(rpart)
library(caret)
library(rpart.plot)

index <- createDataPartition(df_no_outliers$Region, p=0.8, list=FALSE)

x_train <- df_no_outliers[index, ]
x_test <- df_no_outliers[-index, ]

y_train <- df_no_outliers$Region[index]
y_test <- df_no_outliers$Region[-index]

# Shuffle dataframe 
df2 <- df_no_outliers[order(runif(nrow(df_no_outliers))), ]
tail(df2, 25)

# Split your data into training and testing sets
set.seed(123)  # For reproducibility
split_idx <- sample(1:nrow(df2), 0.7 * nrow(df2))  # 70% training, 30% testing
df2_train <- df2[split_idx, ]
df2_test <- df2[-split_idx, ]


# Train the CART model
cart_model <- rpart(Region ~ ., data = df2_train, method = "class")
#plotting
rpart.plot(cart_model)
# CART Predict

cart_predict <- predict(cart_model, df2_test, type = "class")
cart_predict

# Compare predictions
table(df2_test[, 2], cart_predict)


# Define the number of folds (k)
k <- 3

# Create a training control object for k-fold cross-validation
train_control <- trainControl(method = "cv", number = k)

# Initialize a vector to store error rates
error_rates <- numeric(k)

for (fold in 1:k) {
  # Create training and testing subsets for the current fold
  set_index <- createFolds(df2$Region, k = k, list = TRUE)
  df2_train <- df2[-set_index[[fold]], ]
  df2_test <- df2[set_index[[fold]], ]
  
  # Train the CART model
  CART_model <- rpart(Region ~ ., data = df2_train, method = "class")
  
  # Make predictions on the test data
  CART_predict <- predict(CART_model, newdata = df2_test, type = "vector")
  
  # Create a confusion matrix
  confusion_matrix <- table(df2_test$Region, CART_predict)
  
  # Print the confusion matrix for the current fold
  cat("Confusion Matrix (Fold", fold, "):\n")
  print(confusion_matrix)
  
  # Calculate the error rate for the current fold
  error_rate_CART <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Print and store the error rate
  cat("Error Rate (Fold", fold, "):", error_rate_CART, "\n")
  error_rates[fold] <- error_rate_CART
}

# Calculate and print the average error rate across all folds
avg_error_rate_CART <- mean(error_rates)
cat("Average Error Rate:", avg_error_rate_CART, "\n")

cat("Length of error_rates_CART:", length(error_rates), "\n")


# Calculate the difference in error rates
#diff_error_rates <- error_rate_CART - error_rate_C50
#diff_error_rates



# Calculate the standard error of the difference
se_diff <- sqrt((error_rate_C50 * (1 - error_rate_C50) / nrow(df2_test)) + 
                  (error_rate_CART * (1 - error_rate_CART) / nrow(df2_test)))

# Calculate the test statistic (t-value)
t_stat <- diff_error_rates / se_diff

# Calculate the degrees of freedom
df <- 2 * nrow(df2_test) - 2

# Calculate the two-sided p-value
p_value <- 2 * (1 - pt(abs(t_stat), df))

# Define the given confidence level
conf_level <- 0.98

# Calculate the critical value
alpha <- 1 - conf_level
critical_value <- qt(1 - alpha / 2, df)

# Calculate and print the confidence interval of the error difference
margin_of_error <- critical_value * se_diff
lower_bound <- diff_error_rates - margin_of_error
upper_bound <- diff_error_rates + margin_of_error

# Print the confidence level
cat("Confidence Level:", conf_level * 100, "%\n")

# Print the confidence interval
cat("Confidence Interval of the Error Difference: [", lower_bound, ",", upper_bound, "]\n")


# Check the significance of the error rate difference and print the appropriate message
if (lower_bound > 0 && upper_bound < 0) {
  cat("The error rate difference is statistically significant at", conf_level * 100, "% confidence level.\n")
} else {
  cat("The confidence interval contains 0; therefore, the difference may not be statistically significant at", conf_level * 100, "% confidence level.\n")
}


# Indicate the selected model based on the error rate difference
if (diff_error_rates > 0) {
  cat("The C5.0 model has a lower error rate and is the selected model.\n")
} else if (diff_error_rates < 0) {
  cat("The CART model has a lower error rate and is the selected model.\n")
} else {
  cat("Both models have the same error rate.\n")
}

###############LAST TASK#######################

# Determine the selected model
selected_model <- ifelse(error_rate_C50 < error_rate_CART, C50_model, CART_model)

# Function to predict class label for a new tuple
predict_class_label <- function(new_tuple) {
  predicted_label <- predict(selected_model, newdata = new_tuple, type = "class")
  return(predicted_label)
}
# Define a function to predict class label for a given tuple
predict_class_label <- function(tuple) {
  return(predict(CART_model, newdata = tuple, type = "class"))
}





# Function to predict class label for a given tuple
predict_class_label <- function(tuple, model) {
  predicted_label <- predict(model, newdata = tuple, type = "class")
  return(predicted_label)
}

# Function to predict class label for a given tuple
predict_class_label <- function(tuple, model) {
  predicted_label <- predict(model, newdata = tuple, type = "class")
  return(predicted_label)
}

# Testing the function with three different input tuples
tuple1 <- data.frame(Channel=2, Fresh=5283, Milk=13316, Grocery=20399, Frozen=1809, Detergents_Paper=8752, Delicassen=172)
predicted_label1 <- predict_class_label(tuple1, C50_model)
cat("For tuple1:\n")
print(tuple1)
cat("Predicted class label:", predicted_label1, "\n\n")

tuple2 <- data.frame(Channel=1, Fresh=11537, Milk=138, Grocery=5838, Frozen=18119, Detergents_Paper=13381, Delicassen=9806)
predicted_label2 <- predict_class_label(tuple2, C50_model)
cat("For tuple2:\n")
print(tuple2)
cat("Predicted class label:", predicted_label2, "\n\n")

tuple3 <- data.frame(Channel=1, Region=1, Fresh=15000, Milk=2000, Grocery=1000, Frozen=3000, Detergents_Paper=100, Delicassen=1000)
predicted_label3 <- predict_class_label(tuple3, C50_model)
cat("For tuple3:\n")
print(tuple3)
cat("Predicted class label:", predicted_label3, "\n\n")

##2nd time


# Testing the function with three different input tuples
tuple1 <- data.frame(Channel=2, Fresh=2127, Milk=2053, Grocery=65580, Frozen=9049, Detergents_Paper=8171, Delicassen=18954)
predicted_label1 <- predict_class_label(tuple1, C50_model)
cat("For tuple1:\n")
print(tuple1)
cat("Predicted class label:", predicted_label1, "\n\n")

tuple2 <- data.frame(Channel=2, Fresh=15377, Milk=37748, Grocery=58438, Frozen=18159, Detergents_Paper=30381, Delicassen=8061)
predicted_label2 <- predict_class_label(tuple2, C50_model)
cat("For tuple2:\n")
print(tuple2)
cat("Predicted class label:", predicted_label2, "\n\n")

tuple3 <- data.frame(Channel=1, Fresh=1500, Milk=200, Grocery=100, Frozen=300, Detergents_Paper=1007, Delicassen=1080)
predicted_label3 <- predict_class_label(tuple3, C50_model)
cat("For tuple3:\n")
print(tuple3)
cat("Predicted class label:", predicted_label3, "\n\n")

# Testing 3RD TIME
tuple1 <- data.frame(Channel=1, Fresh=2787, Milk=1698, Grocery=2510, Frozen=65, Detergents_Paper=477, Delicassen=52)
predicted_label1 <- predict_class_label(tuple1, C50_model)
cat("For tuple1:\n")
print(tuple1)
cat("Predicted class label:", predicted_label1, "\n\n")

tuple2 <- data.frame(Channel=1, Fresh=2799, Milk=1700, Grocery=2520, Frozen=68, Detergents_Paper=480, Delicassen=60)
predicted_label2 <- predict_class_label(tuple2, C50_model)
cat("For tuple2:\n")
print(tuple2)
cat("Predicted class label:", predicted_label2, "\n\n")


tuple3 <- data.frame(Channel=2,  Fresh=2137, Milk=3737, Grocery=19172, Frozen=1274, Detergents_Paper=17120, Delicassen=1059)
predicted_label3 <- predict_class_label(tuple3, C50_model)
cat("For tuple3:\n")
print(tuple3)
cat("Predicted class label:", predicted_label3, "\n\n")

