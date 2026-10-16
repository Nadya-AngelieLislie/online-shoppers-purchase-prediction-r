# Load the libraries
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(car)

# Read the data from a CSV file
data <- read.csv('online_shoppers_intention.csv')

# Explore the dataset
head(data)
str(data)

######## DATA CLEANING ########

# Checking missing values
sum(is.na(data)) #data has no NA

# Clean column names to make them consistent
data <- data %>% clean_names()

# Changing Data Types by Convert categorical variables to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Checking Duplicate data 
duplicate_rows <- duplicated(data) 
duplicate_rows_data <- data[duplicate_rows, ] 
print(duplicate_rows_data) 
# It is not necessary to delete all of these data because 
# it is not a duplicate and we don't have unique data.

# Checking Outlier
# Focus on numerical columns only 
numeric_columns <- data %>% 
  select(where(is.numeric)) 

# Detect Outliers Using IQR Method 
detect_outliers <- function(x) { 
  Q1 <- quantile(x, 0.25, na.rm = TRUE) 
  Q3 <- quantile(x, 0.75, na.rm = TRUE) 
  IQR_value <- Q3 - Q1 
  lower_bound <- Q1 - 1.5 * IQR_value 
  upper_bound <- Q3 + 1.5 * IQR_value 
  outliers <- x[x < lower_bound | x > upper_bound] 
  list( 
    lower_bound = lower_bound, 
    upper_bound = upper_bound, 
    outliers = outliers, 
    count = length(outliers) 
  )
} 


# Checking Outliers for every numerical column 
outlier_results <- lapply(numeric_columns, detect_outliers) 

# Outliers and counts in every column 
for (col_name in names(outlier_results)) { 
  cat("\nOutliers for", col_name, ": ") 
  cat("outliers:", outlier_results[[col_name]]$count)
}

# See Outliers from Boxplot
numeric_columns %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Checking Outliers Using Boxplot") 

# Data Summary 
summary(data) 

######## EXPLORATORY DATA ANALYSIS #######

# Data Visualization

#### Distribution of Product Related Duration ####
ggplot(data, aes(x = product_related_duration)) +
  geom_histogram(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Product Related Duration", x = "Product Related Duration", y = "Count")

#### Distribution of Administrative Duration ####
ggplot(data, aes(x = administrative_duration)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Administrative Duration", x = "Administrative Duration", y = "Count")

#### Distribution of Exit Rates ####
ggplot(data, aes(x = exit_rates)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Exit Rates", x = "Exit Rates", y = "Count")

#### Count of Visitor Type ####
ggplot(data, aes(x = visitor_type)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Count of Visitor Type", x = "Visitor Type", y = "Count")

#### Count of Visits by Month ####
unique(data$month)
# To make the plot shows month in sorted from February to December
data$month <- factor(data$month, levels = c("Feb", "Mar", "Apr", "May", "June", 
                                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
ggplot(data, aes(x = month)) +
  geom_bar(fill = "lightgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Count of Visits by Month", x = "Month", y = "Count")

#### Distribution of Revenue ####
ggplot(data, aes(x = revenue)) +
  geom_bar(fill = "pink", alpha = 0.75) +
  theme_minimal() +
  labs(title = "Distribution of Revenue", x = "Revenue", y = "Count")

#### Comparison of Relationship Between Revenue by Visitor Type and by Weekend ####
revenue_vs_visitor <- ggplot(data, aes(x = revenue, fill = visitor_type)) +
  geom_bar(position = "stack" , alpha = 0.75) +
  theme_minimal() +
  labs(title = "Revenue by Visitor Type", x = "Revenue", y = "Count")

revenue_vs_weekend <- ggplot(data, aes(x = revenue, fill = weekend)) +
  geom_bar(position = "stack" , alpha = 0.75) +
  theme_minimal() +
  labs(title = "Revenue by Weekend Status", x = "Revenue", y = "Count")

library(gridExtra)
grid.arrange(revenue_vs_visitor, revenue_vs_weekend, nrow = 1)

#### Relationship Between Revenue and Month ####
ggplot(data, aes(x = revenue, fill = month)) +
  geom_bar(position = "dodge", alpha = 0.75) +
  theme_minimal() +
  labs(title = "Revenue by Month", x = "Month", y = "Count")

#### Average Revenue by Month ####
ggplot(data, aes(x = month, y = revenue, group = 1)) +
  stat_summary(fun = mean, geom = "line", color = "darkgreen", size = 1) +
  theme_minimal() +
  labs(title = "Average Revenue by Month", x = "Month", y = "Average Revenue")

#### Relationship Between Revenue and and Special Day ####
ggplot(data, aes(x = special_day, fill = revenue)) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Revenue by Special Day", x = "Special Day", y = "Count")

#### Proportion of Visitor Type by Weekend ####
visitor_weekend <- data %>% 
  count(visitor_type, weekend) %>% 
  mutate(Proportion = n / sum(n)) # Adding new column named Proportion

ggplot(visitor_weekend, aes(x = visitor_type, y = Proportion, fill = weekend)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Proportion of Visitor Type by Weekend", x = "Visitor Type", y = "Proportion", fill = "Weekend")

#### Correlation between variables ####
cor_matrix <- cor(numeric_columns)

# Using ggplot
# Reshape the cor_matrix into a long-format data frame
library(reshape2)
cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Using corplot
library(corrplot)
corrplot(cor_matrix, method = "square", type = "lower", diag = TRUE)

#### Relationship between Exit Rates and Bounce Rates by Revenue ####
ggplot(data, aes(x = bounce_rates, y = exit_rates)) + 
  geom_point(aes(color = revenue)) + 
  theme_light() + 
  labs(title = "Relationship between Exit Rates and Bounce Rates", x = "Bounce Rates", y = "Exit Rates") 

#### Relationship between Product Related and Product Related Duration ####
ggplot(data, aes(x = product_related, y = product_related_duration)) + 
  geom_point(aes(color = revenue)) + 
  theme_light() + 
  labs(title = "Relationship between Product Related and Product Related Duration", x = "Product Related", y = "Product Related Duration") 


#### Trend Line for Revenue Based on Months ####
trend_revenue_vs_month <- data.frame(table(data$month, data$revenue))

names(trend_revenue_vs_month) <- c("months", "revenue", "frequency")

trend_revenue_vs_month$months <- factor(trend_revenue_vs_month$months, 
                                        levels = c("Feb", "Mar", "Apr", "May", "June", 
                                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(trend_revenue_vs_month, aes(x = months, y = frequency)) + 
  geom_line(aes(color = revenue, group = revenue), lwd = 1) + 
  geom_point(aes(color = revenue, group = revenue, size = 0.1), show.legend = FALSE) + 
  scale_color_manual(name = "Revenue", values = c("pink", "lightgreen")) +
  theme_light() + 
  labs(title = "Trend Line for Revenue Based on Months", x = "Month", y = "Visitors")
  
#### Trend Line for Visitor Type Based on Months ####
trend_visitor_type_vs_month <- data.frame(table(data$visitor_type, data$month))

names(trend_visitor_type_vs_month) <- c("visitor_type", "month", "frequency")

trend_visitor_type_vs_month$month <- factor(trend_visitor_type_vs_month$month, 
                                            levels = c("Feb", "Mar", "Apr", "May", "June", 
                                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data = trend_visitor_type_vs_month, mapping = aes(x = month, y = frequency)) + 
  geom_line(mapping = aes(color = visitor_type, group = visitor_type), lwd = 1) + 
  geom_point(mapping = aes(color = visitor_type, group = visitor_type, size = 0.1), show.legend = FALSE) + 
  theme_light() + 
  scale_color_manual(name = "Visitor Type", values = c("lightblue", "pink", "lightgreen")) +
  labs(title = "Trend Line for Visitor Type Based on Months", x = "Months", y = "Visitors")


######## SUPERVISED LEARNING MODELS ########
# Check type of variables
str(data)

# Check no. of null values
sum(is.na(data))

# Converting non-numerical variables to factors
data$month <- as.factor(data$month)
data$visitor_type <- as.factor(data$visitor_type)
data$revenue <- as.factor(data$revenue)

# Normalize numerical columns
num_cols <- c("administrative", "administrative_duration", "informational", "informational_duration", 
              "product_related", "product_related_duration", "bounce_rates", "exit_rates", "page_values", "special_day")
data[num_cols] <- scale(data[num_cols])
data

# Splitting data to training and testing data (70% & 30% respectively) using seed = 123
library(caret)
set.seed(123)
trainIdx <- createDataPartition(data$revenue, p = 0.7, list = FALSE)
train_data <- data[trainIdx,]
test_data <- data[-trainIdx,]


#### Random Forest ####

library(randomForest)

# Building rf model
rf_model <- randomForest(revenue ~ . , data = train_data, ntree = 100, importance = TRUE)

# Making predictions on test data
prediction_rf <- predict(rf_model, test_data)

# Creating a confusion matrix to see accuracy of model
cm_rf <- confusionMatrix(prediction_rf, test_data$revenue); cm_rf

# Model is used to predict whether a customer ended a session with a transaction (making a purchase).
# It uses input features to identify patterns and predict the likelihood of a transaction.

important <- importance(rf_model); important
varImpPlot(rf_model)

#### Logistic Regression ####

log_model <- glm(revenue ~., data = train_data, family = binomial)

prediction_log <- predict(log_model, test_data, type = 'response')
class_prediction <- ifelse(prediction_log > 0.5, 'TRUE', 'FALSE')

cm_log <- confusionMatrix(as.factor(class_prediction), test_data$revenue); cm_log

#### Generalized Linear Model ####
control <- trainControl(method = "cv", number = 10)

set.seed(123)
glm_model <- train(revenue ~ ., 
                   data = train_data, 
                   method = "glm", 
                   family = "binomial",
                   trControl = control)

print(glm_model)

#### Linear Regression ####
lin_model <- lm(product_related_duration ~., data = train_data)

prediction_lin <- predict(lin_model, test_data)

mse <- mean((prediction_lin - test_data$product_related_duration)^2)
print(paste("MSE of the Linear Regression Model:", mse))

plot(test_data$product_related_duration, prediction_lin,
     main = "Linear Regression: Actual vs Predicted",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 19, col = "blue")
abline(0, 1, col = "red")

#### Decision Tree ####

library(rpart)
tree_model <- rpart(revenue ~ ., data = train_data, method = 'class')

library(rpart.plot)
rpart.plot(tree_model, main = 'Decision Tree for revenue')

prediction_tree <- predict(tree_model, test_data, type = 'class')

cm_tree <- confusionMatrix(prediction_tree, test_data$revenue); cm_tree

#### SVM (Support Vector Machines) ####

library(e1071)

classifier <- svm(revenue ~., data = train_data, type = 'C-classification', kernel = 'linear')

prediction_svm <- predict(classifier, test_data)

cm_svm <- confusionMatrix(prediction_svm, test_data$revenue); cm_svm

#### Naive-Bayes Classifier ####

library(caTools)

nb_model <- naiveBayes(revenue ~ ., data = train_data)

prediction_nb <- predict(nb_model, test_data)

cm_nb <- confusionMatrix(test_data$revenue, prediction_nb); cm_nb

#### KNN Algorithm ####

library(class)

# Normalize the data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_normalized <- as.data.frame(lapply(data[, num_cols], normalize))

data_normalized$revenue <- data$revenue
data_normalized$revenue <- as.factor(data_normalized$revenue)

train_indices <- createDataPartition(data_normalized$revenue, p = 0.7, list = FALSE)

train_data2 <- data_normalized[train_indices, ]
test_data2 <- data_normalized[-train_indices, ]

train_features <- train_data2[, -ncol(train_data2)]  # All columns except revenue
train_labels <- train_data2$revenue

test_features <- test_data2[, -ncol(test_data2)]
test_labels <- test_data2$revenue

# Prediction & Confusion matrix
k <- 15
predictions <- knn(train_features, test_features, train_labels, k = k)

cm_knn <- confusionMatrix(predictions, test_labels); cm_knn

# Finding optimal k value
accuracy_values <- c()

for (k in 1:20) {
  predictions <- knn(train_features, test_features, train_labels, k = k)
  cm <- confusionMatrix(predictions, test_labels)
  accuracy_values <- c(accuracy_values, cm$overall["Accuracy"])
}

# Plot accuracy vs. k
plot(1:20, accuracy_values, type = "b", xlab = "k", ylab = "Accuracy", main = "Optimal k Selection")



#### Comparing Supervised Learning Models ####
acc_rf <- cm_rf$overall[1]*100; acc_rf
acc_log <- cm_log$overall[1]*100; acc_log
acc_tree <- cm_tree$overall[1]*100; acc_tree
acc_svm <- cm_svm$overall[1]*100; acc_svm
acc_nb <- cm_nb$overall[1]*100; acc_nb
acc_knn <- cm_knn$overall[1]*100; acc_knn

column_name <- c('Random.Forest', 'Logistic.Regression', 'Decision.Tree', 'SVM', "Naive.Bayes", 'KNN')
acc_vector <- c(acc_rf, acc_log, acc_tree, acc_svm, acc_nb, acc_knn)
acc_vector <- round(acc_vector, digits = 2); acc_vector

acc_data <- data.frame(t(acc_vector)); acc_data
colnames(acc_data) <- column_name
rownames(acc_data) <- 'Accuracy'

library(knitr)
kable(acc_data, align = 'c')

