
# Load necessary libraries
library(caret)

# Read the dataset
data <- read.csv("/mnt/data/output.csv")

# Convert categorical variables
data$Anaemic <- as.factor(ifelse(data$Anaemic == "Yes", 1, 0))

# Split dataset into training (80%) and testing (20%)
set.seed(123)
trainIndex <- createDataPartition(data$Anaemic, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train logistic regression model
model <- glm(Anaemic ~ X.Red.Pixel + X.Green.pixel + X.Blue.pixel, 
             data = trainData, family = binomial)

# Make predictions on test data
predictions <- predict(model, testData, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Compute performance metrics
conf_matrix <- confusionMatrix(factor(predicted_classes), testData$Anaemic)
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
f1_score <- conf_matrix$byClass['F1']

# Save results
results <- data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)
write.csv(results, "/mnt/data/logistic_regression_results.csv", row.names = FALSE)
