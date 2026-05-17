# Load libraries
library(class)      # KNN
library(e1071)      # Naive Bayes
library(caret)
library(dplyr)
library(ggplot2)

# Load CSV file
hr_data <- read.csv("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/PiecewiseModels-DecisionTrees-SVMs/WA_Fn-UseC_-HR-Employee-Attrition.csv")

hr_data <- hr_data %>%
  dplyr::select(
    Attrition,
    Age,
    MonthlyIncome,
    DistanceFromHome,
    YearsAtCompany,
    OverTime
  )

hr_data$Attrition <- as.factor(hr_data$Attrition)
hr_data$OverTime <- ifelse(hr_data$OverTime == "Yes", 1, 0)

set.seed(0464)

train_index <- createDataPartition(hr_data$Attrition, p = 0.7, list = FALSE)
train_data <- hr_data[train_index, ]
test_data <- hr_data[-train_index, ]



train_y <- train_data$Attrition
test_y <- test_data$Attrition

train_x <- train_data %>%
  dplyr::select(Age,
         MonthlyIncome,
         DistanceFromHome,
         YearsAtCompany,
         OverTime)
test_x <- test_data %>%
  dplyr::select(Age,
         MonthlyIncome,
         DistanceFromHome,
         YearsAtCompany,
         OverTime)

# Standardize data
preProcValues <- preProcess(
  train_x,
  method = c("center", "scale")
)
train_x_scaled <- predict(preProcValues, train_x)
test_x_scaled <- predict(preProcValues, test_x)


control <- trainControl(
  method = "cv",
  number = 10
)
knn_cv_model <- train(
  x = train_x_scaled,
  y = train_y,
  method = "knn",
  tuneGrid = data.frame(k = c(7,9,11,13,15,17,19,21,23,25)),
  trControl = control
)

print(knn_cv_model)
ggplot(knn_cv_model) +
         geom_line(linewidth = 1.2) +
         geom_point(size = 3) +
         labs(
           title = "KNN Accuracy vs K Value",
           x = "K Value",
           y = "Accuracy (10 Fold Cross-Validation)")

best_k <- knn_cv_model$bestTune$k
final_pred <- knn(
  train = train_x_scaled,
  test = test_x_scaled,
  cl = train_y,
  k = best_k
)
cm_knn <- confusionMatrix(final_pred, test_y)
cm_knn

cm_table <- as.data.frame(cm_knn$table)
ggplot(cm_table, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  labs(
    title = "KNN Confusion Matrix Heatmap"
  )



nb_model <- naiveBayes(
  Attrition ~ Age + MonthlyIncome +
    DistanceFromHome + YearsAtCompany + OverTime,
  data = train_data
)

print(nb_model)

nb_pred <- predict(
  nb_model,
  test_data
)


cm_nb <- confusionMatrix(nb_pred, test_data$Attrition)
cm_nb

cm_table_nb <- as.data.frame(cm_nb$table)

ggplot(cm_table_nb, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  labs(
    title = "Naive Bayes Confusion Matrix Heatmap"
  )


nb_probs <- predict(
  nb_model,
  test_data,
  type = "raw"
)

prob_df <- data.frame(
  Prob_Yes = nb_probs[, "Yes"]
)

ggplot(prob_df, aes(x = Prob_Yes)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    alpha = 0.7
  ) +
  labs(
    title = "Naive Bayes Predicted Probability of Attrition",
    x = "Probability of Attrition = Yes"
  )