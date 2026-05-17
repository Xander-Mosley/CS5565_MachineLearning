# Load libraries
library(ggplot2)
library(dplyr)
library(caret)

# Load CSV file
hr_data <- read.csv("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/PiecewiseModels-DecisionTrees-SVMs/WA_Fn-UseC_-HR-Employee-Attrition.csv")

# # View structure
# str(hr_data)
# # First few rows
# head(hr_data)

hr_data <- hr_data %>%
  dplyr::select(
    Attrition,
    Age,
    MonthlyIncome,
    DistanceFromHome,
    YearsAtCompany,
    OverTime
  )

# Convert Attrition to binary
hr_data$Attrition <- ifelse(hr_data$Attrition == "Yes", 1, 0)
# Convert Overtime to binary
hr_data$OverTime <- ifelse(hr_data$OverTime == "Yes", 1, 0)

# summary(hr_data)

set.seed(0464)

train_index <- createDataPartition(hr_data$Attrition, p = 0.7, list = FALSE)
train_data <- hr_data[train_index, ]
test_data <- hr_data[-train_index, ]



ggplot(hr_data, aes(x = Age, fill = factor(Attrition))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(
    title = "Age Distribution by Attrition",
    fill = "Attrition"
  )
ggplot(hr_data, aes(x = MonthlyIncome, fill = factor(Attrition))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(
    title = "Monthly Income Distribution by Attrition",
    fill = "Attrition"
  )
ggplot(hr_data, aes(x = DistanceFromHome, fill = factor(Attrition))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(
    title = "Distance From Home Distribution by Attrition",
    fill = "Attrition"
  )
ggplot(hr_data, aes(x = YearsAtCompany, fill = factor(Attrition))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(
    title = "Years at Company by Attrition",
    fill = "Attrition"
  )
ggplot(hr_data, aes(x = factor(OverTime), fill = factor(Attrition))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Overtime vs Attrition",
    x = "Overtime",
    fill = "Attrition"
  )



plot_logistic_curve <- function(var_name, color_name) {
  formula_text <- as.formula(
    paste("Attrition ~", var_name)
  )
  model <- glm(
    formula_text,
    data = train_data,
    family = binomial
  )
  range_df <- data.frame(
    seq(
      min(train_data[[var_name]]),
      max(train_data[[var_name]]),
      length.out = 100
    )
  )
  names(range_df) <- var_name
  range_df$prob <- predict(
    model,
    newdata = range_df,
    type = "response"
  )
  ggplot(train_data, aes_string(x = var_name, y = "Attrition")) +
    geom_jitter(height = 0.05, alpha = 0.4) +
    geom_line(
      data = range_df,
      aes_string(x = var_name, y = "prob"),
      color = color_name,
      linewidth = 1.2
    ) +
    labs(
      title = paste("Logistic Regression:", var_name),
      y = "Probability of Attrition"
    )
}

plot_logistic_curve("Age", "blue")
plot_logistic_curve("MonthlyIncome", "red")
plot_logistic_curve("DistanceFromHome", "green")
plot_logistic_curve("YearsAtCompany", "purple")


overtime_model <- glm(
  Attrition ~ OverTime,
  data = train_data,
  family = binomial
)
overtime_df <- data.frame(
  OverTime = c(0, 1)
)
overtime_df$prob <- predict(
  overtime_model,
  newdata = overtime_df,
  type = "response"
)
ggplot(overtime_df, aes(x = factor(OverTime), y = prob)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "Predicted Attrition Probability by Overtime",
    x = "OverTime (0 = No, 1 = Yes)",
    y = "Probability of Attrition"
  )



log_model <- glm(
  Attrition ~ Age + MonthlyIncome + DistanceFromHome + YearsAtCompany + OverTime,
  data = train_data,
  family = binomial
)

summary(log_model)

prob_predictions <- predict(
  log_model,
  test_data,
  type = "response"
)
class_predictions <- ifelse(prob_predictions > 0.5, 1, 0)


# Store confusion matrix
cm_log <- confusionMatrix(
  factor(class_predictions),
  factor(test_data$Attrition)
)
cm_log

# Convert to dataframe for plotting
cm_table_log <- as.data.frame(cm_log$table)
# Plot heatmap
ggplot(cm_table_log, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  labs(
    title = "Logistic Regression Confusion Matrix Heatmap"
  )