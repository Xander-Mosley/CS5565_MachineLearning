# Load libraries
library(MASS)
library(caret)
library(dplyr)
library(GGally)
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



lda.fit <- lda(
  Attrition ~ Age + MonthlyIncome + DistanceFromHome +
    YearsAtCompany + OverTime,
  data = train_data
)

print(lda.fit)

plot(lda.fit)

ggpairs(
  train_data,
  columns = c("Age",
              "MonthlyIncome",
              "DistanceFromHome",
              "YearsAtCompany",
              "OverTime"),
  aes(color = Attrition, alpha = 0.6)
)



lda.pred <- predict(lda.fit, test_data)


cm_lda <- confusionMatrix(
  lda.pred$class,
  test_data$Attrition
)
cm_lda

cm_table_lda <- as.data.frame(cm_lda$table)
ggplot(cm_table_lda, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  labs(
    title = "LDA Confusion Matrix Heatmap"
  )