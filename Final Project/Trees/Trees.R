library(dplyr)
library(caret)
library(tree)
library(randomForest)
library(gbm)
library(e1071)
library(ggplot2)

setwd("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Final Project/Trees")

mushroom <- read.csv("mushrooms.csv")

mushroom[, ] <- lapply(mushroom[, ], as.factor)

mushroom_reduced <- mushroom %>%
  dplyr::select(-odor, -cap.shape, -cap.surface, -cap.color, 
         -gill.color, -stalk.root, -stalk.color.above.ring,
         -stalk.color.below.ring, -veil.color, -ring.number,
         -ring.type, -spore.print.color, -population,
         -habitat
  )

set.seed(0464)
train_index <- createDataPartition(mushroom_reduced$class, p = 0.7, list = FALSE)
train_data <- mushroom_reduced[train_index, ]
test_data <- mushroom_reduced[-train_index, ]

nzv <- nearZeroVar(train_data)
if(length(nzv) > 0){
  train_data <- train_data[, -nzv]
  test_data  <- test_data[, -nzv]
}



tree_model <- tree(class ~ ., data = train_data)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, test_data, type = "class")
tree_cm <- confusionMatrix(tree_pred, test_data$class)
tree_cm



cv_tree <- cv.tree(tree_model, FUN = prune.misclass)

prune_df <- data.frame(
  TreeSize = cv_tree$size,
  MisclassificationError = cv_tree$dev
)
best_size <- prune_df$TreeSize[which.min(prune_df$MisclassificationError)]
best_error <- min(prune_df$MisclassificationError)
ggplot(prune_df, aes(x = TreeSize, y = MisclassificationError)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 4, color = "darkblue") +
  labs(
    title = "Tree Pruning Results",
    subtitle = "Performed using cross validation",
    x = "Tree Size",
    y = "Misclassification Error"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

pruned_tree <- prune.misclass(tree_model, best = 4)
plot(pruned_tree)
text(pruned_tree, pretty = 0)

pruned_pred <- predict(pruned_tree, test_data, type = "class")
pt_cm <- confusionMatrix(pruned_pred, test_data$class)
pt_cm



rf_model <- randomForest(
  class ~ .,
  data = train_data,
  importance = TRUE,
  ntree = 300
)
print(rf_model)

rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  Variable = rownames(rf_importance),
  MeanDecreaseAccuracy = rf_importance[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini = rf_importance[, "MeanDecreaseGini"]
)
ggplot(
  rf_importance_df %>%
    arrange(MeanDecreaseAccuracy),
  aes(
    x = reorder(Variable, MeanDecreaseAccuracy),
    y = MeanDecreaseAccuracy
  )
) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance",
    subtitle = "Mean Decrease Accuracy",
    x = "Predictor Variables",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )
ggplot(
  rf_importance_df %>%
    arrange(MeanDecreaseGini),
  aes(
    x = reorder(Variable, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col(fill = "darkgreen", width = 0.7) +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance",
    subtitle = "Mean Decrease Gini",
    x = "Predictor Variables",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

rf_pred <- predict(rf_model, test_data)
rf_cm <- confusionMatrix(rf_pred, test_data$class)
rf_cm



train_boost <- train_data
test_boost <- test_data
train_boost$class <- ifelse(train_boost$class == "p", 1, 0)
test_boost$class <- ifelse(test_boost$class == "p", 1, 0)
boost_model <- gbm(
  class ~ .,
  data = train_boost,
  distribution = "bernoulli",
  n.trees = 500,
  interaction.depth = 3,
  shrinkage = 0.01,
  verbose = FALSE
)
importance <- summary(boost_model, plotit = FALSE)

ggplot(importance, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Boosting Variable Importance",
    x = "Predictor",
    y = "Relative Influence"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

boost_prob <- predict(
  boost_model,
  test_boost,
  n.trees = 500,
  type = "response"
)
boost_pred <- ifelse(boost_prob > 0.5, 1, 0)
boost_cm <- confusionMatrix(
  as.factor(boost_pred),
  as.factor(test_boost$class)
)
boost_cm



svm_linear <- svm(
  class ~ .,
  data = train_data,
  kernel = "linear",
  cost = 1
)
summary(svm_linear)

linear_pred <- predict(svm_linear, test_data)
linear_cm <- confusionMatrix(
  linear_pred,
  test_data$class
)
linear_cm



svm_radial <- svm(
  class ~ .,
  data = train_data,
  kernel = "radial",
  cost = 1,
  gamma = 0.1
)
summary(svm_radial)

radial_pred <- predict(svm_radial, test_data)
radial_cm <- confusionMatrix(
  radial_pred,
  test_data$class
)
radial_cm



plot_conf_matrix <- function(cm, title_name){
  cm_table <- as.data.frame(cm$table)
  ggplot(cm_table, aes(Prediction, Reference, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(
      title = title_name,
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
}
plot_conf_matrix(tree_cm, "Classification Tree Confusion Matrix")
plot_conf_matrix(pt_cm, "Pruned Tree Confusion Matrix")
plot_conf_matrix(rf_cm, "Random Forest Confusion Matrix")
plot_conf_matrix(boost_cm, "Boosting Confusion Matrix")
plot_conf_matrix(linear_cm, "Linear SVM Confusion Matrix")
plot_conf_matrix(radial_cm, "Radial SVM Confusion Matrix")
