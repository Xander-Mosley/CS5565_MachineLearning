library(ggplot2)
library(caret)
library(dplyr)
library(pROC)
library(MASS)

setwd("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Final Project/Classification")

mushroom <- read.csv("mushrooms.csv")
# head(mushroom)
# summary(mushroom)
# str(mushroom)
# colSums(is.na(mushroom))

mushroom[, ] <- lapply(mushroom[, ], as.factor)



# for(col in names(mushroom)[-1]){
#   cat("\n====================\n")
#   cat("Variable:", col, "\n")
#   tab <- table(mushroom$class, mushroom[[col]])
#   print(tab)
#   if(any(tab[1,] == 0) | any(tab[2,] == 0)){
#     cat("*** Potential perfect separation detected ***\n")
#   }
# }

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



log_model <- glm(class ~ ., data = train_data, family = binomial)
summary(log_model)

log_probs <- predict(log_model, newdata = test_data, type = "response")
log_pred <- ifelse(log_probs > 0.5, "p", "e")
log_pred <- as.factor(log_pred)

log_cm <- confusionMatrix(log_pred, test_data$class)
log_cm



lda_model <- lda(class ~ ., data = train_data)
summary(lda_model)

lda_probs <- predict(lda_model, newdata = test_data)$posterior[, "p"]
lda_pred <- predict(lda_model, newdata = test_data)
lda_class <- lda_pred$class

lda_cm <- confusionMatrix(lda_class, test_data$class)
lda_cm



plot_conf_matrix <- function(cm_object, title_name) {
  cm_df <- as.data.frame(cm_object$table)
  colnames(cm_df) <- c("Prediction", "Actual", "Freq")
  ggplot(cm_df, aes(x = Actual, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = Freq), 
              color = "white",
              size = 8,
              fontface = "bold") +
    scale_fill_gradient(
      low = "#56B1F7",
      high = "#132B43"
    ) +
    labs(
      title = title_name,
      subtitle = "Confusion Matrix",
      x = "Actual Class",
      y = "Predicted Class",
      fill = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 12
      ),
      axis.text = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

plot_conf_matrix(log_cm, "Logistic Regression")

plot_conf_matrix(lda_cm, "Linear Discriminant Analysis")



roc_log <- roc(test_data$class, log_probs)
roc_lda <- roc(test_data$class, lda_probs)

log_df <- data.frame(
  FPR = 1 - roc_log$specificities,
  TPR = roc_log$sensitivities,
  Model = "Logistic Regression"
)
lda_df <- data.frame(
  FPR = 1 - roc_lda$specificities,
  TPR = roc_lda$sensitivities,
  Model = "LDA"
)
roc_combined <- rbind(log_df, lda_df)

ggplot(roc_combined, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "gray50",
    size = 1
  ) +
  labs(
    title = "ROC Curve Comparison",
    subtitle = "Mushroom Classification Models",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) +
  annotate(
    "text",
    x = 0.65,
    y = 0.2,
    label = paste("Logistic AUC =", round(auc(roc_log), 3)),
    color = "steelblue",
    size = 5
  ) +
  annotate(
    "text",
    x = 0.65,
    y = 0.1,
    label = paste("LDA AUC =", round(auc(roc_lda), 3)),
    color = "tomato",
    size = 5
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



log_coeff <- summary(log_model)$coefficients

importance_df <- data.frame(
  Feature = rownames(log_coeff),
  Importance = abs(log_coeff[, "Estimate"])
)

importance_df <- importance_df %>%
  filter(Feature != "(Intercept)") %>%
  arrange(desc(Importance))

print(importance_df)

top_features <- importance_df %>%
  slice(1:5)

ggplot(top_features,
       aes(x = reorder(Feature, Importance),
           y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 5 Most Important Features",
    subtitle = "Based on logistic regression coefficient magnitude",
    x = "Feature",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = -1),
    plot.subtitle = element_text(hjust = 6)
  )