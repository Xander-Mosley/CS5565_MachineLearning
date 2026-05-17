library(caret)
library(dplyr)
library(leaps)
library(ggplot2)
library(tidyr)
library(plotly)
library(pls)

setwd("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Final Project/Feature Selection")

bank <- read.csv("bank.csv")
# head(bank)
# summary(bank)
# str(bank)
# colSums(is.na(bank))

bank <- bank %>%
  mutate(
    default = ifelse(default == "yes", 1, 0),
    housing = ifelse(housing == "yes", 1, 0),
    loan    = ifelse(loan == "yes", 1, 0),
    deposit = ifelse(deposit == "yes", 1, 0),
    education = recode(
      education,
      "unknown" = 0,
      "primary" = 1,
      "secondary" = 2,
      "tertiary" = 3
    )
  )
bank$job <- gsub("blue-collar", "bluecollar", bank$job)
bank$job <- gsub("self-employed", "selfemployed", bank$job)
# unique(bank$job)
categorical_vars <- c("job", "marital", "contact", "month", "poutcome")
bank[categorical_vars] <- lapply(bank[categorical_vars], as.factor)
dummy_model <- dummyVars(~ job + marital + contact + month + poutcome,
                         data = bank,
                         fullRank = TRUE)
dummy_data <- predict(dummy_model, newdata = bank)
dummy_data <- as.data.frame(dummy_data)
bank_clean <- bank %>%
  select(-all_of(categorical_vars))
bank <- cbind(bank_clean, dummy_data)

set.seed(0464)
train_index <- createDataPartition(bank$deposit, p = 0.7, list = FALSE)
train_data <- bank[train_index, ]
test_data <- bank[-train_index, ]

feature_names <- names(train_data)[names(train_data) != "deposit"]
full_formula <- as.formula(
  paste("deposit ~", paste(feature_names, collapse = " + "))
)



forward_model <- regsubsets(
  full_formula,
  data = train_data,
  method = "forward",
  nvmax = length(feature_names)
)
forward_summary <- summary(forward_model)
best_forward_size <- which.max(forward_summary$adjr2)
forward_rss <- forward_summary$rss
forward_adj_r2 <- forward_summary$adjr2

forward_which <- as.data.frame(forward_summary$which)
forward_which$Step <- 1:nrow(forward_which)
forward_long <- forward_which %>%
  pivot_longer(cols = -Step,
               names_to = "Variable",
               values_to = "Included")
ggplot(forward_long, aes(x = Step, y = Variable, fill = Included)) +
  geom_tile(color = "grey80") +
  scale_fill_manual(
    values = c("FALSE" = "white", "TRUE" = "black"),
    name = "Variable Selected"
  ) +
  labs(
    title = "Forward Stepwise Selection",
    subtitle = "Feature inclusion over time",
    x = "Model Step (Increasing Complexity)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )



backward_model <- regsubsets(
  full_formula,
  data = train_data,
  method = "backward",
  nvmax = length(feature_names)
)
backward_summary <- summary(backward_model)
best_backward_size <- which.max(backward_summary$adjr2)
backward_rss <- backward_summary$rss
backward_adj_r2 <- backward_summary$adjr2


backward_which <- as.data.frame(backward_summary$which)
backward_which$Step <- 1:nrow(backward_which)
backward_long <- backward_which %>%
  pivot_longer(cols = -Step,
               names_to = "Variable",
               values_to = "Included")
ggplot(backward_long, aes(x = Step, y = Variable, fill = Included)) +
  geom_tile(color = "grey80") +
  scale_fill_manual(
    values = c("FALSE" = "white", "TRUE" = "black"),
    name = "Variable Selected"
  ) +
  scale_x_reverse() +
  labs(
    title = "Backward Stepwise Selection",
    subtitle = "Feature removal over time",
    x = "Model Step (Decreasing Complexity)",
    y = "Variables"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )



max_len <- max(length(forward_adj_r2), length(backward_adj_r2))
plot_data <- data.frame(
  Num_Variables = 1:max_len,
  Forward = c(forward_adj_r2, rep(NA, max_len - length(forward_adj_r2))),
  Backward = c(backward_adj_r2, rep(NA, max_len - length(backward_adj_r2)))
)
plot_long <- plot_data %>%
  pivot_longer(cols = c("Forward", "Backward"),
               names_to = "Method",
               values_to = "Adj_R2")
ggplot(plot_long, aes(x = Num_Variables, y = Adj_R2, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Forward vs Backward Stepwise Selection",
    subtitle = "Model performance measured using Adjusted R²",
    x = "Number of Predictors",
    y = "Adjusted R²",
    color = "Selection Method"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Forward" = "steelblue", "Backward" = "tomato")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

rss_plot_long <- data.frame(
  Num_Variables = 1:max_len,
  RSS = c(forward_summary$rss, backward_summary$rss),
  Method = rep(c("Forward", "Backward"),
               each = max_len)
)
ggplot(rss_plot_long, aes(x = Num_Variables, y = RSS, color = Method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Forward vs Backward Stepwise Selection",
    subtitle = "Error measured using Residual Sum of Squares (RSS)",
    x = "Number of Predictors",
    y = "RSS",
    color = "Selection Method"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Forward" = "steelblue", "Backward" = "tomato")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

forward_gain <- c(NA, diff(forward_summary$adjr2))
backward_gain <- c(NA, diff(backward_summary$adjr2))
gain_plot_long <- data.frame(
  Num_Variables = 1:max_len,
  Adj_R2_Gain = c(forward_gain, backward_gain),
  Method = rep(c("Forward", "Backward"),
               each = max_len)
)
ggplot(gain_plot_long, aes(x = Num_Variables, y = Adj_R2_Gain, color = Method)) +
  geom_hline(yintercept = 0.005, linetype = "dashed", size = 1, color = "gray50") +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Forward vs Backward Stepwise Selection",
    subtitle = "Marginal change in Adjusted R² per added variable",
    x = "Number of Predictors",
    y = expression(Delta~Adjusted~R^2),
    color = "Selection Method"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Forward" = "steelblue", "Backward" = "tomato")) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )



pca_data <- train_data %>% select(-deposit)
pca_scaled <- scale(pca_data)
pca_result <- prcomp(pca_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

eigen_scree_df <- data.frame(
  PC = 1:length(pca_result$sdev),
  eigen = pca_result$sdev
)
ggplot(eigen_scree_df, aes(x = PC, y = eigen)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1, color = "red") +
  geom_line(group = 1, size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "PCA Scree Plot",
    x = "Number of Principal Components",
    y = "Eigenvalue"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

pve <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
pve_scree_df <- data.frame(
  PC = 1:length(pve),
  Variance_Explained = pve
)
ggplot(pve_scree_df, aes(x = PC, y = Variance_Explained)) +
  geom_line(group = 1, size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "PCA Scree Plot",
    x = "Number of Principal Components",
    y = "Percentage of Variance Explained"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



cum_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cum_var_df <- data.frame(
  PC = 1:length(pve),
  Cumulative_Variance = cumsum(pve)
)
ggplot(cum_var_df, aes(x = PC, y = Cumulative_Variance)) +
  geom_hline(yintercept = 0.90, linetype = "dashed", linewidth = 1, color = "red") +
  geom_hline(yintercept = 0.80, linetype = "dashed", linewidth = 1, color = "orange") +
  geom_hline(yintercept = 0.70, linetype = "dashed", linewidth = 1, color = "yellow") +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Cumulative Explained Variance (PCA)",
    x = "Number of Principal Components",
    y = "Cumulative Variance Explained"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



num_components <- which(cum_var >= 0.70)[1]
pca_final <- as.data.frame(pca_result$x[, 1:num_components])
pca_final$deposit <- train_data$deposit



pca_scores <- as.data.frame(pca_result$x)
pca_scores$deposit <- train_data$deposit
pca_scores$deposit <- factor(
  pca_scores$deposit,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = deposit)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "tomato")) +
  labs(
    title = "PCA Projection Colored by Deposit Outcome",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



train_data$deposit <- as.numeric(train_data$deposit)
pcr_model <- pcr(
  deposit ~ .,
  data = train_data,
  scale = TRUE,
  validation = "CV"
)
summary(pcr_model)

pcr_validation <- RMSEP(pcr_model)
print(pcr_validation)
rmse_df <- data.frame(
  Components = 1:length(pcr_validation$val[1,1,-1]),
  RMSE = as.vector(pcr_validation$val[1,1,-1])
)
best_components <- rmse_df$Components[which.min(rmse_df$RMSE)]
cat("Optimal number of principal components: ", best_components, "\n")

ggplot(rmse_df, aes(x = Components, y = RMSE)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  labs(
    title = "PCR Error by Number of Components",
    subtitle = "Performed using cross validation",
    x = "Number of Principal Components",
    y = "Root Mean Squared Error"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

predictions <- predict(
  pcr_model,
  newdata = test_data,
  ncomp = best_components
)
predicted_class <- ifelse(predictions > 0.5, 1, 0)
actual_class <- test_data$deposit
table(Predicted = predicted_class, Actual = actual_class)
