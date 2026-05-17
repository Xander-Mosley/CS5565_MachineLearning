library(GGally)
library(caret)
library(ggplot2)
library(dplyr)
library(Metrics)

setwd("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Final Project/Regression")

boston <- read.csv("boston.csv")
# head(boston)
# summary(boston)
# str(boston)
# colSums(is.na(boston))
# ggpairs(boston)

reg_data <- boston %>%
  dplyr::select(MEDV, LSTAT, RM)

set.seed(0464)
train_index <- createDataPartition(reg_data$MEDV, p = 0.7, list = FALSE)
train_data <- reg_data[train_index, ]
test_data <- reg_data[-train_index, ]

train_data$Dataset <- "Training"
test_data$Dataset <- "Testing"
plot_data <- rbind(train_data, test_data)



linear_model_LSTAT <- lm(MEDV ~ LSTAT, data = train_data)
summary(linear_model_LSTAT)

linear_pred_LSTAT <- predict(linear_model_LSTAT, test_data)
linear_rmse_LSTAT <- rmse(test_data$MEDV, linear_pred_LSTAT)
linear_r2_LSTAT <- cor(test_data$MEDV, linear_pred_LSTAT)^2
cat("Linear Regression RMSE: ", linear_rmse_LSTAT, "\n")
cat("Linear Regression R²: ", linear_r2_LSTAT, "\n")

ggplot(plot_data, aes(x = LSTAT, y = MEDV)) +
  geom_point(
    data = subset(plot_data, Dataset == "Training"),
    color = "lightblue",
    alpha = 0.5,
    size = 2.5
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Testing"),
    color = "darkblue",
    alpha = 0.9,
    size = 2.5
  ) +
  stat_smooth(
    data = train_data,
    method = "lm",
    formula = y ~ poly(x, 1),
    color = "red",
    linewidth = 1.2,
    se = TRUE
  ) +
  labs(
    title = "MEDV vs LSTAT with Training/Test Split",
    subtitle = "Linear Regression Model Fit on Training Data",
    x = "LSTAT (% Lower Status Population) [%]",
    y = "MEDV (Median Value of Homes) [$1000s]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

linear_model_RM <- lm(MEDV ~ RM, data = train_data)
summary(linear_model_RM)

linear_pred_RM <- predict(linear_model_RM, test_data)
linear_rmse_RM <- rmse(test_data$MEDV, linear_pred_RM)
linear_r2_RM <- cor(test_data$MEDV, linear_pred_RM)^2
cat("Linear Regression RMSE: ", linear_rmse_RM, "\n")
cat("Linear Regression R²: ", linear_r2_RM, "\n")

ggplot(plot_data, aes(x = RM, y = MEDV)) +
  geom_point(
    data = subset(plot_data, Dataset == "Training"),
    color = "lightblue",
    alpha = 0.5,
    size = 2.5
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Testing"),
    color = "darkblue",
    alpha = 0.9,
    size = 2.5
  ) +
  stat_smooth(
    data = train_data,
    method = "lm",
    formula = y ~ poly(x, 1),
    color = "red",
    linewidth = 1.2,
    se = TRUE
  ) +
  labs(
    title = "MEDV vs RM with Training/Test Split",
    subtitle = "Linear Regression Model Fit on Training Data",
    x = "RM (Average Number of Rooms)",
    y = "MEDV (Median Value of Homes) [$1000s]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



poly_model_LSTAT <- lm(MEDV ~ poly(LSTAT, 2), data = train_data)
summary(poly_model_LSTAT)

poly_pred_LSTAT <- predict(poly_model_LSTAT, test_data)
poly_rmse_LSTAT <- rmse(test_data$MEDV, poly_pred_LSTAT)
poly_r2_LSTAT <- cor(test_data$MEDV, poly_pred_LSTAT)^2
cat("Polynomial Regression RMSE: ", poly_rmse_LSTAT, "\n")
cat("Polynomial Regression R²: ", poly_r2_LSTAT, "\n")

ggplot(plot_data, aes(x = LSTAT, y = MEDV)) +
  geom_point(
    data = subset(plot_data, Dataset == "Training"),
    color = "lightblue",
    alpha = 0.5,
    size = 2.5
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Testing"),
    color = "darkblue",
    alpha = 0.9,
    size = 2.5
  ) +
  stat_smooth(
    data = train_data,
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "red",
    linewidth = 1.2,
    se = TRUE
  ) +
  labs(
    title = "MEDV vs LSTAT with Training/Test Split",
    subtitle = "Polynomial Regression Model Fit on Training Data",
    x = "LSTAT (% Lower Status Population) [%]",
    y = "MEDV (Median Value of Homes) [$1000s]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

poly_model_RM <- lm(MEDV ~ poly(RM, 2), data = train_data)
summary(poly_model_RM)

poly_pred_RM <- predict(poly_model_RM, test_data)
poly_rmse_RM <- rmse(test_data$MEDV, poly_pred_RM)
poly_r2_RM <- cor(test_data$MEDV, poly_pred_RM)^2
cat("Polynomial Regression RMSE: ", poly_rmse_RM, "\n")
cat("Polynomial Regression R²: ", poly_r2_RM, "\n")

ggplot(plot_data, aes(x = RM, y = MEDV)) +
  geom_point(
    data = subset(plot_data, Dataset == "Training"),
    color = "lightblue",
    alpha = 0.5,
    size = 2.5
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Testing"),
    color = "darkblue",
    alpha = 0.9,
    size = 2.5
  ) +
  stat_smooth(
    data = train_data,
    method = "lm",
    formula = y ~ poly(x, 2),
    color = "red",
    linewidth = 1.2,
    se = TRUE
  ) +
  labs(
    title = "MEDV vs RM with Training/Test Split",
    subtitle = "Polynomial Regression Model Fit on Training Data",
    x = "RM (Average Number of Rooms)",
    y = "MEDV (Median Value of Homes) [$1000s]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



multi_model <- lm(MEDV ~ LSTAT + RM, data = train_data)
summary(multi_model)

multi_pred <- predict(multi_model, test_data)
multi_rmse <- rmse(test_data$MEDV, multi_pred)
multi_r2 <- cor(test_data$MEDV, multi_pred)^2
cat("Multiple Linear Regression RMSE: ", multi_rmse, "\n")
cat("Multiple Linear Regression R²: ", multi_r2, "\n")

rm_seq <- seq(
  min(reg_data$RM),
  max(reg_data$RM),
  length.out = 100
)
lstat_seq <- seq(
  min(reg_data$LSTAT),
  max(reg_data$LSTAT),
  length.out = 100
)
grid <- expand.grid(
  RM = rm_seq,
  LSTAT = lstat_seq
)
grid$MEDV_pred <- predict(multi_model, newdata = grid)
ggplot() +
  geom_contour_filled(
    data = grid,
    aes(
      x = RM,
      y = LSTAT,
      z = MEDV_pred
    ),
    alpha = 0.75
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Training"),
    aes(x = RM, y = LSTAT),
    color = "white",
    alpha = 0.5,
    size = 2
  ) +
  geom_point(
    data = subset(plot_data, Dataset == "Testing"),
    aes(x = RM, y = LSTAT),
    color = "red",
    alpha = 0.9,
    size = 2
  ) +
  labs(
    title = "Multiple Linear Regression Contour Map",
    subtitle = "Contours Represent Predicted MEDV",
    x = "RM (Average Number of Rooms)",
    y = "LSTAT (% Lower Status Population)",
    fill = "Predicted\nMEDV"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

test_data$residuals <- test_data$MEDV - multi_pred
ggplot(test_data, aes(x = multi_pred, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residual Plot",
    x = "Predicted MEDV [$1000s]",
    y = "Residual Error [$1000s]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )



results <- data.frame(
  Model = c("Linear Regression (LSTAT)",
            "Linear Regression (RM)",
            "Polynomial Regression (LSTAT)",
            "Polynomial Regression (RM)",
            "Multiple Linear Regression"),
  RMSE = c(linear_rmse_LSTAT,
           linear_rmse_RM,
           poly_rmse_LSTAT,
           poly_rmse_RM,
           multi_rmse),
  R_Squared = c(linear_r2_LSTAT,
                linear_r2_RM,
                poly_r2_LSTAT,
                poly_r2_RM,
                multi_r2)
)
print(results)