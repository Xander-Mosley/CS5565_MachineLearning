library(tidyverse)
library(dplyr)
library(splines)
library(caret)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Final Project/Splines")

boston <- read.csv("boston.csv")

reg_data <- boston %>%
  dplyr::select(MEDV, LSTAT, RM)

set.seed(0464)
train_index <- createDataPartition(reg_data$MEDV, p = 0.7, list = FALSE)
train_data <- reg_data[train_index, ]
test_data <- reg_data[-train_index, ]

train_data$Dataset <- "Training"
test_data$Dataset <- "Testing"
plot_data <- rbind(train_data, test_data)



poly1 <- lm(MEDV ~ poly(LSTAT, 1), data = train_data)
poly2 <- lm(MEDV ~ poly(LSTAT, 2), data = train_data)
poly3 <- lm(MEDV ~ poly(LSTAT, 3), data = train_data)
poly4 <- lm(MEDV ~ poly(LSTAT, 4), data = train_data)
poly5 <- lm(MEDV ~ poly(LSTAT, 5), data = train_data)

mse_function <- function(model, test_data){
  preds <- predict(model, newdata = test_data)
  mean((test_data$MEDV - preds)^2)
}
poly_results <- data.frame(
  Degree = c(1,2,3,4,5),
  Test_MSE = c(
    mse_function(poly1, test_data),
    mse_function(poly2, test_data),
    mse_function(poly3, test_data),
    mse_function(poly4, test_data),
    mse_function(poly5, test_data)
  )
)
print(poly_results)

x_grid <- seq(min(boston$LSTAT),
              max(boston$LSTAT),
              length = 200)
plot_data <- data.frame(LSTAT = x_grid)
plot_data$deg1 <- predict(poly1, newdata = plot_data)
plot_data$deg2 <- predict(poly2, newdata = plot_data)
plot_data$deg3 <- predict(poly3, newdata = plot_data)
plot_data$deg4 <- predict(poly4, newdata = plot_data)
plot_data$deg5 <- predict(poly5, newdata = plot_data)
plot_long <- plot_data %>%
  pivot_longer(
    cols = starts_with("deg"),
    names_to = "Polynomial_Degree",
    values_to = "Predicted_MEDV"
  )
ggplot(boston, aes(LSTAT, MEDV)) +
  geom_point(alpha = 0.5, color = "gray40") +
  geom_line(
    data = plot_long,
    aes(y = Predicted_MEDV,
        color = Polynomial_Degree),
    size = 1.2
  ) +
  labs(
    title = "Polynomial Regression Fits",
    subtitle = "MEDV vs LSTAT",
    x = "LSTAT (% Lower Status Population) [%]",
    y = "MEDV (Median Value of Homes) [$1000s]",
    color = "Polynomial Degree: "
  ) +
  scale_color_manual(
    values = c(
      "deg1" = "steelblue",
      "deg2" = "tomato",
      "deg3" = "lightgreen",
      "deg4" = "orange",
      "deg5" = "purple"
    ),
    labels = c(
      "deg1" = "1 (Linear)",
      "deg2" = "2",
      "deg3" = "3",
      "deg4" = "4",
      "deg5" = "5"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )




break_points <- quantile(boston$LSTAT,
                         probs = seq(0, 1, length.out = 5),
                         na.rm = TRUE)
train_data$LSTAT_cut <- cut(
  train_data$LSTAT,
  breaks = break_points,
  include.lowest = TRUE
)
test_data$LSTAT_cut <- cut(
  test_data$LSTAT,
  breaks = break_points,
  include.lowest = TRUE
)

step_model <- lm(MEDV ~ LSTAT_cut, data = train_data)
summary(step_model)

step_preds <- predict(step_model, newdata = test_data)
step_mse <- mean((test_data$MEDV - step_preds)^2)
cat("Step Function Test MSE:", step_mse)



bs_df6 <- lm(MEDV ~ bs(LSTAT, df = 6),
                 data = train_data)
bs_df9  <- lm(MEDV ~ bs(LSTAT, df = 9),
              data = train_data)
bs_df16 <- lm(MEDV ~ bs(LSTAT, df = 16),
              data = train_data)
bs_df22 <- lm(MEDV ~ bs(LSTAT, df = 22),
              data = train_data)

bs_results <- data.frame(
  Model = c("Default df=6", "df=9", "df=16", "df=22"),
  Test_MSE = c(
    mse_function(bs_df9, test_data),
    mse_function(bs_df9, test_data),
    mse_function(bs_df16, test_data),
    mse_function(bs_df22, test_data)
  )
)
print(bs_results)

plot_data$bs6  <- predict(bs_df9, newdata = plot_data)
plot_data$bs9  <- predict(bs_df9, newdata = plot_data)
plot_data$bs16 <- predict(bs_df16, newdata = plot_data)
plot_data$bs22 <- predict(bs_df22, newdata = plot_data)
bs_plot_data <- plot_data %>%
  dplyr::select(LSTAT, bs6, bs9, bs16, bs22) %>%
  pivot_longer(
    cols = c(bs6, bs9, bs16, bs22),
    names_to = "Spline_Model",
    values_to = "Predicted_MEDV"
  )
bs_plot_data$Spline_Model <- recode(
  bs_plot_data$Spline_Model,
  "bs6" = " 6 (Default)",
  "bs9" = " 9",
  "bs16" = "16",
  "bs22" = "22"
)
ggplot(boston, aes(LSTAT, MEDV)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(
    data = bs_plot_data,
    aes(x = LSTAT,
        y = Predicted_MEDV,
        color = Spline_Model),
    size = 1.2
  ) +
  labs(
    title = "Basis Spline Comparison",
    subtitle = "Increasing Degrees of Freedom",
    x = "LSTAT (% Lower Status Population) [%]",
    y = "MEDV (Median Value of Homes) [$1000s]",
    color = "Spline Degree (df = ):"
  ) +
  scale_color_manual(
    values = c(
      " 6 (Default)" = "steelblue",
      " 9" = "tomato",
      "16" = "lightgreen",
      "22" = "orange"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )



ns_df6 <- lm(MEDV ~ ns(LSTAT, df = 6),
                 data = train_data)
ns_df9 <- lm(MEDV ~ ns(LSTAT, df = 9),
             data = train_data)
ns_df16 <- lm(MEDV ~ ns(LSTAT, df = 16),
              data = train_data)
ns_df22 <- lm(MEDV ~ ns(LSTAT, df = 22),
              data = train_data)

ns_results <- data.frame(
  Model = c("Default df=6", "df=9", "df=16", "df=22"),
  Test_MSE = c(
    mse_function(ns_df6, test_data),
    mse_function(ns_df9, test_data),
    mse_function(ns_df16, test_data),
    mse_function(ns_df22, test_data)
  )
)
print(ns_results)

plot_data$ns6  <- predict(ns_df6, newdata = plot_data)
plot_data$ns9  <- predict(ns_df9, newdata = plot_data)
plot_data$ns16 <- predict(ns_df16, newdata = plot_data)
plot_data$ns22 <- predict(ns_df22, newdata = plot_data)
ns_plot_data <- plot_data %>%
  dplyr::select(LSTAT, ns6, ns9, ns16, ns22) %>%
  pivot_longer(
    cols = c(ns6, ns9, ns16, ns22),
    names_to = "Spline_Model",
    values_to = "Predicted_MEDV"
  )
ns_plot_data$Spline_Model <- recode(
  ns_plot_data$Spline_Model,
  "ns6" = " 6 (Default)",
  "ns9" = " 9",
  "ns16" = "16",
  "ns22" = "22"
)
ggplot(boston, aes(LSTAT, MEDV)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(
    data = ns_plot_data,
    aes(x = LSTAT,
        y = Predicted_MEDV,
        color = Spline_Model),
    size = 1.2
  ) +
  labs(
    title = "Natural Spline Comparison",
    subtitle = "Increasing Degrees of Freedom",
    x = "LSTAT (% Lower Status Population) [%]",
    y = "MEDV (Median Value of Homes) [$1000s]",
    color = "Spline Degree (df = ):"
  ) +
  scale_color_manual(
    values = c(
      " 6 (Default)" = "steelblue",
      " 9" = "tomato",
      "16" = "lightgreen",
      "22" = "orange"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )
