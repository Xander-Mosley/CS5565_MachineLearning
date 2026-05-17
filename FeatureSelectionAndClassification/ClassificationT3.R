library(MASS)
library(caret)

data(iris)
str(iris)
summary(iris)

iris_sub <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species")]
set.seed(0464)
train_index <- sample(1:nrow(iris_sub), 0.75 * nrow(iris_sub))
train_data <- iris_sub[train_index, ]
test_data  <- iris_sub[-train_index, ]

lda_model <- lda(Species ~ ., data = train_data)
lda_model
lda.pred <- predict(lda_model, test_data)
lda.class <- lda.pred$class
table(Predicted = lda.class, Actual = test_data$Species)


set.seed(0464)
train_control <- trainControl(method = "cv", number = 5)
lda_cv_model <- train(
  Species ~ .,
  data = iris_sub,
  method = "lda",
  trControl = train_control
)
lda_cv_model



plot_data <- data.frame(
  LD1 = lda.pred$x[,1],
  LD2 = lda.pred$x[,2],
  Class = test_data$Species
)
stats <- plot_data %>%
  group_by(Class) %>%
  summarise(
    mean_LD1 = mean(LD1),
    mean_LD2 = mean(LD2),
    sd_LD1 = sd(LD1),
    sd_LD2 = sd(LD2)
  )
circle_points <- function(center_x, center_y, radius, n = 100) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x = center_x + radius * cos(theta),
    y = center_y + radius * sin(theta)
  )
}
circle_data <- do.call(rbind, lapply(1:nrow(stats), function(i) {
  circle <- circle_points(
    stats$mean_LD1[i],
    stats$mean_LD2[i],
    radius = stats$sd_LD1[i]
  )
  circle$Class <- stats$Class[i]
  circle
}))
ggplot(plot_data, aes(x = LD1, y = LD2, color = Class)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_path(data = circle_data,
            aes(x = x, y = y, color = Class),
            linewidth = 1) +
  scale_color_manual(values = c("palegreen", "hotpink", "skyblue")) +
  labs(
    title = "Linear Discriminant Analysis",
    x = "LD1",
    y = "LD2",
    color = "Species"
  ) +
  theme_minimal(base_size = 14)