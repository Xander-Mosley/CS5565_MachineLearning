library(MASS)
library(ggplot2)
library(dplyr)

working_directory <- "C:/Users/Xander/OneDrive/Desktop/College/R_Programming/FeatureSelectionAndClassification"

data <- read.csv(
  file.path(working_directory, "ultimate_student_productivity_dataset_5000.csv"),
  header = TRUE
)

data$exam_category <- cut(
  data$exam_score,
  breaks = quantile(data$exam_score, probs = c(0, 1/3, 2/3, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)
data$exam_score <- NULL

data$student_id <- NULL
set.seed(0464)
train_index <- sample(1:nrow(data), 0.75 * nrow(data))
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

lda.fit <- lda(
  exam_category ~ study_hours + upcoming_deadline +
    self_study_hours + social_media_hours +
    gaming_hours + sleep_hours +
    part_time_job + mental_health_score,
  data = train_data
)


lda.fit
# plot(lda.fit)
# pairs(lda.fit)


lda.pred <- predict(lda.fit, test_data)
lda.class <- lda.pred$class
table(lda.class, test_data$exam_category)
mean(lda.class == test_data$exam_category)

plot_data <- data.frame(
  LD1 = lda.pred$x[,1],
  LD2 = lda.pred$x[,2],
  Class = train_data$exam_category
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
    radius = stats$sd_LD1[i]  # using LD1 std dev as radius
  )
  circle$Class <- stats$Class[i]
  return(circle)
}))
ggplot(plot_data, aes(x = LD1, y = LD2, color = Class)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_path(data = circle_data, aes(x = x, y = y, color = Class), size = 1) +
  scale_color_manual(values = c("palegreen", "hotpink", "skyblue")) +
  labs(
    title = "Linear Discriminant Analysis",
    x = "LD1",
    y = "LD2",
    color = "Exam Category"
  ) +
  theme_minimal(base_size = 14)


