library(ISLR2)
library(GGally)
library(nnet)
library(tidyr)

working_directory <- "C:/Users/Xander/OneDrive/Desktop/College/R_Programming/FeatureSelectionAndClassification"

data <- read.csv(
  file.path(working_directory, "ultimate_student_productivity_dataset_5000.csv"),
  header = TRUE
)

# names(data)
# dim(data)
# summary(data)
# ggpairs(data)

data$exam_category <- cut(
  data$exam_score,
  breaks = quantile(data$exam_score, probs = c(0, 1/3, 2/3, 1)),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)
data$exam_score <- NULL

# names(data)
# table(data$exam_category)
# prop.table(table(data$exam_category))
# ggpairs(data)

data$student_id <- NULL
set.seed(0464)
train_index <- sample(1:nrow(data), 0.75 * nrow(data))
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]
# model <- multinom(
#   exam_category ~ .,
#   data = train_data
# )
# summary(model)
# z <- summary(model)$coefficients / summary(model)$standard.errors
# p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
# p_values
# pred_probs <- predict(model, test_data, type = "probs")
# pred_class <- predict(model, test_data)
# table(pred_class, test_data$exam_category)
# mean(pred_class == test_data$exam_category)

model2 <- multinom(
  exam_category ~ study_hours + upcoming_deadline +
    self_study_hours + social_media_hours +
    gaming_hours + sleep_hours +
    part_time_job + mental_health_score,
  data = train_data
)
summary(model2)
z <- summary(model2)$coefficients / summary(model2)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
p_values
pred_probs <- predict(model2, test_data, type = "probs")
pred_class <- predict(model2, test_data)
table(pred_class, test_data$exam_category)
mean(pred_class == test_data$exam_category)


new_data <- data.frame(
  study_hours = seq(min(data$study_hours),
                    max(data$study_hours),
                    length.out = 100)
)
new_data$self_study_hours   <- mean(data$self_study_hours)
new_data$social_media_hours <- mean(data$social_media_hours)
new_data$gaming_hours       <- mean(data$gaming_hours)
new_data$sleep_hours        <- mean(data$sleep_hours)
new_data$mental_health_score <- mean(data$mental_health_score)
new_data$part_time_job      <- 0
new_data$upcoming_deadline  <- 0
pred_probs <- predict(model2, new_data, type = "probs")
plot_data <- cbind(new_data, pred_probs)
plot_data_long <- pivot_longer(
  plot_data,
  cols = c("Low", "Medium", "High"),
  names_to = "Category",
  values_to = "Probability"
)
ggplot(plot_data_long,
       aes(x = study_hours, y = Probability, color = Category)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("palegreen", "hotpink", "skyblue")) +
  labs(
    title = "Effect of Study Hours on Exam Category",
    subtitle = "Other variables held constant",
    x = "Study Hours",
    y = "Predicted Probability"
  ) +
  theme_minimal()
