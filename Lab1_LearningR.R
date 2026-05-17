library(akima)
library(GGally)
library(tidyr)
library(dplyr)
library(ggplot2)

data_lab_directory <- "C:/Users/Xander/OneDrive/Desktop/College/R_Programming/Data Lab 2"

credit_data <- read.csv(
  file.path(data_lab_directory, "Credit.csv"),
  header = TRUE
)
college_data <- read.csv(
  file.path(data_lab_directory, "College.csv"),
  header = TRUE
)
heart_data <- read.csv(
  file.path(data_lab_directory, "Heart.csv"),
  header = TRUE
)



plot(
  college_data$Outstate,
  college_data$S.F.Ratio,
  pch = 19,
  col = "skyblue",
  xlab = "Out-of-State Tuition [USD]",
  ylab = "Student–Faculty Ratio",
  main = "Student–Faculty Ratio vs Out-of-State Tuition",
  las = 1
)
abline(
  lm(S.F.Ratio ~ Outstate, data = college_data),
  lwd = 2, lty = 2
)

cost_data <- college_data %>%
  select(Grad.Rate, Outstate, Room.Board, Books, Personal)
cost_long <- cost_data %>%
  pivot_longer(cols = c(Outstate, Room.Board, Books, Personal),
               names_to = "Cost_Type",
               values_to = "Amount")
cost_long <- cost_long %>% arrange(Grad.Rate)
ggplot(cost_long, aes(x = Grad.Rate, y = Amount, color = Cost_Type)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 1.2) +
  labs(title = "Smoothed Student Costs vs Graduation Rate",
       x = "Graduation Rate (Perc.)",
       y = "Cost [USD]",
       color = "Cost Component") +
  theme_minimal()

ggplot(college_data, aes(x = Private, y = Outstate, fill = Private)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("No" = "skyblue", "Yes" = "salmon")) +
  labs(
    title = "Out-of-State Tuition by College Type",
    x = "College Type",
    y = "Out-of-State Tuition [USD]"
  ) +
  theme_minimal()

interp_college_data <- with(college_data,
                            interp(x = Enroll, y = S.F.Ratio, z = Grad.Rate,
                                   duplicate = "mean")
)
image(
  interp_college_data$x,
  interp_college_data$y,
  interp_college_data$z,
  xlab = "Width of Cheese",
  ylab = "Side of Cheese",
  main = "Hidden CHEESE"
)



hist(
  credit_data$Rating,
  breaks = 10,
  col = "skyblue",
  main = "Distribution of Credit Ratings",
  xlab = "Credit Rating",
  ylab = "Frequency",
  las = 1,
  )

numeric_cols <- credit_data %>% select(where(is.numeric))
ggpairs(
  numeric_cols,
  title = "Credit Data: Pairwise Relationships",
  upper = list(continuous = wrap("cor", size = 4)),
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1.5)),
  diag = list(continuous = wrap("densityDiag", fill = "skyblue"))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

interp_credit_data <- with(credit_data,
                           interp(x = Income, y = Limit, z = Rating,
                                  duplicate = "mean")
                           )
contour(
  interp_credit_data$x,
  interp_credit_data$y,
  interp_credit_data$z,
  nlevels = 15,
  lwd = 2,
  xlab = "Income [x $1000]",
  ylab = "Credit Limit",
  main = "Contour Plot of Credit Rating",
  las = 1
  )



ggplot(heart_data, aes(x = Chol)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Distribution of Cholesterol Levels",
    x = "Cholesterol Level",
    y = "Density"
  ) +
  theme_minimal()

ggplot(heart_data, aes(x = AHD, y = Chol, fill = AHD)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  scale_fill_manual(values = c("No" = "skyblue", "Yes" = "salmon")) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Cholesterol Distribution by Heart Disease",
       x = "Heart Disease",
       y = "Cholesterol Level") +
  theme_minimal() +
  scale_x_discrete(labels = c("No" = "No Heart Disease",
                              "Yes" = "Heart Disease"))

interp_heart_data <- with(heart_data,
                          interp(x = Age, y = Chol, z = RestBP,
                                 duplicate = "mean")
                          )
image(
  interp_heart_data$x,
  interp_heart_data$y,
  interp_heart_data$z,
  xlab = "Age",
  ylab = "Cholesterol Level",
  main = "Resting Blood Pressure",
  las = 1
  )


