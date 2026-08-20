# Load Dataset
library(ggplot2)

clean_data <- read.csv(
    "cleaned_data/cleaned_retail_store_sales.csv",
    stringsAsFactors = FALSE
)

dim(clean_data)
str(clean_data)

summary(clean_data[c(
    "Price.Per.Unit",
    "Quantity",
    "Total.Spent"
)])

# Step 2 — Exploratory Statistical Analysis
# The assignment specifically asks us to perform hypothesis testing, correlations, normality testing, and assumption analysis.

# correlation testing.
# Test 1 — Quantity vs Total Spent

# H₀ (Null hypothesis): There is no significant linear correlation between Quantity and Total Spent.

# H₁ (Alternative hypothesis): There is a significant linear correlation between Quantity and Total Spent.


cor.test(
    clean_data$Quantity,
    clean_data$Total.Spent,
    method = "pearson"
)

# H₀: There is no significant linear correlation between Price Per Unit and Total Spent.

# H₁: There is a significant linear correlation between Price Per Unit and Total Spent.

cor.test(
    clean_data$Price.Per.Unit,
    clean_data$Total.Spent,
    method = "pearson"
)

# --------------------------------------------------
# Normality Testing

set.seed(123)

total_spent_sample <- sample(
    clean_data$Total.Spent,
    5000
)

shapiro.test(total_spent_sample)

# -----------------------------------------------------
# Hypothesis Test for Location
# Is average Total Spent significantly different between Online and In-store transactions?

aggregate(
    Total.Spent ~ Location,
    data = clean_data,
    FUN = mean
)

t.test(
    Total.Spent ~ Location,
    data = clean_data
)

# Predictive Model Building 📈
set.seed(123)

train_index <- sample(
    seq_len(nrow(clean_data)),
    size = 0.80 * nrow(clean_data)
)

train_data <- clean_data[train_index, ]
test_data <- clean_data[-train_index, ]
dim(train_data)
dim(test_data)

# Step 4 — Build Model 1: Multiple Linear Regression

# First convert the categorical predictors to factors:
train_data$Category <- factor(train_data$Category)
train_data$Location <- factor(train_data$Location)
train_data$Payment.Method <- factor(train_data$Payment.Method)
train_data$Discount.Applied <- factor(train_data$Discount.Applied)

test_data$Category <- factor(
    test_data$Category,
    levels = levels(train_data$Category)
)

test_data$Location <- factor(
    test_data$Location,
    levels = levels(train_data$Location)
)

test_data$Payment.Method <- factor(
    test_data$Payment.Method,
    levels = levels(train_data$Payment.Method)
)

test_data$Discount.Applied <- factor(
    test_data$Discount.Applied,
    levels = levels(train_data$Discount.Applied)
)

# Model 1:
model_1 <- lm(
    Total.Spent ~ 
       Quantity +
       Price.Per.Unit +
       Location + 
       Payment.Method + 
       Discount.Applied +
       Category,
    data = train_data
)

summary(model_1)

# Model 2 — Add the interaction
model_2 <- lm(
    Total.Spent ~ 
        Price.Per.Unit * Quantity +
        Location +
        Payment.Method +
        Discount.Applied + 
        Category,
    data = train_data
)

summary(model_2)

# test-set Evaluation
# Predictions on test data
pred_model1 <- predict(
    model_1,
    newdata = test_data
)

pred_model2 <- predict(
    model_2,
    newdata = test_data
)

# Model 1 performance
rmse_1 <- sqrt(
    mean((test_data$Total.Spent - pred_model1)^2)
)

mae_1 <- mean(
    abs(test_data$Total.Spent - pred_model1)
)

r2_1 <- 1 - (
    sum((test_data$Total.Spent - pred_model1)^2) /
    sum((test_data$Total.Spent -
        mean(test_data$Total.Spent))^2)
)

# Model 2 performance
rmse_2 <- sqrt(
    mean((test_data$Total.Spent - pred_model2)^2)
)

mae_2 <- mean(
    abs(test_data$Total.Spent - pred_model2)
)

r2_2 <- 1 - (
    sum((test_data$Total.Spent - pred_model2)^2) /
    sum((test_data$Total.Spent -
        mean(test_data$Total.Spent))^2)
)

# Display results
performance <- data.frame(
    Model = c(
        "Model 1 - Additive",
        "Model 2 - Interaction"
    ),
    RMSE = c(rmse_1, rmse_2),
    MAE = c(mae_1, mae_2),
    R_Squared = c(r2_1, r2_2)
)

performance

# Next: 10-Fold Cross-Validation
set.seed(123)

# Create 10 approximately equal folds
folds <- sample(
    rep(1:10, length.out = nrow(train_data))
)

cv_rmse <- numeric(10)
cv_mae <- numeric(10)
cv_r2 <- numeric(10)

for (i in 1:10) {

    cv_train <- train_data[folds != i, ]
    cv_valid <- train_data[folds == i, ]

    cv_model <- lm(
        Total.Spent ~
            Quantity +
            Price.Per.Unit +
            Location +
            Payment.Method +
            Discount.Applied +
            Category,
        data = cv_train
    )

    cv_pred <- predict(
        cv_model,
        newdata = cv_valid
    )

    cv_rmse[i] <- sqrt(
        mean((cv_valid$Total.Spent - cv_pred)^2)
    )

    cv_mae[i] <- mean(
        abs(cv_valid$Total.Spent - cv_pred)
    )

    cv_r2[i] <- 1 - (
        sum((cv_valid$Total.Spent - cv_pred)^2) /
        sum(
            (cv_valid$Total.Spent -
             mean(cv_valid$Total.Spent))^2
        )
    )
}

cv_results <- data.frame(
    Fold = 1:10,
    RMSE = cv_rmse,
    MAE = cv_mae,
    R_Squared = cv_r2
)

cv_results

# Then calculate the overall CV performance:
colMeans(
    cv_results[c(
        "RMSE",
        "MAE",
        "R_Squared"
    )]
)

# Next — Model Diagnostics 🔍
par(mfrow = c(2,2))
plot(model_1)
par(mfrow = c(1,1))

# One quantitative diagnostic
cooks_d <- cooks.distance(model_1)

# Common screening threshold
cook_threshold <- 4 / nrow(train_data)

cook_threshold

sum(cooks_d > cook_threshold)

max(cooks_d)


# Create data for Actual vs Predicted plot
prediction_plot_data <- data.frame(
    Actual = test_data$Total.Spent,
    Predicted = pred_model1
)

# Actual vs Predicted visualization
actual_predicted_plot <- ggplot(
    prediction_plot_data,
    aes(x = Actual, y = Predicted)
) +
    geom_point(
        alpha = 0.35,
        size = 1.5
    ) +
    geom_abline(
        intercept = 0,
        slope = 1,
        color = "red",
        linewidth = 1
    ) +
    labs(
        title = "Actual vs Predicted Total Spent",
        subtitle = "Performance of the additive multiple linear regression model",
        x = "Actual Total Spent",
        y = "Predicted Total Spent"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(
            face = "bold",
            size = 17
        ),
        axis.title = element_text(
            face = "bold"
        )
    )

actual_predicted_plot

# save the plot:
if (!dir.exists("plots")) {
    dir.create("plots")
}

ggsave(
    "plots/08_actual_vs_predicted.png",
    plot = actual_predicted_plot,
    width = 10,
    height = 7,
    dpi = 300
)
file.exists("plots/08_actual_vs_predicted.png")