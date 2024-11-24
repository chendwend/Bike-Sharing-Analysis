# --------------- Libraries and Setup --------------

# Install Libraries if needed
if(!require(vcd)){install.packages("vcd")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(broom)){install.packages("broom")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(car)){install.packages("car")}
if(!require(caret)){install.packages("caret")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(pROC)){install.packages("pROC")}
if(!require(here)){install.packages("here")}

# if(!require(splitstackshape)){install.packages("splitstackshape")}

# set 
# setwd("/your_project_dir")

common_theme <- theme_grey() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

theme_set(common_theme)

set.seed(123)

df = read.csv("day.csv")

# --------------- Functions ------------
  
add_median_labels <- function(round=0) {
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(..y.., round)),
    color = "black",
    vjust = -0.5
  )
}

plot_histogram <- function(x, data = df, title = NULL) {
  # Dynamically use the variable name as part of the title
  x_name <- deparse(substitute(x))
  
  title <- title %||% paste("Distribution of ", x_name)
  median_value <- median(data[[x_name]], na.rm = TRUE)
  q75 <- quantile(data[[x_name]], probs = 0.75, na.rm = TRUE)
  
  ggplot(data, aes(x = !!ensym(x))) +
    geom_histogram(aes(y = after_stat(density)), fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) + 
    geom_vline(xintercept = q75, color = "blue", linetype = "dashed", size = 1) +
    geom_text(aes(x = q75, y = 0, label = paste("Q75 =", round(q75, 1))),
              color = "blue", vjust = 1, hjust = 1.1) +
    labs(x = x_name, 
         y = "Frequency", 
         title = title)
}

plot_boxplot <- function(x = NULL, y = cnt, data = df, title = NULL,
                         filter_condition = NULL){
  
  # Apply filter condition if provided
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))
  }
  
  
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  if (missing(x)){
    title <- title %||% paste(y_name, "Distribution")
    graph <- ggplot(data, aes(y = !!ensym(y))) +
      geom_boxplot(fill = "lightgreen", color = "black") +
      labs(x = NULL, 
           y = y_name, 
           title = title)
  } else {
  
  title <- title %||% paste(y_name, " Distribution vs ", x_name)
  
 graph <- ggplot(data, aes(!!ensym(x), !!ensym(y))) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    add_median_labels() + 
    labs(x = x_name, 
         y = y_name, 
         title = title)

  }
  
  graph
  
  return(graph)
}

plot_scatter <- function(x, y, data = df, title = NULL, filter_condition = NULL){
  
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!rlang::parse_expr(filter_condition))
  }
  
  title <- title %||% paste(y_name, " Distribution vs ", x_name)
  
  ggplot(data, aes(!!ensym(x), !!ensym(y))) +
    geom_point() + 
    labs(x = x_name, 
         y = y_name,
         title = title) + 
    geom_smooth()
}

models_perf <- function(models, test_data, target_var = 'cnt'){
  # Helper function to calculate metrics
  # Helper function to calculate metrics
  calculate_metrics <- function(actual, predicted, n, p) {
    # Basic metrics
    rmse <- sqrt(mean((actual - predicted)^2))
    mae <- mean(abs(actual - predicted))
    mape <- mean(abs((actual - predicted) / actual)) * 100
    # R-squared
    ss_total <- sum((actual - mean(actual))^2)
    ss_residual <- sum((actual - predicted)^2)
    r_squared <- 1 - (ss_residual / ss_total)
    # Adjusted R-squared
    adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
    
    return(c(RMSE = rmse, MAE = mae, MAPE = mape, R_squared = r_squared, Adjusted_R_squared = adj_r_squared))
  }
  
  # Initialize an empty list to store metrics
  metrics_list <- list()
  
  # Get number of observations
  n <- nrow(test_data)
  
  # Loop through each model and calculate metrics
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    predictions <- exp(predict(model, test_data))
    p <- length(coef(model)) - 1  # Number of predictors
    metrics <- calculate_metrics(test_data[[target_var]], predictions, n, p)
    metrics_list[[model_name]] <- round(metrics, 4)
  }
  
  # Combine results into a data frame for comparison
  comparison <- do.call(cbind, metrics_list)
  rownames(comparison) <- names(metrics_list[[1]])
  return(comparison)
}

plot_feature_importance <- function(model) {
  # Check if the input is a valid lm or glm model
  if (!inherits(model, c("lm", "glm"))) {
    stop("The function requires an 'lm' or 'glm' model.")
  }
  
  # Perform ANOVA to decompose variance
  anova_results <- anova(model)
  
  # Extract the sum of squares for each term, including residuals
  sum_sq <- c(anova_results$`Sum Sq`, Residuals = anova_results$`Sum Sq`[length(anova_results$`Sum Sq`)])
  total_sum_sq <- sum(sum_sq)
  feature_importance <- round(sum_sq / total_sum_sq * 100, 1)
  
  # Prepare a data frame to store results
  importance_df <- data.frame(
    Term = c(rownames(anova_results), "Residuals"),
    Importance = feature_importance
  )
  
  # Sort by importance in descending order
  importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
  
  # Create the bar plot with percentage labels
  ggplot(importance_df, aes(x = reorder(Term, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = sprintf("%.1f%%", Importance)), hjust = -0.1) +
    coord_flip() +
    labs(
      title = "Feature Importance Based on Variance Decomposition",
      x = "Model Term",
      y = "Importance (%)"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_cramers_v_matrix <- function(df) {
  
  calculate_cramers_v <- function(x, y) {
    contingency_table <- table(x, y)
    chi_square <- tryCatch(
      chisq.test(contingency_table, correct=FALSE),
      error = function(e) return(list(statistic = 0))
    )
    n <- sum(contingency_table)
    min_dim <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
    
    if(min_dim == 0) return(0)
    cramers_v <- sqrt(chi_square$statistic / (n * min_dim))
    return(cramers_v)
  }
  
  
  
  
  # Get categorical columns
  categorical_cols <- sapply(df, is.factor)
  if(sum(categorical_cols) < 2) {
    stop("Need at least 2 categorical variables in the dataframe")
  }
  
  df_cat <- df[, categorical_cols]
  n_vars <- ncol(df_cat)
  var_names <- colnames(df_cat)
  
  # Initialize matrix
  cramers_matrix <- matrix(NA, nrow=n_vars, ncol=n_vars)
  
  # Calculate Cramer's V for upper triangle
  for(i in 1:n_vars) {
    for(j in i:n_vars) {
      if(i != j) {  # Skip diagonal
        cramers_matrix[i,j] <- calculate_cramers_v(df_cat[[i]], df_cat[[j]])
      }
    }
  }
  
  # Create plotting data
  plot_data <- expand.grid(row=1:n_vars, col=1:n_vars)
  plot_data$value <- as.vector(cramers_matrix)
  
  # Add formatted text values for non-NA cells
  plot_data$label <- ifelse(!is.na(plot_data$value), 
                            sprintf("%.2f", plot_data$value), 
                            "")
  
  # Create heatmap using ggplot2
  library(ggplot2)
  
  ggplot(plot_data, aes(x=col, y=n_vars - row + 1, fill=value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    # Add text labels
    geom_text(aes(label=label), 
              color=ifelse(!is.na(plot_data$value) & plot_data$value > 0.5, "white", "black"),
              size=3.5) +
    scale_fill_gradient2(
      low = "#FFFFFF",
      mid = "#5AB4AC",
      high = "#D8B365",
      midpoint = 0.5,
      na.value = "white",
      limits = c(0,1),
      name = "Cramer's V"
    ) +
    scale_x_continuous(
      breaks = 1:n_vars,
      labels = var_names,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = 1:n_vars,
      labels = rev(var_names),
      expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey80"),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
      title = "Cramer's V Matrix",
      x = "",
      y = ""
    ) +
    coord_fixed()  # Make cells square
}

print_tidy_model<- function(model){
  tidy_output <- tidy(summary(model)) %>%  
    arrange(p.value) > print(n=40, tidy_output)
}

calc_plot_roc <- function(actual, predicted, title = "ROC Curve") {
  # Sort predictions in descending order
  ord <- order(predicted, decreasing = TRUE)
  pred_sorted <- predicted[ord]
  actual_sorted <- actual[ord]
  
  # Calculate true positives and false positives
  n_pos <- sum(actual == 1)
  n_neg <- sum(actual == 0)
  
  # Initialize vectors
  tpr <- numeric(length(actual) + 1)
  fpr <- numeric(length(actual) + 1)
  thresholds <- c(pred_sorted, min(pred_sorted) - 1)
  
  # Calculate TPR and FPR for each threshold
  tp <- cumsum(actual_sorted)
  fp <- cumsum(!actual_sorted)
  tpr <- c(0, tp/n_pos)
  fpr <- c(0, fp/n_neg)
  
  # Calculate AUC using trapezoidal rule
  auc <- sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)])) / 2
  
  # Create plot
  plot(fpr, tpr, type = "l", col = "blue", lwd = 2,
       xlab = "False Positive Rate", 
       ylab = "True Positive Rate",
       main = paste0(title, "\nAUC = ", round(auc, 3)))
  abline(0, 1, lty = 2, col = "gray")
  
  # Return metrics
  return(list(
    tpr = tpr,
    fpr = fpr,
    thresholds = thresholds,
    auc = auc
  ))
}

#
#---------------- EDA ----------------------
#

str(df)
head(df)

# drop redundant columns
df <- subset(df, select = -c(dteday, instant))

# check for NA
sum(is.na(df))
 
##----------- Correlation plot -------------
numeric_vars <- df %>%
  select(temp, atemp, hum, windspeed, cnt)
  
categorical_vars <- df %>%
  select(holiday, weekday, workingday, mnth, yr, season)

# Create correlation plot
corrplot(numeric_vars, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.7)

plot_cramers_v_matrix(categorical_vars)


# remove atemp 
df <- subset(df, select = -c(atemp, workingday))

## ----------- Factorization ----------
df <- df %>% 
  mutate(
    season = factor(season, levels = 1:4, labels = c("Spring", "Summer", "Fall", "Winter")),
    weathersit = factor(weathersit, levels = 1:4, 
                        labels = c("Clear", "Mist", "Light Snow/Rain", "Heavy Rain/Snow")),
    holiday = factor(holiday, levels = c(0,1), labels = c("No", "Yes")),
    weekday = factor(weekday, levels = 0:6,
                     labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    yr = factor(yr, levels = c(0,1), labels = c("2011", "2012")),
    mnth = factor(mnth, levels=1:12, 
                  labels = c("January", "Feburary", "March", "April", "May",
                             "June", "July", "August", "September", "October",
                             "November", "December"))
  )

##--------------- Temperature -------------
plot_histogram(temp, title = "Temperature Distribution")

plot_scatter(temp, cnt, title = "Total Bike Rental vs. Temperature")

##--------------- Humidity -------------
plot_histogram(hum, title = "Humidity Distribution")
df$hum_trans <- df$hum^1.5
plot_histogram(hum_trans, title = expression(bold("Distribution of " * Humidity^1.5)))

##--------------- Wind speed -------------
plot_histogram(windspeed, title = "Windspeed Distribution")


df$sqrt_windspeed <- df$windspeed^0.5
plot_histogram(sqrt_windspeed, title = expression("Distribution of " * sqrt(windspeed)))


# cnt 

plot_histogram(cnt)
plot_histogram(casual)
plot_histogram(registered)
plot_boxplot()

#--------------------- Modelling ---------------------
# Convert categorical variables to human readable values and factorize



## ------------------ Splitting data ---------------------------
# Split data into training and testing sets

train_index <- createDataPartition(df$cnt, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]


## ------------------ Modelling - Base model ---------------------------------


cnt_form <- as.formula("cnt ~ season + yr + mnth + holiday + weekday + 
                            weathersit + temp + hum + 
                           windspeed")


bm <- lm(cnt_form, data = train_data)

# Linear Regression Assumptions
vif(base_model)
bm.vif <- lm(cnt ~ season + yr + holiday + weekday + weathersit
              + temp + hum + windspeed, data=train_data)
vif(bm.vif)


## ----------- Improved model-----------
new <- lm(log(cnt)~ season + yr + holiday + weekday + weathersit
   + temp + hum + sqrt(temp) + I(temp^2) + hum:weathersit +
     windspeed + I(temp^3) + log(temp), data=train_data)

# get QQ plot and residuals vs fitted
plot(new)

# ----------- High Demand Prediction -----------

df_sum <- df %>%
  filter (season == "Summer") %>%
  mutate(
    # Create target variable using 75th percentile
    high_demand = factor(casual >= quantile(casual, 0.75),
                         levels = c(FALSE, TRUE),
                         labels = c("low", "high")),
    weathersit = ifelse(weathersit == "Clear", 1, 0),
    is_weekend = weekday %in% c("Saturday", "Sunday"),
    across(c(weathersit, mnth, weekday, holiday), factor)
  ) %>%
  select(-c(casual, registered, cnt, season, weekday))


train_index <- createDataPartition(df_sum$high_demand, p = 0.8, list = FALSE)
train_data <- df_sum[train_index, ]
test_data <- df_sum[-train_index, ]

model <- glm(high_demand ~ ., data = train_data, 
             family = binomial)

roc_curve1 <- roc(test_data$high_demand, predict(model, test_data, type = "response"))

# scale temperature, use same scaling in test set to avoid data leakage
train_temp_scaled <- scale(train_data$temp)
train_data$temp <- as.numeric(train_temp_scaled)
test_data$temp <- (test_data$temp - attr(train_temp_scaled, "scaled:center")) / 
  attr(train_temp_scaled, "scaled:scale")



model2 <- glm(high_demand ~ yr + holiday + is_weekend + temp + weathersit,
              data = train_data, family = binomial)


roc_curve2 <- roc(test_data$high_demand, predict(model2, test_data, type = "response"))


plot.roc(roc_curve, main = "ROC Curve", col = "blue", lwd = 2, print.auc = TRUE)
plot.roc(roc_curve2, main = "ROC Curve", col = "red", lwd = 2, print.auc = TRUE)

predictions <- predict(model2, newdata = test_data, type = "response")

predicted_classes <- ifelse(predictions > 0.3, 1, 0)
predicted_classes <- factor(predicted_classes, levels = c(0, 1), labels = c("low", "high"))
conf_matrix <- confusionMatrix(factor(predicted_classes), 
                               factor(test_data$high_demand))
actual = factor(test_data$high_demand)

plot_conf_matrix(actual, factor(predicted_classes))

precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1 <- 2 * (precision * recall) / (precision + recall)

# Print performance metrics
print("Model Performance Metrics:")
print(paste("F1 Score:", round(f1, 3)))
print(paste("Accuracy:", round(conf_matrix$overall['Accuracy'], 3)))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 3)))

