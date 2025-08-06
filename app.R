#Justin Davis
#Data Practicum
#RegenRate
#08/06/25

#libraries 
library(shiny)
library(DT)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(stats)
library(cluster)
library(hms)
library(zoo)
library(tidyr)
library(lubridate)
library(shinydashboard)
library(fresh)
library(waiter)
library(fontawesome)

# Increase upload size limit
options(shiny.maxRequestSize = 600*1024^2)  

# Set modern ggplot theme
theme_set(theme_minimal() +
  theme(
    text = element_text(family = "Segoe UI", size = 12),
    plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(size = 14, color = "#7f8c8d"),
    axis.title = element_text(size = 12, color = "#2c3e50"),
    axis.text = element_text(size = 10, color = "#7f8c8d"),
    panel.grid.major = element_line(color = "#ecf0f1", size = 0.5),
    panel.grid.minor = element_line(color = "#ecf0f1", size = 0.25),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 12, color = "#2c3e50"),
    legend.text = element_text(size = 10, color = "#7f8c8d")
  ))

#custom css 
custom_css <- "
/* Modern Gradient Background */
body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  margin: 0;
  padding: 0;
  min-height: 100vh;
}

/* Main Container Styling */
.main-container {
  background: rgba(255, 255, 255, 0.95);
  border-radius: 20px;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
  margin: 20px auto;
  max-width: 1200px;
  padding: 30px;
  backdrop-filter: blur(10px);
}

/* Header Styling */
.app-header {
  text-align: center;
  margin-bottom: 30px;
  padding: 20px 0;
  border-bottom: 3px solid #667eea;
}

.app-title {
  color: #2c3e50;
  font-size: 2.5em;
  font-weight: 700;
  margin: 0;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.1);
}

.app-subtitle {
  color: #7f8c8d;
  font-size: 1.2em;
  margin: 10px 0 0 0;
  font-weight: 300;
}

/* Progress Indicator */
.progress-container {
  display: flex;
  justify-content: center;
  margin: 20px 0;
  padding: 15px;
  background: rgba(255, 255, 255, 0.8);
  border-radius: 15px;
}

.progress-step {
  display: flex;
  align-items: center;
  margin: 0 10px;
  color: #7f8c8d;
  font-weight: 500;
}

.progress-step.active {
  color: #667eea;
  font-weight: 700;
}

.progress-step.completed {
  color: #27ae60;
}

/* Form Styling */
.form-group {
  margin-bottom: 20px;
}

.form-control {
  border: 2px solid #e0e0e0;
  border-radius: 10px;
  padding: 12px 15px;
  font-size: 16px;
  transition: all 0.3s ease;
  background: rgba(255, 255, 255, 0.9);
}

.form-control:focus {
  border-color: #667eea;
  box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
  outline: none;
}

/* Button Styling */
.btn-primary {
  background: linear-gradient(45deg, #667eea, #764ba2);
  border: none;
  border-radius: 25px;
  color: white;
  font-weight: 600;
  padding: 12px 30px;
  font-size: 16px;
  transition: all 0.3s ease;
  box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
  background: linear-gradient(45deg, #5a6fd8, #6a4190);
}

.btn-secondary {
  background: linear-gradient(45deg, #95a5a6, #7f8c8d);
  border: none;
  border-radius: 25px;
  color: white;
  font-weight: 600;
  padding: 12px 30px;
  font-size: 16px;
  transition: all 0.3s ease;
  box-shadow: 0 4px 15px rgba(149, 165, 166, 0.3);
}

.btn-secondary:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(149, 165, 166, 0.4);
}

/* Card Styling */
.card {
  background: rgba(255, 255, 255, 0.9);
  border-radius: 15px;
  padding: 25px;
  margin: 15px 0;
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.2);
}

.card-header {
  background: linear-gradient(45deg, #667eea, #764ba2);
  color: white;
  padding: 15px 25px;
  border-radius: 15px 15px 0 0;
  margin: -25px -25px 20px -25px;
  font-weight: 600;
  font-size: 18px;
}

/* Plot Styling */
.shiny-plot-output {
  background: white;
  border-radius: 10px;
  padding: 15px;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
  margin: 15px 0;
}

/* Table Styling */
.dataTables_wrapper {
  background: white;
  border-radius: 10px;
  padding: 15px;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
  margin: 15px 0;
}

/* File Upload Styling */
.file-input-wrapper {
  border: 3px dashed #667eea;
  border-radius: 15px;
  padding: 40px;
  text-align: center;
  background: rgba(102, 126, 234, 0.05);
  transition: all 0.3s ease;
}

.file-input-wrapper:hover {
  border-color: #764ba2;
  background: rgba(102, 126, 234, 0.1);
}

/* Notification Styling */
.shiny-notification {
  border-radius: 10px;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
}

/* Recovery Level Indicators */
.recovery-severe {
  background: linear-gradient(45deg, #e74c3c, #c0392b);
  color: white;
  padding: 10px 15px;
  border-radius: 10px;
  font-weight: 600;
}

.recovery-moderate {
  background: linear-gradient(45deg, #f39c12, #e67e22);
  color: white;
  padding: 10px 15px;
  border-radius: 10px;
  font-weight: 600;
}

.recovery-light {
  background: linear-gradient(45deg, #f1c40f, #f39c12);
  color: white;
  padding: 10px 15px;
  border-radius: 10px;
  font-weight: 600;
}

/* Responsive Design */
@media (max-width: 768px) {
  .main-container {
    margin: 10px;
    padding: 20px;
  }
  
  .app-title {
    font-size: 2em;
  }
  
  .btn-primary, .btn-secondary {
    width: 100%;
    margin: 5px 0;
  }
}
"  

# Generate height choices from 2'0" to 7'0"
height_choices <- c()
for (ft in 2:7) {
  for (inch in 0:11) {
    label <- sprintf("%d'%02d\"", ft, inch)
    height_choices <- c(height_choices, label)
  }
}
height_choices <- height_choices[1:((7-2)*12+1)]

#functions
# Progress indicator function
get_progress_steps <- function(current_step) {
  steps <- c("Login", "Profile", "Upload", "Configure", "Results", "Visualize", "Summary")
  step_names <- c("login", "create_profile", "upload", "clustering_config", "analysis", "metric_plot", "finish")
  
  progress_html <- '<div class="progress-container">'
  for (i in seq_along(steps)) {
    step_class <- "progress-step"
    if (step_names[i] == current_step) {
      step_class <- "progress-step active"
    } else if (which(step_names == current_step) > i) {
      step_class <- "progress-step completed"
    }
    progress_html <- paste0(progress_html, 
                           '<div class="', step_class, '">', i, '. ', steps[i], '</div>')
  }
  progress_html <- paste0(progress_html, '</div>')
  return(HTML(progress_html))
}

# Elbow method for optimal k in clustering
elbow_method <- function(data, max_k = 10) {
  max_k <- min(max_k, nrow(data) - 1)
  if (max_k < 2) {
    return(data.frame(k = 1, inertia = var(data)))
  }
  
  inertia <- numeric(max_k)
  k_values <- 1:max_k
  
  for (k in k_values) {
    set.seed(123)
    km <- suppressWarnings(kmeans(data, centers = k, nstart = 25))
    inertia[k] <- km$tot.withinss
  }
  
  return(data.frame(k = k_values, inertia = inertia))
}

#Linear Regression Functions
# Function to compute the best line of fit using matrix operations
standRegres <- function(xArr, yArr) {
  # Convert to matrices
  xMat <- as.matrix(xArr)
  yMat <- as.matrix(yArr)
  
  # Compute X^T * X
  xTx <- t(xMat) %*% xMat
  
  # Check if determinant = 0 (singular matrix)
  if (det(xTx) == 0.0) {
    print("This matrix is singular, cannot do inverse")
    return(NULL)
  }
  
  # Compute coefficients: (X^T * X)^(-1) * (X^T * y)
  ws <- solve(xTx) %*% (t(xMat) %*% yMat)
  return(ws)
}

# Function to compute Residual Sum of Squares (RSS) error
rssError <- function(yArr, yHatArr) {
  # Calculate squared differences between actual and predicted values
  return(sum((yArr - yHatArr)^2))
}

# Function to compute Total Sum of Squares (TSS) error
tssError <- function(yArr, yHatArr) {
  # TSS is sum of squared differences between actual values and mean of predicted values
  return(sum((yArr - mean(yHatArr))^2))
}

# Function to compute R-squared (Coefficient of Determination)
Rsquare <- function(yArr, yHatArr) {
  rss <- rssError(yArr, yHatArr)
  tss <- tssError(yArr, yHatArr)
  return(1 - rss / tss)
}

  # Function to perform comprehensive linear regression analysis
  perform_linear_regression <- function(data, x_var, y_var, train_ratio = 0.75) {
    # Select variables and remove NA values
    lm_data <- data[, c(x_var, y_var)] %>% na.omit()
    
    if (nrow(lm_data) < 10) {
      return(list(error = "Not enough data for regression analysis"))
    }
    
    # Add intercept column (X0 = 1)
    lm_data$X0 <- 1
    
    # Shuffle data
    set.seed(123)
    lm_data <- lm_data[sample(nrow(lm_data)), ]
    
    # Split into training and test sets
    train_size <- floor(train_ratio * nrow(lm_data))
    train <- lm_data[1:train_size, ]
    test <- lm_data[(train_size + 1):nrow(lm_data), ]
    
    # Prepare training data
    X_train <- as.matrix(train[, c("X0", x_var)])
    y_train <- as.matrix(train[, y_var])
    
    # Prepare test data
    X_test <- as.matrix(test[, c("X0", x_var)])
    y_test <- as.matrix(test[, y_var])
    
    # Compute regression coefficients
    beta <- standRegres(X_train, y_train)
    
    if (is.null(beta)) {
      return(list(error = "Could not compute regression coefficients"))
    }
    
    # Compute predicted values for training set
    y_hat_train <- X_train %*% beta
    
    # Compute predicted values for test set
    y_hat_test <- X_test %*% beta
    
    # Compute R-squared for both sets
    r2_train <- Rsquare(y_train, y_hat_train)
    r2_test <- Rsquare(y_test, y_hat_test)
    
    # Compute residuals for training set
    residuals_train <- y_train - y_hat_train
    
    # Convert matrices to vectors for easier plotting
    return(list(
      beta = beta,
      train_data = list(
        X = X_train, 
        y = as.vector(y_train), 
        y_hat = as.vector(y_hat_train), 
        residuals = as.vector(residuals_train)
      ),
      test_data = list(
        X = X_test, 
        y = as.vector(y_test), 
        y_hat = as.vector(y_hat_test)
      ),
      r2_train = r2_train,
      r2_test = r2_test,
      x_var = x_var,
      y_var = y_var
    ))
  }

# Find sleep periods using heart rate threshold
find_sleep_periods <- function(df, threshold = 60, min_duration = 20) {
  hr_df <- df %>%
    filter(metric_name == "Heart Rate") %>%
    mutate(
      value = as.numeric(value),
      datetime = as.POSIXct(paste(date_local, time_of_day)),
      is_sleep = value < threshold
    ) %>%
    arrange(date_local, datetime)
  
  hr_df$block <- cumsum(c(0, diff(hr_df$is_sleep) != 0))
  
  sleep_blocks <- hr_df %>%
    filter(is_sleep) %>%
    group_by(date_local, block) %>%
    summarise(
      start_time = min(datetime),
      end_time = max(datetime),
      duration = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
      .groups = "drop"
    ) %>%
    filter(duration >= min_duration)
  
  return(sleep_blocks)
}

# Smart recovery analysis function with multi-window approach
analyze_recent_activity <- function(raw_data, selected_date, days_back = 14) {
  if (is.null(selected_date) || is.null(raw_data)) return(NULL)
  
  # Get recent activity data (14 days for better analysis)
  recent_data <- raw_data %>%
    filter(metric_name == "Active Energy Burned") %>%
    filter(as.Date(date_local) <= as.Date(selected_date)) %>%
    filter(as.Date(date_local) >= as.Date(selected_date) - days_back) %>%
    group_by(date_local) %>%
    summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") %>%
    arrange(date_local)
  
  if (nrow(recent_data) < 5) return(NULL)
  
  # Calculate baseline metrics
  avg_recent <- mean(recent_data$daily_active, na.rm = TRUE)
  high_activity_threshold <- avg_recent * 1.2  # 20% above average
  
  #Multi-window Analysis
  
  # 1. Last 3 days analysis (immediate stress)
  last_3_days <- tail(recent_data$daily_active, 3)
  last_3_avg <- mean(last_3_days, na.rm = TRUE)
  consecutive_high_3d <- sum(last_3_days > high_activity_threshold)
  
  # 2. Last 7 days analysis (weekly pattern)
  last_7_days <- tail(recent_data$daily_active, 7)
  last_7_avg <- mean(last_7_days, na.rm = TRUE)
  high_days_7d <- sum(last_7_days > high_activity_threshold)
  
  # 3. Last 14 days analysis (bi-weekly pattern)
  last_14_avg <- mean(recent_data$daily_active, na.rm = TRUE)
  high_days_14d <- sum(recent_data$daily_active > high_activity_threshold)
  
  # 4. Trend analysis (last 7 vs previous 7)
  if (nrow(recent_data) >= 14) {
    last_7_avg <- mean(tail(recent_data$daily_active, 7), na.rm = TRUE)
    prev_7_avg <- mean(recent_data$daily_active[(nrow(recent_data)-13):(nrow(recent_data)-7)], na.rm = TRUE)
    weekly_trend <- (last_7_avg - prev_7_avg) / prev_7_avg
  } else {
    weekly_trend <- 0
  }
  
  # 5. Consecutive high days (from most recent)
  consecutive_high_days <- 0
  for (i in nrow(recent_data):1) {
    if (recent_data$daily_active[i] > high_activity_threshold) {
      consecutive_high_days <- consecutive_high_days + 1
    } else {
      break
    }
  }
  
  return(list(
    avg_recent = avg_recent,
    last_3_avg = last_3_avg,
    last_7_avg = last_7_avg,
    last_14_avg = last_14_avg,
    consecutive_high_days = consecutive_high_days,
    consecutive_high_3d = consecutive_high_3d,
    high_days_7d = high_days_7d,
    high_days_14d = high_days_14d,
    weekly_trend = weekly_trend,
    high_activity_threshold = high_activity_threshold
  ))
}

# Enhanced recommendation function incorporating regression and personal metrics
get_enhanced_recommendation <- function(cluster_df, raw_data, regression_results, user_profile, selected_date = NULL) {
  recommendations <- list()
  
  #Clustering-based recommendation
  if (!is.null(cluster_df) && "Cluster" %in% names(cluster_df) && "date_local" %in% names(cluster_df)) {
    # If a specific date is selected, find the cluster for that date
    if (!is.null(selected_date)) {
      date_cluster <- cluster_df %>% 
        filter(as.Date(date_local) == as.Date(selected_date)) %>%
        tail(1)  # Get the most recent entry for that date
      
      if (nrow(date_cluster) > 0) {
        target_cluster <- as.character(date_cluster$Cluster)
      } else {
        # Fall back to most recent cluster if date not found
        target_cluster <- as.character(tail(cluster_df, 1)$Cluster)
      }
    } else {
      # Use most recent cluster if no date selected
      target_cluster <- as.character(tail(cluster_df, 1)$Cluster)
    }
    
    days_in_cluster <- unique(cluster_df %>% filter(Cluster == target_cluster) %>% pull(date_local))
    
    if ("metric_name" %in% names(raw_data) && "value" %in% names(raw_data)) {
      daily_sums <- raw_data %>%
        filter(date_local %in% days_in_cluster, metric_name == "Active Energy Burned") %>%
        group_by(date_local) %>%
        summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop")
      
      if (nrow(daily_sums) > 0) {
        avg_daily_active <- mean(daily_sums$daily_active, na.rm = TRUE)
        
        #Smart Recovery Analysis
        recovery_analysis <- analyze_recent_activity(raw_data, selected_date)
        
        if (!is.null(recovery_analysis)) {
          #Multi-window Recovery Logic
          
          # 1. Severe Recovery (3+ consecutive high days OR 5+ high days in last 7)
          if (recovery_analysis$consecutive_high_days >= 3 || recovery_analysis$high_days_7d >= 5) {
            recovery_factor <- 0.5  # 50% of normal (more aggressive recovery)
            recovery_calories <- avg_daily_active * recovery_factor
            recommendations$clustering <- paste0("Cluster-based (Severe Recovery): ", 
                                               round(recovery_calories, 0), " calories")
            recommendations$recovery_note <- paste0("High activity detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days OR ", 
                                                   recovery_analysis$high_days_7d, " high days in last week. Strong recovery recommended.")
          
          # 2. Moderate Recovery (2 consecutive high days OR 3-4 high days in last 7 OR high weekly trend)
          } else if (recovery_analysis$consecutive_high_days >= 2 || 
                     recovery_analysis$high_days_7d >= 3 || 
                     recovery_analysis$weekly_trend > 0.25) {
            recovery_factor <- 0.7  # 70% of normal
            recovery_calories <- avg_daily_active * recovery_factor
            recommendations$clustering <- paste0("Cluster-based (Moderate Recovery): ", 
                                               round(recovery_calories, 0), " calories")
            recommendations$recovery_note <- paste0("Moderate stress detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days, ", 
                                                   recovery_analysis$high_days_7d, " high days in last week, or ", 
                                                   round(recovery_analysis$weekly_trend * 100, 0), "% weekly increase. Consider lighter activity.")
          
          # 3. Light Recovery (1 consecutive high day OR 2 high days in last 7)
          } else if (recovery_analysis$consecutive_high_days >= 1 || recovery_analysis$high_days_7d >= 2) {
            recovery_factor <- 0.85  # 85% of normal
            recovery_calories <- avg_daily_active * recovery_factor
            recommendations$clustering <- paste0("Cluster-based (Light Recovery): ", 
                                               round(recovery_calories, 0), " calories")
            recommendations$recovery_note <- paste0("Light stress detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days or ", 
                                                   recovery_analysis$high_days_7d, " high days in last week. Slight reduction recommended.")
          
          # 4. Normal recommendation
          } else {
            recommendations$clustering <- paste0("Cluster-based: ", round(avg_daily_active, 0), " calories")
          }
        } else {
          # Fallback to normal recommendation
          recommendations$clustering <- paste0("Cluster-based: ", round(avg_daily_active, 0), " calories")
        }
      }
    }
  }
  
  #2. Regression-based recommendation
  if (!is.null(regression_results) && !is.null(regression_results$beta)) {
    recommendations$regression <- paste0("Regression-based: Model suggests ", 
                                       round(regression_results$r2_train * 100, 1), 
                                       "% correlation between your variables")
  }
  
  #3. Combined clustering + regression recommendation
  if (!is.null(recommendations$clustering) && !is.null(regression_results) && !is.null(regression_results$beta)) {
    # Extract the cluster-based calorie recommendation (handle recovery labels)
    cluster_text <- recommendations$clustering
    cluster_calories <- as.numeric(gsub("[^0-9.]", "", cluster_text))
    
    # Use regression R-squared as a confidence weight
    regression_weight <- regression_results$r2_train
    
    # If regression has good correlation, adjust the recommendation
    if (regression_weight > 0.3) {  # Only adjust if R² > 0.3
      # Adjust calories based on regression strength
      # Higher correlation = more confidence in the pattern
      adjustment_factor <- 1 + (regression_weight - 0.3) * 0.5  # Max 35% adjustment
      adjusted_calories <- cluster_calories * adjustment_factor
      
      # Check if this was a recovery recommendation
      if (grepl("Recovery", cluster_text)) {
        recommendations$combined <- paste0("Combined (Clustering + Regression + Recovery): ", 
                                          round(adjusted_calories, 0), " calories")
      } else {
        recommendations$combined <- paste0("Combined (Clustering + Regression): ", 
                                          round(adjusted_calories, 0), " calories")
      }
    } else {
      # If regression correlation is low, stick with clustering
      if (grepl("Recovery", cluster_text)) {
        recommendations$combined <- paste0("Combined (Clustering + Regression + Recovery): ", 
                                          round(cluster_calories, 0), " calories (low regression correlation)")
      } else {
        recommendations$combined <- paste0("Combined (Clustering + Regression): ", 
                                          round(cluster_calories, 0), " calories (low regression correlation)")
      }
    }
  }
  
  #4. Personal metrics-based recommendation
  if (!is.null(user_profile)) {
    # Calculate BMR using Mifflin-St Jeor Equation
    if (!is.na(user_profile$weight) && !is.na(user_profile$height) && !is.na(user_profile$age)) {
      # Convert height from feet/inches to cm
      height_parts <- strsplit(user_profile$height, "'")[[1]]
      feet <- as.numeric(height_parts[1])
      inches <- as.numeric(gsub("\"", "", height_parts[2]))
      height_cm <- (feet * 12 + inches) * 2.54
      
      # Calculate BMR (Basal Metabolic Rate)
      if (user_profile$gender == "Male" || is.null(user_profile$gender)) {
        bmr <- 10 * user_profile$weight + 6.25 * height_cm - 5 * user_profile$age + 5
      } else {
        bmr <- 10 * user_profile$weight + 6.25 * height_cm - 5 * user_profile$age - 161
      }
      
      # Calculate TDEE (Total Daily Energy Expenditure) with activity factor
      # Assuming moderate activity level (1.55)
      tdee <- bmr * 1.55
      
      # Adjust based on body fat percentage if available
      if (!is.na(user_profile$body_fat)) {
        if (user_profile$body_fat > 25) {
          # Higher body fat - suggest more active energy burn
          recommended_active <- tdee * 0.3  # 30% of TDEE as active energy
        } else if (user_profile$body_fat < 15) {
          # Lower body fat - suggest moderate active energy burn
          recommended_active <- tdee * 0.2  # 20% of TDEE as active energy
        } else {
          # Normal body fat - suggest balanced active energy burn
          recommended_active <- tdee * 0.25  # 25% of TDEE as active energy
        }
      } else {
        recommended_active <- tdee * 0.25
      }
      
      recommendations$personal <- paste0("Recommended Active Energy (based on your profile): ", round(recommended_active, 0), " calories")
      recommendations$bmr <- paste0("Your Basal Metabolic Rate (BMR): ", round(bmr, 0), " calories")
      recommendations$tdee <- paste0("Your Total Daily Energy Expenditure (TDEE): ", round(tdee, 0), " calories")
    }
  }
  
  #5. Combine recommendations
  if (length(recommendations) == 0) {
    return("No data available for personalized recommendations.")
  }
  
  # Create comprehensive recommendation
  final_recommendation <- "PERSONALIZED RECOMMENDATIONS:\n\n"
  
  if (!is.null(recommendations$clustering)) {
    final_recommendation <- paste0(final_recommendation, recommendations$clustering, "\n")
  }
  
  if (!is.null(recommendations$recovery_note)) {
    final_recommendation <- paste0(final_recommendation, "Recovery Note: ", recommendations$recovery_note, "\n")
  }
  
  if (!is.null(recommendations$regression)) {
    final_recommendation <- paste0(final_recommendation, "Regression-based: ", recommendations$regression, "\n")
  }
  
  if (!is.null(recommendations$combined)) {
    final_recommendation <- paste0(final_recommendation, "Combined (Clustering + Regression): ", recommendations$combined, "\n")
  }
  
  if (!is.null(recommendations$personal)) {
    final_recommendation <- paste0(final_recommendation, "Recommended Active Energy (based on your profile): ", recommendations$personal, "\n")
  }
  
  if (!is.null(recommendations$bmr)) {
    final_recommendation <- paste0(final_recommendation, "Your Basal Metabolic Rate (BMR): ", recommendations$bmr, "\n")
  }
  
  if (!is.null(recommendations$tdee)) {
    final_recommendation <- paste0(final_recommendation, "Your Total Daily Energy Expenditure (TDEE): ", recommendations$tdee, "\n")
  }
  
  return(final_recommendation)
}

# Keep the old function for backward compatibility
get_next_day_recommendation <- function(cluster_df, raw_data) {
  if (is.null(cluster_df) || !"Cluster" %in% names(cluster_df) || !"date_local" %in% names(cluster_df)) {
    return("No clustering results available for recommendation.")
  }
  
  last_row <- tail(cluster_df, 1)
  last_cluster <- as.character(last_row$Cluster)
  
  days_in_cluster <- unique(cluster_df %>% filter(Cluster == last_cluster) %>% pull(date_local))
  
  if (!"metric_name" %in% names(raw_data) || !"value" %in% names(raw_data)) {
    return("No Active Energy Burned data available for recommendation.")
  }
  
  daily_sums <- raw_data %>%
    filter(date_local %in% days_in_cluster, metric_name == "Active Energy Burned") %>%
    group_by(date_local) %>%
    summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop")
  
  if (nrow(daily_sums) == 0) {
    return("No Active Energy Burned data available for recommendation.")
  }
  
  avg_daily_active <- mean(daily_sums$daily_active, na.rm = TRUE)
  paste0("Based on your most recent cluster (", last_cluster, "), we recommend aiming for about ", round(avg_daily_active, 0), " Active Energy Burned tomorrow.")
}

#User Interface
ui <- fluidPage(
  useShinyjs(),
  # Include custom CSS
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  # Main container with modern styling
  div(class = "main-container",
    # App header
    div(class = "app-header",
      h1(class = "app-title", "RegenRate"),
      p(class = "app-subtitle", "Know When to Push, Know When to Rest")
    ),
    
    # Progress indicator
    uiOutput("progress_indicator"),
    
    # Main content
  uiOutput("main_ui")
  ),
  
  # Loading screen
  use_waiter(),
  waiter_show_on_load(
    tagList(
      spin_flower(),
      br(),
      h3("Loading RegenRate...")
    )
  )
)

#Server Logic
server <- function(input, output, session) {
  
  #Reactive Values
  step <- reactiveVal("login") # Track current page/step
  user <- reactiveVal(NULL) # Store username
  user_choices <- reactiveValues(analysis = NULL) # Store analysis choices
  raw_data <- reactiveVal(NULL) # Store uploaded data
  wide_data <- reactiveVal(NULL) # Store data in wide format 
  user_profile <- reactiveVal(NULL) # Store user profile data
  selected_metric_date <- reactiveVal(NULL) # Store selected date for metric visualization
  analysis_results <- reactiveValues(
    cluster = NULL, 
    cluster_plot = NULL, 
    reg = NULL, 
    reg_plot = NULL
  ) # Store analysis results
  
  #Progress Indicator
  output$progress_indicator <- renderUI({
    get_progress_steps(step())
  })
  
  # Hide loading screen
  waiter_hide()
  
  #Main UI Rendering 
  output$main_ui <- renderUI({
    switch(step(),
           "login" = {
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-user-circle", style = "margin-right: 10px;"),
                 "Welcome to RegenRate"
               ),
                                div(style = "text-align: center; padding: 20px;",
                   p("Please log in to your account or create a new profile to get started with your personalized fitness analytics."),
               br(),
                   div(class = "form-group", style = "display: flex; justify-content: center;",
                     div(style = "width: 300px;",
                       textInput("username", "Username", placeholder = "Enter your username")
                     )
                   ),
                   div(class = "form-group", style = "display: flex; justify-content: center;",
                     div(style = "width: 300px;",
                       passwordInput("password", "Password", placeholder = "Enter your password")
                     )
                   ),
                 div(style = "margin: 20px 0;",
                   actionButton("login_btn", "Log In", class = "btn-primary")
                 ),
                 p("Don't have an account?"),
                 actionButton("create_profile_btn", "Create New Profile", class = "btn-secondary")
               )
             )
           },
           "create_profile" = {
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-user-plus", style = "margin-right: 10px;"),
                 "Create Your Profile"
               ),
               div(style = "padding: 20px;",
                 p("Tell us about yourself to get personalized fitness recommendations."),
                 br(),
                 div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                   div(class = "form-group",
                     textInput("username_new", "Username", placeholder = "Choose a username")
                   ),
                   div(class = "form-group",
                     passwordInput("password_new", "Password", placeholder = "Create a password")
                   ),
                   div(class = "form-group",
                     textInput("first_name", "First Name", placeholder = "Your first name")
                   ),
                   div(class = "form-group",
                     textInput("last_name", "Last Name", placeholder = "Your last name")
                   ),
                   div(class = "form-group",
                     numericInput("age", "Age", value = NA, min = 0)
                   ),
                   div(class = "form-group",
                     selectInput("gender", "Gender", choices = c("Male", "Female", "Other"))
                   ),
                   div(class = "form-group",
                     numericInput("weight", "Weight (lbs)", value = NA, min = 0)
                   ),
                   div(class = "form-group",
                     selectInput("height", "Height", choices = height_choices)
                   ),
                   div(class = "form-group",
                     numericInput("bmi", "BMI", value = NA, min = 0)
                   ),
                   div(class = "form-group",
                     numericInput("body_fat", "Body Fat %", value = NA, min = 0, max = 100)
                   )
                 ),
                 div(style = "text-align: center; margin-top: 30px;",
                   actionButton("submit_profile_btn", "Create Profile", class = "btn-primary")
                 )
               )
             )
           },
           "upload" = {
             div(
               # Data Upload Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-cloud-upload-alt", style = "margin-right: 10px;"),
                   "Upload Your Health Data"
                 ),
                 div(style = "padding: 20px;",
                   p("Upload your Apple Health data CSV file to begin your personalized analysis."),
                   div(class = "file-input-wrapper",
                     fileInput("datafile", "Choose CSV File", accept = ".csv", buttonLabel = "Browse Files")
                   ),
                   DTOutput("raw_table")
                 )
               ),
               
               # Profile Update Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-user-edit", style = "margin-right: 10px;"),
                   "Update Your Profile"
                 ),
                 div(style = "padding: 20px;",
                   p("Keep your profile information up to date for the most accurate recommendations."),
                   div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                     div(class = "form-group",
                       textInput("update_first_name", "First Name", value = "", placeholder = "Your first name")
                     ),
                     div(class = "form-group",
                       textInput("update_last_name", "Last Name", value = "", placeholder = "Your last name")
                     ),
                     div(class = "form-group",
                       numericInput("update_age", "Age", value = NA, min = 0)
                     ),
                     div(class = "form-group",
                       selectInput("update_gender", "Gender", choices = c("Male", "Female", "Other"))
                     ),
                     div(class = "form-group",
                       numericInput("update_weight", "Weight (lbs)", value = NA, min = 0)
                     ),
                     div(class = "form-group",
                       selectInput("update_height", "Height", choices = height_choices)
                     ),
                     div(class = "form-group",
                       numericInput("update_bmi", "BMI", value = NA, min = 0)
                     ),
                     div(class = "form-group",
                       numericInput("update_body_fat", "Body Fat %", value = NA, min = 0, max = 100)
                     )
                   ),
                   div(style = "text-align: center; margin-top: 20px;",
                     actionButton("update_profile_btn", "Update Profile", class = "btn-secondary"),
                     actionButton("upload_next", "Continue to Analysis", class = "btn-primary", style = "margin-left: 10px;")
                   )
                 )
               )
             )
           },

           "clustering_config" = {
             div(
               # Variable Selection Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-cogs", style = "margin-right: 10px;"),
                   "Configure Your Analysis"
                 ),
                 div(style = "padding: 20px;",
                   p("Select the variables you want to use for both clustering and regression analysis:"),
                   uiOutput("analysis_vars_ui")
                 )
               ),
               
               # Clustering Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"),
                   "Clustering Analysis"
                 ),
                 div(style = "padding: 20px;",
                   div(style = "text-align: center; margin-bottom: 20px;",
                     actionButton("run_elbow_btn", "Run Elbow Method", class = "btn-secondary")
                   ),
               plotOutput("elbow_plot"),
                   div(style = "text-align: center; margin: 20px 0;",
               numericInput("optimal_k", "Number of Clusters (k):", value = 3, min = 2, max = 10),
                     br(),
                     actionButton("run_clustering_btn", "Run Clustering", class = "btn-primary")
                   )
                 )
               ),
               
               # Regression Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"),
                   "Linear Regression Analysis"
                 ),
                 div(style = "padding: 20px;",
                   p("Both clustering and regression will be performed with your selected variables (X = Variable 1, Y = Variable 2)"),
                   div(style = "text-align: center; margin: 20px 0;",
                     actionButton("run_regression_btn", "Run Linear Regression", class = "btn-primary"),
                     br(), br(),
                     actionButton("analysis_config_next", "Continue to Results", class = "btn-secondary")
                   )
                 )
               )
             )
           },
           "analysis" = {
             div(
               # Analysis Summary Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-bar", style = "margin-right: 10px;"),
                   "Analysis & Modeling Results"
                 ),
                 div(style = "padding: 20px;",
                   p("Your analysis includes the following components:"),
               tags$ul(
                 lapply(user_choices$analysis, function(x) tags$li(x))
                   )
                 )
               ),
               
               # Clustering Results Section
               if ("clustering" %in% user_choices$analysis) {
                 div(class = "card",
                   div(class = "card-header",
                     tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"),
                     "Clustering Results"
                   ),
                   div(style = "padding: 20px;",
                   plotOutput("cluster_plot"),
                   DTOutput("cluster_table")
                   )
                 )
               },
               
               # Regression Results Section
               if ("regression" %in% user_choices$analysis) {
                 div(class = "card",
                   div(class = "card-header",
                     tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"),
                     "Linear Regression Results"
                   ),
                   div(style = "padding: 20px;",
                   verbatimTextOutput("reg_summary"),
                     div(style = "margin: 20px 0;",
                       h5("Training Data with Regression Line"),
                   plotOutput("reg_plot")
                     ),
                     div(style = "margin: 20px 0;",
                       h5("Actual vs Predicted Values"),
                       plotOutput("actual_vs_predicted_plot")
                     ),
                     div(style = "margin: 20px 0;",
                       h5("Residual Plot"),
                       plotOutput("residual_plot")
                     )
                   )
                 )
               },
               
               # Navigation
               div(style = "text-align: center; margin-top: 30px;",
                 actionButton("analysis_next", "Continue to Visualization", class = "btn-primary")
               )
             )
           },
           "metric_plot" = {
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-calendar-alt", style = "margin-right: 10px;"),
                 "Explore Your Data"
               ),
               div(style = "padding: 20px;",
                 p("Select a date to visualize your health metrics and get personalized recommendations."),
               uiOutput("metric_date_ui"),
               plotOutput("metric_day_plot"),
                 div(style = "text-align: center; margin-top: 30px;",
                   actionButton("metric_next", "View Recommendations", class = "btn-primary")
                 )
               )
             )
           },
           "finish" = {
             div(
               # Thank You Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-heart", style = "margin-right: 10px;"),
                   "Your Personalized Recommendations"
                 ),
                 div(style = "padding: 20px; text-align: center;",
               h3("Thank you for using RegenRate!"),
                   p("Here's your complete fitness analysis and personalized recommendations."),
                   verbatimTextOutput("recommendation_text")
                 )
               ),
               
               # Results Summary Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-bar", style = "margin-right: 10px;"),
                   "Analysis Summary"
                 ),
                 div(style = "padding: 20px;",
                   div(style = "margin: 20px 0;",
               h4("Clustering Results:"),
               plotOutput("cluster_plot"),
                     DTOutput("cluster_table")
                   ),
                   div(style = "margin: 20px 0;",
               h4("Regression Results:"),
               verbatimTextOutput("reg_summary"),
                     plotOutput("reg_plot")
                   ),
                   div(style = "margin: 20px 0;",
               h4("Metric by Date:"),
                     plotOutput("metric_day_plot")
                   ),
                   div(style = "margin: 20px 0;",
               h4("Heart Rate by Date:"),
                     plotOutput("finish_hr_plot")
                   )
                 )
               ),
               
               # Restart Section
               div(style = "text-align: center; margin-top: 30px;",
                 actionButton("restart_btn", "Start New Analysis", class = "btn-primary")
               )
             )
           }
    )
  })
  
  # ---- Navigation and Step Transitions ----
  observeEvent(input$create_profile_btn, {
    step("create_profile")
  })
  
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    profile_file <- "profiles.csv"
    if (file.exists(profile_file)) {
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE)
      match_row <- which(
        trimws(as.character(profiles$username)) == trimws(input$username) &
          trimws(as.character(profiles$password)) == trimws(input$password)
      )
      if (length(match_row) == 1) {
        user(input$username)
        step("upload")
        showNotification("Logged in successfully.", type = "message")
      } else {
        showNotification("Invalid username or password.", type = "error")
      }
    } else {
      showNotification("No profiles found. Please create a profile first.", type = "error")
    }
  })
  
  observeEvent(input$submit_profile_btn, {
    # Gather profile info
    profile <- data.frame(
      username = input$username_new,
      password = input$password_new,
      first_name = input$first_name,
      last_name = input$last_name,
      age = input$age,
      gender = input$gender,
      weight = input$weight,
      height = input$height,
      bmi = input$bmi,
      body_fat = input$body_fat,
      stringsAsFactors = FALSE
    )
    profile_file <- "profiles.csv"
    if (file.exists(profile_file)) {
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE)
      if (input$username_new %in% profiles$username) {
        showNotification("Username already exists. Please choose another.", type = "error")
        return()
      }
      write.table(profile, profile_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE, qmethod = "double")
    } else {
      write.csv(profile, profile_file, row.names = FALSE, quote = TRUE, qmethod = "double")
    }
    step("login")
    showNotification("Profile created! Please log in.", type = "message")
  })
  
  # ---- Data Upload and Processing ----
  observeEvent(input$datafile, {
    req(input$datafile)
    df <- read.csv(input$datafile$datapath)
    
    # Clean column names
    names(df) <- gsub("[. ]", "_", tolower(names(df)))
    
    # Date parsing
    if ("date_local" %in% names(df)) {
      df$date_local <- suppressWarnings(lubridate::ymd(df$date_local))
      if (all(is.na(df$date_local))) {
        df$date_local <- suppressWarnings(lubridate::mdy(df$date_local))
      }
      if (all(is.na(df$date_local))) {
        df$date_local <- suppressWarnings(lubridate::dmy(df$date_local))
      }
      if (all(is.na(df$date_local))) {
        df$date_local <- as.Date(df$date_local)
      }
    }

    # Energy metric hourly aggregation logic
    if (!"hour" %in% names(df) && "time_of_day" %in% names(df)) {
      df$hour <- sprintf("%02d:00:00", lubridate::hour(hms::as_hms(df$time_of_day)))
    }
    if ("hourly_sum" %in% names(df)) {
      df <- df %>%
        group_by(date_local, hour, metric_name) %>%
        mutate(
          is_energy_metric = metric_name %in% c("Basal Energy Burned", "Active Energy Burned", "Steps")
        ) %>%
        filter(
          !is_energy_metric | (is_energy_metric & !duplicated(hourly_sum))
        ) %>%
        ungroup()
      df <- df %>%
        mutate(
          value = ifelse(
            metric_name %in% c("Basal Energy Burned", "Active Energy Burned", "Steps") & !is.na(hourly_sum),
            hourly_sum,
            value
          )
        )
    }

    raw_data(df)
    # Automatically set both analysis types
    user_choices$analysis <- c("clustering", "regression")
    step("clustering_config")
  })
  
  # ---- Profile Management ----
  # Load user profile data when reaching upload page
  observe({
    if (step() == "upload" && !is.null(user())) {
      profile_file <- "profiles.csv"
      if (file.exists(profile_file)) {
        profiles <- read.csv(profile_file, stringsAsFactors = FALSE)
        user_profile <- profiles[profiles$username == user(), ]
        if (nrow(user_profile) == 1) {
          # Store profile in reactive value
          user_profile(user_profile)
          
          # Update UI fields
          updateTextInput(session, "update_first_name", value = user_profile$first_name)
          updateTextInput(session, "update_last_name", value = user_profile$last_name)
          updateNumericInput(session, "update_age", value = user_profile$age)
          updateSelectInput(session, "update_gender", selected = user_profile$gender)
          updateNumericInput(session, "update_weight", value = user_profile$weight)
          updateSelectInput(session, "update_height", selected = user_profile$height)
          updateNumericInput(session, "update_bmi", value = user_profile$bmi)
          updateNumericInput(session, "update_body_fat", value = user_profile$body_fat)
        }
      }
    }
  })
  
  # Handle profile update
  observeEvent(input$update_profile_btn, {
    profile_file <- "profiles.csv"
    history_file <- "profile_history.csv"
    
    if (file.exists(profile_file)) {
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE)
      user_row <- which(profiles$username == user())
      if (length(user_row) == 1) {
        # Update the user's profile data
        profiles$first_name[user_row] <- input$update_first_name
        profiles$last_name[user_row] <- input$update_last_name
        profiles$age[user_row] <- input$update_age
        profiles$gender[user_row] <- input$update_gender
        profiles$weight[user_row] <- input$update_weight
        profiles$height[user_row] <- input$update_height
        profiles$bmi[user_row] <- input$update_bmi
        profiles$body_fat[user_row] <- input$update_body_fat
        
        write.csv(profiles, profile_file, row.names = FALSE, quote = TRUE, qmethod = "double")
        
        # Save to history file with timestamp
        history_entry <- data.frame(
          username = user(),
          update_date = Sys.Date(),
          update_time = format(Sys.time(), "%H:%M:%S"),
          first_name = input$update_first_name,
          last_name = input$update_last_name,
          age = input$update_age,
          gender = input$update_gender,
          weight = input$update_weight,
          height = input$update_height,
          bmi = input$update_bmi,
          body_fat = input$update_body_fat,
          stringsAsFactors = FALSE
        )
        
        if (file.exists(history_file)) {
          write.table(history_entry, history_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE, qmethod = "double")
        } else {
          write.csv(history_entry, history_file, row.names = FALSE, quote = TRUE, qmethod = "double")
        }
        
        showNotification("Profile updated successfully! Historical data saved.", type = "message")
      }
    }
  })
  
  # ---- Analysis Navigation ----
  observeEvent(input$upload_next, {
    # Automatically set both analysis types
    user_choices$analysis <- c("clustering", "regression")
    step("clustering_config")
  })
  

  
  # ---- Analysis Configuration ----
  # UI for variable selection (used for both clustering and regression)
  output$analysis_vars_ui <- renderUI({
    df <- raw_data()
    req(df)
    
    available_metrics <- sort(unique(df$metric_name))
    
    tagList(
      selectInput("analysis_var1", "Select First Variable (X):", choices = available_metrics),
      selectInput("analysis_var2", "Select Second Variable (Y):", choices = available_metrics, selected = if(length(available_metrics) > 1) available_metrics[2] else available_metrics[1]),
      p(strong("Note:"), "For clustering, both variables will be used. For regression, Variable 1 will be X (independent) and Variable 2 will be Y (dependent).")
    )
  })
  
  # Reactive value to store elbow results
  elbow_results <- reactiveVal(NULL)
  
  # Handle elbow method button
  observeEvent(input$run_elbow_btn, {
    req(input$analysis_var1, input$analysis_var2)
    
    df <- raw_data()
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) {
      wide <- suppressWarnings(df %>%
        mutate(metric_name = make.names(metric_name)) %>%
        tidyr::pivot_wider(
          id_cols = c(date_local, time_of_day),
          names_from = metric_name,
          values_from = value,
          values_fn = ~mean(as.numeric(.), na.rm = TRUE)
        ))
      
      var1 <- make.names(input$analysis_var1)
      var2 <- make.names(input$analysis_var2)
      
      if (all(c(var1, var2) %in% names(wide))) {
        cluster_data <- wide %>% select(all_of(c(var1, var2))) %>% na.omit()
        
        if (nrow(cluster_data) >= 3) {
          results <- elbow_method(cluster_data, max_k = 10)
          elbow_results(results)
          
          if (nrow(results) > 2) {
            diff_inertia <- diff(results$inertia)
            diff_diff <- diff(diff_inertia)
            suggested_k <- which.max(diff_diff) + 2
            updateNumericInput(session, "optimal_k", value = min(suggested_k, nrow(cluster_data) - 1))
          } else {
            updateNumericInput(session, "optimal_k", value = 2)
          }
          
          showNotification("Elbow method complete! Suggested k value updated. You can now run clustering.", type = "message")
        } else {
          showNotification(paste("Not enough data points for elbow method. Need at least 3 data points, but only have", nrow(cluster_data), "."), type = "error")
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error")
      }
    }
  })
  
  # Elbow plot output
  output$elbow_plot <- renderPlot({
    req(elbow_results())
    
    ggplot(elbow_results(), aes(x = k, y = inertia)) +
      geom_line() +
      geom_point(size = 3) +
      labs(title = "Elbow Method for Optimal k",
           x = "Number of Clusters (k)",
           y = "Inertia (Sum of Squared Distances)") +
      theme_bw()
  })
  
  # Handle clustering button
  observeEvent(input$run_clustering_btn, {
    req(input$analysis_var1, input$analysis_var2, input$optimal_k)
    
    df <- raw_data()
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) {
      wide <- suppressWarnings(df %>%
        mutate(metric_name = make.names(metric_name)) %>%
        tidyr::pivot_wider(
          id_cols = c(date_local, time_of_day),
          names_from = metric_name,
          values_from = value,
          values_fn = ~mean(as.numeric(.), na.rm = TRUE)
        ))
      
      var1 <- make.names(input$analysis_var1)
      var2 <- make.names(input$analysis_var2)
      
      if (all(c(var1, var2) %in% names(wide))) {
        cluster_data <- wide %>% select(date_local, all_of(c(var1, var2))) %>% na.omit()
        
        # Remove outliers using IQR
        Q1 <- apply(cluster_data[, 2:3], 2, quantile, 0.25, na.rm = TRUE)
        Q3 <- apply(cluster_data[, 2:3], 2, quantile, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        keep <- (cluster_data[[2]] >= (Q1[1] - 1.5 * IQR[1])) & (cluster_data[[2]] <= (Q3[1] + 1.5 * IQR[1])) &
                (cluster_data[[3]] >= (Q1[2] - 1.5 * IQR[2])) & (cluster_data[[3]] <= (Q3[2] + 1.5 * IQR[2]))
        cluster_data <- cluster_data[keep, ]
        
        if (nrow(cluster_data) >= input$optimal_k) {
          set.seed(123)
          clust <- suppressWarnings(kmeans(cluster_data[, 2:3], centers = input$optimal_k, nstart = 25))
          cluster_data$Cluster <- as.factor(clust$cluster)
          analysis_results$cluster <- cluster_data
          analysis_results$cluster_plot <- ggplot(cluster_data, aes_string(x = var1, y = var2, color = "Cluster")) +
            geom_point(size = 2) +
            labs(title = paste("K-means Clustering with", input$optimal_k, "Clusters"),
                 x = input$analysis_var1,
                 y = input$analysis_var2) +
            theme_bw()
          
          showNotification("Clustering analysis complete! Click 'Continue to Results' to view.", type = "message")
        } else {
          showNotification("Not enough data points for clustering. Need at least as many points as clusters after outlier removal.", type = "error")
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error")
      }
    }
  })
  
  # Handle regression button
  observeEvent(input$run_regression_btn, {
    req(input$analysis_var1, input$analysis_var2)
    
    df <- raw_data()
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) {
      wide <- suppressWarnings(df %>%
        mutate(metric_name = make.names(metric_name)) %>%
        tidyr::pivot_wider(
          id_cols = c(date_local, time_of_day),
          names_from = metric_name,
          values_from = value,
          values_fn = ~mean(as.numeric(.), na.rm = TRUE)
        ))
      
      x_var <- make.names(input$analysis_var1)
      y_var <- make.names(input$analysis_var2)
      
      if (all(c(x_var, y_var) %in% names(wide))) {
        # Perform regression using selected variables
        results <- perform_linear_regression(wide, x_var, y_var)
        
        if (!is.null(results$error)) {
          showNotification(results$error, type = "error")
        } else {
          regression_results(results)
          
          # Create summary text
          summary_text <- paste0(
            "Linear Regression Results:\n",
            "Dependent Variable (Y): ", input$analysis_var2, "\n",
            "Independent Variable (X): ", input$analysis_var1, "\n",
            "Intercept (β₀): ", round(results$beta[1], 4), "\n",
            "Slope (β₁): ", round(results$beta[2], 4), "\n",
            "Training R²: ", round(results$r2_train, 4), "\n",
            "Test R²: ", round(results$r2_test, 4), "\n",
            "Training Data Points: ", nrow(results$train_data$X), "\n",
            "Test Data Points: ", nrow(results$test_data$X)
          )
          
          analysis_results$reg <- summary_text
          showNotification("Linear regression analysis complete! Click 'Continue to Results' to view.", type = "message")
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error")
      }
    }
  })
  
  # Handle continue to analysis results
  observeEvent(input$analysis_config_next, {
    df <- raw_data()
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) {
      wide <- suppressWarnings(df %>%
        mutate(metric_name = make.names(metric_name)) %>%
        tidyr::pivot_wider(
          id_cols = c(date_local, time_of_day),
          names_from = metric_name,
          values_from = value,
          values_fn = ~mean(as.numeric(.), na.rm = TRUE)
        ))
      metric_cols <- setdiff(names(wide), c("date_local", "time_of_day"))
      wide <- wide %>% mutate(across(all_of(metric_cols), as.numeric))
      wide_data(wide)
    } else {
      wide_data(NULL)
    }
    step("analysis")
  })
  
  # ---- Regression Analysis ----
  # Reactive value to store comprehensive regression results
  regression_results <- reactiveVal(NULL)
  
  # ---- Output Rendering ----
  output$raw_table <- renderDT({
    req(raw_data())
    datatable(raw_data())
  })
  
  # Clustering outputs
  output$cluster_table <- renderDT({
    req(analysis_results$cluster)
    if (nrow(analysis_results$cluster) > 0) datatable(analysis_results$cluster) else NULL
  })
  
  output$cluster_plot <- renderPlot({
    req(analysis_results$cluster_plot)
    analysis_results$cluster_plot
  })
  
  # Regression outputs
  output$reg_summary <- renderPrint({
    req(analysis_results$reg)
      cat(analysis_results$reg)
  })
  
  # Training data scatter plot with regression line
  output$reg_plot <- renderPlot({
    req(regression_results())
    results <- regression_results()
    
    # Debug: Print the structure of results
    print("Regression results structure:")
    print(str(results))
    
    # Create training data dataframe for plotting
    train_df <- data.frame(
      x = as.numeric(results$train_data$X[, 2]),  # Independent variable (skip intercept column)
      y = as.numeric(results$train_data$y),
      y_hat = as.numeric(results$train_data$y_hat)
    )
    
    # Debug: Print the dataframe
    print("Training dataframe:")
    print(head(train_df))
    
    ggplot(train_df, aes(x = x, y = y)) +
      geom_point(color = 'red', size = 2, alpha = 0.6) +
      geom_line(aes(x = x, y = y_hat), color = 'blue', size = 1) +
      labs(
        title = paste("Linear Regression: Training Data"),
        subtitle = paste(results$y_var, "~", results$x_var),
        x = results$x_var,
        y = results$y_var
      ) +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Actual vs Predicted plot
  output$actual_vs_predicted_plot <- renderPlot({
    req(regression_results())
    results <- regression_results()
    
    # Debug: Print results structure
    print("Actual vs Predicted - Results structure:")
    print(str(results))
    
    # Training data
    train_df <- data.frame(
      actual = as.numeric(results$train_data$y),
      predicted = as.numeric(results$train_data$y_hat)
    )
    
    # Test data
    test_df <- data.frame(
      actual = as.numeric(results$test_data$y),
      predicted = as.numeric(results$test_data$y_hat)
    )
    
    # Debug: Print dataframes
    print("Training dataframe:")
    print(head(train_df))
    print("Test dataframe:")
    print(head(test_df))
    
    # Combine for plotting
    plot_df <- rbind(
      data.frame(train_df, type = "Training"),
      data.frame(test_df, type = "Test")
    )
    
    # Debug: Print combined dataframe
    print("Combined plot dataframe:")
    print(head(plot_df))
    
    # Perfect prediction line range
    min_val <- min(plot_df$actual, plot_df$predicted, na.rm = TRUE)
    max_val <- max(plot_df$actual, plot_df$predicted, na.rm = TRUE)
    
    ggplot(plot_df, aes(x = actual, y = predicted, color = type)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) +
      labs(
        title = "Actual vs Predicted Values",
        subtitle = paste(results$y_var, "~", results$x_var),
        x = paste("Actual", results$y_var),
        y = paste("Predicted", results$y_var),
        color = "Data Set"
      ) +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # Residual plot
  output$residual_plot <- renderPlot({
    req(regression_results())
    results <- regression_results()
    
    # Debug: Print results structure
    print("Residual plot - Results structure:")
    print(str(results))
    
    # Training data residuals
    train_df <- data.frame(
      predicted = as.numeric(results$train_data$y_hat),
      residuals = as.numeric(results$train_data$residuals)
    )
    
    # Debug: Print dataframe
    print("Residual dataframe:")
    print(head(train_df))
    
    ggplot(train_df, aes(x = predicted, y = residuals)) +
      geom_point(color = 'blue', alpha = 0.6, size = 2) +
      geom_hline(yintercept = 0, color = 'black', linetype = 'solid', size = 1) +
      labs(
        title = "Residual Plot: Training Data",
        subtitle = paste(results$y_var, "~", results$x_var),
        x = paste("Predicted", results$y_var),
        y = "Residuals"
      ) +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold"))
  })
  
  # ---- Metric Visualization ----
  observeEvent(input$analysis_next, {
    df <- raw_data()
    if (all(c("date_local", "metric_name", "value", "time_of_day") %in% names(df))) {
      step("metric_plot")
    } else {
      step("finish")
    }
  })
  
  # Metric/date selection UI
  output$metric_date_ui <- renderUI({
    df <- raw_data()
    req(df)
    available_dates <- sort(unique(as.Date(df$date_local)))
    available_metrics <- sort(unique(df$metric_name))
    tagList(
      dateInput("metric_date", "Select Date:", value = available_dates[1], min = min(available_dates), max = max(available_dates)),
      selectInput("metric_metric", "Select Metric:", choices = available_metrics, selected = available_metrics[1])
    )
  })

  # Create plot for selected metric and date
  output$metric_day_plot <- renderPlot({
    req(input$metric_date, input$metric_metric)
    
    # Store the selected date for recommendations
    selected_metric_date(input$metric_date)
    
    df <- raw_data()
    
    plot_df <- df %>%
      filter(as.Date(date_local) == as.Date(input$metric_date), metric_name == input$metric_metric)
    
    if (nrow(plot_df) == 0) return(NULL)
    
    plot_df <- plot_df %>%
      mutate(
        value = as.numeric(value),
        time_of_day = hms::as_hms(time_of_day)
      )
    
    # Remove implausible values for heart rate
    if (grepl("heart rate", tolower(input$metric_metric))) {
      plot_df <- plot_df %>% filter(value >= 40, value <= 200)
    }
    
    # Remove outliers using rolling median if enough data
    if (nrow(plot_df) >= 5) {
      plot_df <- plot_df %>%
        arrange(time_of_day) %>%
        mutate(
          rolling_median = zoo::rollmedian(value, k = 5, fill = NA),
          is_outlier = abs(value - rolling_median) > 30
        ) %>%
        filter(!is_outlier | is.na(is_outlier))
    }
    
    # Aggregate by minute for smoother plot
    plot_df <- plot_df %>%
      mutate(minute = format(time_of_day, "%H:%M")) %>%
      group_by(minute) %>%
      summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
    
    ggplot(plot_df, aes(x = hms::as_hms(paste0(minute, ":00")), y = mean_value)) +
      geom_line() +
      labs(
        title = paste(input$metric_metric, "by Time of Day (", input$metric_date, ")"),
        x = "Time of Day",
        y = input$metric_metric
      ) +
      theme_bw()
  })
  
  # ---- Sleep Analysis and Recommendations ----
  # Reactive for sleep periods
  sleep_periods <- reactive({
    req(raw_data())
    find_sleep_periods(raw_data(), threshold = 60, min_duration = 20)
  })

  # Daily sleep summary
  sleep_summary <- reactive({
    req(sleep_periods())
    sleep_periods() %>%
      group_by(date_local) %>%
      summarise(
        bedtime = format(min(start_time), "%H:%M"),
        waketime = format(max(end_time), "%H:%M"),
        total_sleep_hours = round(sum(duration) / 60, 2),
        .groups = "drop"
      )
  })

  output$sleep_summary_table <- renderDT({
    req(sleep_summary())
    datatable(sleep_summary())
  })

  output$sleep_recommendation <- renderText({
    ss <- sleep_summary()
    if (nrow(ss) == 0) return("No sleep detected.")
    last <- tail(ss, 1)
    paste0(
      "We recommend you go to bed around ", last$bedtime,
      " and aim for at least ", last$total_sleep_hours, " hours of sleep tonight."
    )
  })

  # ---- Final Page and Navigation ----
  observeEvent(input$metric_next, { 
    step("finish") 
  })
  
  observeEvent(input$restart_btn, {
    user(NULL)
    user_choices$analysis <- NULL
    raw_data(NULL)
    wide_data(NULL)
    analysis_results$cluster <- NULL
    analysis_results$cluster_plot <- NULL
    analysis_results$reg <- NULL
    analysis_results$reg_plot <- NULL
    step("login")
  })
  
  # Heart rate plot for final page
  output$finish_hr_plot <- renderPlot({
    req(raw_data(), selected_metric_date())
    df <- raw_data()
    hr_day <- df %>%
      filter(as.Date(date_local) == as.Date(selected_metric_date())) %>%
      filter(metric_name == "Heart Rate") %>%
      mutate(
        value = as.numeric(value),
        time_of_day = hms::as_hms(time_of_day)
      )
    if (nrow(hr_day) == 0) return(NULL)
    ggplot(hr_day, aes(x = time_of_day, y = value)) +
      geom_line() +
      labs(
        title = paste("Heart Rate by Time of Day (", selected_metric_date(), ")", sep = ""),
        x = "Time of Day",
        y = "Heart Rate (BPM)"
      ) +
      theme_bw()
  })
  
  # Recommendation text
  output$recommendation_text <- renderText({
    get_enhanced_recommendation(analysis_results$cluster, raw_data(), regression_results(), user_profile(), selected_metric_date())
  })
}

# ---- Launch the App ----
shinyApp(ui, server)