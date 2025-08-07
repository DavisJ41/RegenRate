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

# Increase upload size limit to 600MB for large health data files
options(shiny.maxRequestSize = 600*1024^2)  

#Set modern ggplot theme
theme_set(theme_minimal() +
  theme(
    text = element_text(family = "Segoe UI", size = 12), # Modern font family
    plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"), # Bold titles
    plot.subtitle = element_text(size = 14, color = "#7f8c8d"), # Subtitle styling
    axis.title = element_text(size = 12, color = "#2c3e50"), # Axis labels
    axis.text = element_text(size = 10, color = "#7f8c8d"), # Axis text
    panel.grid.major = element_line(color = "#ecf0f1", size = 0.5), # Major grid lines
    panel.grid.minor = element_line(color = "#ecf0f1", size = 0.25), # Minor grid lines
    panel.background = element_rect(fill = "white"), # White background
    plot.background = element_rect(fill = "white"), # White plot background
    legend.title = element_text(size = 12, color = "#2c3e50"), # Legend title
    legend.text = element_text(size = 10, color = "#7f8c8d") # Legend text
  ))

#custom css - Modern styling for the entire application
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

/* Button Styling - Primary and secondary buttons */
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

/* File Upload Styling - Drag and drop file upload area */
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

/* Notification Styling - Pop-up notifications */
.shiny-notification {
  border-radius: 10px;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
}

/* Recovery Level Indicators - Color-coded recovery recommendations */
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

/* Responsive Design - Mobile-friendly adjustments */
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
for (ft in 2:7) { # Loop through feet from 2 to 7
  for (inch in 0:11) { # Loop through inches from 0 to 11
    label <- sprintf("%d'%02d\"", ft, inch) # Format as "5'08"" style
    height_choices <- c(height_choices, label) # Add to choices vector
  }
}
height_choices <- height_choices[1:((7-2)*12+1)] # Limit to valid range (2'0" to 7'0")

#functions
# Progress indicator function
get_progress_steps <- function(current_step) {
  steps <- c("Login", "Profile", "Upload", "Configure", "Results", "Visualize", "Summary") # Step names for display
  step_names <- c("login", "create_profile", "upload", "clustering_config", "analysis", "metric_plot", "finish") # Internal step names
  
  progress_html <- '<div class="progress-container">' # Start progress container
  for (i in seq_along(steps)) { # Loop through each step
    step_class <- "progress-step" # Default class
    if (step_names[i] == current_step) { # If this is the current step
      step_class <- "progress-step active" # Mark as active
    } else if (which(step_names == current_step) > i) { # If we've passed this step
      step_class <- "progress-step completed" # Mark as completed
    }
    progress_html <- paste0(progress_html, 
                           '<div class="', step_class, '">', i, '. ', steps[i], '</div>')
  }
  progress_html <- paste0(progress_html, '</div>')
  return(HTML(progress_html))
}

# Elbow method for optimal k in clustering
elbow_method <- function(data, max_k = 10) {
  max_k <- min(max_k, nrow(data) - 1) # Don't exceed data points - 1
  if (max_k < 2) { # If not enough data for clustering
    return(data.frame(k = 1, inertia = var(data))) # Return single cluster
  }
  
  inertia <- numeric(max_k) # Vector to store inertia values
  k_values <- 1:max_k # K values to test
  
  for (k in k_values) { # Loop through each k value
    set.seed(123) # Set random seed for reproducibility
    km <- suppressWarnings(kmeans(data, centers = k, nstart = 25)) # Run k-means clustering
    inertia[k] <- km$tot.withinss # Store total within-cluster sum of squares
  }
  
  return(data.frame(k = k_values, inertia = inertia))
}

#Linear Regression Functions
# Function to compute the best line of fit using matrix operations
standRegres <- function(xArr, yArr) {
  # Convert to matrices
  xMat <- as.matrix(xArr)
  yMat <- as.matrix(yArr)
  
  # Compute X^T * X (transpose of X times X)
  xTx <- t(xMat) %*% xMat
  
  # Check if determinant = 0 (singular matrix)
  if (det(xTx) == 0.0) {
    print("This matrix is singular, cannot do inverse")
    return(NULL)
  }
  
  # Compute coefficients: (X^T * X)^(-1) * (X^T * y) - Normal equation
  ws <- solve(xTx) %*% (t(xMat) %*% yMat)
  return(ws)
}

# Function to compute Residual Sum of Squares (RSS) error - Sum of squared residuals
rssError <- function(yArr, yHatArr) {
  # Calculate squared differences between actual and predicted values
  return(sum((yArr - yHatArr)^2))
}

# Function to compute Total Sum of Squares (TSS) error - Total variation in dependent variable
tssError <- function(yArr, yHatArr) {
  # TSS is sum of squared differences between actual values and mean of predicted values
  return(sum((yArr - mean(yHatArr))^2))
}

# Function to compute R-squared (Coefficient of Determination) - How well model fits data
Rsquare <- function(yArr, yHatArr) {
  rss <- rssError(yArr, yHatArr) # Get residual sum of squares
  tss <- tssError(yArr, yHatArr) # Get total sum of squares
  return(1 - rss / tss) # R-squared = 1 - (RSS/TSS)
}

  # Function to perform comprehensive linear regression analysis
  perform_linear_regression <- function(data, x_var, y_var, train_ratio = 0.75) {
    # Select variables and remove NA values
    lm_data <- data[, c(x_var, y_var)] %>% na.omit()
    
    if (nrow(lm_data) < 10) { # Need at least 10 data points for reliable regression
      return(list(error = "Not enough data for regression analysis"))
    }
    
    # Add intercept column (X0 = 1) for matrix operations
    lm_data$X0 <- 1
    
    # Shuffle data for random train/test split
    set.seed(123)
    lm_data <- lm_data[sample(nrow(lm_data)), ]
    
    # Split into training and test sets
    train_size <- floor(train_ratio * nrow(lm_data)) # 75% for training
    train <- lm_data[1:train_size, ] # Training data
    test <- lm_data[(train_size + 1):nrow(lm_data), ] # Test data
    
    # Prepare training data
    X_train <- as.matrix(train[, c("X0", x_var)])
    y_train <- as.matrix(train[, y_var])
    
    # Prepare test data
    X_test <- as.matrix(test[, c("X0", x_var)])
    y_test <- as.matrix(test[, y_var])
    
    # Compute regression coefficients using normal equation
    beta <- standRegres(X_train, y_train)
    
    if (is.null(beta)) {
      return(list(error = "Could not compute regression coefficients"))
    }
    
    # Compute predicted values for training set
    y_hat_train <- X_train %*% beta
    
    # Compute predicted values for test set
    y_hat_test <- X_test %*% beta
    
    # Compute R-squared for both sets
    r2_train <- Rsquare(y_train, y_hat_train) # Training R-squared
    r2_test <- Rsquare(y_test, y_hat_test) # Test R-squared
    
    # Compute residuals for training set
    residuals_train <- y_train - y_hat_train
    
    # Convert matrices to vectors for easier plotting
    return(list(
      beta = beta, # Regression coefficients
      train_data = list(
        X = X_train, # Training design matrix
        y = as.vector(y_train), # Training response
        y_hat = as.vector(y_hat_train), # Training predictions
        residuals = as.vector(residuals_train) # Training residuals
      ),
      test_data = list(
        X = X_test, # Test design matrix
        y = as.vector(y_test), # Test response
        y_hat = as.vector(y_hat_test) # Test predictions
      ),
      r2_train = r2_train, # Training R-squared
      r2_test = r2_test, # Test R-squared
      x_var = x_var, # Independent variable name
      y_var = y_var # Dependent variable name
    ))
  }

# Find sleep periods using heart rate threshold - Identifies sleep periods based on low heart rate
find_sleep_periods <- function(df, threshold = 60, min_duration = 20) {
  hr_df <- df %>%
    filter(metric_name == "Heart Rate") %>%
    mutate(
      value = as.numeric(value), # Convert to numeric
      datetime = as.POSIXct(paste(date_local, time_of_day)), # Combine date and time
      is_sleep = value < threshold # Mark as sleep if heart rate below threshold
    ) %>%
    arrange(date_local, datetime) # Sort by date and time
  
  hr_df$block <- cumsum(c(0, diff(hr_df$is_sleep) != 0)) # Create blocks for consecutive sleep periods
  
  sleep_blocks <- hr_df %>%
    filter(is_sleep) %>%
    group_by(date_local, block) %>%
    summarise(
      start_time = min(datetime), # Start time of sleep period
      end_time = max(datetime), # End time of sleep period
      duration = as.numeric(difftime(max(datetime), min(datetime), units = "mins")), # Duration in minutes
      .groups = "drop"
    ) %>%
    filter(duration >= min_duration) # Only keep periods longer than minimum duration
  
  return(sleep_blocks)
}

# Smart recovery analysis function with multi-window approach
analyze_recent_activity <- function(raw_data, selected_date, days_back = 14) {
  if (is.null(selected_date) || is.null(raw_data)) return(NULL)
  
  # Get recent activity data (14 days for better analysis)
  recent_data <- raw_data %>%
    filter(metric_name == "Active Energy Burned") %>% # Filter for active energy data
    filter(as.Date(date_local) <= as.Date(selected_date)) %>% # Up to selected date
    filter(as.Date(date_local) >= as.Date(selected_date) - days_back) %>% # Last 14 days
    group_by(date_local) %>% # Group by date
    summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") %>% # Sum daily active energy
    arrange(date_local) # Sort by date
  
  if (nrow(recent_data) < 5) return(NULL) # Need at least 5 days of data
  
  # Calculate baseline metrics
  avg_recent <- mean(recent_data$daily_active, na.rm = TRUE) # Average daily active energy
  high_activity_threshold <- avg_recent * 1.2  # 20% above average is considered high activity
  
  #Multi-window Analysis
  
  # 1. Last 3 days analysis (immediate stress)
  last_3_days <- tail(recent_data$daily_active, 3) # Get last 3 days
  last_3_avg <- mean(last_3_days, na.rm = TRUE) # Average of last 3 days
  consecutive_high_3d <- sum(last_3_days > high_activity_threshold) # Count high activity days
  
  # 2. Last 7 days analysis (weekly pattern)
  last_7_days <- tail(recent_data$daily_active, 7) # Get last 7 days
  last_7_avg <- mean(last_7_days, na.rm = TRUE) # Average of last 7 days
  high_days_7d <- sum(last_7_days > high_activity_threshold) # Count high activity days
  
  # 3. Last 14 days analysis (bi-weekly pattern)
  last_14_avg <- mean(recent_data$daily_active, na.rm = TRUE) # Average of last 14 days
  high_days_14d <- sum(recent_data$daily_active > high_activity_threshold) # Count high activity days
  
  # 4. Trend analysis (last 7 vs previous 7)
  if (nrow(recent_data) >= 14) { # If we have at least 14 days
    last_7_avg <- mean(tail(recent_data$daily_active, 7), na.rm = TRUE) # Last 7 days average
    prev_7_avg <- mean(recent_data$daily_active[(nrow(recent_data)-13):(nrow(recent_data)-7)], na.rm = TRUE) # Previous 7 days average
    weekly_trend <- (last_7_avg - prev_7_avg) / prev_7_avg # Calculate trend percentage
  } else {
    weekly_trend <- 0 # No trend if not enough data
  }
  
  # 5. Consecutive high days (from most recent)
  consecutive_high_days <- 0 # Initialize counter
  for (i in nrow(recent_data):1) { # Loop backwards from most recent
    if (recent_data$daily_active[i] > high_activity_threshold) { # If day was high activity
      consecutive_high_days <- consecutive_high_days + 1 # Increment counter
    } else {
      break # Stop counting when we hit a low activity day
    }
  }
  
  return(list(
    avg_recent = avg_recent, # Average recent activity
    last_3_avg = last_3_avg, # Last 3 days average
    last_7_avg = last_7_avg, # Last 7 days average
    last_14_avg = last_14_avg, # Last 14 days average
    consecutive_high_days = consecutive_high_days, # Consecutive high activity days
    consecutive_high_3d = consecutive_high_3d, # High activity days in last 3
    high_days_7d = high_days_7d, # High activity days in last 7
    high_days_14d = high_days_14d, # High activity days in last 14
    weekly_trend = weekly_trend, # Weekly trend percentage
    high_activity_threshold = high_activity_threshold # Threshold for high activity
  ))
}

# Enhanced recommendation function incorporating regression and personal metrics
get_enhanced_recommendation <- function(cluster_df, raw_data, regression_results, user_profile, selected_date = NULL) {
  recommendations <- list() # Initialize recommendations list
  
  #Clustering-based recommendation
  if (!is.null(cluster_df) && "Cluster" %in% names(cluster_df) && "date_local" %in% names(cluster_df)) {
    # If a specific date is selected, find the cluster for that date
    if (!is.null(selected_date)) {
      date_cluster <- cluster_df %>% 
        filter(as.Date(date_local) == as.Date(selected_date)) %>% # Filter for selected date
        tail(1)  # Get the most recent entry for that date
      
      if (nrow(date_cluster) > 0) { # If we found a cluster for that date
        target_cluster <- as.character(date_cluster$Cluster) # Get cluster number
      } else {
        # Fall back to most recent cluster if date not found
        target_cluster <- as.character(tail(cluster_df, 1)$Cluster)
      }
    } else {
      # Use most recent cluster if no date selected
      target_cluster <- as.character(tail(cluster_df, 1)$Cluster)
    }
    
    days_in_cluster <- unique(cluster_df %>% filter(Cluster == target_cluster) %>% pull(date_local)) # Get all days in this cluster
    
    if ("metric_name" %in% names(raw_data) && "value" %in% names(raw_data)) {
      daily_sums <- raw_data %>%
        filter(date_local %in% days_in_cluster, metric_name == "Active Energy Burned") %>% # Filter for cluster days and active energy
        group_by(date_local) %>% # Group by date
        summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") # Sum daily active energy
      
      if (nrow(daily_sums) > 0) { # If we have data for this cluster
        avg_daily_active <- mean(daily_sums$daily_active, na.rm = TRUE) # Calculate average daily active energy
        
        #Smart Recovery Analysis
        recovery_analysis <- analyze_recent_activity(raw_data, selected_date) # Analyze recent activity for recovery
        
        if (!is.null(recovery_analysis)) { # If recovery analysis worked
          #Multi-window Recovery Logic
          
          # 1. Severe Recovery (3+ consecutive high days OR 5+ high days in last 7)
          if (recovery_analysis$consecutive_high_days >= 3 || recovery_analysis$high_days_7d >= 5) {
            recovery_factor <- 0.5  # 50% of normal (more aggressive recovery)
            recovery_calories <- avg_daily_active * recovery_factor # Calculate recovery calories
            recommendations$clustering <- paste0("Cluster-based (Severe Recovery): ", 
                                               round(recovery_calories, 0), " calories") # Format recommendation
            recommendations$recovery_note <- paste0("High activity detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days OR ", 
                                                   recovery_analysis$high_days_7d, " high days in last week. Strong recovery recommended.") # Recovery note
          
          # 2. Moderate Recovery (2 consecutive high days OR 3-4 high days in last 7 OR high weekly trend)
          } else if (recovery_analysis$consecutive_high_days >= 2 || 
                     recovery_analysis$high_days_7d >= 3 || 
                     recovery_analysis$weekly_trend > 0.25) {
            recovery_factor <- 0.7  # 70% of normal
            recovery_calories <- avg_daily_active * recovery_factor # Calculate recovery calories
            recommendations$clustering <- paste0("Cluster-based (Moderate Recovery): ", 
                                               round(recovery_calories, 0), " calories") # Format recommendation
            recommendations$recovery_note <- paste0("Moderate stress detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days, ", 
                                                   recovery_analysis$high_days_7d, " high days in last week, or ", 
                                                   round(recovery_analysis$weekly_trend * 100, 0), "% weekly increase. Consider lighter activity.") # Recovery note
          
          # 3. Light Recovery (1 consecutive high day OR 2 high days in last 7)
          } else if (recovery_analysis$consecutive_high_days >= 1 || recovery_analysis$high_days_7d >= 2) {
            recovery_factor <- 0.85  # 85% of normal
            recovery_calories <- avg_daily_active * recovery_factor # Calculate recovery calories
            recommendations$clustering <- paste0("Cluster-based (Light Recovery): ", 
                                               round(recovery_calories, 0), " calories") # Format recommendation
            recommendations$recovery_note <- paste0("Light stress detected: ", 
                                                   recovery_analysis$consecutive_high_days, " consecutive high days or ", 
                                                   recovery_analysis$high_days_7d, " high days in last week. Slight reduction recommended.") # Recovery note
          
          # 4. Normal recommendation
          } else {
            recommendations$clustering <- paste0("Cluster-based: ", round(avg_daily_active, 0), " calories") # Normal recommendation
          }
        } else {
          # Fallback to normal recommendation
          recommendations$clustering <- paste0("Cluster-based: ", round(avg_daily_active, 0), " calories") # Fallback recommendation
        }
      }
    }
  }
  
  #2. Regression-based recommendation
  if (!is.null(regression_results) && !is.null(regression_results$beta)) { # If regression results exist
    recommendations$regression <- paste0("Regression-based: Model suggests ", 
                                       round(regression_results$r2_train * 100, 1), 
                                       "% correlation between your variables") # Format regression recommendation
  }
  
  #3. Combined clustering + regression recommendation
  if (!is.null(recommendations$clustering) && !is.null(regression_results) && !is.null(regression_results$beta)) {
    # Extract the cluster-based calorie recommendation (handle recovery labels)
    cluster_text <- recommendations$clustering # Get cluster recommendation text
    cluster_calories <- as.numeric(gsub("[^0-9.]", "", cluster_text)) # Extract numeric calories
    
    # Use regression R-squared as a confidence weight
    regression_weight <- regression_results$r2_train # Get regression R-squared
    
    # If regression has good correlation, adjust the recommendation
    if (regression_weight > 0.3) {  # Only adjust if R² > 0.3
      # Adjust calories based on regression strength
      # Higher correlation = more confidence in the pattern
      adjustment_factor <- 1 + (regression_weight - 0.3) * 0.5  # Max 35% adjustment
      adjusted_calories <- cluster_calories * adjustment_factor # Calculate adjusted calories
      
      # Check if this was a recovery recommendation
      if (grepl("Recovery", cluster_text)) { # If it's a recovery recommendation
        recommendations$combined <- paste0("Combined (Clustering + Regression + Recovery): ", 
                                          round(adjusted_calories, 0), " calories") # Format combined recommendation
      } else {
        recommendations$combined <- paste0("Combined (Clustering + Regression): ", 
                                          round(adjusted_calories, 0), " calories") # Format combined recommendation
      }
    } else {
      # If regression correlation is low, stick with clustering
      if (grepl("Recovery", cluster_text)) {
        recommendations$combined <- paste0("Combined (Clustering + Regression + Recovery): ", 
                                          round(cluster_calories, 0), " calories (low regression correlation)") # Format combined recommendation
      } else {
        recommendations$combined <- paste0("Combined (Clustering + Regression): ", 
                                          round(cluster_calories, 0), " calories (low regression correlation)") # Format combined recommendation
      }
    }
  }
  
  #4. Personal metrics-based recommendation
  if (!is.null(user_profile)) { # If user profile exists
    # Calculate BMR using Mifflin-St Jeor Equation
    if (!is.na(user_profile$weight) && !is.na(user_profile$height) && !is.na(user_profile$age)) {
      # Convert height from feet/inches to cm
      height_parts <- strsplit(user_profile$height, "'")[[1]] # Split height string
      feet <- as.numeric(height_parts[1]) # Extract feet
      inches <- as.numeric(gsub("\"", "", height_parts[2])) # Extract inches
      height_cm <- (feet * 12 + inches) * 2.54 # Convert to centimeters
      
      # Calculate BMR (Basal Metabolic Rate)
      if (user_profile$gender == "Male" || is.null(user_profile$gender)) { # Male BMR calculation
        bmr <- 10 * user_profile$weight + 6.25 * height_cm - 5 * user_profile$age + 5
      } else { # Female BMR calculation
        bmr <- 10 * user_profile$weight + 6.25 * height_cm - 5 * user_profile$age - 161
      }
      
      # Calculate TDEE (Total Daily Energy Expenditure) with activity factor
      # Assuming moderate activity level (1.55)
      tdee <- bmr * 1.55 # Total daily energy expenditure
      
      # Adjust based on body fat percentage if available
      if (!is.na(user_profile$body_fat)) { # If body fat percentage is available
        if (user_profile$body_fat > 25) { # Higher body fat - suggest more active energy burn
          recommended_active <- tdee * 0.3  # 30% of TDEE as active energy
        } else if (user_profile$body_fat < 15) { # Lower body fat - suggest moderate active energy burn
          recommended_active <- tdee * 0.2  # 20% of TDEE as active energy
        } else { # Normal body fat - suggest balanced active energy burn
          recommended_active <- tdee * 0.25  # 25% of TDEE as active energy
        }
      } else {
        recommended_active <- tdee * 0.25 # Default 25% of TDEE
      }
      
      recommendations$personal <- paste0("Recommended Active Energy (based on your profile): ", round(recommended_active, 0), " calories") # Personal recommendation
      recommendations$bmr <- paste0("Your Basal Metabolic Rate (BMR): ", round(bmr, 0), " calories") # BMR information
      recommendations$tdee <- paste0("Your Total Daily Energy Expenditure (TDEE): ", round(tdee, 0), " calories") # TDEE information
    }
  }
  
  #5. Combine recommendations
  if (length(recommendations) == 0) {
    return("No data available for personalized recommendations.")
  }
  
  # Create comprehensive recommendation
  final_recommendation <- "PERSONALIZED RECOMMENDATIONS:\n\n" # Start recommendation header
  
  if (!is.null(recommendations$clustering)) { # If clustering recommendation exists
    final_recommendation <- paste0(final_recommendation, recommendations$clustering, "\n") # Add clustering recommendation
  }
  
  if (!is.null(recommendations$recovery_note)) { # If recovery note exists
    final_recommendation <- paste0(final_recommendation, "Recovery Note: ", recommendations$recovery_note, "\n") # Add recovery note
  }
  
  if (!is.null(recommendations$regression)) { # If regression recommendation exists
    final_recommendation <- paste0(final_recommendation, "Regression-based: ", recommendations$regression, "\n") # Add regression recommendation
  }
  
  if (!is.null(recommendations$combined)) { # If combined recommendation exists
    final_recommendation <- paste0(final_recommendation, "Combined (Clustering + Regression): ", recommendations$combined, "\n") # Add combined recommendation
  }
  
  if (!is.null(recommendations$personal)) { # If personal recommendation exists
    final_recommendation <- paste0(final_recommendation, "Recommended Active Energy (based on your profile): ", recommendations$personal, "\n") # Add personal recommendation
  }
  
  if (!is.null(recommendations$bmr)) { # If BMR information exists
    final_recommendation <- paste0(final_recommendation, "Your Basal Metabolic Rate (BMR): ", recommendations$bmr, "\n") # Add BMR information
  }
  
  if (!is.null(recommendations$tdee)) { # If TDEE information exists
    final_recommendation <- paste0(final_recommendation, "Your Total Daily Energy Expenditure (TDEE): ", recommendations$tdee, "\n") # Add TDEE information
  }
  
  return(final_recommendation) # Return final recommendation string
}

# Keep the old function for backward compatibility
get_next_day_recommendation <- function(cluster_df, raw_data) {
  if (is.null(cluster_df) || !"Cluster" %in% names(cluster_df) || !"date_local" %in% names(cluster_df)) {
    return("No clustering results available for recommendation.")
  }
  
  last_row <- tail(cluster_df, 1) # Get most recent cluster
  last_cluster <- as.character(last_row$Cluster) # Get cluster number
  
  days_in_cluster <- unique(cluster_df %>% filter(Cluster == last_cluster) %>% pull(date_local)) # Get days in this cluster
  
  if (!"metric_name" %in% names(raw_data) || !"value" %in% names(raw_data)) {
    return("No Active Energy Burned data available for recommendation.")
  }
  
  daily_sums <- raw_data %>%
    filter(date_local %in% days_in_cluster, metric_name == "Active Energy Burned") %>% # Filter for cluster days and active energy
    group_by(date_local) %>% # Group by date
    summarise(daily_active = sum(as.numeric(value), na.rm = TRUE), .groups = "drop") # Sum daily active energy
  
  if (nrow(daily_sums) == 0) {
    return("No Active Energy Burned data available for recommendation.")
  }
  
  avg_daily_active <- mean(daily_sums$daily_active, na.rm = TRUE) # Calculate average daily active energy
  paste0("Based on your most recent cluster (", last_cluster, "), we recommend aiming for about ", round(avg_daily_active, 0), " Active Energy Burned tomorrow.") # Return recommendation
}

#User Interface
ui <- fluidPage(
  useShinyjs(), # Enable JavaScript functionality
  # Include custom CSS
  tags$head(
    tags$style(HTML(custom_css)), # Apply custom CSS styling
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css") # Font Awesome icons
  ),
  
  # Main container with modern styling
  div(class = "main-container",
    # App header
    div(class = "app-header",
      h1(class = "app-title", "RegenRate"), # Main app title
      p(class = "app-subtitle", "Know When to Push, Know When to Rest") # App subtitle
    ),
    
    # Progress indicator
    uiOutput("progress_indicator"),
    
    # Main content - Dynamic UI based on current step
  uiOutput("main_ui")
  ),
  
  # Loading screen - Shows while app is loading
  use_waiter(), # Enable waiter functionality
  waiter_show_on_load(
    tagList(
      spin_flower(), # Loading spinner
      br(), # Line break
      h3("Loading RegenRate...") # Loading message
    )
  )
)

#Server Logic
server <- function(input, output, session) {
  
  #Reactive Values - Store app state and data
  step <- reactiveVal("login") # Track current page/step
  user <- reactiveVal(NULL) # Store username
  user_choices <- reactiveValues(analysis = NULL) # Store analysis choices
  raw_data <- reactiveVal(NULL) # Store uploaded data
  wide_data <- reactiveVal(NULL) # Store data in wide format 
  user_profile <- reactiveVal(NULL) # Store user profile data
  selected_metric_date <- reactiveVal(NULL) # Store selected date for metric visualization
  analysis_results <- reactiveValues(
    cluster = NULL, # Store clustering results
    cluster_plot = NULL, # Store clustering plot
    reg = NULL, # Store regression results
    reg_plot = NULL # Store regression plot
  ) # Store analysis results
  
  #Progress Indicator
  output$progress_indicator <- renderUI({
    get_progress_steps(step())
  })
  
  # Hide loading screen
  waiter_hide()
  
  #Main UI Rendering 
  output$main_ui <- renderUI({
    switch(step(), # Switch based on current step
           "login" = { # Login page UI
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-user-circle", style = "margin-right: 10px;"), # User icon
                 "Welcome to RegenRate" # Header text
               ),
                                div(style = "text-align: center; padding: 20px;",
                   p("Please log in to your account or create a new profile to get started with your personalized fitness analytics."), # Welcome message
               br(), # Line break
                   div(class = "form-group", style = "display: flex; justify-content: center;", # Center form
                     div(style = "width: 300px;", # Form width
                       textInput("username", "Username", placeholder = "Enter your username") # Username input
                     )
                   ),
                   div(class = "form-group", style = "display: flex; justify-content: center;", # Center form
                     div(style = "width: 300px;", # Form width
                       passwordInput("password", "Password", placeholder = "Enter your password") # Password input
                     )
                   ),
                 div(style = "margin: 20px 0;", # Button container
                   actionButton("login_btn", "Log In", class = "btn-primary") # Login button
                 ),
                 p("Don't have an account?"), # Account creation prompt
                 actionButton("create_profile_btn", "Create New Profile", class = "btn-secondary") # Create profile button
               )
             )
           },
           "create_profile" = { # Profile creation page UI
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-user-plus", style = "margin-right: 10px;"),
                 "Create Your Profile"
               ),
               div(style = "padding: 20px;",
                 p("Tell us about yourself to get personalized fitness recommendations."), # Instructions
                 br(), # Line break
                 div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;", # Grid layout
                   div(class = "form-group",
                     textInput("username_new", "Username", placeholder = "Choose a username") # New username input
                   ),
                   div(class = "form-group",
                     passwordInput("password_new", "Password", placeholder = "Create a password") # New password input
                   ),
                   div(class = "form-group",
                     textInput("first_name", "First Name", placeholder = "Your first name") # First name input
                   ),
                   div(class = "form-group",
                     textInput("last_name", "Last Name", placeholder = "Your last name") # Last name input
                   ),
                   div(class = "form-group",
                     numericInput("age", "Age", value = NA, min = 0) # Age input
                   ),
                   div(class = "form-group",
                     selectInput("gender", "Gender", choices = c("Male", "Female", "Other")) # Gender selection
                   ),
                   div(class = "form-group",
                     numericInput("weight", "Weight (lbs)", value = NA, min = 0) # Weight input
                   ),
                   div(class = "form-group",
                     selectInput("height", "Height", choices = height_choices) # Height selection
                   ),
                   div(class = "form-group",
                     numericInput("bmi", "BMI", value = NA, min = 0) # BMI input
                   ),
                   div(class = "form-group",
                     numericInput("body_fat", "Body Fat %", value = NA, min = 0, max = 100) # Body fat input
                   )
                 ),
                 div(style = "text-align: center; margin-top: 30px;", # Button container
                   actionButton("submit_profile_btn", "Create Profile", class = "btn-primary") # Submit profile button
                 )
               )
             )
           },
           "upload" = { # Data upload page UI
             div(
               # Data Upload Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-cloud-upload-alt", style = "margin-right: 10px;"), # Upload icon
                   "Upload Your Health Data" # Header text
                 ),
                 div(style = "padding: 20px;",
                   p("Upload your Apple Health data CSV file to begin your personalized analysis."), # Instructions
                   div(class = "file-input-wrapper", # File upload area
                     fileInput("datafile", "Choose CSV File", accept = ".csv", buttonLabel = "Browse Files") # File input
                   ),
                   DTOutput("raw_table") # Data table display
                 )
               ),
               
               # Profile Update Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-user-edit", style = "margin-right: 10px;"), # Edit user icon
                   "Update Your Profile" # Header text
                 ),
                 div(style = "padding: 20px;",
                   p("Keep your profile information up to date for the most accurate recommendations."), # Instructions
                   div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;", # Grid layout
                     div(class = "form-group",
                       textInput("update_first_name", "First Name", value = "", placeholder = "Your first name") # First name update
                     ),
                     div(class = "form-group",
                       textInput("update_last_name", "Last Name", value = "", placeholder = "Your last name") # Last name update
                     ),
                     div(class = "form-group",
                       numericInput("update_age", "Age", value = NA, min = 0) # Age update
                     ),
                     div(class = "form-group",
                       selectInput("update_gender", "Gender", choices = c("Male", "Female", "Other")) # Gender update
                     ),
                     div(class = "form-group",
                       numericInput("update_weight", "Weight (lbs)", value = NA, min = 0) # Weight update
                     ),
                     div(class = "form-group",
                       selectInput("update_height", "Height", choices = height_choices) # Height update
                     ),
                     div(class = "form-group",
                       numericInput("update_bmi", "BMI", value = NA, min = 0) # BMI update
                     ),
                     div(class = "form-group",
                       numericInput("update_body_fat", "Body Fat %", value = NA, min = 0, max = 100) # Body fat update
                     )
                   ),
                   div(style = "text-align: center; margin-top: 20px;", # Button container
                     actionButton("update_profile_btn", "Update Profile", class = "btn-secondary"), # Update profile button
                     actionButton("upload_next", "Continue to Analysis", class = "btn-primary", style = "margin-left: 10px;") # Continue button
                   )
                 )
               )
             )
           },

           "clustering_config" = { # Clustering configuration page UI
             div(
               # Variable Selection Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-cogs", style = "margin-right: 10px;"), # Settings icon
                   "Configure Your Analysis" # Header text
                 ),
                 div(style = "padding: 20px;",
                   p("Select the variables you want to use for both clustering and regression analysis:"), # Instructions
                   uiOutput("analysis_vars_ui") # Dynamic variable selection UI
                 )
               ),
               
               # Clustering Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"), # Pie chart icon
                   "Clustering Analysis" # Header text
                 ),
                 div(style = "padding: 20px;",
                   div(style = "text-align: center; margin-bottom: 20px;", # Button container
                     actionButton("run_elbow_btn", "Run Elbow Method", class = "btn-secondary") # Elbow method button
                   ),
               plotOutput("elbow_plot"), # Elbow plot display
                   div(style = "text-align: center; margin: 20px 0;", # Input container
               numericInput("optimal_k", "Number of Clusters (k):", value = 3, min = 2, max = 10), # K input
                     br(), # Line break
                     actionButton("run_clustering_btn", "Run Clustering", class = "btn-primary") # Clustering button
                   )
                 )
               ),
               
               # Regression Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"), # Line chart icon
                   "Linear Regression Analysis" # Header text
                 ),
                 div(style = "padding: 20px;",
                   p("Both clustering and regression will be performed with your selected variables (X = Variable 1, Y = Variable 2)"), # Instructions
                   div(style = "text-align: center; margin: 20px 0;", # Button container
                     actionButton("run_regression_btn", "Run Linear Regression", class = "btn-primary"), # Regression button
                     br(), br(), # Line breaks
                     actionButton("analysis_config_next", "Continue to Results", class = "btn-secondary") # Continue button
                   )
                 )
               )
             )
           },
           "analysis" = { # Analysis results page UI
             div(
               # Analysis Summary Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-bar", style = "margin-right: 10px;"), # Bar chart icon
                   "Analysis & Modeling Results" # Header text
                 ),
                 div(style = "padding: 20px;",
                   p("Your analysis includes the following components:"), # Instructions
               tags$ul(
                 lapply(user_choices$analysis, function(x) tags$li(x)) # List analysis components
                   )
                 )
               ),
               
               # Clustering Results Section
               if ("clustering" %in% user_choices$analysis) { # Only show if clustering was selected
                 div(class = "card",
                   div(class = "card-header",
                     tags$i(class = "fas fa-chart-pie", style = "margin-right: 10px;"), # Pie chart icon
                     "Clustering Results" # Header text
                   ),
                   div(style = "padding: 20px;",
                   plotOutput("cluster_plot"), # Clustering plot display
                   DTOutput("cluster_table") # Clustering table display
                   )
                 )
               },
               
               # Regression Results Section
               if ("regression" %in% user_choices$analysis) { # Only show if regression was selected
                 div(class = "card",
                   div(class = "card-header",
                     tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"), # Line chart icon
                     "Linear Regression Results" # Header text
                   ),
                   div(style = "padding: 20px;",
                   verbatimTextOutput("reg_summary"), # Regression summary text
                     div(style = "margin: 20px 0;", # Plot container
                       h5("Training Data with Regression Line"), # Plot title
                   plotOutput("reg_plot") # Regression plot display
                     ),
                     div(style = "margin: 20px 0;", # Plot container
                       h5("Actual vs Predicted Values"), # Plot title
                       plotOutput("actual_vs_predicted_plot") # Actual vs predicted plot
                     ),
                     div(style = "margin: 20px 0;", # Plot container
                       h5("Residual Plot"), # Plot title
                       plotOutput("residual_plot") # Residual plot display
                     )
                   )
                 )
               },
               
               # Navigation
               div(style = "text-align: center; margin-top: 30px;", # Button container
                 actionButton("analysis_next", "Continue to Visualization", class = "btn-primary") # Continue button
               )
             )
           },
           "metric_plot" = { # Metric visualization page UI
             div(class = "card",
               div(class = "card-header",
                 tags$i(class = "fas fa-calendar-alt", style = "margin-right: 10px;"), # Calendar icon
                 "Explore Your Data" # Header text
               ),
               div(style = "padding: 20px;",
                 p("Select a date to visualize your health metrics and get personalized recommendations."), # Instructions
               uiOutput("metric_date_ui"), # Dynamic date selection UI
               plotOutput("metric_day_plot"), # Metric plot display
                 div(style = "text-align: center; margin-top: 30px;", # Button container
                   actionButton("metric_next", "View Recommendations", class = "btn-primary") # Continue button
                 )
               )
             )
           },
           "finish" = { # Final results page UI
             div(
               # Thank You Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-heart", style = "margin-right: 10px;"), # Heart icon
                   "Your Personalized Recommendations" # Header text
                 ),
                 div(style = "padding: 20px; text-align: center;", # Content container
               h3("Thank you for using RegenRate!"), # Thank you message
                   p("Here's your complete fitness analysis and personalized recommendations."), # Instructions
                   verbatimTextOutput("recommendation_text") # Recommendations display
                 )
               ),
               
               # Results Summary Section
               div(class = "card",
                 div(class = "card-header",
                   tags$i(class = "fas fa-chart-bar", style = "margin-right: 10px;"), # Bar chart icon
                   "Analysis Summary" # Header text
                 ),
                 div(style = "padding: 20px;",
                   div(style = "margin: 20px 0;", # Clustering results container
               h4("Clustering Results:"), # Section title
               plotOutput("cluster_plot"), # Clustering plot
                     DTOutput("cluster_table") # Clustering table
                   ),
                   div(style = "margin: 20px 0;", # Regression results container
               h4("Regression Results:"), # Section title
               verbatimTextOutput("reg_summary"), # Regression summary
                     plotOutput("reg_plot") # Regression plot
                   ),
                   div(style = "margin: 20px 0;", # Metric results container
               h4("Metric by Date:"), # Section title
                     plotOutput("metric_day_plot") # Metric plot
                   ),
                   div(style = "margin: 20px 0;", # Heart rate results container
               h4("Heart Rate by Date:"), # Section title
                     plotOutput("finish_hr_plot") # Heart rate plot
                   )
                 )
               ),
               
               # Restart Section
               div(style = "text-align: center; margin-top: 30px;", # Button container
                 actionButton("restart_btn", "Start New Analysis", class = "btn-primary") # Restart button
               )
             )
           }
    )
  })
  
  # ---- Navigation and Step Transitions ----
  observeEvent(input$create_profile_btn, { # Handle create profile button click
    step("create_profile") # Navigate to profile creation page
  })
  
  observeEvent(input$login_btn, { # Handle login button click
    req(input$username, input$password) # Require username and password
    profile_file <- "profiles.csv" # Profile file path
    if (file.exists(profile_file)) { # If profile file exists
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE) # Read profiles
      match_row <- which( # Find matching profile
        trimws(as.character(profiles$username)) == trimws(input$username) & # Username match
          trimws(as.character(profiles$password)) == trimws(input$password) # Password match
      )
      if (length(match_row) == 1) { # If exactly one match found
        user(input$username) # Set current user
        step("upload") # Navigate to upload page
        showNotification("Logged in successfully.", type = "message") # Success notification
      } else {
        showNotification("Invalid username or password.", type = "error") # Error notification
      }
    } else {
      showNotification("No profiles found. Please create a profile first.", type = "error") # Error notification
    }
  })
  
  observeEvent(input$submit_profile_btn, { # Handle profile submission
    # Gather profile info
    profile <- data.frame( # Create profile dataframe
      username = input$username_new, # Username
      password = input$password_new, # Password
      first_name = input$first_name, # First name
      last_name = input$last_name, # Last name
      age = input$age, # Age
      gender = input$gender, # Gender
      weight = input$weight, # Weight
      height = input$height, # Height
      bmi = input$bmi, # BMI
      body_fat = input$body_fat, # Body fat percentage
      stringsAsFactors = FALSE # Don't convert to factors
    )
    profile_file <- "profiles.csv" # Profile file path
    if (file.exists(profile_file)) { # If profile file exists
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE) # Read existing profiles
      if (input$username_new %in% profiles$username) { # If username already exists
        showNotification("Username already exists. Please choose another.", type = "error") # Error notification
        return() # Exit function
      }
      write.table(profile, profile_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE, qmethod = "double") # Append new profile
    } else {
      write.csv(profile, profile_file, row.names = FALSE, quote = TRUE, qmethod = "double") # Create new profile file
    }
    step("login") # Navigate back to login page
    showNotification("Profile created! Please log in.", type = "message") # Success notification
  })
  
  # ---- Data Upload and Processing ----
  observeEvent(input$datafile, { # Handle file upload
    req(input$datafile) # Require file input
    df <- read.csv(input$datafile$datapath) # Read uploaded CSV file
    
    # Clean column names - Remove spaces and dots, convert to lowercase
    names(df) <- gsub("[. ]", "_", tolower(names(df)))
    
    # Date parsing - Try different date formats
    if ("date_local" %in% names(df)) { # If date column exists
      df$date_local <- suppressWarnings(lubridate::ymd(df$date_local)) # Try YYYY-MM-DD format
      if (all(is.na(df$date_local))) { # If all dates are NA
        df$date_local <- suppressWarnings(lubridate::mdy(df$date_local)) # Try MM/DD/YYYY format
      }
      if (all(is.na(df$date_local))) { # If still all NA
        df$date_local <- suppressWarnings(lubridate::dmy(df$date_local)) # Try DD/MM/YYYY format
      }
      if (all(is.na(df$date_local))) { # If still all NA
        df$date_local <- as.Date(df$date_local) # Try default format
      }
    }

    # Energy metric hourly aggregation logic - Handle hourly data
    if (!"hour" %in% names(df) && "time_of_day" %in% names(df)) { # If no hour column but time exists
      df$hour <- sprintf("%02d:00:00", lubridate::hour(hms::as_hms(df$time_of_day))) # Create hour column
    }
    if ("hourly_sum" %in% names(df)) { # If hourly sum column exists
      df <- df %>%
        group_by(date_local, hour, metric_name) %>% # Group by date, hour, and metric
        mutate(
          is_energy_metric = metric_name %in% c("Basal Energy Burned", "Active Energy Burned", "Steps") # Mark energy metrics
        ) %>%
        filter(
          !is_energy_metric | (is_energy_metric & !duplicated(hourly_sum)) # Remove duplicates for energy metrics
        ) %>%
        ungroup() # Ungroup
      df <- df %>%
        mutate(
          value = ifelse(
            metric_name %in% c("Basal Energy Burned", "Active Energy Burned", "Steps") & !is.na(hourly_sum), # If energy metric and hourly sum exists
            hourly_sum, # Use hourly sum
            value # Otherwise use original value
          )
        )
    }

    raw_data(df) # Store processed data
    # Automatically set both analysis types
    user_choices$analysis <- c("clustering", "regression") # Set analysis types
    step("clustering_config") # Navigate to clustering configuration
  })
  
  # ---- Profile Management ----
  # Load user profile data when reaching upload page
  observe({
    if (step() == "upload" && !is.null(user())) { # If on upload page and user is logged in
      profile_file <- "profiles.csv" # Profile file path
      if (file.exists(profile_file)) { # If profile file exists
        profiles <- read.csv(profile_file, stringsAsFactors = FALSE) # Read profiles
        user_profile <- profiles[profiles$username == user(), ] # Get current user's profile
        if (nrow(user_profile) == 1) { # If profile found
          # Store profile in reactive value
          user_profile(user_profile) # Store user profile
          
          # Update UI fields with profile data
          updateTextInput(session, "update_first_name", value = user_profile$first_name) # Update first name
          updateTextInput(session, "update_last_name", value = user_profile$last_name) # Update last name
          updateNumericInput(session, "update_age", value = user_profile$age) # Update age
          updateSelectInput(session, "update_gender", selected = user_profile$gender) # Update gender
          updateNumericInput(session, "update_weight", value = user_profile$weight) # Update weight
          updateSelectInput(session, "update_height", selected = user_profile$height) # Update height
          updateNumericInput(session, "update_bmi", value = user_profile$bmi) # Update BMI
          updateNumericInput(session, "update_body_fat", value = user_profile$body_fat) # Update body fat
        }
      }
    }
  })
  
  # Handle profile update
  observeEvent(input$update_profile_btn, { # Handle profile update button
    profile_file <- "profiles.csv" # Profile file path
    history_file <- "profile_history.csv" # History file path
    
    if (file.exists(profile_file)) { # If profile file exists
      profiles <- read.csv(profile_file, stringsAsFactors = FALSE) # Read profiles
      user_row <- which(profiles$username == user()) # Find current user's row
      if (length(user_row) == 1) { # If user found
        # Update the user's profile data
        profiles$first_name[user_row] <- input$update_first_name # Update first name
        profiles$last_name[user_row] <- input$update_last_name # Update last name
        profiles$age[user_row] <- input$update_age # Update age
        profiles$gender[user_row] <- input$update_gender # Update gender
        profiles$weight[user_row] <- input$update_weight # Update weight
        profiles$height[user_row] <- input$update_height # Update height
        profiles$bmi[user_row] <- input$update_bmi # Update BMI
        profiles$body_fat[user_row] <- input$update_body_fat # Update body fat
        
        write.csv(profiles, profile_file, row.names = FALSE, quote = TRUE, qmethod = "double") # Save updated profiles
        
        # Save to history file with timestamp
        history_entry <- data.frame( # Create history entry
          username = user(), # Username
          update_date = Sys.Date(), # Current date
          update_time = format(Sys.time(), "%H:%M:%S"), # Current time
          first_name = input$update_first_name, # First name
          last_name = input$update_last_name, # Last name
          age = input$update_age, # Age
          gender = input$update_gender, # Gender
          weight = input$update_weight, # Weight
          height = input$update_height, # Height
          bmi = input$update_bmi, # BMI
          body_fat = input$update_body_fat, # Body fat
          stringsAsFactors = FALSE # Don't convert to factors
        )
        
        if (file.exists(history_file)) { # If history file exists
          write.table(history_entry, history_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE, quote = TRUE, qmethod = "double") # Append history entry
        } else {
          write.csv(history_entry, history_file, row.names = FALSE, quote = TRUE, qmethod = "double") # Create new history file
        }
        
        showNotification("Profile updated successfully! Historical data saved.", type = "message") # Success notification
      }
    }
  })
  
  # ---- Analysis Navigation ----
  observeEvent(input$upload_next, { # Handle continue to analysis button
    # Automatically set both analysis types
    user_choices$analysis <- c("clustering", "regression") # Set analysis types
    step("clustering_config") # Navigate to clustering configuration
  })
  

  
  # ---- Analysis Configuration ----
  # UI for variable selection (used for both clustering and regression)
  output$analysis_vars_ui <- renderUI({ # Render variable selection UI
    df <- raw_data() # Get raw data
    req(df) # Require data
    
    available_metrics <- sort(unique(df$metric_name)) # Get available metrics
    
    tagList( # Create UI elements
      selectInput("analysis_var1", "Select First Variable (X):", choices = available_metrics), # First variable selection
      selectInput("analysis_var2", "Select Second Variable (Y):", choices = available_metrics, selected = if(length(available_metrics) > 1) available_metrics[2] else available_metrics[1]), # Second variable selection
      p(strong("Note:"), "For clustering, both variables will be used. For regression, Variable 1 will be X (independent) and Variable 2 will be Y (dependent).") # Instructions
    )
  })
  
  # Reactive value to store elbow results
  elbow_results <- reactiveVal(NULL) # Store elbow method results
  
  # Handle elbow method button
  observeEvent(input$run_elbow_btn, { # Handle elbow method button click
    req(input$analysis_var1, input$analysis_var2) # Require variable selection
    
    df <- raw_data() # Get raw data
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) { # If required columns exist
      wide <- suppressWarnings(df %>% # Create wide format data
        mutate(metric_name = make.names(metric_name)) %>% # Clean metric names
        tidyr::pivot_wider( # Pivot to wide format
          id_cols = c(date_local, time_of_day), # ID columns
          names_from = metric_name, # Metric names as columns
          values_from = value, # Values
          values_fn = ~mean(as.numeric(.), na.rm = TRUE) # Mean function for duplicates
        ))
      
      var1 <- make.names(input$analysis_var1) # Clean variable 1 name
      var2 <- make.names(input$analysis_var2) # Clean variable 2 name
      
      if (all(c(var1, var2) %in% names(wide))) { # If both variables exist
        cluster_data <- wide %>% select(all_of(c(var1, var2))) %>% na.omit() # Select variables and remove NA
        
        if (nrow(cluster_data) >= 3) { # If enough data points
          results <- elbow_method(cluster_data, max_k = 10) # Run elbow method
          elbow_results(results) # Store results
          
          if (nrow(results) > 2) { # If we have more than 2 k values
            diff_inertia <- diff(results$inertia) # First differences
            diff_diff <- diff(diff_inertia) # Second differences
            suggested_k <- which.max(diff_diff) + 2 # Suggested k (elbow point)
            updateNumericInput(session, "optimal_k", value = min(suggested_k, nrow(cluster_data) - 1)) # Update k input
          } else {
            updateNumericInput(session, "optimal_k", value = 2) # Default to 2 clusters
          }
          
          showNotification("Elbow method complete! Suggested k value updated. You can now run clustering.", type = "message") # Success notification
        } else {
          showNotification(paste("Not enough data points for elbow method. Need at least 3 data points, but only have", nrow(cluster_data), "."), type = "error") # Error notification
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error") # Error notification
      }
    }
  })
  
  # Elbow plot output
  output$elbow_plot <- renderPlot({ # Render elbow plot
    req(elbow_results()) # Require elbow results
    
    ggplot(elbow_results(), aes(x = k, y = inertia)) + # Create plot
      geom_line() + # Add line
      geom_point(size = 3) + # Add points
      labs(title = "Elbow Method for Optimal k", # Plot title
           x = "Number of Clusters (k)", # X axis label
           y = "Inertia (Sum of Squared Distances)") + # Y axis label
      theme_bw() # Theme
  })
  
  # Handle clustering button
  observeEvent(input$run_clustering_btn, { # Handle clustering button click
    req(input$analysis_var1, input$analysis_var2, input$optimal_k) # Require variables and k
    
    df <- raw_data() # Get raw data
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) { # If required columns exist
      wide <- suppressWarnings(df %>% # Create wide format data
        mutate(metric_name = make.names(metric_name)) %>% # Clean metric names
        tidyr::pivot_wider( # Pivot to wide format
          id_cols = c(date_local, time_of_day), # ID columns
          names_from = metric_name, # Metric names as columns
          values_from = value, # Values
          values_fn = ~mean(as.numeric(.), na.rm = TRUE) # Mean function for duplicates
        ))
      
      var1 <- make.names(input$analysis_var1) # Clean variable 1 name
      var2 <- make.names(input$analysis_var2) # Clean variable 2 name
      
      if (all(c(var1, var2) %in% names(wide))) { # If both variables exist
        cluster_data <- wide %>% select(date_local, all_of(c(var1, var2))) %>% na.omit() # Select variables and remove NA
        
        # Remove outliers using IQR
        Q1 <- apply(cluster_data[, 2:3], 2, quantile, 0.25, na.rm = TRUE) # First quartile
        Q3 <- apply(cluster_data[, 2:3], 2, quantile, 0.75, na.rm = TRUE) # Third quartile
        IQR <- Q3 - Q1 # Interquartile range
        keep <- (cluster_data[[2]] >= (Q1[1] - 1.5 * IQR[1])) & (cluster_data[[2]] <= (Q3[1] + 1.5 * IQR[1])) & # Keep if within 1.5*IQR for var1
                (cluster_data[[3]] >= (Q1[2] - 1.5 * IQR[2])) & (cluster_data[[3]] <= (Q3[2] + 1.5 * IQR[2])) # Keep if within 1.5*IQR for var2
        cluster_data <- cluster_data[keep, ] # Filter data
        
        if (nrow(cluster_data) >= input$optimal_k) { # If enough data points after outlier removal
          set.seed(123) # Set random seed
          clust <- suppressWarnings(kmeans(cluster_data[, 2:3], centers = input$optimal_k, nstart = 25)) # Run k-means clustering
          cluster_data$Cluster <- as.factor(clust$cluster) # Add cluster assignments
          analysis_results$cluster <- cluster_data # Store cluster results
          analysis_results$cluster_plot <- ggplot(cluster_data, aes_string(x = var1, y = var2, color = "Cluster")) + # Create cluster plot
            geom_point(size = 2) + # Add points
            labs(title = paste("K-means Clustering with", input$optimal_k, "Clusters"), # Plot title
                 x = input$analysis_var1, # X axis label
                 y = input$analysis_var2) + # Y axis label
            theme_bw() # Theme
          
          showNotification("Clustering analysis complete! Click 'Continue to Results' to view.", type = "message") # Success notification
        } else {
          showNotification("Not enough data points for clustering. Need at least as many points as clusters after outlier removal.", type = "error") # Error notification
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error") # Error notification
      }
    }
  })
  
  # Handle regression button
  observeEvent(input$run_regression_btn, { # Handle regression button click
    req(input$analysis_var1, input$analysis_var2) # Require variable selection
    
    df <- raw_data() # Get raw data
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) { # If required columns exist
      wide <- suppressWarnings(df %>% # Create wide format data
        mutate(metric_name = make.names(metric_name)) %>% # Clean metric names
        tidyr::pivot_wider( # Pivot to wide format
          id_cols = c(date_local, time_of_day), # ID columns
          names_from = metric_name, # Metric names as columns
          values_from = value, # Values
          values_fn = ~mean(as.numeric(.), na.rm = TRUE) # Mean function for duplicates
        ))
      
      x_var <- make.names(input$analysis_var1) # Clean independent variable name
      y_var <- make.names(input$analysis_var2) # Clean dependent variable name
      
      if (all(c(x_var, y_var) %in% names(wide))) { # If both variables exist
        # Perform regression using selected variables
        results <- perform_linear_regression(wide, x_var, y_var) # Run linear regression
        
        if (!is.null(results$error)) { # If regression failed
          showNotification(results$error, type = "error") # Show error notification
        } else {
          regression_results(results) # Store regression results
          
          # Create summary text
          summary_text <- paste0( # Create summary text
            "Linear Regression Results:\n", # Header
            "Dependent Variable (Y): ", input$analysis_var2, "\n", # Dependent variable
            "Independent Variable (X): ", input$analysis_var1, "\n", # Independent variable
            "Intercept (β₀): ", round(results$beta[1], 4), "\n", # Intercept
            "Slope (β₁): ", round(results$beta[2], 4), "\n", # Slope
            "Training R²: ", round(results$r2_train, 4), "\n", # Training R-squared
            "Test R²: ", round(results$r2_test, 4), "\n", # Test R-squared
            "Training Data Points: ", nrow(results$train_data$X), "\n", # Training data points
            "Test Data Points: ", nrow(results$test_data$X) # Test data points
          )
          
          analysis_results$reg <- summary_text # Store regression summary
          showNotification("Linear regression analysis complete! Click 'Continue to Results' to view.", type = "message") # Success notification
        }
      } else {
        showNotification("Selected variables not found in the data.", type = "error") # Error notification
      }
    }
  })
  
  # Handle continue to analysis results
  observeEvent(input$analysis_config_next, { # Handle continue to results button
    df <- raw_data() # Get raw data
    if (all(c("date_local", "time_of_day", "metric_name", "value") %in% names(df))) { # If required columns exist
      wide <- suppressWarnings(df %>% # Create wide format data
        mutate(metric_name = make.names(metric_name)) %>% # Clean metric names
        tidyr::pivot_wider( # Pivot to wide format
          id_cols = c(date_local, time_of_day), # ID columns
          names_from = metric_name, # Metric names as columns
          values_from = value, # Values
          values_fn = ~mean(as.numeric(.), na.rm = TRUE) # Mean function for duplicates
        ))
      metric_cols <- setdiff(names(wide), c("date_local", "time_of_day")) # Get metric columns
      wide <- wide %>% mutate(across(all_of(metric_cols), as.numeric)) # Convert to numeric
      wide_data(wide) # Store wide data
    } else {
      wide_data(NULL) # Set to NULL if no data
    }
    step("analysis") # Navigate to analysis results
  })
  
  # ---- Regression Analysis ----
  # Reactive value to store comprehensive regression results
  regression_results <- reactiveVal(NULL) # Store regression results
  
  # ---- Output Rendering ----
  output$raw_table <- renderDT({ # Render raw data table
    req(raw_data()) # Require raw data
    datatable(raw_data()) # Create data table
  })
  
  # Clustering outputs
  output$cluster_table <- renderDT({ # Render cluster table
    req(analysis_results$cluster) # Require cluster results
    if (nrow(analysis_results$cluster) > 0) datatable(analysis_results$cluster) else NULL # Create data table if data exists
  })
  
  output$cluster_plot <- renderPlot({ # Render cluster plot
    req(analysis_results$cluster_plot) # Require cluster plot
    analysis_results$cluster_plot # Display cluster plot
  })
  
  # Regression outputs
  output$reg_summary <- renderPrint({ # Render regression summary
    req(analysis_results$reg) # Require regression results
      cat(analysis_results$reg) # Print regression summary
  })
  
  # Training data scatter plot with regression line
  output$reg_plot <- renderPlot({ # Render regression plot
    req(regression_results()) # Require regression results
    results <- regression_results() # Get regression results
    
    # Debug: Print the structure of results
    print("Regression results structure:") # Debug message
    print(str(results)) # Print structure
    
    # Create training data dataframe for plotting
    train_df <- data.frame( # Create training dataframe
      x = as.numeric(results$train_data$X[, 2]),  # Independent variable (skip intercept column)
      y = as.numeric(results$train_data$y), # Dependent variable
      y_hat = as.numeric(results$train_data$y_hat) # Predicted values
    )
    
    # Debug: Print the dataframe
    print("Training dataframe:") # Debug message
    print(head(train_df)) # Print first few rows
    
    ggplot(train_df, aes(x = x, y = y)) + # Create plot
      geom_point(color = 'red', size = 2, alpha = 0.6) + # Add actual data points
      geom_line(aes(x = x, y = y_hat), color = 'blue', size = 1) + # Add regression line
      labs(
        title = paste("Linear Regression: Training Data"), # Plot title
        subtitle = paste(results$y_var, "~", results$x_var), # Subtitle
        x = results$x_var, # X axis label
        y = results$y_var # Y axis label
      ) +
      theme_bw() + # Theme
      theme(plot.title = element_text(size = 14, face = "bold")) # Title styling
  })
  
  # Actual vs Predicted plot
  output$actual_vs_predicted_plot <- renderPlot({ # Render actual vs predicted plot
    req(regression_results()) # Require regression results
    results <- regression_results() # Get regression results
    
    # Debug: Print results structure
    print("Actual vs Predicted - Results structure:") # Debug message
    print(str(results)) # Print structure
    
    # Training data
    train_df <- data.frame( # Create training dataframe
      actual = as.numeric(results$train_data$y), # Actual values
      predicted = as.numeric(results$train_data$y_hat) # Predicted values
    )
    
    # Test data
    test_df <- data.frame( # Create test dataframe
      actual = as.numeric(results$test_data$y), # Actual values
      predicted = as.numeric(results$test_data$y_hat) # Predicted values
    )
    
    # Debug: Print dataframes
    print("Training dataframe:") # Debug message
    print(head(train_df)) # Print first few rows
    print("Test dataframe:") # Debug message
    print(head(test_df)) # Print first few rows
    
    # Combine for plotting
    plot_df <- rbind( # Combine dataframes
      data.frame(train_df, type = "Training"), # Training data with type
      data.frame(test_df, type = "Test") # Test data with type
    )
    
    # Debug: Print combined dataframe
    print("Combined plot dataframe:") # Debug message
    print(head(plot_df)) # Print first few rows
    
    # Perfect prediction line range
    min_val <- min(plot_df$actual, plot_df$predicted, na.rm = TRUE) # Minimum value
    max_val <- max(plot_df$actual, plot_df$predicted, na.rm = TRUE) # Maximum value
    
    ggplot(plot_df, aes(x = actual, y = predicted, color = type)) + # Create plot
      geom_point(alpha = 0.6, size = 2) + # Add points
      geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 1) + # Add perfect prediction line
      labs(
        title = "Actual vs Predicted Values", # Plot title
        subtitle = paste(results$y_var, "~", results$x_var), # Subtitle
        x = paste("Actual", results$y_var), # X axis label
        y = paste("Predicted", results$y_var), # Y axis label
        color = "Data Set" # Legend title
      ) +
      theme_bw() + # Theme
      theme(plot.title = element_text(size = 14, face = "bold")) # Title styling
  })
  
  # Residual plot
  output$residual_plot <- renderPlot({ # Render residual plot
    req(regression_results()) # Require regression results
    results <- regression_results() # Get regression results
    
    # Debug: Print results structure
    print("Residual plot - Results structure:") # Debug message
    print(str(results)) # Print structure
    
    # Training data residuals
    train_df <- data.frame( # Create training dataframe
      predicted = as.numeric(results$train_data$y_hat), # Predicted values
      residuals = as.numeric(results$train_data$residuals) # Residuals
    )
    
    # Debug: Print dataframe
    print("Residual dataframe:") # Debug message
    print(head(train_df)) # Print first few rows
    
    ggplot(train_df, aes(x = predicted, y = residuals)) + # Create plot
      geom_point(color = 'blue', alpha = 0.6, size = 2) + # Add residual points
      geom_hline(yintercept = 0, color = 'black', linetype = 'solid', size = 1) + # Add zero line
      labs(
        title = "Residual Plot: Training Data", # Plot title
        subtitle = paste(results$y_var, "~", results$x_var), # Subtitle
        x = paste("Predicted", results$y_var), # X axis label
        y = "Residuals" # Y axis label
      ) +
      theme_bw() + # Theme
      theme(plot.title = element_text(size = 14, face = "bold")) # Title styling
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

#Launch the App
shinyApp(ui, server)