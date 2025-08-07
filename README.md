# RegenRate

**Know When to Push, Know When to Rest**

RegenRate is a comprehensive fitness analytics and recovery recommendation system built with R Shiny. The application analyzes Apple Health data to provide personalized fitness insights, clustering analysis, regression modeling, and smart recovery recommendations based on user activity patterns.

## Project Overview

RegenRate helps users optimize their fitness routine by:
- Analyzing health metrics from Apple Health data
- Identifying activity patterns through clustering analysis
- Building predictive models using linear regression
- Providing personalized recovery recommendations
- Tracking sleep patterns and heart rate variability
- Offering multi-window recovery analysis

## Key Features

### User Management
- Secure login system with profile creation
- User profile management with biometric data
- Historical profile tracking

### Data Analysis
- **Clustering Analysis**: K-means clustering with elbow method optimization
- **Regression Analysis**: Linear regression with train/test split
- **Recovery Analysis**: Multi-window approach (3-day, 7-day, 14-day patterns)
- **Sleep Analysis**: Automatic sleep period detection using heart rate thresholds

### Modern UI/UX
- Beautiful gradient background with glassmorphism effects
- Responsive design for mobile and desktop
- Interactive progress indicators
- Real-time data visualization
- Font Awesome icons

### Data Visualization
- Interactive plots and charts
- Real-time metric visualization by date
- Heart rate analysis and sleep tracking
- Clustering and regression result displays

## Installation

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages
```r
# Core packages
install.packages(c(
  "shiny",
  "DT", 
  "shinyjs",
  "ggplot2",
  "dplyr",
  "stats",
  "cluster",
  "hms",
  "zoo",
  "tidyr",
  "lubridate",
  "shinydashboard",
  "fresh",
  "waiter",
  "fontawesome"
))
```

### Setup Instructions
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/RegenRate.git
   cd RegenRate
   ```

2. Install required packages (see above)

3. Run the application:
   ```r
   # In R or RStudio
   source("app.R")
   # Or simply run the app.R file
   ```

## Project Structure

```
RegenRate/
├── app.R                          # Main Shiny application
├── Intensity_Recovery_csv.R       # Data processing script
├── apple_data.csv                 # Processed Apple Health data
├── profiles.csv                   # User profiles database
├── profile_history.csv            # Profile update history
├── export.xml                     # Raw Apple Health export
├── README.md                      # This file
└── .git/                          # Git repository
```

## Data Processing

### Apple Health Data Processing (Intensity_Recovery_csv.R)

The `Intensity_Recovery_csv.R` script processes raw Apple Health export data:

**Key Features:**
- Extracts 25+ health metrics from Apple Health XML export
- Converts timestamps to local timezone (America/New_York)
- Aggregates energy metrics by hour
- Filters data to last 2 years
- Handles missing values and data cleaning

**Processed Metrics:**
- Heart Rate (including resting, walking average, recovery)
- Energy Burned (Basal and Active)
- Steps and Distance
- Sleep Analysis
- VO2 Max
- Respiratory Rate
- Oxygen Saturation
- Exercise Minutes
- Stand Time
- Running Metrics (stride length, power, speed)
- Heart Rate Variability

### Data Cleaning and Manipulation

**Data Processing Steps:**
1. **XML Parsing**: Extracts records from Apple Health export.xml
2. **Timezone Conversion**: Converts UTC timestamps to local timezone
3. **Metric Filtering**: Selects relevant health metrics
4. **Hourly Aggregation**: Sums energy metrics by hour
5. **Data Validation**: Checks for consistency and removes outliers
6. **CSV Export**: Saves processed data to apple_data.csv

**Data Units:**
- Heart Rate: BPM (Beats Per Minute)
- Energy: Calories
- Distance: Miles/Kilometers
- Weight: Grams
- Time: Hours, Minutes, Seconds
- Sleep Duration: Hours

## Core Functionality

### 1. Clustering Analysis
- **Elbow Method**: Automatically determines optimal number of clusters
- **K-means Clustering**: Groups similar activity patterns
- **Outlier Detection**: Removes outliers using IQR method
- **Visualization**: Interactive cluster plots

### 2. Regression Analysis
- **Linear Regression**: Manual implementation using normal equations
- **Train/Test Split**: 75/25 split for model validation
- **R-squared Calculation**: Model performance metrics
- **Residual Analysis**: Diagnostic plots

### 3. Recovery Analysis
- **Multi-window Approach**:
  - 3-day analysis (immediate stress)
  - 7-day analysis (weekly patterns)
  - 14-day analysis (bi-weekly patterns)
- **Recovery Levels**:
  - Severe Recovery (50% of normal activity)
  - Moderate Recovery (70% of normal activity)
  - Light Recovery (85% of normal activity)

### 4. Sleep Analysis
- **Sleep Detection**: Uses heart rate threshold (<60 BPM)
- **Sleep Periods**: Identifies consecutive sleep blocks
- **Duration Analysis**: Calculates total sleep hours
- **Sleep Recommendations**: Personalized bedtime suggestions

## Data Sources

### Primary Data Source: Apple Health Export
- **Source**: Apple Health app data export
- **Format**: XML export file (export.xml)
- **Collection Method**: Automatic collection via Apple Health app
- **Date Range**: Last 2 years of health data
- **Access**: Personal health data export from Apple Health

### Secondary Data Sources
- **User Profiles**: Self-reported biometric data
- **Profile History**: Historical profile updates
- **Analysis Results**: Generated clustering and regression results

## Technical Implementation

### Architecture
- **Frontend**: R Shiny with custom CSS
- **Backend**: R with statistical analysis packages
- **Data Storage**: CSV files for simplicity
- **Processing**: R scripts for data transformation

### Key Algorithms
1. **K-means Clustering**: Activity pattern identification
2. **Linear Regression**: Predictive modeling
3. **Elbow Method**: Optimal cluster determination
4. **IQR Outlier Detection**: Data cleaning
5. **Sleep Detection**: Heart rate threshold analysis

### Performance Optimizations
- **Data Caching**: Reactive values for efficient data handling
- **Lazy Loading**: Load data only when needed
- **Memory Management**: Efficient data structures
- **UI Responsiveness**: Asynchronous processing

## UI/UX Design

### Design Principles
- **Modern Aesthetics**: Gradient backgrounds and glassmorphism
- **User-Centric**: Intuitive navigation and clear progress indicators
- **Responsive**: Mobile-friendly design
- **Accessible**: Clear typography and color contrast

### Color Scheme
- **Primary**: #667eea (Blue gradient)
- **Secondary**: #764ba2 (Purple gradient)
- **Text**: #2c3e50 (Dark blue)
- **Accent**: #27ae60 (Green for success)

## Usage Examples

### 1. Initial Setup
1. Create user profile with biometric data
2. Upload Apple Health export.xml file
3. Configure analysis parameters
4. Run clustering and regression analysis

### 2. Daily Analysis
1. Select date for metric visualization
2. Review activity patterns and recommendations
3. Check recovery status and sleep analysis
4. Adjust training intensity based on recommendations

### 3. Long-term Tracking
1. Monitor clustering patterns over time
2. Track regression model performance
3. Review historical recommendations
4. Update profile as needed

## Data Validation

### Quality Checks
- **Data Completeness**: Checks for missing values
- **Data Consistency**: Validates metric ranges
- **Outlier Detection**: Removes statistical outliers
- **Format Validation**: Ensures proper data types

### Validation Rules
- Heart Rate: 40-200 BPM
- Energy: Positive values only
- Sleep Duration: 0-24 hours
- Steps: Non-negative integers

## Data Formulas

### Recovery Score Calculation
```
Recovery Score = (Consecutive High Days × 0.3) + 
                 (High Days in Last 7 × 0.4) + 
                 (Weekly Trend × 0.3)
```

### Clustering Distance Metric
```
Euclidean Distance = √[(x₁-x₂)² + (y₁-y₂)²]
```

### Regression Coefficients
```
β = (X^T × X)^(-1) × (X^T × y)
```

---

**RegenRate** - Empowering users to make data-driven fitness decisions through intelligent analytics and personalized recommendations.
