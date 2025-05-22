# R Projects Portfolio

This repository contains a collection of R-based data science projects focused on Exploratory Data Analysis (EDA), Machine Learning, and Data Visualization using powerful libraries like `tidyverse`, `ggplot2`, `dplyr`, and `caret`. Ideal for roles involving RStudio, data cleaning, reporting, and model building.

## Project 1: Retail Analysis - EDA

A deep dive into retail sales data to understand sales patterns, customer behaviors, and product performance.

### Highlights:

* Used `tidyverse` for data manipulation (`dplyr`) and plotting (`ggplot2`)
* Cleaned and prepared data using `janitor`
* Explored seasonal trends, high-performing categories, and customer segmentation

### Files:

* `analysis_retail.R`: Main script for EDA
* `sales_analysis.Rmd`: R Markdown report (Use **Knit** to generate the HTML report)

### Sample Insights:

* Monthly and category-wise sales trends
* Top 10 products by revenue
* Customer purchasing behavior by region

## Project 2: House Price Prediction

Predicting house prices using classic regression models to understand which features influence housing cost the most.

### Techniques Used:

* Linear Regression: Baseline model to understand linear relationships
* Random Forest: Ensemble method to improve accuracy and capture non-linear effects

### Files:

* `linear regression.R`: Linear model implementation
* `prediction-random forest model.R`: Random forest training & evaluation

### Key Learnings:

* Feature selection & handling multicollinearity
* Model tuning using cross-validation
* Performance comparison using RMSE and R²

## Project 3: Core `ggplot2` and `dplyr` Applications

Mini-projects to demonstrate mastery of core R tools for manipulation and visualization.

### Tools:

* `dplyr` applied to internal sales dataset (`jatayudata`)
* `ggplot2` used for business dashboards with SuperStore data

### Files:

* `dplyr - jatayudata.R`: Filtering, summarizing, and transforming sales data
* `ggplot2 - SuperStore.R`: Interactive dashboards and charts

## Skills Demonstrated

* Languages: R
* Libraries: `tidyverse`, `ggplot2`, `dplyr`, `janitor`, `randomForest`, `caret`
* Topics: Data Wrangling, Visualization, EDA, Regression, Reporting
* Tools: RStudio, RMarkdown, Git

## To Run

Make sure the required packages are installed:

```r
install.packages(c("tidyverse", "ggplot2", "dplyr", "janitor", "randomForest", "caret"))
```

To view the HTML report for the retail project:

```r
# Open sales_analysis.Rmd in RStudio and click 'Knit'
```

