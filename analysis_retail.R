library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(scales)
library(ggthemes)

sales <- read_excel("C:\\Users\\hp\\Desktop\\Lr_Tiwar_college\\Datasets\\Retail(26_2).xlsx", sheet = "Sales")
products <- read_excel("C:\\Users\\hp\\Desktop\\Lr_Tiwar_college\\Datasets\\Retail(26_2).xlsx", sheet = "Products")
customers <- read_excel("C:\\Users\\hp\\Desktop\\Lr_Tiwar_college\\Datasets\\Retail(26_2).xlsx", sheet = "Customer")

glimpse(sales)
glimpse(products)
glimpse(customers)


## fixing column names to maintain consistency
sales <- sales %>% clean_names()
customers <- customers %>% clean_names()
products <- products %>% clean_names()

colnames(sales)
colnames(customers)
colnames(products)

# -----------------------------

colSums(is.na(sales))
colSums(is.na(products))
colSums(is.na(customers))


sum(duplicated(sales))
sum(duplicated(customers))
sum(duplicated(products))

# Remove exact duplicates
customers <- customers %>% distinct()
products <- products %>% distinct()

# remove null records
sales <- sales %>% drop_na(price, quantity)
customers <- customers %>% drop_na(age)

# -------------------------------------


summary_sales = summary(sales)

# Sales Quantity Distribution
# Min quantity: 1, Max quantity: 5
# Most common (median) quantity: 3
# Average quantity sold: ~3
# Customers typically buy in small quantities (1-5 items per transaction).
# Check if certain categories have higher/lower quantities purchased.

category_wise_quantity = sales %>%
  group_by(category) %>%
  summarise(total_quantity = sum(quantity)) %>%
  ggplot(aes(x = reorder(category, total_quantity), y = total_quantity, fill = category)) +
  geom_col() +
  geom_text(aes(label = total_quantity), hjust = 1.2, size = 5) +
  coord_flip() +
  labs(title = "Total Quantity Sold by Category", x = "Category", y = "Total Quantity", subtitle = "Analyzing which categories have the highest sales volume") +
  theme_minimal() 

## Highest sales volume: CLOTHING: 62366
## Lowest sales volume: Technology: 8989

# Pricing Patterns
# Min price: $5.23
# Max price: $5250.00
# Median price: $203.30
# Mean price: $687.57
# The average price is significantly higher than the median price, 
# indicating some high-priced products are skewing the data.
#  Identify high-priced outliers by plotting a boxplot of price.

product_distribution = sales %>%
  mutate(unit_price = price / quantity) %>%  # Calculate unit price
  ggplot(aes(y = unit_price)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "Unit Price Distribution of Products", y = "Unit Price (₹)") +
  theme_minimal()

purchase_distribution = sales %>%
  ggplot(aes(y = price)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "Price of Purchase", y = "Price (₹)") +
  theme_minimal()
purchase_distribution

sales %>%
  group_by(customer_id, category) %>%
  summarise(total_spent = sum(price)) %>% 
  arrange(desc(total_spent))

summary(sales$price)

Q1 <- quantile(sales$price, 0.25, na.rm = TRUE)
Q3 <- quantile(sales$price, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 3.5 * IQR_value
upper_bound <- Q3 + 3.5 * IQR_value

# Filter outliers
outliers <- sales %>% filter(price < lower_bound | price > upper_bound)
print(outliers)

sales_n_out = sales %>% filter(price >= lower_bound & price <= upper_bound)

purchase_distribution_no_out = sales_n_out %>%
  ggplot(aes(y = price)) +
  geom_boxplot(fill = "skyblue" ) +
  labs(title = "Price of Purchase", y = "Price (₹)") +
  theme_minimal()
purchase_distribution_no_out


#----------------------------------------------

sales %>%
  group_by(category, customer_id) %>%  
  summarise(total_spent = sum(price), total_quantity = sum(quantity), .groups = "drop") %>%  
  group_by(category) %>%  # Group again by category before slicing  
  slice_max(total_spent, n = 1, with_ties = FALSE) %>%  
  arrange(desc(total_spent)) %>% 
  print(n = Inf)

value_customer = sales %>%
  group_by(category, customer_id) %>%  
  summarise(total_spent = sum(price), total_quantity = sum(quantity), .groups = "drop") %>%  
  group_by(category) %>%  
  slice_max(total_spent, n = 1, with_ties = FALSE) %>%  
  ggplot(aes(x = reorder(category, total_spent), y = total_spent, fill = category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0("$", comma(total_spent), "\n", "Cust: ", customer_id)), 
            hjust = 0.1, size = 2.5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top-Spending Customer in Each Category",
       x = "Category",
       y = "Total Spent ($)") +
  theme_minimal()



# Sales Trend Over Time
# Earliest sale date: January 1, 2021
# Latest sale date: March 8, 2023
# The data covers 26 months (just over 2 years).
# Plot monthly sales trends to check for seasonality or growth trends.

monthly_sales = sales %>%
  mutate(invoice_date = as.Date(invoice_date),
         month = format(invoice_date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_sales = sum(price)) %>%
  ggplot(aes(x = month, y = total_sales, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  geom_text(aes(label = total_sales), hjust = 1.2, size = 3) +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sales %>%
  mutate(invoice_date = as.Date(invoice_date),
         month = format(invoice_date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(total_sales = sum(price)) %>% 
  arrange(desc(total_sales)) %>% 
  slice_head(n = 5)

#there is sudden fall in sales in month of 2023-03 might be because we have not
#recorded for entire month

# Shopping Mall & Region Insights
# Sales are distributed across multiple malls & regions.
# Some malls/regions might be outperforming others.
# Check total sales by mall & region to find top & low-performing areas.

shop_sales = sales %>%
  group_by(shopping_mall) %>%
  summarise(total_sales = sum(price)) %>%
  ggplot(aes(x = reorder(shopping_mall, total_sales), y = total_sales, fill = shopping_mall)) +
  geom_col() +
  geom_text(aes(label = comma(total_sales)), hjust = 1.2, size = 5) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Sales by Shopping Mall", x = "Shopping Mall", y = "Total Sales", subtitle = "Comparison of total revenue across different shopping malls") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


category_wise_revenue = sales %>%
  group_by(category) %>%
  summarise(total_revenue = sum(price)) %>%
  arrange(desc(total_revenue)) %>%
  ggplot(aes(x = reorder(category, total_revenue), y = total_revenue, fill = category)) +
  geom_col() +
  geom_text(aes(label = comma(total_revenue)), hjust = 1.2, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top-Selling Categories by Revenue", x = "Category", y = "Total Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(products)
summary(customers)




