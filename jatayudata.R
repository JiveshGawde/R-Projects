library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)



#Rename
#1.	How would you rename the Customer ID column to Cust_ID?
rename_cust = jatayu %>% 
  rename(Cust_ID = Customer.ID)

#2.	Write a command to rename Total Sales to Sales.
rename_sales = jatayu %>% 
  rename(Sales = Total.Sales)

#3.	How can you rename multiple columns, for example, Order ID to OrderID and Payment Method to PayMethod?
rename_order = jatayu %>% 
  rename(OrderID = Order.ID, PayMethod = Payment.Method)

#Arrange
#4.	How would you arrange the dataset in ascending order of Total Sales?
arrange_sales = jatayu %>% 
  arrange(Total.Sales)
arrange_sales

#5.	Write a command to arrange the dataset in descending order of Profit.
arrage_profit = jatayu %>% 
  arrange(desc(Profit))

#6.	How can you arrange the dataset first by Region and then by City?
arrange_region = jatayu %>% 
  arrange(Region, City)

#Combined Questions
#7.	How would you select Customer ID, Total Sales, and Profit, filter for Total Sales greater than 50,000, and arrange by Profit in descending order?
cq1 = jatayu %>% 
  filter(Total.Sales > 50000) %>% 
  select(Customer.ID, Total.Sales, Profit) %>% 
  arrange(desc(Profit))
cq1 

#8.	Write a command to group by Product Line, summarise the total Total Sales and arrange the result in descending order of Total Sales.
cq2 = jatayu %>% 
  group_by(Product.Line) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales)) %>% 
  arrange(desc(TOTAL.SALES))
cq2

#9.	How can you select City and Total Sales, filter for Region as "South India", and summarise the average Total Sales for each city?
cq3 = jatayu %>%
  filter(Region == "South India") %>%
  group_by(City, Total.Sales) %>% 
  summarise(AVG.SALES = sum(Total.Sales))
cq3
 
#10.	How would you create a new column Sales Category based on Total Sales (e.g., "High" if Total Sales > 100,000, "Medium" if between 50,000 and 100,000, and "Low" otherwise), and then arrange by Sales Category?
cq10 = jatayu %>% 
  mutate(Sales_Category = case_when(Total.Sales > 100000 ~ "High",
                                    Total.Sales <= 100000 & Total.Sales >= 50000 ~ "Medium",
                                    TRUE ~ "Low"))
cq10

#11.	Write a command to filter for Customer Type as "Regular", group by Region, and summarise the total Profit.
cq11 = jatayu %>% 
  filter(Customer.Type == "Regular") %>% 
  group_by(Region) %>% 
  summarise(TOTAL.PROFIT = sum(Profit))

#12.	How can you select Product Line and Profit, filter for Profit less than 0, and arrange by Profit in ascending order?
cq12 = jatayu %>% 
  filter(Profit < 0) %>% 
  select(Product.Line, Profit) %>% 
  arrange(Profit)
cq12
  
#13.	How would you rename Date of Order to OrderDate, filter for orders in 2023, and summarise the total Total Sales?
cq13 = jatayu %>% 
  rename(OrderDate = Date.of.Order) %>%
  mutate(OrderDate = as.Date(OrderDate, format = "%m/%d/%Y")) %>%  
  filter(year(OrderDate) == "2023") %>% 
  summarise(TOTAL.SALES = sum(Total.Sales))
cq13
class(cq13$OrderDate)

#14.	Write a command to select Customer ID, City, and Total Sales, filter for Payment Method as "Credit Card", and arrange by Total Sales in descending order.
cq14 = jatayu %>% 
  filter(Payment.Method == "Credit Card") %>% 
  select(Customer.ID, City, Total.Sales) %>% 
  arrange(desc(Total.Sales))
  
#15.	How can you group by Customer Type, summarise the average Unit Price, and rename the result column to Avg_Unit_Price?
cq15 = jatayu %>% 
  group_by(Customer.Type) %>% 
  summarise(Result = mean(Unit.Price)) %>% 
  rename(Avg_Unit_Price = Result)
 
#16.	How can you group by Region and summarise the total Total Sales, total Profit, and average Unit Price?
cq16 = jatayu %>% 
  group_by(Region) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVERAGE.UNIT.PRICE = mean(Unit.Price))
  
#17.	How would you group by Customer Type and summarise the total Total Sales, total Profit, and average Unit Price?
cq17 = jatayu %>% 
  group_by(Customer.Type) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVERAGE.UNIT.PRICE = mean(Unit.Price))  

#18.	Write a command to group by City and summarise the total Total Sales, total Profit, and count the number of orders.
cq18 = jatayu %>% 
  group_by(City) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            ORDER.COUNT = n())

#19.	How can you group by Product Line and summarise the total Total Sales, total Profit, and average Unit Price?
cq19 = jatayu %>% 
  group_by(Product.Line) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVERAGE.UNIT.PRICE = mean(Unit.Price))
  
#20.	How would you group by Region and summarise the total Total Sales, total Profit, and average Quantity?
cq20 = jatayu %>% 
  group_by(Region) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVG.QUANTITY = mean(Quantity))
  
#21.	Write a command to group by Customer Type and summarise the total Total Sales, total Profit, and count the number of orders.
cq21 = jatayu %>% 
  group_by(Customer.Type) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            ORDER.COUNT = n())

#22.	How can you group by City and summarise the total Total Sales, total Profit, and average Unit Price?
cq22 = jatayu %>% 
  group_by(City) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVERAGE.UNIT.PRICE = mean(Unit.Price))
  
#23.	How would you group by Payment Method and summarise the total Total Sales, total Profit, and count the number of orders?
cq23 = jatayu %>% 
  group_by(Payment.Method) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            ORDER.COUNT = n())  

#24.	Write a command to group by Region and summarise the total Total Sales, total Profit, and average Unit Price.
cq24 = jatayu %>% 
  group_by(Region) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            AVERAGE.UNIT.PRICE = mean(Unit.Price))

#25.	How can you group by Product Line and summarise the total Total Sales, total Profit, and count the number of orders?
cq25 = jatayu %>% 
  group_by(Product.Line) %>% 
  summarise(TOTAL.SALES = sum(Total.Sales),
            TOTAL.PROFIT = sum(Profit),
            ORDER.COUNT = n())  
  
  