# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 4 - CUSTOMER LIFETIME VALUE
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- SEGMENT CUSTOMERS IN 2014 AND 2015 -------------------


# Load text file into local variable called 'data'
data = read.delim(file = 'Documents/PERSONAL/Useful\ Resources/Lessons/Coursera/Foundation\ of\ Marketing\ Analytics/purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Invoke library to compute key marketing indicators using SQL language
library(sqldf)

# Segment customers in 2015
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

# Segment customers in 2014
customers_2014 = sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                               MAX(days_since) - 365 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))


# --- COMPUTE TRANSITION MATRIX ----------------------------


# Compute transition matrix
new_data = merge(x = customers_2014, y = customers_2015, by = "customer_id", all.x = TRUE)
head(new_data)
transition = table(new_data$segment.x, new_data$segment.y)
print(transition)

# Divide each row by its sum
# 3
# 4 --> cold customers adalah customers yang beli antara 2 - 3 tahun yg lalu, nggak mungkin dari active lgsg ke cold
transition = transition / rowSums(transition)
print(transition)


# --- USE TRANSITION MATRIX TO MAKE PREDICTIONS ------------


# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments = matrix(nrow = 8, ncol = 11)
print(segments)

segments[, 1] = table(customers_2015$segment)
print(segments)

colnames(segments) = 2015:2025
print(segments)

row.names(segments) = levels(customers_2015$segment)
print(segments)

# Compute for each an every period
for (i in 2:11) {
   segments[, i] = segments[, i-1] %*% transition
}

# insert 1000 for new customer
for (i in 2:11) {
  segments[8, i] <- 1000
}

print(segments)

# Plot inactive, active high value customers over time
barplot(segments[1, ])
barplot(segments[2, ])
barplot(segments[3, ])
barplot(segments[4, ])
barplot(segments[5, ])
barplot(segments[6, ])

# Display how segments will evolve over time

# 1
print(round(segments))

# 5
print(colSums(segments))


# --- COMPUTE THE (DISCOUNTED) CLV OF A DATABASE -----------

### FROM MODULE 2 ###
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")

actual <- merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] <- 0

revenue_segment <- aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)
revenue_segment

# Yearly revenue per segment
# yearly_revenue = c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17) # OLD

### APPLY FROM MODULE 2 ###
yearly_revenue = revenue_segment$x
### 

print(segments)
print(yearly_revenue)

# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print(revenue_per_segment)

# Compute yearly revenue --> revenue prediction
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))
barplot(yearly_revenue)

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

# Create a discount factor
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))

barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# What is the database worth?
# 2
print(disc_cumulated_revenue[11] - yearly_revenue[1])
