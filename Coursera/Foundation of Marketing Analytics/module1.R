
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 1 - STATISTICAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# getwd()
# setwd("Documents/PERSONAL/Useful\ Resources/Lessons/Coursera/Foundation\ of\ Marketing\ Analytics/")

# Load text file into local variable called 'data'
data = read.delim(file = 'Documents/PERSONAL/Useful\ Resources/Lessons/Coursera/Foundation\ of\ Marketing\ Analytics/purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
install.packages("sqldf")
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")

# Explore the data
View(customers)
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
d = dist(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

View(new_data_sample)

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)

# Cut at 9 segments --> 5 segments!!!
members = cutree(c, k = 9)
members = cutree(c, k = 5)

# Show 30 first customers, frequency table
members[1:30]
View(members)
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)

# What is the size of the largest segment?
  
# What is the average purchase amount of the segment which contains, on average, the customers who have made their last purchase the most recently?
  
# Of the five following criteria to determine the best number of segments, which one is the least relevant?
  
# Does customer #260 belongs to the same segment than customer #5920?
# No

# Looking at the average profile of segment 1, would you say that members of this segment are typically...(you'll have to choose the right proposition)
# Paling Jelek
