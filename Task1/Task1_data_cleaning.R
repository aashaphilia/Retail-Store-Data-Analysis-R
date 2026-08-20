# Task 1 - Retail Store Sales Data Analysis
# Raw Data Inspection

# ---------------------------------------------
# 🧠 What str(data) does

# It gives us a compact overview of the structure of the entire data object.

# So it can tell us things like:

# What kind of object data is
# How many observations (rows)
# How many variables (columns)
# Each column's name
# Each column's data type
# A small preview of the values
# -------------------------------------------------------

retail_data <- read_csv("raw_data/retail_store_sales.csv")
str(retail_data)

# Count of missing values
colSums(is.na(retail_data))

# inspect some actual rows containing these variables.
head(retail_data[c(
    "Price Per Unit",
    "Quantity",
    "Total Spent",
    "Discount Applied"
)],20)

# Are Quantity and Total Spent actually missing in the same rows?
table(
    Quantity_missing = is.na(retail_data$Quantity),
    Total_missing = is.na(retail_data$'Total Spent'))


# know whether all 609 missing prices have Quantity and Total available.
table(
    Price_missing = is.na(retail_data$'Price Per Unit'),
    Quantity_missing = is.na(retail_data$Quantity)
)


# Among rows where all three values exist, how many rows violate our expected mathematical relationship
sum(
    !is.na(retail_data$'Price Per Unit')&
    !is.na(retail_data$Quantity)&
    !is.na(retail_data$'Total Spent')&
    retail_data$'Price Per Unit' * retail_data$Quantity != retail_data$'Total Spent'
)


# Let's ask R whether Item is missing in exactly those same rows.(Price per unit)
table(
    Item_missing = is.na(retail_data$Item),
    Price_missing = is.na(retail_data$'Price Per Unit')
)

# Separately*quantity)
table(
    Item_missing = is.na(retail_data$Item),
    Quantity_missing = is.na(retail_data$Quantity)
)

# let's create a copy.
clean_data <- retail_data

# Then fill only the missing Price Per Unit values: Find rows where Price is NA → replace Price with Total Spent / Quantity.

clean_data$'Price Per Unit'[
    is.na(clean_data$'Price Per Unit')
] <- 
  clean_data$'Total Spent'[
    is.na(clean_data$'Price Per Unit')
  ]/
  clean_data$Quantity[
    is.na(clean_data$'Price Per Unit')
  ]

# VERIFY Cleaned data
sum(is.na(clean_data$'Price Per Unit'))

# Orginal data 
sum(is.na(retail_data$'Price Per Unit'))


# Maybe Category + Price Per Unit can uniquely tell us which Item it is.

# Show me transactions where Category = Patisserie AND Price = 18.5, and only display Category, Item, and Price.

clean_data[
    clean_data$Category == "Patisserie" & 
    clean_data$'Price Per Unit' == 18.5,
    c("Category","Item","Price Per Unit")
]

# Run this on the original non-missing Item records:
# unique(x) asks:

# What different Item names occur for this Category + Price?

# Then length(...) counts how many different items there are.

item_check <- aggregate(
  Item ~ Category + `Price Per Unit`,
  data = retail_data,
  FUN = function(x) length(unique(x))
)

table(item_check$Item)

# First, create our lookup table

# We want R to learn the valid combinations from the original rows where Item is known.

# Category + Price Per Unit → Item
item_lookup <- unique(
    retail_data[
        !is.na(retail_data$Item),
        c("Category","Price Per Unit","Item")
    ]
)

head(item_lookup,10)


# Now we're ready for the next step: bring the matching Item from item_lookup into clean_data, but only where Item is missing.


# The first is your original Item column, and the second is the Item suggested by the lookup table.

matched_item <- merge(
    clean_data,
    item_lookup,
    by = c("Category","Price Per Unit"),
    all.x = TRUE,
    suffixes = c("","_lookup")
)


# That will show us 10 rows where the original Item is missing, alongside the Item R matched from the lookup.
head(
    matched_item[
        is.na(matched_item$Item),
        c("Category","Price Per Unit","Item","Item_lookup")
    ],10
)

# Are there any missing Item_lookup values among the rows where original Item is missing?
sum(
    is.na(matched_item$Item)&
    is.na(matched_item$Item_lookup)
    )


# Fill out the missing values
# That line copies the values from Item_lookup into the missing positions of Item.

matched_item$Item[
    is.na(matched_item$Item)
] <- matched_item$Item_lookup[
    is.na(matched_item$Item)
]

VERIFY
sum(is.na(matched_item$Item))

# remove item_lookup as this was a temperory table
matched_item$Item_lookup <- NULL

# Continue using cleandata
clean_data <- matched_item

# verify
colSums(is.na(clean_data))

# ------------------------------------------------------
# So our next problem is the 604 rows where Quantity and Total Spent are missing together.

# The first tells us things like minimum, median, mean and maximum. The second tells us how frequently each quantity occurs, including the 604 missing ones.

summary(clean_data$Quantity)

table(clean_data$Quantity, useNA = "ifany")

# Let's inspect whether Quantity depends meaningfully on Item.
# This calculates the average known Quantity for each Item.

quantity_by_item <- aggregate(
    Quantity ~ Item,
    data  = clean_data,
    FUN = mean,
    na.rm = TRUE
)

head(quantity_by_item,20)


nrow(clean_data)

# keep rows where quantity is not missing
clean_data <- clean_data[
    !is.na(clean_data$Quantity),

]

nrow(clean_data)

colSums(is.na(clean_data))

# ----------------------------------------------
# Now: Discount Applied 👀
table(clean_data$`Discount Applied`,useNA = "ifany")

# The second one gives us the percentage distribution.
prop.table(
    table(clean_data$`Discount Applied`,useNA = "ifany")
)*100

# convert that column from logical values into a categorical variable with three levels:

# Read it from the inside:

# if value is NA → "Unknown"
# otherwise, if it is TRUE → "Yes"
# otherwise → "No"

clean_data$`Discount Applied` <- ifelse(
    is.na(clean_data$`Discount Applied`),
    "Unknown",
    ifelse(clean_data$`Discount Applied`,"Yes","No")
)


# Convert it into factor 
clean_data$`Discount Applied` <- factor(
    clean_data$`Discount Applied`,
    levels = c("No","Yes","Unknown")
)

# Verify
table(clean_data$`Discount Applied`,useNA = "ifany")

colSums(is.na(clean_data))

# Now: outliers.
Q1 <- quantile(clean_data$Quantity,0.25)
Q3 <- quantile(clean_data$Quantity,0.75)

IQR_quantity <- IQR(clean_data$Quantity)

lower_quantity <- Q1 - 1.5 * IQR_quantity
upper_quantity <- Q3 + 1.5 *IQR_quantity

lower_quantity
upper_quantity

# Then count values outside those boundaries:
sum(
    clean_data$Quantity < lower_quantity |
    clean_data$Quantity > upper_quantity
)

summary(clean_data$`Price Per Unit`)

summary(clean_data$`Total Spent`)

# investigate the outliers
Q1_total <- quantile(clean_data$`Total Spent`,0.25)
Q3_Total <- quantile(clean_data$`Total Spent`,0.75)

IQR_total <- IQR(clean_data$`Total Spent`)

lower_total <- Q1_total - 1.5 * IQR_total
upper_total <- Q3_Total + 1.5 * IQR_total

sum(clean_data$'Total Spent' < lower_total |
clean_data$`Total Spent` > upper_total
)

# look for transactions
clean_data[
    clean_data$`Total Spent` < lower_total|
    clean_data$`Total Spent` > upper_total,
    c("Item","Price Per Unit","Quantity","Total Spent")
]

# -------------------------------------------------------

# Min-Max normalization(inbetween 0 and 1)
clean_data$Price_Normalized <- (
    clean_data$`Price Per Unit` - min(clean_data$`Price Per Unit`)
) / (
    max(clean_data$`Price Per Unit`) - min(clean_data$`Price Per Unit`)
)

# normalization for total spent
clean_data$Total_spent_Normalization <- (
    clean_data$`Total Spent` - min(
        clean_data$`Total Spent`)
    ) / (
        max(clean_data$`Total Spent`)- min
        (
            clean_data$`Total Spent`

        )
    )

summary(clean_data$Price_Normalized)

summary(clean_data$Total_spent_Normalization)

head(
    clean_data[c(
        "Total Spent",
        "Total_spent_Normalization"
    )],10
)

# Categorical encoding ⏳
unique(clean_data$Location)
unique(clean_data$`Payment Method`)
unique(clean_data$Category)

# Let's start with the simplest one: Location
clean_data$Location_Encoded <- ifelse(
    clean_data$Location == "Online",
    1,
    0
)

# verify
table(
    clean_data$Location,
    clean_data$Location_Encoded
)

# Now Payment Method is different because it has three categories:
model.matrix()

payment_encode <- model.matrix(
    ~`Payment Method` - 1,
    data = clean_data
)

head(payment_encode,10)

rowSums(payment_encode)[1:10]

# add them to clean_data
colnames(payment_encode) <- c(
    "Payment_Cash",
    "Payment_Credit_Card",
    "Payment_Digital_Wallet"
)

# Here, cbind() means column bind — basically:
# Take these columns and attach them side-by-side.

clean_data <- cbind(
    clean_data,
    payment_encode
)

# check
head(
    clean_data[c(
        "Payment Method",
        "Payment_Cash",
        "Payment_Credit_Card",
        "Payment_Digital_Wallet"
    )],10
)

class(clean_data$Category)
class(clean_data$`Payment Method`)
class(clean_data$Location)
class(clean_data$`Discount Applied`)

# Category          → character
# Payment Method    → character
# Location          → character
# Discount Applied  → factor ✅

clean_data$Category <- factor(clean_data$Category)

clean_data$`Payment Method` <- factor(
    clean_data$`Payment Method`
)

clean_data$Location <- factor(clean_data$Location)

# verify
class(clean_data$Category)
class(clean_data$`Payment Method`)
class(clean_data$Location)
class(clean_data$`Discount Applied`)


# date datatype fixing 
# where %d = day, %m = month, and %Y = four-digit year.
clean_data$`Transaction Date` <- as.Date(
    clean_data$`Transaction Date`,
    format = "%d-%m-%Y%"
)

class(clean_data$`Transaction Date`)
# verify
head(clean_data$`Transaction Date`,10)

head(retail_data$`Transaction Date`, 20)
unique(substr(retail_data$`Transaction Date`, 1, 10))[1:20]

sum(duplicated(retail_data$`Transaction ID`))

clean_data$`Transaction Date` <- retail_data$`Transaction Date`[
    match(
        clean_data$`Transaction ID`,
        retail_data$`Transaction ID`
    )
]

head(clean_data$`Transaction Date`,10)

clean_data$`Transaction Date` <- as.Date(
    clean_data$`Transaction Date`,
    format = "%d-%m-%Y"
)

# Verify
class(clean_data$`Transaction Date`)
head(clean_data$`Transaction Date`,10)
sum(is.na(clean_data$`Transaction Date`))

# Exploratory Analysis
# 1. Which Location has more total revenue
revenue_by_location <- aggregate(
    `Total Spent` ~ Location,
    data = clean_data,
    FUN = sum
)

revenue_by_location

# 2. Which location has the higher average transaction value?
average_by_location <- aggregate(
    `Total Spent` ~ Location,
    data = clean_data,
    FUN = mean
)
average_by_location

# 3. 1. Which category sells the highest quantity? → compares product categories.
quantity_by_category <- aggregate(
    Quantity ~ Category,
    data = clean_data,
    FUN = sum
)

quantity_by_category <- quantity_by_category[
    order(-quantity_by_category$Quantity),
]

quantity_by_category

# 4.How do sales change over time? → looks for trends in revenue across dates/months/years.

head(
    format(clean_data$`Transaction Date`,"%Y-%m"),10
)

clean_data$Year_Month <- format(
    clean_data$`Transaction Date`,
    "%Y-%m"
)

# aggregate code
monthly_sales <- aggregate(
    `Total Spent` ~ Year_Month,
    data = clean_data,
    FUN = sum
)
monthly_sales

min(clean_data$`Transaction Date`)
max(clean_data$`Transaction Date`)

# Line Graph
plot(
    monthly_sales$`Total Spent`,
    type = "l",
    xaxt = "n",
    xlab = "Month",
    ylab = "Total Sales",
    main = "Monthly Sales Trend"
)

axis(
    side = 1,
    at = seq(1,nrow(monthly_sales),by = 3),
    labels = monthly_sales$Year_Month[
        seq(1,nrow(monthly_sales),by = 3)
    ]
)

# Correlation analysis
cor(
    clean_data[c(
        "Price Per Unit",
        "Quantity",
        "Total Spent"
    )]
)

# Check and the total for duplicate rows
sum(duplicated(clean_data))
dim(clean_data)
colSums(is.na(clean_data))
str(clean_data)

# rename
names(clean_data)[
    names(clean_data) == "Total_spent_Normalization"
] <- "Total_Spent_Normalized"
# verify
names(clean_data)

# load the cleaned data
unique(clean_data$Category)
write_csv(
    clean_data,
    "cleaned_retail_store_sales.csv"
)