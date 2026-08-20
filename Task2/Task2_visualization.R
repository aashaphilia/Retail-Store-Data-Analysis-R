# ============================================================
# TASK 2 - DATA VISUALIZATION
# Retail Store Sales Dataset
# ============================================================

# install libraries
# install.packages("ggplot2")
# load required libraries
library(tidyverse)
library(ggplot2)

# load cleaned dataset
clean_data <- read_csv(
    "cleaned_data/cleaned_retail_store_sales.csv"
)

dim(clean_data)
str(clean_data)

# convert the categorical variables to factors
clean_data$Category <- factor(
    clean_data$Category
)
clean_data$`Payment Method` <- factor(
    clean_data$`Payment Method`
)

clean_data$Location <- factor(clean_data$Location)

clean_data$`Discount Applied` <- factor(
    clean_data$`Discount Applied`
)

# verify
class(clean_data$Category)
class(clean_data$`Payment Method`)
class(clean_data$Location)
class(clean_data$`Discount Applied`)

# Visualization 1: Quantity Sold by Category.
# total Quantity sold for each Category
quantity_by_category <- aggregate(
    Quantity ~ Category,
    data = clean_data,
    FUN = sum
)

quantity_by_category <- quantity_by_category[
    order(-quantity_by_category$Quantity),
]

quantity_by_category

# Visualization 1 — Total Quantity Sold by Category
# Which product category has the highest total quantity sold?
quantity_category_plot <- ggplot(
    quantity_by_category,
    aes(
        x = reorder(Category, Quantity),
        y = Quantity
    )
) +
    geom_col(width = 0.7) +
    geom_text(
        aes(label = Quantity),
        hjust = -0.15,
        size = 3.8
    ) +
    coord_flip() +
    scale_y_continuous(
        breaks = seq(0, 9000, 1000)
    ) +
    labs(
        title = "Total Quantity Sold by Product Category",
        subtitle = "Furniture recorded the highest quantity sold",
        x = NULL,
        y = "Total Quantity Sold"
    ) +
    expand_limits(y = 9000) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold")
    )

quantity_category_plot

# Visualization 2: Total Revenue by Location.
# Which purchase location generates more total revenue: Online or In-store?

revenue_by_location <- aggregate(
    `Total Spent` ~ Location,
    data = clean_data,
    FUN = sum
)

revenue_by_location


revenue_location_plot <- ggplot(
    revenue_by_location,
    aes(
        x = Location,
        y = `Total Spent`,
        fill = Location
    )
) +
    geom_col(
        width = 0.6,
        show.legend = FALSE
    ) +
    geom_text(
        aes(label = format(
            `Total Spent`,
            big.mark = ",",
            scientific = FALSE
        )),
        vjust = -0.5,
        size = 4
    ) +
    scale_y_continuous(
        labels = scales::comma,
        limits = c(0, 850000)
    ) +
    labs(
        title = "Total Revenue by Purchase Location",
        subtitle = "Online sales generated slightly higher revenue than in-store sales",
        x = "Purchase Location",
        y = "Total Revenue"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

revenue_location_plot

# Visualization 3 — Distribution of Total Spent

# We'll use a histogram to answer:

# What does the distribution of transaction spending look like?

total_spent_histogram <- ggplot(
    clean_data,
    aes(x = `Total Spent`)
)+
    geom_histogram(
        bins = 30,
        boundary = 0
    ) +
    labs(
        title = "Distribution of Total Spent per Transaction",
        subtitle = "Distribution of individual transcation values",
        x = "Total Spent",
        y = "Number of Transaction"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

total_spent_histogram

# Visualization 4 — Quantity vs Total Spent
# Question

# Does purchasing a larger quantity generally lead to a higher Total Spent?

quantity_spent_plot <- ggplot(
    clean_data,
    aes(
        x = Quantity,
        y = `Total Spent`
    )
) +
    geom_point(
        alpha = 0.35,
        size = 1.5
    ) + 
    geom_smooth(
        method = "lm",
        se = FALSE
    ) +
    labs(
        title = "Relationship Between Quantity and Total Spent",
        subtitle = "Higher quantities generally correspond to higher transaction values",
        x = "Quantity Purchased",
        y = "Total Spent"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold")
    )

quantity_spent_plot

# Visualization 5 — Monthly Sales Trend in ggplot2.
# How does total sales revenue change over time?
monthly_sales <- aggregate(
    `Total Spent` ~ Year_Month,
    data = clean_data,
    FUN = sum
)

monthly_sales$Month_Date <- as.Date(
    paste0(monthly_sales$Year_Month, "-01")
)


# plot
monthly_sales_plot <- ggplot(
    monthly_sales,
    aes(
        x = Month_Date,
        y = `Total Spent`
    )
) +
    geom_line(
        linewidth = 0.9
    ) +
    geom_point(
        size = 2
    ) +
    annotate(
    "text",
    x = as.Date("2025-01-01"),
    y = 27000,
    label = "Partial month\n(18 days)",
    hjust = 1.1,
    size = 3.5
    ) +
    scale_x_date(
        date_breaks = "3 months",
        date_labels = "%Y-%m"
    ) +
    scale_y_continuous(
        labels = scales::comma
    ) +
    labs(
        title = "Monthly Sales Trend",
        subtitle = "Monthly revenue fluctuated across the period; January 2025 contains only 18 days of data",
        x = "Month",
        y = "Total Sales Revenue"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(
            angle = 45,
            hjust = 1
        )
    )

monthly_sales_plot

# Visualization 6 — Total Spent by Product Category
# How does transaction spending vary across product categories?

category_spending_plot <- ggplot(
    clean_data,
    aes(
        x = Category,
        y = `Total Spent`
    )
) +
    geom_boxplot() +
    coord_flip() +
    labs(
        title = "Distribution of Total Spent by Product Category",
        subtitle = "Transaction values vary across product categories",
        x = NULL,
        y = "Total Spent"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold")
    )

category_spending_plot

# Visualization 7
# Does payment-method usage differ between Online and In-store purchases

payment_location <- as.data.frame(
    table(
        clean_data$Location,
        clean_data$`Payment Method`
    )
)

names(payment_location) <- c(
    "Location",
    "Payment_Method",
    "Count"
)

payment_location

# plot
payment_location_plot <- ggplot(
    payment_location,
    aes(
        x = Payment_Method,
        y = Count,
        fill = Location
    )
) +
    geom_col(
        position = position_dodge(width = 0.8),
        width = 0.7
    ) +
    geom_text(
        aes(label = Count),
        position = position_dodge(width = 0.8),
        vjust = -0.4,
        size = 3.8
    ) +
    scale_y_continuous(
        limits = c(0, 2300),
        breaks = seq(0, 2000, 500)
    ) +
    labs(
        title = "Payment Method Usage by Purchase Location",
        subtitle = "Payment preferences were relatively balanced across locations",
        x = "Payment Method",
        y = "Number of Transactions",
        fill = "Purchase Location"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
            face = "bold",
            size = 16
        ),
        plot.subtitle = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        legend.position = "top"
    )

payment_location_plot


# save the plot graphs in the folder called plots
ggsave(
    "plots/01_quantity_by_category.png",
    plot = quantity_category_plot,
    width = 10,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/02_revenue_by_location.png",
    plot = revenue_location_plot,
    width = 8,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/03_total_spent_histogram.png",
    plot = total_spent_histogram,
    width = 9,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/04_quantity_vs_total_spent.png",
    plot = quantity_spent_plot,
    width = 9,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/05_monthly_sales_trend.png",
    plot = monthly_sales_plot,
    width = 10,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/06_total_spent_by_category_boxplot.png",
    plot = category_spending_plot,
    width = 10,
    height = 6,
    dpi = 300
)

ggsave(
    "plots/07_payment_method_by_location.png",
    plot = payment_location_plot,
    width = 9,
    height = 6,
    dpi = 300
)

# check
list.files("plots")
