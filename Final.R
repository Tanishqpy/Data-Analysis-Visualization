library(tidyr)
library(dplyr)
library(ggplot2)
library(lattice)

UBC_monthly <- read.csv("/home/avstr/Tanishq Laptop/Tanishq/Hackathon/IIP/IndicesIIP2011-12Monthly_annual_Jan25.csv")

UBC_monthly_indices_data <- UBC_monthly[6:11, 3:156]
UBC_monthly_indices_data[] <- lapply(UBC_monthly_indices_data, function(x) as.numeric(x))
UBC_monthly_indices_data <- as.data.frame(UBC_monthly_indices_data)

UBC_monthly_indices_date <- UBC_monthly[5, 3:156]
UBC_monthly_indices_data_date <- rbind(UBC_monthly_indices_date, UBC_monthly_indices_data)
####
UBC_monthly_indices_data_date_f <- t(UBC_monthly_indices_data_date)
colnames(UBC_monthly_indices_data_date_f) <- c('date', 'Primary Goods', 'Capital Goods', 'Intermediate Goods', 'Consumer Goods', 'Basic Goods', 'Infrastructure Goods')
UBC_monthly_indices_data_date_f <- as.data.frame(UBC_monthly_indices_data_date_f)
UBC_monthly_indices_data_date_f <- UBC_monthly_indices_data_date_f[-1,]
data <- UBC_monthly_indices_data_date_f[-1,]

#####
dates<-as.data.frame(UBC_monthly_indices_data_date_f)[[1]] 
year_2012 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[1:9] |>as.numeric()
year_2013 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[10:21] |>as.numeric()
year_2014 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[22:33] |>as.numeric()
year_2015 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[34:45] |>as.numeric()
year_2016 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[46:57] |>as.numeric()
year_2017 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[58:69] |>as.numeric()
year_2018 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[70:81] |>as.numeric()
year_2019 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[82:93] |>as.numeric()
year_2020 <- as.data.frame(UBC_monthly_indices_data_date_f)$'Primary Goods'[104:115] |>as.numeric()

year_2012_plot <- xyplot(year_2012~(4:12), xlab = 'Month', ylab = 'Indices',type = 'o', main='Primary Goods Indices for Year 2012')
year_2013_plot <- xyplot(year_2013~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2013')
year_2014_plot <- xyplot(year_2014~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2014')
year_2015_plot <- xyplot(year_2015~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2015')
year_2016_plot <- xyplot(year_2016~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2016')
year_2017_plot <- xyplot(year_2017~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2017')
year_2018_plot <- xyplot(year_2018~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2018')
year_2019_plot <- xyplot(year_2019~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2019')
year_2020_plot <- xyplot(year_2020~(1:12), xlab = 'date', ylab = 'indices',type = 'o', main='Primary Goods Indices for Year 2020')

year_2012_plot
year_2013_plot
year_2014_plot
year_2015_plot
year_2016_plot
year_2017_plot
year_2018_plot
year_2019_plot
year_2020_plot


# Create data for the graph.
barplot(as.character(UBC_monthly_indices_data_date_f[1,])[2:7]|>as.numeric(), names.arg = c('Primary Goods', 'Capital Goods', 'Intermediate Goods', 'Consumer Goods', 'Basic Goods', 'Infrastructure Goods'),
        col = 'steelblue', main = 'Contribution of Sectors to Industrial Performance (April 2012)', xlab = 'Monthly indices of industrial production as per use-based classification', ylab = 'Value',
        cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1)



######Processing The Data for further analysis ######

# Extract indices data and pivot
UBC_monthly_indices_data <- UBC_monthly[6:11, 3:156] %>%
  mutate(Category = c("Primary Goods", "Capital Goods", "Intermediate Goods", 
                      "Infrastructure/Construction Goods", "Consumer Durables", 
                      "Consumer Non-durables")) %>%
  pivot_longer(cols = -Category, names_to = "date_col", values_to = "Index") %>%
  mutate(Index = as.numeric(Index))

# Extract dates from the header
dates <- as.character(UBC_monthly[5, 3:156])
UBC_monthly_indices_data$Date <- rep(dates, each = 6)  # 6 categories

UBC_monthly_indices_data <- UBC_monthly_indices_data %>%
  mutate(Date = as.Date(paste0("01/", Date), format = "%d/%m/%y"))


# Boxplot for all categories
ggplot(UBC_monthly_indices_data, aes(x = Category, y = Index)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot of Indices by Category", x = "Category", y = "Index") +
  theme_minimal()

xyplot(UBC_monthly_indices_data$Index~UBC_monthly_indices_data$Date, xlab = 'Date', ylab = 'Indices',type = 'l', main='Primary Goods Indices for Year 2012')

bwplot(UBC_monthly_indices_data$Index~UBC_monthly_indices_data$Category, xlab = 'Category', ylab = 'Indices', main='Primary Goods Indices for Year 2012')

histogram(~UBC_monthly_indices_data$Index, xlab = 'Indices', ylab = 'Frequency', main='Primary Goods Indices (Histogram)')

# Line plot of mean Index values by Category
mean_indices_line <- UBC_monthly_indices_data %>%
  group_by(Category, Date) %>%
  summarise(Mean_Index = mean(Index, na.rm = TRUE), .groups = "drop")

ggplot(mean_indices_line, aes(x = Date, y = Mean_Index)) + 
  geom_line() +
  labs(title = "Mean Indices by Category (Line Plot)", x = "Date", y = "Mean Index") +
  theme_minimal()

# Heatmap for all categories
heatmap_data_all <- UBC_monthly_indices_data %>%
  mutate(Year = format(Date, "%Y"), Month = format(Date, "%m")) %>%
  group_by(Category, Year, Month) %>%
  summarise(Index = mean(Index, na.rm = TRUE), .groups = "drop")

ggplot(heatmap_data_all, aes(x = Month, y = Year, fill = Index)) +
  geom_tile() +
  facet_wrap(~Category, scales = "free_y") +
  scale_fill_viridis_c(name = "Index") +
  labs(title = "Monthly Indices by Category (Heatmap)", x = "Month", y = "Year") +
  theme_minimal()

# Line plot for all categories
lineplot_data_all <- UBC_monthly_indices_data %>%
  mutate(YearMonth = as.Date(paste0(format(Date, "%Y-%m"), "-01"))) %>%
  group_by(Category, YearMonth) %>%
  summarise(Index = mean(Index, na.rm = TRUE), .groups = "drop")

ggplot(lineplot_data_all, aes(x = YearMonth, y = Index, color = Category)) +
  geom_line() +
  labs(title = "Monthly Indices by Category (Line Plot)", x = "Date", y = "Index") +
  theme_minimal()

# Smoothed line plot for all categories
ggplot(lineplot_data_all, aes(x = YearMonth, y = Index, color = Category)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = "Monthly Indices by Category (Smoothed Line Plot)", x = "Date", y = "Index") +
  theme_minimal()

# Scatter plot for Index vs. Date (Primary Goods)
primary_goods_data <- UBC_monthly_indices_data %>%
  filter(Category == "Primary Goods")

ggplot(primary_goods_data, aes(x = Date, y = Index)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Primary Goods Indices", x = "Date", y = "Index") +
  theme_minimal()

# Histogram of Index values for all categories
ggplot(UBC_monthly_indices_data, aes(x = Index, fill = Category)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge") +
  labs(title = "Histogram of Indices by Category", x = "Index", y = "Frequency") +
  theme_minimal()

# Density plot for Index values
ggplot(UBC_monthly_indices_data, aes(x = Index, color = Category, fill = Category)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Indices by Category", x = "Index", y = "Density") +
  theme_minimal()

# Bar plot of mean Index values by Category
mean_indices <- UBC_monthly_indices_data %>%
  group_by(Category) %>%
  summarise(Mean_Index = mean(Index, na.rm = TRUE), .groups = "drop")

ggplot(mean_indices, aes(x = Category, y = Mean_Index, fill = Category)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean Indices by Category", x = "Category", y = "Mean Index") +
  theme_minimal()

# Pie chart of total Index values by Category
total_indices <- UBC_monthly_indices_data %>%
  group_by(Category) %>%
  summarise(Total_Index = sum(Index, na.rm = TRUE), .groups = "drop")

ggplot(total_indices, aes(x = "", y = Total_Index, fill = Category)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Total Indices by Category", x = "", y = "") +
  theme_minimal()

# Stacked bar plot of total Index values by Category
ggplot(UBC_monthly_indices_data, aes(x = Date, y = Index, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Indices by Category", x = "Date", y = "Index") +
  theme_minimal()



