# List of required packages
required_packages <- c("stringr", "tidyr", "dplyr", "zoo", "Metrics", "caret", "MASS", "ggplot2", "reshape2", "mltools", "DescTools", "plotly", "naniar", "corrplot", "car", "caret")

# Check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Apply all required packages
sapply(required_packages, install_if_missing)
cat("\014")
sessionInfo()
# Read data and read some first rows
CPU_data <- read.csv("C:/Users/Asus/Downloads/Intel_CPUs.csv", na.strings = c("", "N/A"))
head(CPU_data)
# Summary statistics
summary_table <- summary(CPU_data)
print(summary_table)

# Convert all N/A values into NA
CPU_data[CPU_data == "N/A"] <- NA

# Create a table for number of missing values and the corresponding percentages
na_summary <- data.frame(
  Feature = names(CPU_data),
  NA_Count = sapply(CPU_data, function(x) sum(is.na(x))),
  NA_Percentage = round(sapply(CPU_data, function(x) mean(is.na(x)) * 100), 2)
) %>%
  arrange(desc(NA_Percentage))

# Display the table
print(na_summary)
# Check the NA percentages
plot_ly(
  data = na_summary,
  x = ~NA_Percentage,
  y = ~Feature,
  type = 'bar',
  orientation = 'h',
  marker = list(color = 'steelblue'),
  height = nrow(na_summary) * 20
) %>%
  layout(
    title = list(
      text = "Percentage of Missing Values by Feature",
      font = list(size = 18, color = "black")
    ),
    xaxis = list(
      title = "Percentage of Missing values",
      tickformat = '.2f',
      gridcolor = "lightgray",
      zeroline = TRUE
    ),
    yaxis = list(
      title = "Feature",
      tickfont = list(size = 12),
      automargin = TRUE, 
      range = c(0, nrow(na_summary))
    ),
    margin = list(l = 100, r = 50, b = 50, t = 50)
  ) %>%
  config(displayModeBar = FALSE)
#Printing the chosen features
CPU_data = CPU_data[,c("Product_Collection", "Vertical_Segment", "Status", "Launch_Date", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Instruction_Set", "TDP", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")]
print(CPU_data)


#Data transformation
product_collect <- c('Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon', 'Core')

# Iterate through product categories and replace
for (category in product_collect) {
  CPU_data$Product_Collection <- ifelse(
    grepl(category, CPU_data$Product_Collection, ignore.case = TRUE),
    ifelse(grepl("Legacy", CPU_data$Product_Collection, ignore.case = TRUE),
           paste("Legacy", category),
           category),
    CPU_data$Product_Collection
  )
}

# View the modified data
print(CPU_data)

# Launch date
CPU_data <- CPU_data[!is.na(CPU_data$Launch_Date),]
CPU_data$Year <- as.numeric(gsub(".*'(\\d+)", "\\1", CPU_data$Launch_Date))
CPU_data$Year <- ifelse(CPU_data$Year <= 99 & CPU_data$Year >= 50, 
                        CPU_data$Year + 1900, 
                        CPU_data$Year + 2000)
CPU_data <- CPU_data[order(-CPU_data$Year), ]
CPU_data <- CPU_data %>% relocate(Year, .after = Launch_Date)
print(CPU_data)

#Lithography 
CPU_data$Lithography <- as.numeric(gsub(" nm", "", CPU_data$Lithography))

# Replace missing values in Lithography by group averages
CPU_data <- CPU_data %>%
  group_by(Product_Collection) %>%
  mutate(
    Lithography = ifelse(
      is.na(Lithography),
      round(median(Lithography, na.rm = TRUE), 2),
      Lithography
    )
  ) %>%
  ungroup()

CPU_data <- CPU_data[!is.na(CPU_data$Lithography), ]
print(CPU_data)

#Number of threads 

# Replace missing values in Number of Threads by group medians
CPU_data <- CPU_data %>%
  group_by(Product_Collection) %>%
  mutate(
    nb_of_Threads = ifelse(
      is.na(nb_of_Threads),
      round(median(nb_of_Threads, na.rm = TRUE), 2),
      nb_of_Threads
    )
  ) %>%
  ungroup()

CPU_data <- CPU_data[!is.na(CPU_data$nb_of_Threads), ]
print(CPU_data)

#Processor base frequency

CPU_data$Processor_Base_Frequency <- ifelse(
  grepl("GHz", CPU_data$Processor_Base_Frequency),
  as.numeric(gsub(" GHz", "", CPU_data$Processor_Base_Frequency)) * 1000,
  as.numeric(gsub(" MHz", "", CPU_data$Processor_Base_Frequency))
)

CPU_data <- CPU_data %>%
  group_by(Vertical_Segment) %>%
  mutate(
    Processor_Base_Frequency = ifelse(
      is.na(Processor_Base_Frequency),
      round(median(Processor_Base_Frequency, na.rm = TRUE), 2),
      Processor_Base_Frequency
    )
  ) %>%
  ungroup()

print(CPU_data)

#Cache

cache_units <- gsub("^\\d+\\.?\\d*\\s*|\\s+", "", CPU_data$Cache)

# Remove any empty strings or spaces
cache_units <- cache_units[cache_units != ""]

# Get unique units
unique_units <- unique(cache_units)

print(unique_units)
#Seperate to 2 features 
CPU_data <- separate(CPU_data, Cache, into = c("Cache", "Cache_Type"), sep = "B")
CPU_data$Cache_Type <- ifelse(
  CPU_data$Cache_Type == "",
  "Normal",
  sub(" ", "", CPU_data$Cache_Type)
)
CPU_data <- CPU_data[!(is.na(CPU_data$Cache)), ]
CPU_data <- CPU_data[!(is.na(CPU_data$Cache_Type)), ]
CPU_data$Cache <- gsub("(MB|KB).*", "\\1", CPU_data$Cache)
print(CPU_data)

#Convert to KB

CPU_data$Cache <- ifelse(
  grepl("M", CPU_data$Cache),
  as.numeric(gsub(" M", "", CPU_data$Cache)) * 1024,
  as.numeric(gsub(" K", "", CPU_data$Cache))
)

CPU_data$Cache <- log(CPU_data$Cache + 1)

print(CPU_data)

#Instruction set

CPU_data$Instruction_Set <- as.numeric(gsub("[^0-9]", "", CPU_data$Instruction_Set))

CPU_data <- CPU_data %>%
  group_by(Product_Collection) %>%
  mutate(
    Instruction_Set = ifelse(
      is.na(Instruction_Set),
      median(Instruction_Set, na.rm = TRUE),
      Instruction_Set
    )
  ) %>%
  ungroup()

print(CPU_data)

#TDP

CPU_data$TDP <- as.numeric(gsub(" W", "", CPU_data$TDP))
CPU_data <- CPU_data[!(is.na(CPU_data$TDP)), ]
print(CPU_data)

#Memory

# Max Memory Size
CPU_data$Max_Memory_Size <- ifelse(
  grepl("TB", CPU_data$Max_Memory_Size),
  as.numeric(gsub(" TB", "", CPU_data$Max_Memory_Size)) * 1024,
  as.numeric(gsub(" GB", "", CPU_data$Max_Memory_Size))
)

CPU_data <- CPU_data %>%
  group_by(Product_Collection, Vertical_Segment, Year) %>%
  mutate(
    Max_Memory_Size = ifelse(
      is.na(Max_Memory_Size),
      median(Max_Memory_Size, na.rm = TRUE),
      Max_Memory_Size
    )
  ) %>%
  ungroup()

CPU_data <- CPU_data[(!is.na(CPU_data$Max_Memory_Size)), ]

CPU_data$Max_Memory_Size <- log(CPU_data$Max_Memory_Size + 1)
# Max Number of Memory Channels
CPU_data <- CPU_data %>%
  group_by(Product_Collection, Vertical_Segment) %>%
  mutate(
    Max_nb_of_Memory_Channels = ifelse(
      is.na(Max_nb_of_Memory_Channels),
      median(Max_nb_of_Memory_Channels, na.rm = TRUE),
      Max_nb_of_Memory_Channels
    )
  ) %>%
  ungroup()

CPU_data <- CPU_data[(!is.na(CPU_data$Max_nb_of_Memory_Channels)), ]

# Max Memory Bandwidth
CPU_data$Max_Memory_Bandwidth <- as.numeric(gsub(" GB/s", "", CPU_data$Max_Memory_Bandwidth))

CPU_data <- CPU_data %>%
  group_by(Product_Collection, Max_Memory_Size) %>%
  mutate(
    Max_Memory_Bandwidth = ifelse(
      is.na(Max_Memory_Bandwidth),
      median(Max_Memory_Bandwidth, na.rm = TRUE),
      Max_Memory_Bandwidth
    )
  ) %>%
  ungroup()

CPU_data <- CPU_data[(!is.na(CPU_data$Max_Memory_Bandwidth)), ]
print(CPU_data)

#Reccommended Price

# Compute average for price ranges
CPU_data$Recommended_Customer_Price <- gsub("\\$", "", CPU_data$Recommended_Customer_Price)

CPU_data$Recommended_Customer_Price <- sapply(CPU_data$Recommended_Customer_Price, function(price) {
  if (grepl("-", price)) {
    prices <- as.numeric(unlist(strsplit(price, "-")))
    mean(prices, na.rm = TRUE)
  } else {
    as.numeric(price)
  }
})

CPU_data <- CPU_data %>%
  group_by(Product_Collection) %>%
  fill(Recommended_Customer_Price, .direction = "updown")

# CPU_data <- CPU_data[(!is.na(CPU_data$Recommended_Customer_Price)), ]
CPU_data$Recommended_Customer_Price <- as.double(CPU_data$Recommended_Customer_Price)
print(CPU_data)

#III/Descriptive Statistic
print(dim(CPU_data))
print(names(CPU_data))
str(CPU_data)
sapply(CPU_data, function(x) sum(is.na(x)))

#Categorical and numerical data
numerical_data = c("Year", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "TDP", "Cache", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")

categorical_data = c("Product_Collection", "Vertical_Segment", "Status", "Cache_Type", "Instruction_Set")

numerical_subset = CPU_data[, numerical_data]
categorical_subset = CPU_data[, categorical_data]

#2.  Categorical and Numerical Data Analysis

summary_numeric_table <- data.frame(
  Statisctic = c("Count", "Mean", "STD", "Min", "First Quantile", "Median", "Third Quantile", "Max")
)

for (i in numerical_data) {
  count <- sum(!is.na(CPU_data[[i]]))
  mean <- mean(CPU_data[[i]], na.rm = TRUE)
  std <- sd(CPU_data[[i]], na.rm = TRUE)
  min <- min(CPU_data[[i]], na.rm = TRUE)
  first_quantile <- quantile(CPU_data[[i]], 0.25, na.rm = TRUE)
  median <- median(CPU_data[[i]], na.rm = TRUE)
  third_quantile <- quantile(CPU_data[[i]], 0.75, na.rm = TRUE)
  max <- max(CPU_data[[i]], na.rm = TRUE)
  
  summary_numeric_table <- cbind(
    summary_numeric_table, 
    new_col = c(count, mean, std, min, first_quantile, median, third_quantile, max)
  )
}

colnames(summary_numeric_table) <- c("Statistic", numerical_data)

summary_categorical_table <- data.frame(
  Statistic = c("Count", "Unique", "Mode", "Frequency")
)

Mode <- function(x) {
  unique_values <- unique(x)
  freq <- tabulate(match(x, unique_values))
  mode_val <- unique_values[which.max(freq)]
  return(list(value = mode_val, freq = max(freq)))
}

for (i in categorical_data) {
  count <- sum(!is.na(CPU_data[[i]]))
  unique_values <- length(unique(CPU_data[[i]]))
  mode_result <- Mode(CPU_data[[i]])
  mode <- mode_result$value
  freq <- mode_result$freq
  
  summary_categorical_table <- cbind(
    summary_categorical_table, 
    new_col = c(count, unique_values, mode, freq)
  )
}

colnames(summary_categorical_table) <- c("Statistic", categorical_data)

print(summary_numeric_table)
print(summary_categorical_table)
#correlation matrix
cor_matrix <- cor(numerical_subset, use = "pairwise.complete.obs")
print(cor_matrix)

corrplot(
  cor_matrix,
  method = "number",  # Display correlation coefficients
  type = "full",      # Show the full matrix
  order = "original", # Retain the original ordering
  tl.col = "black",   #Text label color
  tl.cex = 0.7,       # Text label size
  number.cex = 0.6,
  cl.pos = "r",       # Color legend on the right
  col = colorRampPalette(c("blue", "white", "red"))(200) # Color palette
)

cor_strong <- as.data.frame(as.table(cor_matrix))

cor_strong <- cor_strong[cor_strong$Var1 != cor_strong$Var2, ]

cor_strong <- cor_strong[!duplicated(data.frame(t(apply(cor_strong[, 1:2], 1, sort)))), ]

cor_strong <- cor_strong[order(-abs(cor_strong$Freq)), ]

print(cor_strong)


#Histogram
hist(numerical_subset$TDP, xlab = "TDP", main = "Histogram of TDP", col = "orange")
hist(numerical_subset$Lithography, xlab = "Lithography", main = "Histogram of Lithography", col = "orange")
hist(numerical_subset$Max_Memory_Bandwidth, xlab = "Max_Memory_Bandwidth", main = "Histogram of Max_Memory_Bandwidth", col = "orange")
hist(numerical_subset$Max_nb_of_Memory_Channels, xlab = "Max_nb_of_Memory_Channels", main = "Histogram of Max_nb_of_Memory_Channels", col = "orange")
hist(numerical_subset$Max_Memory_Size, xlab = "Memory_Size_Normalized", main = "Histogram of Memory_Size_Normalized", col = "orange")
hist(numerical_subset$Cache, xlab = "Cache", main = "Histogram of Cache", col = "orange")
hist(numerical_subset$nb_of_Cores, xlab = "Number of cores", main = "Histogram of Number of Cores", col = "orange")
hist(numerical_subset$nb_of_Threads, xlab = "Number of threads", main = "Histogram of Number of threads", col = "orange")
hist(numerical_subset$Processor_Base_Frequency, xlab= "Processor_Base_Frequency", main= "Histogram of Processor_Base_Frequency", col ="orange")
options(repr.plot.width = 10, repr.plot.height =10)
p <- ggplot(CPU_data,aes(Recommended_Customer_Price))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density() +
  labs(title = "Histogram of Normalized Recommended Customer Price")

ggplotly(p)
#Divided the price
segments <- cut(numerical_subset$Recommended_Customer_Price, 
                breaks = c(-Inf, 1500, 4000, Inf), 
                labels = c("Low", "Medium", "High"))
price_data <- data.frame(price=numerical_subset$Recommended_Customer_Price,segment=segments)
boxplot(price~segment, data=price_data,main="Boxplot of Price by Segment", 
        xlab="Segment", ylab="Price", col=c("lightblue", "lightgreen", "lightpink"))
grid()

### Box Plot
#    With Product Collections
low_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Low"],
  Product_Collection = CPU_data$Product_Collection[segments == "Low"]
)

boxplot(Recommended_Customer_Price ~ Product_Collection, 
        data = low_price_data,
        main = "Boxplot of Low Price Segment by Product_Collection",
        xlab = "Product_Collection",
        ylab = "Price",
        col = "lightblue")
grid()


mid_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Medium"],
  Product_Collection = CPU_data$Product_Collection[segments == "Medium"]
)
str(mid_price_data)
boxplot(Recommended_Customer_Price ~ Product_Collection, 
        data = mid_price_data,
        main = "Boxplot of Medium Price Segment by Product_Collection",
        xlab = "Product_Collection",
        ylab = "Price",
        col = "lightblue")
grid()

high_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "High"],
  Product_Collection = CPU_data$Product_Collection[segments == "High"]
)

boxplot(Recommended_Customer_Price ~ Product_Collection, 
        data = high_price_data,
        main = "Boxplot of High Price Segment by Product_Collection",
        xlab = "Xeon",
        ylab = "Price",
        col = "lightblue")
grid()
## Vertical Segments


low_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Low"],
  Vertical_Segment = CPU_data$Vertical_Segment[segments == "Low"]
)

boxplot(Recommended_Customer_Price ~ Vertical_Segment, 
        data = low_price_data,
        main = "Boxplot of Low Price Segment by Vertical Segment",
        xlab = "Server",
        ylab = "Price",
        col = "lightblue")
grid()
mid_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Medium"],
  Vertical_Segment = CPU_data$Vertical_Segment[segments == "Medium"]
)
boxplot(Recommended_Customer_Price ~ Vertical_Segment, 
        data = mid_price_data,
        main = "Boxplot of Medium Price Segment by Vertical Segment",
        xlab = "Vertical Segment",
        ylab = "Price",
        col = "lightblue")
grid()

high_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "High"],
  Vertical_Segment = CPU_data$Vertical_Segment[segments == "High"]
)

boxplot(Recommended_Customer_Price ~ Vertical_Segment, 
        data = high_price_data,
        main = "Boxplot of High Price Segment by Vertical Segment",
        xlab = "Server",
        ylab = "Price",
        col = "lightblue")
grid()

# with Cache_Type

high_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "High"],
  Cache_Type = CPU_data$Cache_Type[segments == "High"]
)

boxplot(Recommended_Customer_Price ~ Cache_Type, 
        data = high_price_data,
        main = "Boxplot of High Price Segment by Cache_Type",
        xlab = "L3",
        ylab = "Price",
        col = "lightblue")
grid()

mid_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Medium"],
  Cache_Type = CPU_data$Cache_Type[segments == "Medium"]
)

boxplot(Recommended_Customer_Price ~ Cache_Type, 
        data = mid_price_data,
        main = "Boxplot of Medium Price Segment by Cache_Type",
        xlab = "Cache_Type",
        ylab = "Price",
        col = "lightblue")
grid()

low_price_data <- data.frame(
  Recommended_Customer_Price = numerical_subset$Recommended_Customer_Price[segments == "Low"],
  Cache_Type = CPU_data$Cache_Type[segments == "Low"]
)

boxplot(Recommended_Customer_Price ~ Cache_Type, 
        data = low_price_data,
        main = "Boxplot of Low Price Segment by Cache_Type",
        xlab = "Cache_Type",
        ylab = "Price",
        col = "lightblue")
grid()

##### Scatter graphs


high_price_data <- subset(numerical_subset)
plot(high_price_data$Lithography, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Lithography vs  Price",
     xlab="Lithography", ylab="Price",
     pch=19, col="lightpink")
grid()


high_price_data <- subset(numerical_subset)
plot(high_price_data$TDP, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of TDP vs  Price",
     xlab="TDP", ylab="Price",
     pch=19, col="lightpink")
grid()

high_price_data <- subset(numerical_subset)
plot(high_price_data$nb_of_Threads, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of threads vs  Price",
     xlab="threads", ylab="Price",
     pch=19, col="lightpink")
grid()
high_price_data <- subset(numerical_subset)
plot(high_price_data$nb_of_Cores, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of cores vs  Price",
     xlab="cores", ylab="Price",
     pch=19, col="lightpink")
grid()
high_price_data <- subset(numerical_subset)
plot(high_price_data$Cache, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Cache vs  Price",
     xlab="Cache", ylab="Price",
     pch=19, col="lightpink")
grid()

high_price_data <- subset(numerical_subset)
plot(high_price_data$Max_Memory_Bandwidth, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Max_Memory_Bandwidth vs  Price",
     xlab="Max_Memory_Bandwidth", ylab="Price",
     pch=19, col="lightpink")
grid()

high_price_data <- subset(numerical_subset)
plot(high_price_data$Max_Memory_Size, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Max_Memory_Size vs  Price",
     xlab="Max_Memory_Size", ylab="Price",
     pch=19, col="lightpink")
grid()

high_price_data <- subset(numerical_subset)
plot(high_price_data$Max_nb_of_Memory_Channels, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Max_nb_of_Memory_Channels vs  Price",
     xlab="Max_nb_of_Memory_Channels", ylab="Price",
     pch=19, col="lightpink")
grid()

high_price_data <- subset(numerical_subset)
plot(high_price_data$Processor_Base_Frequency, high_price_data$Recommended_Customer_Price,
     main="Scatter Plot of Processor_Base_Frequency vs  Price",
     xlab="Processor_Base_Frequency", ylab="Price",
     pch=19, col="lightpink")
grid()

### IV/ Inferential Statistics
CPU_data$Recommended_Customer_Price <- log(CPU_data$Recommended_Customer_Price + 1)

#train test split 
CPU_lm <- CPU_data %>%
  select(Recommended_Customer_Price, Product_Collection, Vertical_Segment, 
         Year, Lithography, Status,
         nb_of_Cores, nb_of_Threads, Processor_Base_Frequency, Cache, Cache_Type, 
         Instruction_Set, TDP, Max_Memory_Size, Max_nb_of_Memory_Channels, 
         Max_Memory_Bandwidth)
CPU_lm$Product_Collection <- as.factor(CPU_lm$Product_Collection)
CPU_lm$Vertical_Segment <- as.factor(CPU_lm$Vertical_Segment)
CPU_lm$Status <- as.factor(CPU_lm$Status)
CPU_lm$Cache_Type <- as.factor(CPU_lm$Cache_Type)
print(CPU_lm)

set.seed(42)
# Splitting ratio (80% train, 20% test)
splitIndex <- createDataPartition(CPU_lm$Recommended_Customer_Price, p = 0.8, list = FALSE)
train_data <- CPU_lm[splitIndex, ]
test_data <- CPU_lm[-splitIndex, ]

#Train the model

model <- lm(Recommended_Customer_Price ~., data=train_data)
print(summary(model))

#Remove Irrelevant Variables and Retrain model (LM2)

# Rebuild the model with significant variables
re_model <- lm(Recommended_Customer_Price ~ Product_Collection + Vertical_Segment +
                 Year + Status + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Cache + Cache_Type + Max_Memory_Size + Max_nb_of_Memory_Channels + Max_Memory_Bandwidth, data = train_data)

# Summary of the new model
summary(re_model)

#Model Evaluation 
y_train_pred <- predict(model,newdata=train_data,response = "Recommended_Customer_Price")
y_test_pred <- predict(model, newdata = test_data,response = "Recommended_Customer_Price")

y_train_actual <- train_data$Recommended_Customer_Price
y_test_actual <- test_data$Recommended_Customer_Price

mse_train <- mean((y_train_pred - y_train_actual)^2)
rmse_train <- sqrt(mse_train)
r_squared_train <- 1 - sum((y_train_pred - y_train_actual)^2) / sum((y_train_actual - mean(y_train_actual))^2)  # R-squared

# Test Data Metrics
mse_test <- mean((y_test_pred - y_test_actual)^2)
rmse_test <- sqrt(mse_test)
r_squared_test <- 1 - sum((y_test_pred - y_test_actual)^2) / sum((y_test_actual - mean(y_test_actual))^2) 

# Print the results
cat("Training Data of LM1:\n")
cat("\tMSE:", mse_train, "\n")
cat("\tRMSE:", rmse_train, "\n")
cat("\tR-squared:", r_squared_train, "\n\n")

cat("Test Data of LM1:\n")
cat("\tMSE:", mse_test, "\n")
cat("\tRMSE:", rmse_test, "\n")
cat("\tR-squared:", r_squared_test, "\n")

y_train_pred <- predict(re_model,newdata=train_data,response = "Recommended_Customer_Price")
y_test_pred <- predict(re_model, newdata = test_data,response = "Recommended_Customer_Price")

y_train_actual <- train_data$Recommended_Customer_Price
y_test_actual <- test_data$Recommended_Customer_Price

mse_train <- mean((y_train_pred - y_train_actual)^2)
rmse_train <- sqrt(mse_train)
r_squared_train <- 1 - sum((y_train_pred - y_train_actual)^2) / sum((y_train_actual - mean(y_train_actual))^2)  # R-squared

# Test Data Metrics
mse_test <- mean((y_test_pred - y_test_actual)^2)
rmse_test <- sqrt(mse_test)
r_squared_test <- 1 - sum((y_test_pred - y_test_actual)^2) / sum((y_test_actual - mean(y_test_actual))^2) 

# Print the results
cat("Training Data of LM2:\n")
cat("\tMSE:", mse_train, "\n")
cat("\tRMSE:", rmse_train, "\n")
cat("\tR-squared:", r_squared_train, "\n\n")

cat("Test Data of LM2:\n")
cat("\tMSE:", mse_test, "\n")
cat("\tRMSE:", rmse_test, "\n")
cat("\tR-squared:", r_squared_test, "\n")

#Assumption testing

std_residuals <- rstandard(model)
fitted_values <- model$fitted.values

p <- ggplot(data = data.frame(Fitted_Values = fitted_values, Residuals = std_residuals), 
            aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  # Add scatter points
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Add horizontal line at y=0
  labs(
    title = "Standardized Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Standardized Residuals"
  ) +
  theme_minimal()

ggplotly(p)

#Normality 
residuals <- residuals(model)

qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

#Price prediction

cpu_1 <- data.frame(
  Product_Collection = "Core",
  Vertical_Segment = "Mobile",
  Status = "Launched",
  Year = mean(CPU_data$Year, na.rm = TRUE),
  Lithography = mean(CPU_data$Lithography, na.rm = TRUE),
  nb_of_Cores = mean(CPU_data$nb_of_Cores, na.rm = TRUE),
  nb_of_Threads = mean(CPU_data$nb_of_Threads, na.rm = TRUE),
  Processor_Base_Frequency = mean(CPU_data$Processor_Base_Frequency, na.rm = TRUE),
  Cache = mean(CPU_data$Cache, na.rm = TRUE),
  Cache_Type = "L3",
  Instruction_Set = mean(CPU_data$Instruction_Set, na.rm = TRUE),
  TDP = mean(CPU_data$TDP, na.rm = TRUE),
  Max_Memory_Size = mean(CPU_data$Max_Memory_Size, na.rm = TRUE),
  Max_nb_of_Memory_Channels = mean(CPU_data$Max_nb_of_Memory_Channels, na.rm = TRUE),
  Max_Memory_Bandwidth = mean(CPU_data$Max_Memory_Bandwidth, na.rm = TRUE)
)

cpu_2 <- data.frame(
  Product_Collection = "Xeon",
  Vertical_Segment = "Server",
  Status = "Launched",
  Year = 2022,
  Lithography = 14,
  nb_of_Cores = 24,
  nb_of_Threads = 48,
  Processor_Base_Frequency = 3000,
  Cache = 10.47,
  Cache_Type = "L3",
  Instruction_Set = 64,
  TDP = 205,
  Max_Memory_Size = 6.93,
  Max_nb_of_Memory_Channels = 6,
  Max_Memory_Bandwidth = 60
)

predicted_prices <- predict(re_model, newdata = cpu_1)
predicted_prices <- exp(predicted_prices)
cat("CPU 1 predicted price: ", predicted_prices)

cat("\n")

predicted_prices <- predict(re_model, newdata = cpu_2)
predicted_prices <- exp(predicted_prices)
cat("CPU 2 predicted price: ", predicted_prices,"\n")

