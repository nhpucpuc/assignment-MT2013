# List of required packages
required_packages <- c("stringr", "tidyr", "dplyr", "zoo", "Metrics", "caret", "MASS", "ggplot2", "reshape2", "mltools", "DescTools", "plotly", "naniar", "corrplot", "car", "caret","ggpubr")

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
CPU_data <- read.csv("D:/ĐHBK-ĐHQGHCM/hk242/Xác Suất Thống Kê/btl_xstk_3/CPU-Price-Analysis-Assignment-main/archive/Intel_CPUs.csv", na.strings = c("", "N/A"))
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
  height = nrow(na_summary) * 10
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




# Bước 1. Lọc những sản phẩm Atom C Series
atom_data <- subset(CPU_data, grepl("Intel® Atom™ Processor C", Product_Collection, fixed = TRUE))
print(atom_data)
# Bước 2. chỉnh sửa lại cột Max_Turbo_Frequency (xóa bỏ đơn vị GHz)
atom_data$Max_Turbo_Frequency <- as.numeric(gsub(" GHz", "", atom_data$Max_Turbo_Frequency, fixed = TRUE))

# Bước 3. Loại bỏ giá trị NA
atom_data <- atom_data[!is.na(atom_data$Max_Turbo_Frequency), ]

# Bước 4. Xử lý ngoại lai
Q1 <- quantile(atom_data$Max_Turbo_Frequency, 0.25)
Q3 <- quantile(atom_data$Max_Turbo_Frequency, 0.75)
IQR <- Q3 - Q1
atom_data <- atom_data[atom_data$Max_Turbo_Frequency >= Q1 - 1.5*IQR &
                         atom_data$Max_Turbo_Frequency <= Q3 + 1.5*IQR, ]

# Bước 5. Kiểm định Shapiro (phân phối chuẩn)
shapiro.test(atom_data$Max_Turbo_Frequency)
#p-value = 0.2252 > 0.05 -> có thể xem là phân phối chuẩn -> 


# Bước 6. T-Test với giả thuyết trung bình = 2.4
t.test(atom_data$Max_Turbo_Frequency, mu = 2.4)
#p-value = 0.0004507 < 0.05 -> bác bỏ H0 -> trung bình tần số Turbo tối đa không phải là 2.4 GHz





