#-------------------------------------------------------------------

# Danh sách những thư viện được sử dụng
required_packages <- c("stringi", "stringr", "tidyr", "dplyr", "zoo", "Metrics", "caret", "MASS", "ggplot2", "reshape2", "mltools", "DescTools", "plotly", "naniar", "corrplot", "car", "caret")

# Tải những gói thư viện
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Áp dụng các gói thư viện
sapply(required_packages, install_if_missing)
cat("\014")
sessionInfo()

# Đọc dữ liệu
CPU_data <- read.csv("C:/Users/Asus/Downloads/Intel_CPUs.csv", na.strings = c("", "N/A"))
#sao lưu dữ liệu gốc cho bài toán 1 biến
onevar_data <- CPU_data
#head(CPU_data)

# Bảng tổng kết dữ liệu
summary_table <- summary(CPU_data)
print(summary_table)

# Chuyển chuỗi N/A thành NA
CPU_data[CPU_data == "N/A"] <- NA

# Tạo bảng tổng kết dữ liệu khuyết
na_summary <- data.frame(
  Feature = names(CPU_data),
  NA_Count = sapply(CPU_data, function(x) sum(is.na(x))),
  NA_Percentage = round(sapply(CPU_data, function(x) mean(is.na(x)) * 100), 2)
) %>%
  arrange(desc(NA_Percentage))
#print(na_summary)

# Kiểm tra phần trăm NA
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

# Chọn lọc những tính năng
CPU_data = CPU_data[,c("Product_Collection", "Vertical_Segment", "Status", "Launch_Date", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Instruction_Set", "TDP", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth", "Max_Turbo_Frequency")]
print(CPU_data)
#print(CPU_data[,c("Recommended_Customer_Price","Processor_Base_Frequency", "Cache", "Instruction_Set", "TDP", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")])
#-------------------------------------------------------------------


#-----------------------CHUYỂN ĐỔI DỮ LIỆU--------------------------

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
#-------------------------------------------------------------------


#------------------------THỐNG KÊ MÔ TẢ-----------------------------

print(dim(CPU_data))
print(names(CPU_data))
str(CPU_data)
sapply(CPU_data, function(x) sum(is.na(x)))

# Dữ liệu phân loại và dữ liệu số
numerical_data = c("Year", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "TDP", "Cache", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")

categorical_data = c("Product_Collection", "Vertical_Segment", "Status", "Cache_Type", "Instruction_Set")

numerical_subset = CPU_data[, numerical_data]
categorical_subset = CPU_data[, categorical_data]

# Phân tích 2 loại dữ liệu

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
# Ma trận tương quan
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


# Biểu đồ Histogram
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

# Phân chia giá cả
segments <- cut(numerical_subset$Recommended_Customer_Price,
                breaks = c(-Inf, 1500, 4000, Inf),
                labels = c("Low", "Medium", "High"))
price_data <- data.frame(price=numerical_subset$Recommended_Customer_Price,segment=segments)
boxplot(price~segment, data=price_data,main="Boxplot of Price by Segment",
        xlab="Segment", ylab="Price", col=c("lightblue", "lightgreen", "lightpink"))
grid()


# Biểu đồ Box Plot
#  Product Collections
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

# Vertical Segments
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
#-------------------------------------------------------------------


#----------------------  BÀI TOÁN 1 MẪU ----------------------------

# Bước 1. Lọc những sản phẩm Atom C Series
atom_data <- subset(onevar_data, grepl("Intel® Atom™ Processor C", Product_Collection, fixed = TRUE))
print(atom_data[, c("Product_Collection", "Max_Turbo_Frequency")])

# Bước 2. Chỉnh sửa lại cột Max_Turbo_Frequency (xóa bỏ đơn vị GHz)
atom_data$Max_Turbo_Frequency <- as.numeric(gsub(" GHz", "", atom_data$Max_Turbo_Frequency, fixed = TRUE))

# Bước 3. Loại bỏ giá trị NA
atom_data <- atom_data[!is.na(atom_data$Max_Turbo_Frequency), ]
print(atom_data[, c("Product_Collection", "Max_Turbo_Frequency")])

# Bước 4. Xử lý ngoại lai
Q1 <- quantile(atom_data$Max_Turbo_Frequency, 0.25)
Q3 <- quantile(atom_data$Max_Turbo_Frequency, 0.75)
IQR <- Q3 - Q1
atom_data <- atom_data[atom_data$Max_Turbo_Frequency >= Q1 - 1.5*IQR &
                         atom_data$Max_Turbo_Frequency <= Q3 + 1.5*IQR, ]
print(atom_data[, c("Product_Collection", "Max_Turbo_Frequency")])

# Bước 5. Kiểm định Shapiro (phân phối chuẩn)
shapiro.test(atom_data$Max_Turbo_Frequency)
#p-value = 0.2252 > 0.05 -> có thể xem là phân phối chuẩn

# Bước 6. T-Test với giả thuyết trung bình = 2.4
t.test(atom_data$Max_Turbo_Frequency, mu = 2.4)
#p-value = 0.0004507 < 0.05 -> bác bỏ H0 -> trung bình tần số Turbo tối đa không phải là 2.4 GHz
#-------------------------------------------------------------------


#----------------------- BÀI TOÁN 2 MẪU ----------------------------

# Phân chia dữ liệu thành 2 nhóm theo yêu cầu
group1 <- CPU_data$Recommended_Customer_Price[CPU_data$Lithography > 32]
group2 <- CPU_data$Recommended_Customer_Price[CPU_data$Lithography <= 32]

hist(group1, main = "Histogram of Group 1 (Lithography > 32)", 
     xlab = "Recommended Customer Price", col = "orange")
print(group1)

hist(group2, main = "Histogram of Group 2 (Lithography <= 32)", 
     xlab = "Recommended Customer Price", col = "orange")
print(group2)

# Tính toán trung bình và độ lệch chuẩn cho group1
mean_group1 <- mean(group1, na.rm = TRUE)
sd_group1 <- sd(group1, na.rm = TRUE)
n1 <- length(group1)

# Tính toán trung bình và độ lệch chuẩn cho group2
mean_group2 <- mean(group2, na.rm = TRUE)
sd_group2 <- sd(group2, na.rm = TRUE)
n2 <- length(group2)

# In kết quả
cat("Group 1: Mean =", mean_group1, ", SD =", sd_group1, ", n =", n1, "\n")
cat("Group 2: Mean =", mean_group2, ", SD =", sd_group2, ", n =", n2, "\n")

# Tính giá trị Z
Z <- (mean_group1 - mean_group2) / sqrt((sd_group1^2 / n1) + (sd_group2^2 / n2))
cat("Z =", Z, "\n")

# Vẽ QQ-plot cho group1
# qqplot1 <- ggqqplot(group1, main = "QQ Plot for Group 1 (Lithography > 32)")
# print(qqplot1)

# Vẽ QQ-plot cho group2
# qqplot2 <- ggqqplot(group2, main = "QQ Plot for Group 2 (Lithography <= 32)")
# print(qqplot2)

# Thực hiện kiểm định F-test
var_test_result <- var.test(group1, group2)

# In kết quả
print(var_test_result)
#-> Vi p < 0.05 nên Ho bị bác bỏ, dẫn đến phương sai của 2 mẫu khác nhau.

# Kiểm định t cho hai mẫu độc lập với phương sai khác nhau
result <- t.test(group1, group2, var.equal = FALSE)

# In kết quả kiểm định t
print(result)
#-> Vi p < 0.05 nên Ho bị bác bỏ, dẫn đến giá thành trung bình của 2 nhóm kích thước 
# là khác nhau
#-------------------------------------------------------------------


#----------------------- KIỂM ĐỊNH ANNOVA --------------------------

# Tạo bản sao của product_collect để sử dụng trong phân tích
product_collect_for_anova <- c('Atom', 'Celeron', 'Pentium', 'Xeon', 'Core')

# Lọc dữ liệu chỉ giữ lại các loại CPU cần phân tích
selected_data <- CPU_data[CPU_data$Product_Collection %in% product_collect_for_anova, ]

# Loại bỏ các quan sát có giá trị NA trong Recommended_Customer_Price
selected_data <- selected_data[!is.na(selected_data$Recommended_Customer_Price), ]

# 1. Kiểm tra phân phối chuẩn cho từng loại CPU bằng QQ-plot và Shapiro-Wilk test
par(mfrow = c(2, 3)) # Thiết lập layout đồ thị

for(cpu_type in product_collect_for_anova) {
  cpu_data <- subset(selected_data, Product_Collection == cpu_type)
  qqnorm(cpu_data$Recommended_Customer_Price, main = paste("QQ-plot cho", cpu_type))
  qqline(cpu_data$Recommended_Customer_Price)
  print(paste("Shapiro-Wilk test cho", cpu_type))
  print(shapiro.test(cpu_data$Recommended_Customer_Price))
}

par(mfrow = c(1, 1)) # Reset layout đồ thị

# 2. Kiểm tra phương sai đồng nhất bằng Levene's test
library(car)
print(leveneTest(Recommended_Customer_Price ~ as.factor(Product_Collection), data = selected_data))

# 3. Thực hiện mô hình ANOVA
anova_model <- aov(Recommended_Customer_Price ~ Product_Collection, data = selected_data)
print(summary(anova_model))

# 4. Nếu ANOVA có ý nghĩa thống kê (p-value < 0.05), thực hiện kiểm định hậu nghiệm
if (summary(anova_model)[[1]]$'Pr(>F)'[1] < 0.05) {
  # Sử dụng TukeyHSD cho kiểm định đa so sánh
  tukey_result <- TukeyHSD(anova_model)
  print(tukey_result)
  
  # Visualize kết quả
  par(mar = c(5, 6, 6, 2))
  plot(tukey_result, las = 1)
  
  # Vẽ boxplot so sánh giữa các nhóm
  library(ggplot2)
  ggplot(selected_data, aes(x = Product_Collection, y = Recommended_Customer_Price, 
                            fill = Product_Collection)) +
    geom_boxplot() +
    labs(title = "So sánh giá bán đề xuất giữa các loại CPU",
         x = "Loại CPU",
         y = "Giá bán đề xuất") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
} else {
  print("Không có sự khác biệt có ý nghĩa thống kê về giá bán giữa các loại CPU")
}
#-------------------------------------------------------------------


#------------------------ HỒI QUY BỘI ------------------------------

#---------------- Biến đổi log giá CPU (+ 1 để tránh log(0)) --------------------
CPU_data $ Recommended_Customer_Price <- log(CPU_data $ Recommended_Customer_Price + 1)

#---------------- Lọc các cột từ CPU_data để phân tích hồi quy ---------------------
#---------------- Tất cả các cột hiện tại đều được chọn, trừ Launch Date -----------------------------------
#---------------- Giá CPU phụ thuộc vào 11 cột định lượng và 4 cột định tính -------------------------------
#---------------- Số hệ số của đường hồi quy = 
#------------------------- 1 (intercept) + 
#------------------------- 11 (định lượng) + 
#------------------------- 10 (product_collection dummy vars) +
#------------------------- 3 (vertical_segment dummy vars) +
#------------------------- 3 (status dummy vars) +
#------------------------- 4 (cache_type dummy vars) =
#------------------------- 32
CPU_regression <- select(
  CPU_data, 
  Recommended_Customer_Price, Product_Collection, Vertical_Segment, Year, Lithography, Status,
  nb_of_Cores, nb_of_Threads, Processor_Base_Frequency, Cache, Cache_Type, Instruction_Set, TDP, 
  Max_Memory_Size, Max_nb_of_Memory_Channels, Max_Memory_Bandwidth
)

#---------------- Chuyển đổi các cột định tính thành định lượng bằng factor ------------
CPU_regression $ Product_Collection <- as.factor(CPU_regression $ Product_Collection)
CPU_regression $ Vertical_Segment <- as.factor(CPU_regression $ Vertical_Segment)
CPU_regression $ Status <- as.factor(CPU_regression $ Status)
CPU_regression $ Cache_Type <- as.factor(CPU_regression $ Cache_Type)

#------------------- Split 80% train, 20% test ----------------
set.seed(0)
#------------ Chọn index cho cho tập train (80% dữ liệu, dùng stratified sampling) ------
train_indices <- createDataPartition(CPU_regression $ Recommended_Customer_Price, p = 0.8, list = FALSE)
#------------ Chia dữ liệu thành train và test ---------------
train_data <- CPU_regression[train_indices, ]
test_data <- CPU_regression[-train_indices, ]

#------------ Huấn luyện mô hình để tìm các hệ số của đường hồi quy ---------------
regression_model <- lm(Recommended_Customer_Price ~ ., train_data)
regression_model_summary <- summary(regression_model)
print(regression_model_summary)

#------------ Mean Squared Error -------------------
sse <- sum((train_data$Recommended_Customer_Price - regression_model$fitted.values)^2)
n <- length(regression_model$fitted.values)
df = length(coef(regression_model))
train_mse_unbiased <- sse / (n - df)
#------------ Root Mean Squared Error --------------
train_rmse <- train_mse_unbiased ^ 0.5

cat("\tMSE:", train_mse_unbiased, "\n")
cat("\tRMSE:", train_rmse, "\n")

#------------ Test mô hình 1 -----------------
predicted_test_price <- predict(regression_model, newdata = test_data)
true_test_price <- test_data $ Recommended_Customer_Price

#------------ Mean Squared Error -------------
sse <- sum((true_test_price - predicted_test_price)^2)
n <- length(true_test_price)
df <- length(coef(regression_model))
test_mse_unbiased <- sse / (n - df)
#------------ Root Mean Squared Error --------
test_rmse <- test_mse_unbiased ^ 0.5
#------------ R Squared ----------------------
test_r2 <- 1 - sum((true_test_price - predicted_test_price) ^ 2) / sum((true_test_price - mean(true_test_price)) ^ 2) 
#------------ Adjusted R Squared ----------------------
n <- nrow(test_data)
k <- ncol(test_data) - 1
test_adjusted_r2 = 1 - (1 - test_r2 ^ 2) * (n - 1) / (n - k - 1)

cat("Test Data of Model 1:\n")
cat("\tMSE:", test_mse_unbiased, "\n")
cat("\tRMSE:", test_rmse, "\n")
cat("\tR-squared:", test_r2, "\n")
cat("\tAdjusted R-squared:", test_adjusted_r2, "\n")

#------------ Sau khi xem xét P Value của các hệ số đường hồi quy, tiến hành chọn lại ------
#------------ các cột có P Value < 0.05 để Train mô hình mới với ít cột hơn ----------------
#------------ (chỉ bao gồm các biến có ảnh hưởng lớn tới giá CPU) --------------------------

#---------------- Giá CPU phụ thuộc chủ yếu vào 8 cột định lượng và 4 cột định tính -------------------------------
#---------------- Số hệ số của đường hồi quy = 
#------------------------- 1 (intercept) + 
#------------------------- 8 (định lượng) + 
#------------------------- 10 (product_collection dummy vars) +
#------------------------- 3 (vertical_segment dummy vars) +
#------------------------- 3 (status dummy vars) +
#------------------------- 4 (cache_type dummy vars) =
#------------------------- 29
CPU_regression <- select(
  CPU_regression, 
  Recommended_Customer_Price, Product_Collection, Vertical_Segment, Year, Status,
  nb_of_Cores, nb_of_Threads, Processor_Base_Frequency, Cache, Cache_Type, 
  Max_Memory_Size, Max_nb_of_Memory_Channels, Max_Memory_Bandwidth
)

#--------------- Split 80% train - 20% test ----------------
train_data <- CPU_regression[train_indices, ]
test_data <- CPU_regression[-train_indices, ]

#------------ Huấn luyện mô hình để tìm các hệ số của đường hồi quy ---------------
regression_model <- lm(Recommended_Customer_Price ~ ., train_data)
regression_model_summary <- summary(regression_model)
print(regression_model_summary)

#------------ Mean Squared Error -------------------
sse <- sum((train_data$Recommended_Customer_Price - regression_model$fitted.values)^2)
n <- length(regression_model$fitted.values)
df = length(coef(regression_model))
train_mse_unbiased <- sse / (n - df)
#------------ Root Mean Squared Error --------------
train_rmse <- train_mse_unbiased ^ 0.5

cat("\tMSE:", train_mse_unbiased, "\n")
cat("\tRMSE:", train_rmse, "\n")

#------------ Test mô hình 2 -----------------
predicted_test_price <- predict(regression_model, newdata = test_data)
true_test_price <- test_data $ Recommended_Customer_Price

#------------ Mean Squared Error -------------
sse <- sum((true_test_price - predicted_test_price)^2)
n <- length(true_test_price)
df <- length(coef(regression_model))
test_mse_unbiased <- sse / (n - df)
#------------ Root Mean Squared Error --------
test_rmse <- test_mse_unbiased ^ 0.5
#------------ R Squared ----------------------
test_r2 <- 1 - sum((true_test_price - predicted_test_price) ^ 2) / sum((true_test_price - mean(true_test_price)) ^ 2) 
#------------ Adjusted R Squared ----------------------
n <- nrow(test_data)
k <- ncol(test_data) - 1
test_adjusted_r2 = 1 - (1 - test_r2 ^ 2) * (n - 1) / (n - k - 1)

cat("Test Data of Model 2:\n")
cat("\tMSE:", test_mse_unbiased, "\n")
cat("\tRMSE:", test_rmse, "\n")
cat("\tR-squared:", test_r2, "\n")
cat("\tAdjusted R-squared:", test_adjusted_r2, "\n")

#-- Vẽ biểu đồ Scatter, trục hoành là giá trị dự đoán ------
#-- trục tung là phần dư tại điểm tương ứng đã chuẩn hóa ---
#-- điểm màu đỏ là điểm ngoại lai với phần dư chuẩn hóa > 2 ---------
#-- điểm màu tím là điểm ngoại lai với phần dư chuẩn hóa > 3 --------

#------ Tạo data frame với giá trị dự đoán và phần dư chuẩn hóa ----------
df <- data.frame(
  Fitted_Values = regression_model$fitted.values,
  Residuals = rstandard(regression_model)
)

#------- Thêm cột phân loại theo mức độ ngoại lai (phần dư chuẩn hóa > 2 và > 3) ---------
df$Outlier_Level <- with(df, ifelse(
  abs(Residuals) > 3, "Outlier > 3",
  ifelse(abs(Residuals) > 2, "Outlier > 2", "Normal")
))

#-------- Gán màu theo phân loại --------------
p <- ggplot(df, aes(x = Fitted_Values, y = Residuals, color = Outlier_Level)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c(
    "Normal" = "green",
    "Outlier > 2" = "red",
    "Outlier > 3" = "purple"
  )) +
  labs(
    title = "Standardized Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Standardized Residuals",
    color = "Point Type"
  ) +
  theme_minimal()

ggplotly(p)

#--- Biểu đồ Q-Q, trục hoành là phân vị của phân phối chuẩn theo lí thuyết -----
#--- trục tung là phân vị thực tế của phần dư ----
#--- Các điểm càng phân bổ dọc theo đường chéo thì phân phối của phần dư càng gần phân phối chuẩn -------------
qqnorm(regression_model $ residuals, main = "Q-Q Plot of Residuals")
qqline(regression_model $ residuals, col = "red", lwd = 2)

#------------ Tạo 2 CPU ngẫu nhiên và dự đoán giá của chúng bằng mô hình 2 -------------------
first_cpu <- data.frame(
  Product_Collection = "Pentium",
  Vertical_Segment = "Desktop",
  Status = "Launched",
  Year = 2013,
  Lithography = 14,
  nb_of_Cores = 3,
  nb_of_Threads = 8,
  Processor_Base_Frequency = 3000,
  Cache = 9,
  Cache_Type = "Normal",
  Instruction_Set = 64,
  TDP = 100,
  Max_Memory_Size = 6,
  Max_nb_of_Memory_Channels = 10,
  Max_Memory_Bandwidth = 50
)

second_cpu <- data.frame(
  Product_Collection = "Celeron",
  Vertical_Segment = "Embedded",
  Status = "End of Life",
  Year = 2015,
  Lithography = 16,
  nb_of_Cores = 10,
  nb_of_Threads = 24,
  Processor_Base_Frequency = 4000,
  Cache = 12.34,
  Cache_Type = "L3",
  Instruction_Set = 32,
  TDP = 164,
  Max_Memory_Size = 7.99,
  Max_nb_of_Memory_Channels = 8,
  Max_Memory_Bandwidth = 105
)

log_predicted_price_1 <- predict(regression_model, newdata = first_cpu, se.fit = TRUE)
predicted_price_1 <- exp(log_predicted_price_1 $ fit) - 1
lower_bound_1 = exp((log_predicted_price_1 $ fit) - 1.645 * (log_predicted_price_1 $ se.fit)) - 1
upper_bound_1 = exp((log_predicted_price_1 $ fit) + 1.645 * (log_predicted_price_1 $ se.fit)) - 1
cat("First CPU recommended price: ", predicted_price_1, "\n")
cat("90% Confident Interval: [", lower_bound_1, ", ", upper_bound_1, "]\n\n")

log_predicted_price_2 <- predict(regression_model, newdata = second_cpu, se.fit = TRUE)
predicted_price_2 <- exp(log_predicted_price_2 $ fit) - 1
lower_bound_2 = exp((log_predicted_price_2 $ fit) - 1.645 * (log_predicted_price_2 $ se.fit)) - 1
upper_bound_2 = exp((log_predicted_price_2 $ fit) + 1.645 * (log_predicted_price_2 $ se.fit)) - 1
cat("Second CPU recommended price: ", predicted_price_2, "\n")
cat("90% Confident Interval: [", lower_bound_2, ", ", upper_bound_2, "]\n\n")
#-------------------------------------------------------------------
