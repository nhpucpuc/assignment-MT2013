#-----------PREPROCESSING----------------
required_packages <- c("stringi", "stringr", "tidyr", "dplyr", "zoo", "Metrics", "caret", "MASS", "ggplot2", "reshape2", "mltools", "DescTools", "plotly", "naniar", "corrplot", "car", "caret")

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

sapply(required_packages, install_if_missing)
CPU_data <- read.csv("Intel_CPUs.csv", na.strings = c("", "N/A"))
cat("\014")
str(CPU_data)

na_summary <- data.frame(
  Feature = names(CPU_data),
  NA_Count = sapply(CPU_data, function(x) sum(is.na(x))),
  NA_Percentage = round(sapply(CPU_data, function(x) mean(is.na(x)) * 100), 2)
) %>%
  arrange(desc(NA_Percentage))

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
cat("\014")
print(na_summary)

CPU_data = CPU_data[,c("Product_Collection", "Vertical_Segment", "Status", "Launch_Date", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Instruction_Set", "TDP", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")]
product_collect <- c('Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon', 'Core')

for (category in product_collect) {
  CPU_data$Product_Collection <- ifelse(
    grepl(category, CPU_data$Product_Collection, ignore.case = TRUE),
    ifelse(grepl("Legacy", CPU_data$Product_Collection, ignore.case = TRUE),
           paste("Legacy", category),
           category),
    CPU_data$Product_Collection
  )
}

CPU_data <- CPU_data[!is.na(CPU_data$Launch_Date),]
CPU_data$Year <- as.numeric(gsub(".*'(\\d+)", "\\1", CPU_data$Launch_Date))
CPU_data$Year <- ifelse(CPU_data$Year <= 99 & CPU_data$Year >= 50, 
                        CPU_data$Year + 1900, 
                        CPU_data$Year + 2000)
CPU_data <- CPU_data[order(-CPU_data$Year), ]
CPU_data <- CPU_data %>% relocate(Year, .after = Launch_Date)

CPU_data$Lithography <- as.numeric(gsub(" nm", "", CPU_data$Lithography))

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

#------ Seperate Cache column to Cache column (size) and Cache type column, size follow KB (already log e) ----------
cache_units <- gsub("^\\d+\\.?\\d*\\s*|\\s+", "", CPU_data$Cache)
cache_units <- cache_units[cache_units != ""]
unique_units <- unique(cache_units)
CPU_data <- separate(CPU_data, Cache, into = c("Cache", "Cache_Type"), sep = "B")
CPU_data$Cache_Type <- ifelse(
  CPU_data$Cache_Type == "",
  "Normal",
  sub(" ", "", CPU_data$Cache_Type)
)
CPU_data <- CPU_data[!(is.na(CPU_data$Cache)), ]
CPU_data <- CPU_data[!(is.na(CPU_data$Cache_Type)), ]
CPU_data$Cache <- gsub("(MB|KB).*", "\\1", CPU_data$Cache)
CPU_data$Cache <- ifelse(
  grepl("M", CPU_data$Cache),
  as.numeric(gsub(" M", "", CPU_data$Cache)) * 1024,
  as.numeric(gsub(" K", "", CPU_data$Cache))
)
CPU_data$Cache <- log(CPU_data$Cache + 1)

#---------- Fill NA Instruction set column and change to number like 64, 32 -------------
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
#----------------------- Remove NA row in TDP , convert to number -------------------------------------------
CPU_data$TDP <- as.numeric(gsub(" W", "", CPU_data$TDP))
CPU_data <- CPU_data[!(is.na(CPU_data$TDP)), ]
print(CPU_data)
#------------- Max memory size column to number, unit GB, log e -----------------
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
#----------- Max Number of Memory Channels Fill NA----------------------------------------------
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
#---------------------Max Memory Bandwidth to number and fill NA ---------------------------------
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
#------------- Reccommended Price to number, fill NA, if range convert to average ----------------------
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

CPU_data$Recommended_Customer_Price <- as.double(CPU_data$Recommended_Customer_Price)

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
train_mse <- mean((train_data $ Recommended_Customer_Price - regression_model $ fitted.values) ^ 2)
#------------ Root Mean Squared Error --------------
train_rmse <- train_mse ^ 0.5

cat("\tMSE:", train_mse, "\n")
cat("\tRMSE:", train_rmse, "\n")

#------------ Test mô hình 1 -----------------
predicted_test_price <- predict(regression_model, newdata = test_data)
true_test_price <- test_data $ Recommended_Customer_Price

#------------ Mean Squared Error -------------
test_mse <- mean((true_test_price - predicted_test_price) ^ 2)
#------------ Root Mean Squared Error --------
test_rmse <- test_mse ^ 0.5
#------------ R Squared ----------------------
test_r2 <- 1 - sum((true_test_price - predicted_test_price) ^ 2) / sum((true_test_price - mean(true_test_price)) ^ 2) 
#------------ Adjusted R Squared ----------------------
n <- nrow(test_data)
k <- ncol(test_data) - 1
test_adjusted_r2 = 1 - (1 - test_r2 ^ 2) * (n - 1) / (n - k - 1)

cat("Test Data of Model 1:\n")
cat("\tMSE:", test_mse, "\n")
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
train_mse <- mean((train_data $ Recommended_Customer_Price - regression_model $ fitted.values) ^ 2)
#------------ Root Mean Squared Error --------------
train_rmse <- train_mse ^ 0.5

cat("\tMSE:", train_mse, "\n")
cat("\tRMSE:", train_rmse, "\n")

#------------ Test mô hình 2 -----------------
predicted_test_price <- predict(regression_model, newdata = test_data)
true_test_price <- test_data $ Recommended_Customer_Price

#------------ Mean Squared Error -------------
test_mse <- mean((true_test_price - predicted_test_price) ^ 2)
#------------ Root Mean Squared Error --------
test_rmse <- test_rmse ^ 0.5
#------------ R Squared ----------------------
test_r2 <- 1 - sum((true_test_price - predicted_test_price) ^ 2) / sum((true_test_price - mean(true_test_price)) ^ 2) 
#------------ Adjusted R Squared ----------------------
n <- nrow(test_data)
k <- ncol(test_data) - 1
test_adjusted_r2 = 1 - (1 - test_r2 ^ 2) * (n - 1) / (n - k - 1)

cat("Test Data of Model 2:\n")
cat("\tMSE:", test_mse, "\n")
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