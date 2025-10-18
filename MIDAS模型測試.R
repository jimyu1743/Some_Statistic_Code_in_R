# 安裝並載入所需套件
install.packages("midasr")   # 若尚未安裝，請取消註解安裝
library(midasr)

# 1. 隨機生成年、季、月資料（這是範例資料，根據實際情況修改）
set.seed(123)
years <- 2000:2020
quarters <- rep(seq(1, 4), each = 21)   # 21年，4季
months <- rep(1:12, each = 7)  # 每年12個月

# 隨機生成數據
year_data <- data.frame(
  year = years,
  variable = rnorm(length(years), mean = 5, sd = 1)  # 年資料
)

quarter_data <- data.frame(
  year = rep(years, each = 4),
  quarter = quarters,
  variable = rnorm(length(quarters), mean = 5, sd = 1)  # 季資料
)

month_data <- data.frame(
  year = rep(years, each = 12),
  month = months,
  variable = rnorm(length(months), mean = 5, sd = 1)  # 月資料
)

# 2. 結合混頻資料
# 這裡假設月資料為基準，其它資料以 NA 補全
data_combined <- merge(month_data, quarter_data, by = "year", all.x = TRUE)
data_combined <- merge(data_combined, year_data, by = "year", all.x = TRUE)

# 對混頻資料進行填補 NA（可以根據需要進行插值或其他處理）
data_combined$variable.x <- ifelse(is.na(data_combined$variable.x), data_combined$variable.y, data_combined$variable.x)
data_combined$variable.x <- ifelse(is.na(data_combined$variable.x), data_combined$variable, data_combined$variable.x)

# 3. 構建 MIDAS 模型
# 使用季資料對月資料進行回歸（這是一個範例，具體模型可能會根據你的需求有所調整）
midas_model <- midas_r(variable.x ~ 1 + mls(variable, 1:4, 1), 
                       data = data_combined, 
                       start = NULL)
# 查看 MIDAS 模型的結果
summary(midas_model)
