# 安裝必要套件（若未安裝）
packages <- c("tidyr", "dplyr", "FactoMineR", "missMDA", "zoo", "factoextra")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# 載入必要套件
library(tidyr)
library(dplyr)
library(FactoMineR)
library(missMDA) 
library(zoo)        # 用於移動平均
library(factoextra) # PCA 可視化

# 假設我們有季頻 GDP 和月頻的工業生產、貿易數據
set.seed(123)
monthly_data <- data.frame(
  date = seq(as.Date("2010-01-01"), as.Date("2023-12-01"), by = "month"),
  ind_production = rnorm(168, 100, 5),  # 工業生產（月頻）
  trade = rnorm(168, 500, 20)           # 貿易數據（月頻）
)

# 假設 GDP 是季頻數據（每 3 個月一個數值）
quarterly_gdp <- data.frame(
  date = seq(as.Date("2010-01-01"), as.Date("2023-12-01"), by = "quarter"),
  gdp = rnorm(56, 2000, 50)  # 季頻 GDP
)

# **對齊頻率**

#將季度GDP數據轉換為月頻（展開季頻數據）
monthly_gdp <- quarterly_gdp %>%
  mutate(date = as.Date(date)) %>%
  tidyr::complete(date = seq(min(date), max(date), by = "month")) %>%
  dplyr::arrange(date)

#對延展後GDP進行線性插值
monthly_gdp$gdp <- zoo::na.approx(monthly_gdp$gdp, x = monthly_gdp$date, xout = monthly_gdp$date, na.rm = TRUE)

#合併數據
combined_data <- left_join(monthly_data, monthly_gdp, by = "date")

#檢查資料結構
str(combined_data)

#檢查是否有缺失值並進行處理
sum(is.na(combined_data))
combined_data<-na.omit(combined_data)
#處理非數值型欄位
combined_data <- combined_data %>%
  mutate(across(everything(), as.numeric))  # 確保所有欄位為數值型

# **再次檢查是否還有缺失值**
sum(is.na(combined_data))

# **標準化數據**
scaled_data <- scale(combined_data)

# **執行 PCA**
pca_result <- PCA(scaled_data, scale.unit = TRUE, ncp = 3)

# **視覺化**
fviz_pca_var(pca_result, col.var = "contrib", repel = TRUE)  # 變數貢獻度
fviz_pca_ind(pca_result, repel = TRUE)  # 觀測值投影

# **顯示主要成分的解釋比例**
summary(pca_result)


