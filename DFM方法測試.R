# 安裝並載入必要套件
install.packages(c("dfms", "xts", "ggplot2"))
library(dfms); library(xts); library(ggplot2)

set.seed(123)
n_obs <- 100; n_vars <- 5
time_index <- seq(as.Date("2000-01-01"), by="month", length.out=n_obs)

# 產生 AR(1) 過程的潛在因子
true_factor <- as.numeric(arima.sim(list(order = c(1,0,0), ar = 0.7), n = n_obs))

# 確保 `true_factor` 是數值型態
X <- 0.6 * true_factor + matrix(rnorm(n_obs * n_vars, sd=0.5), nrow=n_obs)

# 轉換為時間序列格式
df_xts <- xts(X, order.by=time_index)
colnames(df_xts) <- paste0("Var", 1:n_vars)

head(df_xts)  # 查看數據是否正確


# 估計動態因子模型（DFM）
dfm_model <- DFM(df_xts, r = 1, p = 1, method = "twoStep")

# 提取估計的因子
estimated_factors <- factors.DFM(dfm_model)  # 使用 factors.DFM() 而不是 factors()

# 轉換為 xts 格式
factor_xts <- xts(estimated_factors, order.by=time_index)

# 定義繪圖函數
plot_factor <- function(data, title, color) {
  ggplot(fortify(data, melt = TRUE), aes(x = Index, y = Value)) +
    geom_line(color = color) +
    labs(title = title, x = "Time", y = "Factor Value") +
    theme_minimal()
}

# 畫出因子時間趨勢
plot_factor(factor_xts, "Estimated Dynamic Factor", "blue")
