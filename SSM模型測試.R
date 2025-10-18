install.packages("dlm")
library(dlm)
# ﹚竡 SSM
buildSSM <- function(theta) {
  dlmModPoly(order = 1, dV = exp(theta[1]), dW = exp(theta[2]))  # dV 琌芠代粇畉, dW 琌篈锣簿粇畉
}

# ︳璸家把计
fit <- dlmMLE(y, parm = c(log(0.2^2), log(0.5^2)), build = buildSSM)

# ミ SSM
model <- buildSSM(fit$par)

# Kalman Filter ︳璸
kf <- dlmFilter(y, model)

# キ菲篈︳璸
ks <- dlmSmooth(kf)
plot(ks$s[-1], type = "l", col = "blue", lwd = 2, ylab = "State Estimate")
lines(x, col = "red", lwd = 2, lty = 2)  # 痷龟篈跑计
legend("topright", legend = c("Estimated State", "True State"), col = c("blue", "red"), lty = c(1, 2))
