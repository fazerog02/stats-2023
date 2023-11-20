# (1)
raw_data <- read.table("./data_r.txt", header = TRUE)  # txtファイルからデータを読み込む
png("kadai06-01-A.png", width = 512, height = 512)  # グラフを画像で保存する
hist(raw_data$A, breaks = 15, xlab = "value", ylab = "frequency", main = "Group A of data_r.txt")  # Aのデータでヒストグラムを描画
dev.off()  # 画像保存
png("kadai06-01-B.png", width = 512, height = 512)  # グラフを画像で保存する
hist(raw_data$B, breaks = 15, xlab = "value", ylab = "frequency", main = "Group B of data_r.txt")  # Bのデータでヒストグラムを描画
dev.off()  # 画像保存

# (2)
n <- 10000  # 反復回数を設定
diff <- abs(mean(raw_data$A) - mean(raw_data$B))  # 実際の観測値を計算
diff_r <- numeric(n)  # データを格納するための配列を用意
data <- c(raw_data$A, raw_data$B)  # 新しく母集団を作る
# ランダマイゼーション検定を行う
for (i in 1:n - 1) {
  data_r <- sample(data, replace = FALSE)  # サンプリング
  a_r <- data_r[seq_along(raw_data$A)]  # サンプリングした中からaを生成する
  b_r <- data_r[(length(raw_data$A) + 1):length(data_r)]  # サンプリングした中からbを生成する
  diff_r[i] <- abs(mean(a_r) - mean(b_r))  # データを配列に保存
}
diff_r[n] <- diff  # 実際の観測値をデータに加える
p <- sum(diff_r >= diff) / n  # p値を計算する
cat("p-value: ")
print(p)
png("kadai06-02.png", width = 512, height = 512)  # グラフを画像で保存する
hist(raw_data$A, breaks = 15, xlab = "value", ylab = "frequency", main = "Group A of data_r.txt")  # Aのデータでヒストグラムを描画
par(new = TRUE)  # グラフの重ね合わせを許可
# points(diff, y_center, pch = 16, col = "red", cex = 1.5)  # 実測値を点で描画
dev.off()  # 画像保存

# (3)
# ps <- 0
# mean_p <- 0
# sd_p <- 0
# n_p <- 0

# r <- 10
# n <- 25
# m <- 0

# while (n < 50000) {
#   n <- n * 2
#   m <- m + 1
#   for (j in 1:r) {
#     diff_r <- 0
#     for(i in 1:n) {
#       data_r <- sample(data, replace = FALSE)
#       a_r <- data_r[seq_along(raw_data$A)]
#       b_r <- data_r[(length(raw_data$A) + 1):length(data_r)]
#       diff_r[i] <- (mean(a_r) - mean(b_r))
#     }
#     diff_r[n + 1] <- diff
#     ps[j] <- sum(diff_r >= diff) / (n + 1)
#     mean_p[m] <- mean(ps)
#     sd_p[m] <- sd(ps)
#     n_p[m] <- n
#   }
#   print(n)
# }
# # 各種値の表示
# cat("m: ")
# print(m)
# cat("ps: ")
# print(ps)
# cat("mean_p: ")
# print(mean_p)
# cat("sd_p: ")
# print(sd_p)
# cat("n_p: ")
# print(n_p)
# png("kadai06-03.png", width = 512, height = 512)  # グラフを画像で保存する
# matplot(n_p, cbind(mean_p, sd_p), type = "l", col = c("blue", "red"), lwd = 2, xlab = "N", ylab = "value", lty = 1)
# legend("topleft", legend = c("Mean", "SD"), col = c("blue", "red"), lwd = 2, lty = 1)
# title("Mean and SD of p-value")
# dev.off()  # 画像を保存
# # n >= 12800からあまり値が変化していないので，n = 10000程度が好ましい

# (4)
library(perm)
perm_result <- permTS(raw_data$A, raw_data$B, alternative = c("greater"))
print(perm_result)

#        Permutation Test using Asymptotic Approximation

# data:  raw_data$A and raw_data$B
# Z = 1.4978, p-value = 0.06709
# alternative hypothesis: true mean raw_data$A - mean raw_data$B is greater than 0
# sample estimates:
# mean raw_data$A - mean raw_data$B
#                          1.075268

t_result <- t.test(raw_data$A, raw_data$B, equal.var = TRUE, alternative = "g")
print(t_result)

#        Welch Two Sample t-test

# data:  raw_data$A and raw_data$B
# t = 1.4997, df = 361.18, p-value = 0.06729
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  -0.1071251        Inf
# sample estimates:
# mean of x mean of y
#  50.70256  49.62729

# t検定は前提条件として等分散性と正規性を満たさないといけないため，今回のp値は信頼できる値ではなく，ランダマイゼーションが必要となる．