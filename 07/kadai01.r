set.seed(25)

# (1)
raw_data <- read.table("./data_p.txt", header = TRUE)  # txtファイルからデータを読み込む
png("kadai07-01-01-A.png", width = 512, height = 512)  # グラフを画像で保存する
hist(raw_data$A, breaks = 15, xlab = "value", ylab = "frequency", main = "Group A of data_p.txt")  # Aのデータでヒストグラムを描画
dev.off()  # 画像保存
png("kadai07-01-01-B.png", width = 512, height = 512)  # グラフを画像で保存する
hist(raw_data$B, breaks = 15, xlab = "value", ylab = "frequency", main = "Group B of data_p.txt")  # Bのデータでヒストグラムを描画
dev.off()  # 画像保存

# (2)
n <- 10000  # 反復回数を設定
diff <- abs(mean(raw_data$A) - mean(raw_data$B))  # 実際の観測値を計算
diff_r <- numeric(n)  # データを格納するための配列を用意
data <- c(raw_data$A, raw_data$B)  # 新しく母集団を作る
# ランダマイゼーション検定を行う
for (i in 1:n - 1) {
  a_r <- sample(data, length(raw_data$A), replace = TRUE)  # aをbootstrapサンプリング
  b_r <- sample(data, length(raw_data$B), replace = TRUE)  # サンプリングした中からbを生成する
  diff_r[i] <- mean(a_r) - mean(b_r)  # データを配列に保存
}
diff_r[n] <- diff  # 実際の観測値をデータに加える
p <- sum(diff_r >= diff) / n  # p値を計算する
cat("p-value: ")
print(p)
# p-value: [1] 0.032
# p = 0.0318 < α = 0.05 なので帰無仮説は棄却されず，AとBに有意な差があるとは言えない．

png("kadai07-01-02.png", width = 512, height = 512)  # グラフを画像で保存する
curve(dnorm(x, mean = mean(diff_r), sd = sd(diff_r)), from = -3, to = 3, xlab = "x", ylab = "dnorm(x)", main = "dnorm with actual value")  # 理論分布を描画
par(new = TRUE)  # グラフの重ね合わせを許可
arrows(diff, 0.1, diff, 0.03, col = "blue", lwd = 1.5)  # 実測値を点で描画
dev.off()  # 画像保存

# (3)
# p-value: [1] 0.032
# p = 0.0318 < α = 0.05 なので帰無仮説は棄却され，AとBに有意な差がないとは言えない．

# (4)
t_result <- t.test(raw_data$A, raw_data$B, equal.var = TRUE, alternative = "g")
print(t_result)
# p-value: [1] 0.032

#         Welch Two Sample t-test

# data:  raw_data$A and raw_data$B
# t = 1.8629, df = 86.06, p-value = 0.03295
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#  0.3078325       Inf
# sample estimates:
# mean of x mean of y 
#   53.1654   50.3000 

# #        Permutation Test using Asymptotic Approximation

# # data:  raw_data$A and raw_data$B
# # Z = 1.4978, p-value = 0.06709
# # alternative hypothesis: true mean raw_data$A - mean raw_data$B is greater than 0
# # sample estimates:
# # mean raw_data$A - mean raw_data$B
# #                          1.075268

# t_result <- t.test(raw_data$A, raw_data$B, equal.var = TRUE, alternative = "g")  # t検定をする
# print(t_result)  # 結果を表示

# #        Welch Two Sample t-test

# # data:  raw_data$A and raw_data$B
# # t = 1.4997, df = 361.18, p-value = 0.06729
# # alternative hypothesis: true difference in means is greater than 0
# # 95 percent confidence interval:
# #  -0.1071251        Inf
# # sample estimates:
# # mean of x mean of y
# #  50.70256  49.62729

# # t検定は前提条件として等分散性と正規性を満たさないといけないため，今回のp値は信頼できる値ではなく，ランダマイゼーションが必要となる．