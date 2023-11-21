set.seed(625)
data <- c(rnorm(2000, mean = 0, sd = 5))

# (1)
n <- 10
r <- 100
bs100 <- numeric(r)
for (i in 1:r) bs100[i] <- mean(sample(data, n, replace = TRUE))

cat("mean_bs: ")
print(mean(bs100))
cat("sd_bs: ")
print(sd(bs100))
png("kadai07-prac-01.png", width = 512, height = 512)
hist(bs100, xlab = "value", ylab = "frequency", main = "mean of bootstraps")
dev.off()

# (2)
n <- 100  # サンプル数を100に設定
r <- 10  # 反復回数の初期値を10に設定
bs <- 0  # すべてのbootstrap sampleの平均値を一次元配列に保存する
mean_bs <- 0  # 各試行回数の平均値
r_bs <- 0  # 各試行回数を保存
count <- 1  # 反復回数の変更回数
r_count <- 0  # bsに対する現在使用しているインデックス
# 10240までループ
while (r <= 10240) {
  for (i in 1:r) bs[r_count + i] <- mean(sample(data, n, replace = TRUE))  # bootstrap sampleから平均値を計算して格納
  mean_bs[count] <- mean(bs[r_count:r_count + r])  # 今回の試行回数における平均値を計算
  r_bs[count] <- r  # rの値を保存
  r <- 2 * r  # rを2倍ずつ増やしていく
  count <- count + 1  # rの変更回数を+1
  r_count <- r_count + r  # 使用しているインデックスの上限を更新
}

# 試行回数の一覧と平均値を表示
cat("r_bs: ")
print(r_bs)
cat("mean_bs: ")
print(mean_bs)

png("kadai-07-prac-02.png", width = 512, height = 512)  # 画像を生成
hist(bs[1:10], breaks = 15, col = "#FF00007F", xlim = c(-2.5, 2.5), ylim = c(0, 40), main = "the histograms of bootstrap samples", xlab = "value", ylab = "frequency")  # 試行回数10のヒストグラムを描画
hist(bs[31:70], breaks = 15, col = "#0000FF7F", add = TRUE)  # 試行回数40のヒストグラムを描画
hist(bs[311:630], breaks = 15, col = "#00FF007F", add = TRUE)  # 試行回数320のヒストグラムを描画
legend("topleft", legend = c("10", "40", "320"), col = c("red", "blue", "green"), pch = 15)  # 凡例を描画
dev.off()  # 画像を保存

# mean_bs:  [1]  0.07640089  0.52124564 -0.54734619 -0.29540084 -0.42588074  0.63228544
#  [7]  0.58582188  0.03888773  1.24735826 -0.26450813  0.22992921