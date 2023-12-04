set.seed(25)

# (1)
kvals <- 0
for(i in 1:500) {
  kvals[i] <- i * 10
}

percents <- 0
for (i in seq_along(kvals)) {
  k <- kvals[i]
  insides <- 0
  x <- runif(k, 0, 1)
  y <- runif(k, 0, 1)
  for (j in 1:k) {
    if (x[j] ^ 2 + y[j] ^ 2 < 1) {
      insides <- insides + 1
    }
  }
  percent <- insides / k
  percents[i] <- percent * 4
}

png("kadai02-01.png", width = 512, height = 512)
plot(kvals, percents, type = "l", xlab = "number of random values", ylab = "PI", main = "Estimated PI value")
dev.off()

# (2)
# 1から5000まで10刻みで増加する等差数列を生成
kvals <- 0
for(i in 1:500) {
  kvals[i] <- i * 10
}
r <- 100  # 試行回数を100に設定

percent_means <- 0  # PIの平均値を格納する配列を初期化
percent_sds <- 0  # PIの分散を格納する配列を初期化
# 各kについてPIを求める
for (i in seq_along(kvals)) {
  k <- kvals[i]  # kを等差数列から取得
  x <- matrix(1:(k * r), nrow = r, ncol = k)  # ランダム生成するx保存用の行列を初期化
  y <- matrix(1:(k * r), nrow = r, ncol = k)  # ランダム生成するy保存用の行列を初期化
  pis <- 0  # 計算したPIを保存するための配列を初期化
  # r(100)回試行する
  for (j in 1:r) {
    insides <- 0  # 円の内部にあった点をカウントするための変数を初期化
    x[j, ] <- runif(k, 0, 1)  # k個0以上1以下の乱数xを生成
    y[j, ] <- runif(k, 0, 1)  # k個0以上1以下の乱数yを生成
    # k個生成したランダムな座標についてチェックを行う
    for (l in 1:k) {
      # 点が円の内部にあればカウントする
      if (x[j, l] ^ 2 + y[j, l] ^ 2 < 1) {
        insides <- insides + 1
      }
    }
    pis[j] <- insides / k * 4  # PIを推定する。第一象限のシミュレーションだったので円全体の値にするために4倍する
  }
  percent_means[i] <- mean(pis)  # PIの平均値を計算
  percent_sds[i] <- sd(pis)  # PIの分散を保存
}

png("kadai02-02-mean.png", width = 512, height = 512)  # グラフをpngに描画する
plot(kvals, percent_means, type = "l", xlab = "number of random values", ylab = "PI mean", main = "100 times estimated PI value mean")  # kの値とPIの平均値について折れ線グラフを描画
dev.off()  # 画像を保存
png("kadai02-02-sd.png", width = 512, height = 512) # グラフをpngに描画する
plot(kvals, percent_sds, type = "l", xlab = "number of random values", ylab = "PI sd", main = "100 times estimated PI value sd")  # kの値とPIの分散について折れ線グラフを描画
dev.off()  # 画像を保存