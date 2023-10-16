# (0)
sample_means <- c(1:10000)  # 標本平均を格納する変数
# 10000回単純無作為抽出を行い，その平均値を変数に格納する
for(i in 1:10000) {
    sample_means[i] <- mean(rnorm(n=10, mean=100, sd=10))
}
sample_means_mean <- mean(sample_means)

png("kadai0.png", width=512, height=512)  # グラフを画像に保存
hist(sample_means, xlab='sample mean', ylab='frequency', main='histogram')  # ヒストグラムを描く
par(new=TRUE)  # グラフを重ねられるようにする
arrows(mean(sample_means), 0, mean(sample_means), 400, lwd=3, col="blue")  # 平均値に矢印を描く
text(x=min(sample_means)+2.5, y=2000, paste("平均値:", floor(sample_means_mean * 10^3 + 0.5) / 10^3, sep=" "))
dev.off()