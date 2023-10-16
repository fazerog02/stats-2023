# (1)
ReadData <- read.table("./week2-data.txt")  # ファイルからデータを読み込む

# (2)
Data <- ReadData$V1  # データ部分を取り出す
Mean <- mean(Data)  # 平均値を求める
SD <- sd(Data)  # 標準偏差を求める

# (3)
png("kadai01-05.png", width=512, height=512)  # グラフを画像に保存
# hist(Data, breaks=25, xlab='value', ylab='frequency', main='histogram')  # binが25個のヒストグラムを表示
hist(Data, breaks=25, xlab='value', ylab='density', main='histogram', freq=FALSE)  # binが25個のヒストグラムを表示

# (4)
# 上から1～5番目、1~10番目、1~20番目のデータを使用した平均値と標準誤差を計算する
Mean1 <- mean(Data[1:5])
SE1 <- sd(Data[1:5]) / sqrt(5)
Mean2 <- mean(Data[1:10])
SE2 <- sd(Data[1:10]) / sqrt(10)
Mean3 <- mean(Data[1:20])
SE3 <- sd(Data[1:20]) / sqrt(20)

# (5)
par(new=TRUE)  # グラフの重ね書きを許可

# 平均値と標準偏差について描画する
arrows(Mean, 0, Mean, 0.1, lwd=3, col="blue")
arrows(Mean, 0, Mean-SD, 0, lwd=3, col="red")
arrows(Mean, 0, Mean+SD, 0, lwd=3, col="red")

curve(dnorm(x, Mean, SD), add=TRUE, col="green", lwd=3)  # 正規分布で近似した曲線を描画

# 標準誤差を計算して描画
SE <- SD / sqrt(length(Data))
text(50, 0.03, paste("標準誤差:", floor(SE * 10^3 + 0.5) / 10^3, sep=" "))

dev.off()