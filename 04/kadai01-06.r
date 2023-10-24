# (1)
# H_0: 投与した薬物の間に平均値差がない
# H_1: 投与した薬物の間に平均値差がある

# (2)
raw_data <- read.table("week4-example.txt", header = TRUE)  # ファイルからデータを読み込む
# factorのlevelを入れたvectorを作る
group_a <- factor(rep(1, length = nrow(raw_data)))  # A群は1
group_b <- factor(rep(2, length = nrow(raw_data)))  # B群は2
group_c <- factor(rep(3, length = nrow(raw_data)))  # C群は3
# 各群のデータフレームを生成
a <- data.frame(weight = raw_data$A, group = group_a)
b <- data.frame(weight = raw_data$B, group = group_b)
c <- data.frame(weight = raw_data$C, group = group_c)
data <- rbind(a, b, c)  # データフレームを結合する
# 各群の平均値と標準偏差を求めて配列で保管する
means <- double(3)
sds <- double(3)
for (i in 1:3) {
  means[i] <- mean(data$weight[data$group == i])  # 平均値
  sds[i] <- sd(data$weight[data$group == i])  # 標準偏差
}
# 値を表示
cat("means: ")
print(means)  # 平均値
cat("sds: ")
print(sds)  # 標準偏差
cat("\n")
# 各群のヒストグラムを作成
for (i in 1:3) {
  png(paste("kadai03-", i, ".png", sep = ""), width = 512, height = 512)  # グラフを画像で保存する
  hist_data <- hist(data$weight[data$group == i], xlab = "weight", ylab = "frequency", main = paste("group", i, " histogram", sep = ""))  # ヒストグラムを描画
  par(new = TRUE)  # グラフの重ね合わせを許可
  y_center <- max(hist_data$counts) / 2
  points(means[i], y_center, pch = 16, col = "red", cex = 1.5)  # 平均値を点で描画
  # 標準偏差を矢印で描画
  arrows(means[i], y_center, means[i] - sds[i], y_center, col = "blue", lwd = 1.5)
  arrows(means[i], y_center, means[i] + sds[i], y_center, col = "blue", lwd = 1.5)
  dev.off()  # 画像を保存
}

# (3)
aov_result <- summary(aov(formula = weight ~ group, data = data))  # aovで分散分析
# 結果を表示
cat("AOV RESULT\n")
print(aov_result)
cat("\n")
# AOV RESULT
#             Df Sum Sq Mean Sq F value  Pr(>F)
# group        2  283.4  141.69   5.351 0.00643 **
# Residuals   87 2303.9   26.48
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (4)
# 有意水準を1%とするとα=0.01となる．今回aovで求めたp値は0.00643となっており，αより小さい．
# よって，帰無仮説は棄却され，投与した薬物の間に有意な平均値さが見られた．

# (5)
# 各群の等分散性をチェックする
var_test_result1_2 <- var.test(data$weight[data$group == 1], data$weight[data$group == 2])
var_test_result1_3 <- var.test(data$weight[data$group == 1], data$weight[data$group == 3])
var_test_result2_3 <- var.test(data$weight[data$group == 2], data$weight[data$group == 3])
# 結果を表示
cat("VAR TEST RESULT\n")
print(var_test_result1_2)
print(var_test_result1_3)
print(var_test_result2_3)
# VAR TEST RESULT

#         F test to compare two variances

# data:  data$weight[data$group == 1] and data$weight[data$group == 2]
# F = 1.0089, num df = 29, denom df = 29, p-value = 0.9811
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.4802063 2.1197187
# sample estimates:
# ratio of variances
#           1.008911


#         F test to compare two variances

# data:  data$weight[data$group == 1] and data$weight[data$group == 3]
# F = 1.0102, num df = 29, denom df = 29, p-value = 0.9785
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.4808015 2.1223460
# sample estimates:
# ratio of variances
#           1.010162


#         F test to compare two variances

# data:  data$weight[data$group == 2] and data$weight[data$group == 3]
# F = 1.0012, num df = 29, denom df = 29, p-value = 0.9974
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.4765547 2.1035999
# sample estimates:
# ratio of variances
#           1.001239

oneway_test_result <- oneway.test(formula = weight ~ group, data = data, var.equal = TRUE)  # oneway.testで分散分析(先程の等分散性テストで等分散性が確認できたので var.equal = TRUE にしている)
# 結果を表示
cat("ONEWAY TEST RESULT\n")
print(oneway_test_result)
# ONEWAY TEST RESULT

#         One-way analysis of means

# data:  weight and group
# F = 5.3506, num df = 2, denom df = 87, p-value = 0.006434

# (6)
effect <- aov_result[[1]]["group", "Sum Sq"] / aov_result[[1]]["Residuals", "Sum Sq"]  # (3)で行った検定の効果量を計算
# 効果量を表示
cat("effect: ")
print(effect)
# effect: [1] 0.1230013