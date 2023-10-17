# (1)
# H_0: 体重の増減はしない(R_1 = R_2)
# H_1: 体重は増減する(R_1 != R_2)

# (2)
data <- read.table("weight.txt", header = TRUE)  # ファイルからデータを読み込む
previous_status <- c(mean(data$A), sd(data$A))  # 投薬前のデータの平均値と標準偏差を計算
current_status <- c(mean(data$B), sd(data$B))  # 投薬後のデータの平均値と標準偏差を計算

png("previous.png", width = 512, height = 512)
previous_hist <- hist(data$A, xlab = "weight", ylab = "frequency", main = "previous histogram")
par(new = TRUE)
previous_label_height <- max(previous_hist$counts) / 2  # 矢印等を画面中央に表示するために最大出現頻度/2を計算
points(previous_status[1], previous_label_height, pch = 16, col = "red", cex = 1.5)
arrows(previous_status[1], previous_label_height, previous_status[1] - previous_status[2], previous_label_height, col = "blue", lwd = 1.5)
arrows(previous_status[1], previous_label_height, previous_status[1] + previous_status[2], previous_label_height, col = "blue", lwd = 1.5)
dev.off()

png("current.png", width = 512, height = 512)
current_hist <- hist(data$B, xlab = "weight", ylab = "frequency", main = "current histogram")
par(new = TRUE)
current_label_height <- max(current_hist$counts) / 2  # 矢印等を画面中央に表示するために最大出現頻度/2を計算
points(current_status[1], current_label_height, pch = 16, col = "red", cex = 1.5)
arrows(current_status[1], current_label_height, current_status[1] - current_status[2], current_label_height, col = "blue", lwd = 1.5)
arrows(current_status[1], current_label_height, current_status[1] + current_status[2], current_label_height, col = "blue", lwd = 1.5)
dev.off()

# (3)
t_result <- t.test(data$A, data$B, paired = TRUE, var.equal = TRUE)

# (4)
print(t_result)

#         Paired t-test

# data:  data$A and data$B
# t = 7.891, df = 49, p-value = 2.805e-10
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  0.7439932 1.2524068
# sample estimates:
# mean difference
#          0.9982

# (5)
no_relation_t_result <- t.test(data$A, data$B)
print(no_relation_t_result)

#        Welch Two Sample t-test

# data:  data$A and data$B
# t = 0.31214, df = 97.997, p-value = 0.7556
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -5.347886  7.344286
# sample estimates:
# mean of x mean of y
#   60.9742   59.9760

# (6)
a_ks_test_result <- ks.test(data$A, "pnorm", mean = mean(data$A), sd = sd(data$A))
b_ks_test_result <- ks.test(data$B, "pnorm", mean = mean(data$A), sd = sd(data$A))
print(a_ks_test_result)
print(b_ks_test_result)

#         Exact one-sample Kolmogorov-Smirnov test

# data:  data$A
# D = 0.10999, p-value = 0.544
# alternative hypothesis: two-sided

#         Exact one-sample Kolmogorov-Smirnov test

# data:  data$B
# D = 0.13286, p-value = 0.3124
# alternative hypothesis: two-sided

var_test_result <- var.test(data$A, data$B)
print(var_test_result)

#       F test to compare two variances

# data:  data$A and data$B
# F = 1.0104, num df = 49, denom df = 49, p-value = 0.9713
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.5733619 1.7804654
# sample estimates:
# ratio of variances
#           1.010372
