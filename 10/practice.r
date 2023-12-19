# (1)
load("./week10-data.dat")
cor_result <- cor(FiveSubjPoints)
print(cor_result)
#           Japanese  SocStudy  Mathmtcs   Science   English
# Japanese 1.0000000 0.5768752 0.2369803 0.1031250 0.3990430
# SocStudy 0.5768752 1.0000000 0.3079368 0.2355391 0.4957613
# Mathmtcs 0.2369803 0.3079368 1.0000000 0.5622259 0.5771277
# Science  0.1031250 0.2355391 0.5622259 1.0000000 0.5643663
# English  0.3990430 0.4957613 0.5771277 0.5643663 1.0000000

# 1. English - Mathmatcs
# 2. SocStudy - Japanese
# 最も高い相関はどちらでもない英語と理系科目の数学のため、理系因子は存在していると言える。2番目に高い相関はどちらも文系科目の社会と国語のため、文系因子も存在していると言える。

# (2)
order <- c(1, 2, 3, 4, 5)
eigen_result <- eigen(cor_result)
print(eigen_result)
# eigen() decomposition
# $values
# [1] 2.6510026 1.1568524 0.4509878 0.3928713 0.3482859

# $vectors
#            [,1]        [,2]       [,3]        [,4]       [,5]
# [1,] -0.3724654  0.61327811  0.3613246  0.57495868 -0.1550051
# [2,] -0.4352708  0.47306551 -0.4361432 -0.57913536 -0.2472442
# [3,] -0.4631475 -0.35438950  0.6919028 -0.39771948 -0.1516318
# [4,] -0.4233420 -0.51441472 -0.4140284  0.40890187 -0.4664105
# [5,] -0.5273374 -0.09942113 -0.1705130  0.09296892  0.8211655
png("02.png", width = 512, height = 512)
plot(order, eigen_result$values, xlab = "eigen order", ylab = "value", main = "eigens of the cor matrix", type = "b")
dev.off()

# 算出された固有値のうち第3固有値以降は減衰が緩やかになっているので、因子数は2とすることが妥当であると言える。

# (3)
no_rotate_factanal_result <- factanal(FiveSubjPoints, factors = 2, rotation = "none")
print(no_rotate_factanal_result, cutoff = 0)
# Call:
# factanal(x = FiveSubjPoints, factors = 2, rotation = "none")

# Uniquenesses:
# Japanese SocStudy Mathmtcs  Science  English
#    0.405    0.415    0.469    0.374    0.326

# Loadings:
#          Factor1 Factor2
# Japanese  0.546   0.544
# SocStudy  0.642   0.415
# Mathmtcs  0.677  -0.269
# Science   0.649  -0.453
# English   0.817  -0.080

#                Factor1 Factor2
# SS loadings      2.259   0.752
# Proportion Var   0.452   0.150
# Cumulative Var   0.452   0.602

# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 0.57 on 1 degree of freedom.
# The p-value is 0.449

# 第一因子では英語が最も因子負荷が高くその他の教科間ではあまり差がないため、文系因子とも理系因子とも言えない。また、第二因子では国語と社会の因子負荷が高いため、文系因子であると言える。

# (4)
rotate_factanal_result <- factanal(FiveSubjPoints, factors = 2, rotation = "varimax")
print(rotate_factanal_result, cutoff = 0)
# Call:
# factanal(x = FiveSubjPoints, factors = 2, rotation = "varimax")

# Uniquenesses:
# Japanese SocStudy Mathmtcs  Science  English
#    0.405    0.415    0.469    0.374    0.326

# Loadings:
#          Factor1 Factor2
# Japanese 0.091   0.766
# SocStudy 0.247   0.724
# Mathmtcs 0.698   0.209
# Science  0.790   0.047
# English  0.691   0.444

#                Factor1 Factor2
# SS loadings      1.658   1.353
# Proportion Var   0.332   0.271
# Cumulative Var   0.332   0.602

# Test of the hypothesis that 2 factors are sufficient.
# The chi square statistic is 0.57 on 1 degree of freedom.
# The p-value is 0.449

# 第一因子では数学、理科、英語の因子負荷が高いため、理系因子と言える。また、第二因子では国語と社会の因子負荷が高いため、文系因子であると言える。