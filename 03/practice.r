data <- read.table("week3-example.txt", header = TRUE)
result <- t.test(data$previous, data$current, var.equal = TRUE)
