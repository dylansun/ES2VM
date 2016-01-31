a = matrix(0.1*(1:12), nrow = 3, ncol = 4 )
a[1,1] = 1.3
which(a == max(a), arr.ind = TRUE)
