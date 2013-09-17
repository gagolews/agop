#points in which we calculate ecdf
#we know that if T>0.75 then 1-ecdf(T) is approximately <0.2-0.25; 
#we know that for T>=3 then 1-ecdf(T) is approximately <0.0009;
#the user wants the biggest accuracy for significance level of 0.001-0.2
x0 <- c(0, 0.125, 0.25, 0.375, 0.5, 0.625, seq(0.75, 3, by=0.01), 3.5, 4, 10)

#the smallest sample size
n_min <- 3 

#the largest sample size (for n > n_max we use approximation obtained for n_max)
n_max <- 75

n_max2 <- 100

#matrix of ecdf values for different n
crit <- matrix(ncol=length(x0), nrow=n_max)

#the first row of the crit matrix contains x0
crit[1,] <- x0

#number of MC simulations
M <- 2500000

for (n in n_min:n_max) {  
  n2 <- if (n < n_max) n else n_max2
  
  Tn <- replicate(M, {
    exp_test(rexp(n2, 1))
  }) 
  
  Tn <- sort(Tn)
  
  Fn <- ecdf(Tn)
  
  crit[n,] <- Fn(x0)
  cat(sprintf("\r%d", n))
}

saveRDS(crit,"exp_test_critical_values.rds")
