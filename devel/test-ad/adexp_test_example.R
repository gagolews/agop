n <- 14


replicate(10000,{
  c(rexp_1=adexp_test(rexp(n))$p.value,
    rexp_0_01=adexp_test(rexp(n,0.01))$p.value,
    rexp_100=adexp_test(rexp(n,100))$p.value,
    rexp_1c=adexp_test(ceiling(rexp(n)))$p.value,
    rchisq_8=adexp_test(rchisq(n,8))$p.value,
#     minrexp=adexp_test(-rexp(n))$p.value,
    rgamma_10_3=adexp_test(rgamma(n,10,3))$p.value,
    rpareto2_2_4=adexp_test(rpareto2(n,2,4))$p.value,
    absrnorm=adexp_test(abs(rnorm(n)))$p.value,
#     rnorm=adexp_test(rnorm(n))$p.value,
    runif=adexp_test(runif(n))$p.value,
    rpois=adexp_test(1+rpois(n,1))$p.value)
}) -> res

mean(res>0.01)

A <- c(0.5,0.25,0.15,0.1,0.075,0.05,0.025,0.01,0.001)
power <- sapply(A,
       function(a) apply(res,1,function(p) mean(p<a)))
dimnames(power)[[2]] <- format(A)

print(power)