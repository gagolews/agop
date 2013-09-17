#Anderson, T.W. and Darling, D.A. (1954). 
#"A Test of Goodness-of-Fit". Journal of the American Statistical Association 49: 765–769.

crit <- readRDS("exp_test_critical_values.rds")

adexp_test<-cmpfun(function(x){
  DNAME <- deparse(substitute(x))
  
  x <- x[!is.na(x)]
  n <- length(x)

  n2 <- if(n < nrow(crit)) n else nrow(crit) 
  if (any(is.na(crit[n2,])))
    stop("Sample size too small")

  if (any(x <= 0.0)) {
    W <- Inf
    pv <- 0
    warning('non-positive observations detected')
  }
  else {
    W <- exp_test(x)
    pv <- if (W > crit[1,ncol(crit)]) 1e-16 else
      1-splinefun(crit[1,],crit[n2,],method="monoH.FC")(W)
  }
  
  res<-list(statistic=c(W=W), p.value=pv, 
              method="Anderson-Darling exponentiality test",
              data.name=DNAME)
  attr(res, "class")<-"htest"
  res
})


