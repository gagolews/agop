# create sysdata.rda
# (C) 2013 MG

exp_test_ad_cdf <- readRDS('devel/test-ad/exp_test_ad_cdf.rds')
save(exp_test_ad_cdf, file='R/sysdata.rda')
tools::resaveRdaFiles('R/sysdata.rda')
