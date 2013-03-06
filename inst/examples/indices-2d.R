require('agop')

x <- seq(0, 4, by=0.05)
z <- matrix(NA, nrow=length(x), ncol=length(x))
for (i in 1:nrow(z))
   for (j in 1:ncol(z)) {
#       z[i,j] <- index.h(c(x[i], x[j]))
#       z[i,j] <- index.g(c(x[i], x[j]))
      z[i,j] <- index.rp(c(x[i], x[j]), 1) # index w
#       z[i,j] <- max(pmin(sort(c(x[i], x[j]), dec=TRUE), 1:2))
   }

filled.contour(x, x, z, color.palette=gray.colors)

