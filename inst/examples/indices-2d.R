require('agop')
require('ggplot2')


t <- seq(0, 4, by=0.1)
x <- rep(t, times=length(t))
y <- rep(t, each=length(t))
z <- apply(cbind(x, y), 1, function(w) {
   index_h(w)
#    index.rp(w,1)
#    index.g(w)
#    max(pmin(sort(w), 1:2))
})

data <- data.frame(x, y, z)


# graphics.off()
ggplot(data, aes(x, y, z = z)) + geom_tile(aes(fill = z)) + stat_contour()
