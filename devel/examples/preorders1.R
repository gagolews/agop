ex1 <- list(
   U = 10:0,           # some upper bound
   A = c(5,5,5,5),     # moderate productivity & quality
   B = c(4,3,2,1,1,0), # high productivity
   C = c(8,7),         # high quality
   L = c(1,1)          # some lower bound
)


for (i in seq_along(ex1))
   plot_producer(ex1[[i]], add=(i>1), col=i)
legend("topright", legend=names(ex1), col=1:length(ex1), lty=1)

ord <- rel_graph(ex1, pord_weakdom)
print(ord)
is_reflexive(ord)  # is reflexive
is_transitive(ord) # is transitive
is_total(ord)      # not a total preorder...



hasse <- graph.adjacency(de_transitive(ord))
set.seed(1234567) # igraph's draving facilities are far from perfect
plot(hasse, layout=layout.fruchterman.reingold(hasse, dim=2))


ord_total <- closure_transitive(closure_total_fair(ord)) # a total preorder
print(ord_total)
hasse <- graph.adjacency(de_transitive(ord_total))
set.seed(1234)
plot(hasse, layout=layout.fruchterman.reingold(hasse, dim=2))
print(get_equivalence_classes(ord_total))




ordg <- graph.adjacency(ord)
graph.bfs(ordg, 2, unreachable=FALSE, order=TRUE, dist=TRUE)




ex2 <- list(
   C=c(4,4,2,1,0,0,0),
   A=c(1,0),
   D=c(9,8,6,2,1,1,1,1,1,0),
   B=c(5,4,2)
)

ord2 <- rel_graph(ex2, pord_weakdom)
ord_total2 <- closure_transitive(closure_total_fair(ord2)) # a total preorder
print(get_equivalence_classes(ord_total2))
