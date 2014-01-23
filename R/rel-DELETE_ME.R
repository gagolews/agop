invisible(NULL) # TO BE DELETED


# #' @title
# #' Get Incomparable Pairs in an Adjacency Matrix
# #'
# #' @description
# #' A pair (x,y) is incomparable iff
# #' not xRy and not xRy
# #'
# #' @details
# #' See also \code{\link{get_independent_sets}} of how to generate
# #' all maximal independent sets.
# #'
# #' @param B object of class \code{igraph} or a square
# #' 0-1 matrix of class \code{Matrix} or \code{matrix}
# #' @return integer matrix with two columns (indices of incomparable elements,
# #' not that these are pairs, and not sets: you'll get (i,j) and (j,i))
# #' @export
# #' @family binary_relations
# get_incomparable_pairs <- function(B)
# {
#    if (is(B, 'igraph')) B <- get.adjacency(B)
#    stopifnot(is.matrix(B) || is(B, 'Matrix'), nrow(B) == ncol(B), nrow(B) > 0)
#
#    B2 <- B + Matrix::t(B)
#    # 0s lie symmetricly over diagonal and indicate incomparable pairs
#    Matrix::which(B2 == 0, arr.ind=TRUE)
# }
#
#
# #' @title
# #' Get All Maximal Independent Sets
# #'
# #' @description
# #' The function generates vectors of indices \eqn{S_j=\{i_1,...,i_{k_j}\}}
# #' such that all pairs from \eqn{S_j} are incomparable
# #' (A pair (i,i') is incomparable iff
# #' not \eqn{i R i'} and not \eqn{i' R i},
# #' see also \code{\link{get_incomparable_pairs}}.
# #'
# #'
# #' @details
# #' Note that we assume that \eqn{B} is transitive.
# #' Loops are not taken into account at all.
# #'
# #' @param B object of class \code{igraph} or a square
# #' 0-1 matrix of class \code{Matrix} or \code{matrix}
# #' @return list of integer vectors; each list element defines
# #' an independent set of vertices numbers
# #'
# #' @export
# #' @family binary_relations
# get_independent_sets <- function(B)
# {
#    if (!is(B, 'igraph')) B <- graph.adjacency(B)
#
#    # create graph with all not-directly-connected nodes
#    C <- (shortest.paths(B, mode="all") > 1) # symmetric
#    diag(C) <- 0
#
#    # find all maximal cliques (== independent sets)
#    out <- maximal.cliques(graph.adjacency(C, mode="undirected"))
#    out[sapply(out, length) > 1] # remove singletons
# }
#
#
#
# #' @title
# #' Get All Equivalence Classes of a Total Binary Relation
# #'
# #'
# #' @description
# #' Note that we assume that \eqn{B} is total, reflexive and transitive.
# #'
# #' @param B object of class \code{igraph} or a square
# #' 0-1 matrix of class \code{Matrix} or \code{matrix}
# #' @return list of integer vectors; each list element defines
# #' an equivalence class by listing vertices' numbers;
# #' each vector is ordered by the outdegrees of their nodes (they are
# #' the same in each class)
# #'
# #' @export
# #' @family binary_relations
# get_equivalence_classes <- function(B)
# {
#    if (!is(B, 'igraph')) { BAD <- as.matrix(B); B <- graph.adjacency(B) }
#    else { BAD <- as.matrix(get.adjacency(B)) }
#    stopifnot(is_total(BAD), is_transitive(BAD))
#
#    # now B - igraph
#    # now BAD - matrix
#
#    n <- nrow(BAD)
#
#    # create graph with all `symmetric` pairs
#    C <- BAD & t(BAD) # logical AND
#    out <- maximal.cliques(graph.adjacency(C, mode="undirected"))
#
#    # now sort according to the out-degree (decreasingly)
#    # every vertex in an equivalence class has the same degree
#    # if x<y, then outdegree(x)>outdegree(y)
#    extractFirst <- sapply(out, function(e) e[1])
#    ord <- order(rowSums(BAD)[extractFirst], decreasing=TRUE)
#    out[ord]
# }
#
#
