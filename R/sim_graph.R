##' Reduce for a chosen set of target variables
##'
##' @param graph a \code{mixedgraph} object.
##' @param B set of vertices for directed edges to converge on
##' @param w (optionally) an additional parent vertex of the intrinsic closure
##' of \code{B} for when it consists of a single vertex.
##'
##' @details \code{reduce_graph} implements application of Proposition 5.2 from
##' Evans (2021) to
##' obtain a smaller graph with respect to which perfect dependence between
##' two variables is still possible.
##'
##' \code{reduce_graph2} implements Algorithm 1 from Evans (2021) to obtain a
##' minimal subgraph of the output from \code{reduce_graph}.
##'
##' @export
reduce_graph <- function (graph, B, w) {

  C <- intrinsicClosure(graph, B)
  graphC <- graph[C]

  if (length(districts(graphC)) > 1) stop("Intrinsic closure of B must be bidirected connected")

  if (length(ch(graphC, B)) > 0) stop("Need to deal with children")

  topOrd <- topologicalOrder(graphC[-B])
  topOrd <- c(topOrd)

  A <- withAdjMatrix(graphC[edges="directed"])$edges$directed
  A[] <- 0

  B2 <- B
  path_len <- rep(NA, length(graphC$vnames))
  path_len[B] <- 0

  for (i in rev(seq_along(topOrd))) {
    ch_i <- ch(graphC, topOrd[i])
    lkat <- intersect(ch_i, B2)
    if (length(lkat) > 0) {
      ch_kp <- lkat[which.min(path_len[lkat])]
      A[topOrd[i],ch_kp] <- 1
      path_len[topOrd[i]] <- path_len[ch_kp] + 1
      B2 <- c(topOrd[i], B2)
    }
    else stop(paste0("Error in graph at vertex ", i))
  }

  ## get a minimum spanning tree over the bidirected edges
  Bi <- withAdjMatrix(graphC[edges="bidirected"])$edges$bidirected
  Bi <- BFS(Bi, B[1])
  Bi <- Bi + t(Bi)

  if (!missing(w))
    if (w %in% pa(graph, C)) {
      ch_w <- max(intersect(ch(graph, w), C))
      A[w, ch_w] <- 1
      Bi[w,] <- Bi[,w] <- 0
    } else {
      stop("w must be a parent of the intrinsic closure of B")
    }

  edges <- list(directed=adjMatrix(A), bidirected=adjMatrix(Bi))
  class(edges) <- "edgeList"
  graph$edges <- edges

  graph
}

##' @describeIn reduce_graph Make minimal using Algorithm 1
##' @param v,w two vertices w.r.t. which we wish to reduce the graph
##' @export
reduce_graph2 <- function (graph, v, w, pref) {

  if (missing(graph) || missing(v) || missing(w)) stop("must specify a graph and two vertices v,w")

  ## if preference is missing, then infer from graph structure
  if (missing(pref)) {
    if (w %in% anc(graph, v)) pref = "anc"
    else if (v %in% anc(graph, w)) {
      tmp <- w
      w <- v
      v <- tmp
      pref = "anc"
    }
    else if (v %in% dis(graph, w)) {
      pref = "dis"
    }
  }
  else {
    if (pref == "anc") {
      if (v %in% anc(graph, w)) {
        tmp <- w
        w <- v
        v <- tmp
      }
      else if (!(w %in% anc(graph, v))) stop("one of v,w must be the ancestor of the other to use ancestor preference")
    }
    else if (pref == "dis") {
      if (!(v %in% dis(graph, w))) stop("v and w must be in the same district to use district preference")
    }
  }

  # ## take the intrinsic closure of v
  # int_v <- intrinsicClosure(graph, v)
  # if (!missing(parent)) if (!(parent %in% pa(graph, int_v))) stop("parent is not in pa(<v>)")
  # else if (!missing(w)) int_w <- intrinsicClosure(graph, w)
  # if (v %in% pa(graph, int_w)) {
  #
  # }
  # else if (w %in% pa(graph, int_v)) {
  #
  # }
  # else {
  #   int_vw <- intrinsicClosure(graph, c(v,w))
  #
  # }

  Adj <- withAdjMatrix(graph[edges="bidirected"])$edges$bidirected

  ##
  if (pref == "dis") {
    A <- c(v,w)

    a <- anc(graph, v, sort=2)
    b <- anc(graph, w, sort=2)

    for (i in rev(a)) {
      if (match(1, Adj[i,rev(b)], nomatch = 0) > 0) {
        a <- i
        b <- match(1, Adj[i,rev(b)], nomatch = 0)
        break
      }
    }
    pths_v <- treeTop(Adj, v)
    pths_w <- treeTop(Adj, w)

    W <- c(pths_v[[a]], pths_w[[b]])
    if (anyDuplicated(W)) stop("Error here")
  }
  else if (pref == "anc") {
    W <- A <- c(v,w)
    pths_v <- treeTop(Adj, v)
  }
  else stop("Shouldn't get here!")

  ## now enter the while loop
  while(length(A) > 0) {
    A <- setdiff(dec(graph, W), W)
    A <- setdiff(sort.int(unique.default(unlist(pths_v[A]))), W)
    W <- c(W, A)
  }

  ## return the graph over W
  graph[W]
}

##' Get the canonical DAG for an ADMG
##'
##' @param ADMG of class \code{mixedgraph}
##'
##' @export
canonicalDAG <- function (graph) {
  if (!is.ADMG(graph)) stop("Graph should be an ADMG")
  if (is.DAG(graph)) return(graph)

  nbi <- nedge(graph, "bidirected")
  new_v <- seq_len(nbi)
  old_v <- seq_len(length(vnames(graph))) + nbi
  tot_v <- last(old_v)

  A <- matrix(0, nrow=tot_v, ncol=tot_v)
  A[old_v, old_v] <- withAdjMatrix(graph[edges="directed"])$edges$directed
  biA <- withAdjMatrix(graph[edges="bidirected"])$edges$bidirected
  biA <- biA*upper.tri(biA)
  ind <- which(biA > 0, arr.ind = TRUE)

  for (i in seq_len(nrow(ind))) {
    A[i, ind[i,]+nbi] <- 1
  }
  class(A) <- "adjMatrix"

  edges <- list(directed=A)
  class(edges) <- "edgeList"

  vnms <- c(paste0("H",new_v), vnames(graph))

  mixedgraph(tot_v, edges = edges, vnames = vnms)
}
