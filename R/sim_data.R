##' Simulate according to a DAG
##'
##' @param n number of observations to simulate
##' @param graph canonical DAG
##' @param hide should latent variables be removed?
##'
##' @name simulate_DAG
NULL

##' @describeIn simulate_DAG with binary variables
##' @export
rBinDAG <- function(n, graph, hide=FALSE) {

  ## set up matrix for output
  out <- matrix(NA, n, nv(graph))
  colnames(out) <- vnames(graph)

  ## get a topological order
  topOrd <- topologicalOrder(graph)

  ## simulate data for each variable
  for (i in topOrd) {
    pa_i <- pa(graph, i)
    if (length(pa_i) > 0) {
      out[,i] <- rowSums(out[,pa_i,drop=FALSE]) %% 2
    }
    else out[,i] <- rbinom(n, 1, prob=0.5)
  }

  ## if hide == TRUE, then remove variables beginning with 'H'
  if (hide) {
    first_char <- substr(colnames(out), 1, 1)
    out <- out[,-grep("H", first_char)]
  }

  out
}

##' @describeIn simulate_DAG with uniform variables
##' @export
rUnifDAG <- function(n, graph, hide=FALSE) {

  ## set up matrix for output
  out <- matrix(NA, n, nv(graph))
  colnames(out) <- vnames(graph)

  ## get a topological order
  topOrd <- topologicalOrder(graph)

  seen <- integer(0)

  ## simulate data for each variable
  for (i in topOrd) {
    pa_i <- pa(graph, i)

    ## if this is the second instance of a hidden variable,
    ## then we should subtract it
    second <- match(seen, pa_i, nomatch = 0L)
    if (any(second > 0)) {
      sgn <- rep(1, ncol(out))
      sgn[pa_i[second]] <- -1
      out2 <- out*rep(sgn, each=n)
    }
    else out2 <- out

    if (length(pa_i) > 0) {
      out[,i] <- rowSums(out2[,pa_i,drop=FALSE]) %% 1
    }
    else out[,i] <- runif(n)

    seen <- c(seen, pa_i)
  }

  ## if hide == TRUE, then remove variables beginning with 'H'
  if (hide) {
    first_char <- substr(colnames(out), 1, 1)
    out <- out[,-grep("H", first_char)]
  }

  out
}

