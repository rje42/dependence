BFS <- function (A, r=nrow(A)) {
  if (!is.matrix(A) || nrow(A) != ncol(A)) stop("A must be a square matrix")
  if (nrow(A) <= 1) return(matrix(0,nrow(A),ncol(A)))
  if (nrow(A) < r) stop("r must be a natural number <= number of rows of A")

  out <- 0*A

  disc <- rep(FALSE, nrow(A))
  disc[r] <- TRUE
  queue <- r

  while (length(queue) > 0) {
    new_nb <- which(A[queue[1], ] > 0 & !disc)

    out[queue[1], new_nb] = 1
    queue <- c(queue[-1], new_nb)
    disc[new_nb] = TRUE
  }

  out
}

##' Get list of paths from each vertex to a root
##'
treeTop <- function(A, r) {
  if (!is.matrix(A) || nrow(A) != ncol(A)) stop("A must be a square matrix")
  if (nrow(A) <= 1) return(matrix(0,nrow(A),ncol(A)))
  if (nrow(A) < r) stop("r must be a natural number <= number of rows of A")

  ## add in check for cycles

  out <- vector(length=nrow(A), mode="list")

  ## set up root node path
  disc <- rep(FALSE, nrow(A))
  disc[r] <- TRUE
  curr_nb <- r
  out[[r]] = r

  while (!all(disc)) {
    new_nb <- integer(0)
    for (i in curr_nb) {
      tmp <- c(setdiff(which(A[,i] > 0), which(disc)))
      for (j in tmp) out[[j]] = c(out[[i]], j)
      new_nb <- c(new_nb, tmp)
    }
    disc[new_nb] = TRUE
    if (length(new_nb) == 0) break

    curr_nb <- new_nb
  }

  out
}

shortestPath <- function (A, a, b) {

}
