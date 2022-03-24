#' Create a unipartite adjacency matrix
#'
#' @param A Bipartite adjacency matrix
#' @param MARGIN The margin (row=1 or column=2) over which to define the unipartite projection.
#'
#' @return
#' @export
create_unipartite_adjacency <- function(A, MARGIN ) {
    if (MARGIN== 1) {
        B <- A %*% t(A)
        return(B)

    }

    if (MARGIN==2) {
        B <- t(A) %*% A
        return(B)
    }
}
