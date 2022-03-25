#' Simulate a merger of nodes in the bipartite adjacency matrix
#'
#' @param A The bipartite adjacency matrix
#' @param .x The absorbing firm
#' @param .y The absorbee.
#'
#' @return
#' @export
#'
simulate_merger <- function(A,.x,.y) {
    merged <- c(.x,.y)
    A_same <- A[,-which(colnames(A) %in% merged)]
    A_new <- apply(A[,which(colnames(A) %in% merged)],1,sum)
    A_merged <- cbind(A_same,A_new)
    colnames(A_merged) <- c(colnames(A_same),paste0(merged,collapse ="_"))
    A_merged
}
