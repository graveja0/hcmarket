
#' Calculate Willingness-to-Pay
#'
#' @param A Bipartite adjacency matrix
#'
#' @return
#' @export

calculate_wtp <- function(A) {
    d_z <- apply(A,1,sum); names(d_z) <- rownames(A)
    D_z <- diag(d_z); colnames(D_z) = rownames(D_z) = rownames(A)
    S_z <- solve(D_z) %*% A
    Spr_z <- log(1-S_z)
    Spr_z[is.infinite(Spr_z)] <- log(1-.999999999)

    -apply(D_z %*% Spr_z,2,sum)
}
