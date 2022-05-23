
#' Calculate Willingness-to-Pay
#'
#' @param A Bipartite adjacency matrix
#'
#' @return
#' @export

calculate_wtp <- function(A) {
    A_ <- A[apply(A,1,sum)>0,]
    if (is.null(dim(A_))) {
        A_ <- as.matrix(A_)
        colnames(A_) <- colnames(A)
    }
    d_z <- apply(A_,1,sum); names(d_z) <- rownames(A_)
    D_z <- diag(d_z); colnames(D_z) = rownames(D_z) = rownames(A_)
    S_z <- solve(D_z) %*% A_
    Spr_z <- log(1-S_z)
    Spr_z[is.infinite(Spr_z)] <- log(1-.999999999)

    -apply(D_z %*% Spr_z,2,sum)
}
