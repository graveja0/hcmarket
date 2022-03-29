#' Create an example adjacency matrix
#'
#' @return
#' @export

create_example_adjacency <- function() {
    A <-
    data.frame(A = c(rpois(2,lambda=10),rep(0,10)),
               B = c(rpois(3,lambda=15),rep(0,9)),
               C = c(rep(0,1),rpois(5,lambda=100),rep(0,6)),
               D = c(rep(0,2),rpois(4,lambda=50),rep(0,6)),
               E = c(rep(0,2),rpois(4,lambda=400),rep(0,6)),
               F = c(rep(0,2),rpois(4,lambda=100),rep(0,6)),
               G = c(rep(0,5),rpois(1,lambda=50), rpois(3,lambda=100),rep(0,1),rpois(2,lambda=500)),
               H = c(rep(0,6),rpois(5,lambda=100),0),
               I = c(rep(0,6),rpois(5,lambda=50),0),
               J = c(rep(0,7),rpois(4,lambda=200),0)
    ) %>%
    as.matrix()
    colnames(A) <- paste0("HOSP_",colnames(A))
    # force geo_8 to be a small monopoly from the perspective of the ZIP
    A[8,] = c(rep(0,9),25)
    rownames(A) <- paste0("GEO_",1:12)
    geo_hosp_A <- A
    return(geo_hosp_A)
}
