#' Calculate outflow HHI measures from a bipartite adjacency matrix
#'
#' @param A The bipartite adjacency matrix
#' @param markets Named vector mapping each micromarket to its market
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
calculate_outflow_hhi <- function(A, markets) {
    d_z <- apply(A,1,sum); names(d_z) <- rownames(A)
    D_z <- diag(d_z); colnames(D_z) = rownames(D_z) = rownames(A)
    S_z <- solve(D_z) %*% A
    hhi_mm <- 10000*(S_z * S_z) %>% apply(.,1,sum)

    M_ <- split(markets , f= unlist(markets))

    hhi_m <-
        purrr::map(M_,~({
        data.frame(hhi= sum(hhi_mm[names(.x)]*d_z[names(.x)]) / sum(d_z[names(.x)]))
        })) %>%
        dplyr::bind_rows(.id ="market")

    out <- list(market_hhi = hhi_m,
                micromarket_hhi = data.frame(hhi = hhi_mm) %>% tibble::rownames_to_column(var = "name"),
                markets = markets,
                d_z = d_z)
    return(out)
}
