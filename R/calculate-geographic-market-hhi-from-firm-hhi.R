#' Construct a geographic HHI measure from firm-level HHI measures
#'
#' @param A Bipartite adjacency matrix
#' @param markets Named vector of geographic markets
#' @importFrom magrittr %>%
#' @return
#' @export
calculate_geographic_market_hhi_from_firm_hhi <- function(A,markets) {
    firm_market <- rep(1,ncol(A))
    names(firm_market) <- colnames(A)
    colnames(A)
    hhi_j <- calculate_firm_hhi(A,markets = firm_market) %>% purrr::pluck("firm_hhi_km") %>% tibble::deframe()

    d_z <- apply(A,1,sum); names(d_z) <- rownames(A)
    D_z <- diag(d_z); colnames(D_z) = rownames(D_z) = rownames(A)
    S_z <- solve(D_z) %*% A

    hhi_mm<- S_z %*% diag(hhi_j) %>% apply(.,1,sum)

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
