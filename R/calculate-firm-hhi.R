#' Calculate firm-level HHI
#'
#' @param A Bipartite adjacency matrix
#' @param markets Named vector mapping each firm to its market
#'
#' @return
#' @export
calculate_firm_hhi <- function(A, markets) {

    # Kessler-McClellan / Zwanziger Approach

    d_z <- apply(A,1,sum); names(d_z) <- rownames(A)
    D_z <- diag(d_z); colnames(D_z) = rownames(D_z) = rownames(A)
    S_z <- solve(D_z) %*% A
    hhi_mm <- 10000*(S_z * S_z) %>% apply(.,1,sum)

    d_j <- apply(A,2,sum); names(d_j) <- colnames(A)
    D_j <- diag(d_j); colnames(D_j) = rownames(D_j) = colnames(A)
    S_j <- A %*% solve(D_j)

    hhi_j <- apply(diag(hhi_mm) %*% S_j,2,sum)

    M_ <- split(markets , f= unlist(markets))

    hhi_km <-
        purrr::map(M_,~({
            data.frame(N_km = sum(d_j[names(.x)]),hhi_km= sum(hhi_j[names(.x)]*d_j[names(.x)]) / sum(d_j[names(.x)]))
        })) %>%
        dplyr::bind_rows(.id ="market")

    hhi <-
        purrr::map(M_,~({
            A_ <- A[,names(.x)]

            singleton = FALSE

            if (is.null(colnames(A_))) {
                singleton = TRUE
                tmp <- as.matrix(A_)
                colnames(tmp) = names(.x)
                A_ <- tmp
            }

            d_j_ <- apply(A_,2,sum); names(d_j_) = names(.x)
            data.frame(N = sum(d_j_),hhi = sum((100*d_j_/sum(d_j_))^2))
        })) %>%
        dplyr::bind_rows(.id= "market")

    out <- list(market_hhi = hhi %>%
                    dplyr::left_join(hhi_km,"market"),
                firm_hhi_km = data.frame(hhi = hhi_j) %>% tibble::rownames_to_column(var = "name"),
                markets = markets,
                d_j = d_j)
    return(out)

}
