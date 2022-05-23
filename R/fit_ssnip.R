#' Fit a SSSNIP test based on a clustering of firm-specific markets.
#'
#' @param A The input adjacency matrix
#' @param target The target hospital
#'
#' @return
#' @export

fit_ssnip <- function(A,target) {

    which_micromarkets_target <- names(which(A[,target]>0))
    which_hospitals <- apply(A[which_micromarkets_target,],2,sum)[apply(A[which_micromarkets_target,],2,sum)>0] %>%
        names()
    which_micromarkets <- apply(A[,which_hospitals],1,sum)[apply(A[,which_hospitals],1,sum)>0] %>% names()
    A <- A[which_micromarkets,which_hospitals]

    M <-
        A %>%
        detect_markets(2) %>%
        purrr::pluck("markets") %>%
        dplyr::bind_rows(.id = "level")  %>%
        dplyr::group_by(level) %>%
        tidyr::nest()

   # if (plot_dendro) {
        dg <-  A %>%
            detect_markets(2) %>%
            purrr::pluck("dendro")  %>%
            ggdendro::ggdendrogram(rotate = TRUE)
        #return(list(A = A, dendro = dg))
    #}

    res <-
        M %>%
        dplyr::mutate(n = map_dbl(data,
                           ~({
                               .x %>%
                                   data.frame() %>%
                                   tidyr::gather(hospital,market) %>%
                                   dplyr::group_by(market) %>%
                                   dplyr::mutate(N = n()) %>%
                                   dplyr::filter(hospital==target) %>%
                                   dplyr::pull(N)
                           }))) %>%
        dplyr::filter(n>1) %>%
        dplyr::mutate(ssnip = purrr::map(data,~ssnip_(.x, A=A, target = target))) %>%
        tidyr::unnest(cols = c(data,ssnip)) %>%
        dplyr::ungroup()

    return(list(res = res, A = A, dendro = dg))

}
