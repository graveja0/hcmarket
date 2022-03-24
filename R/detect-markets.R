#' Title
#'
#' @param A Bipartite adjacency matrix
#' @param MARGIN The margin (row=1 or column=2) over which to define markets.
#'
#' @return
#' @export
detect_markets <- function(A,MARGIN) {
    if (MARGIN==1) {
        B_ <- A %>% create_unipartite_adjacency(1)
        G_ <-
            igraph::graph_from_adjacency_matrix(B_, weighted=TRUE)
    } else if (MARGIN==2) {
        B_ <- A %>% create_unipartite_adjacency(2)
        G_ <-
            igraph::graph_from_adjacency_matrix(B_, weighted=TRUE)
    }
    market <- igraph::walktrap.community(G_)
    dendro <- as.dendrogram(market)
    dendro_plot <- dendro %>% ggdendro::ggdendrogram(rotate = FALSE)
    max_height <- ggplot2::ggplot_build(dendro_plot)$data[[2]]$y[1]

    markets <-
        0:max_height %>% purrr::map(~({
            tmp <- igraph::cut_at(market,steps = .x)
            names(tmp) <- rownames(B_)
            tmp
        })) %>%
        purrr::set_names(paste0(0:max_height))

    modularity <-
        market$modularity

    market_max <-
        markets[[paste0(which(modularity==max(modularity))-1)]]

    out <- list(markets = markets,
                market_max = market_max,
                dendro = dendro,
                max_height = max_height,
                modularity = modularity)

    return(out)
}
