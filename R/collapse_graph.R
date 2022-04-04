#' Collapse a graph network by an identifying node-level group variable
#'
#' @param G The graph network
#' @param x The group variable name (node attribute)
#'
#' @return
#' @export

collapse_graph <- function(G,x) {
    x <- rlang::enquo(x)

    G_ <-
        G %>%
        tidygraph::activate(edges) %>%
        add_node_attribute({{x}})

    A_ <-
        G_ %>%
        tidygraph::activate(edges) %>%
        data.frame() %>%
        dplyr::group_by(from_name, to_name) %>%
        dplyr::summarise(weight = sum(weight)) %>%
        tidyr::spread(to_name, weight) %>%
        tibble::column_to_rownames(var = "from_name") %>%
        as.matrix()
    A_[is.na(A_)]    <- 0

    A_ %>%
        get_bipartite_graph()

}
