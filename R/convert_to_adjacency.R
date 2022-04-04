#' Convert a graph network to a weighted adjacency matrix
#'
#' @param G The graph network
#' @param id The ID variable name
#'
#' @return
#' @export

convert_to_adjacency <- function(G,id=name) {
    id <- rlang::enquo(id)
    A_ <-
        G %>%
        tidygraph::activate(edges) %>%
        add_node_attribute({{id}}) %>%
        tidygraph::activate(edges) %>%
        data.frame() %>%
        dplyr::group_by(from_name, to_name) %>%
        dplyr::summarise(weight = sum(weight)) %>%
        tidyr::spread(to_name, weight) %>%
        tibble::column_to_rownames(var = "from_name") %>%
        as.matrix()
    A_[is.na(A_)]    <- 0
    return(A_)
}
