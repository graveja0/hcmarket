#' Construct a bipartite graph network from a bipartite adjacency matrix
#'
#' @param A The bipartite adjacency matrix
#' @param types Names for the bipartite nodes c(row_name, column_name)
#'
#' @return A bipartite graph object
#' @importFrom magrittr %>%
#' @export

get_bipartite_graph <- function(A,types = c("geography","hospital")) {
    nodes <- type <- NULL
    # Vectorized volume by geography and firm
    k_z <- apply(A,1,sum); names(k_z)  = rownames(A)
    k_j <- apply(A,2,sum); names(k_j) = colnames(A)

    G_ <-
        igraph::graph.incidence(A, weighted=TRUE)

    G <-
        G_ %>%
        tidygraph::as_tbl_graph() %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(type = ifelse(type==TRUE, types[2],types[1]))

    out <- list(G_ = G_, G = G)
    return(out)
}
