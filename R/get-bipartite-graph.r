#' Construct a bipartite graph network from a bipartite adjacency matrix
#'
#' @param A The bipartite adjacency matrix
#'
#' @return A bipartite graph object
#' @export
#'
#' @examples
get_bipartite_graph <- function(A) {

    # Vectorized volume by geography and firm
    k_z <- apply(A,1,sum); names(k_z)  = rownames(A)
    k_j <- apply(A,2,sum); names(k_j) = colnames(A)

    G_ <-
        igraph::graph.incidence(A, weighted=TRUE)

    G <-
        G_ %>%
        as_tbl_graph() %>%
        activate(nodes) %>%
        mutate(type = ifelse(type==TRUE, "hospital","geography")) %>%
        mutate(volume = c(k_j,k_z)[name])

    out <- list(G_ = G_, G = G)
    return(out)
}
