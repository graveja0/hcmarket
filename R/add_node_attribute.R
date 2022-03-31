#' Add note identifiers to edge list
#'
#' @param G Graph object
#' @param id The new ID variable
#'
#' @return
#' @export
add_node_attribute <- function(G,id) {
    id=rlang::enquo(id)

    G %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(tmp = {{id}})  %>%
        tidygraph::activate(edges) %>%
        tidygraph::mutate(from_name = (.N()$tmp[from]),
               to_name = (.N()$tmp[to])) %>%
        tidygraph::activate(nodes) %>%
        dplyr::select(tmp)
}
