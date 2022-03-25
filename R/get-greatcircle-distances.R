#' Get great circle distances between coordinates in a data file
#'
#' @param df The input data file
#' @param id The ID variable
#' @param coords A nested data frame with x,y coordinates of the point
#'
#' @return
#' @export

get_greatcircle_distances <- function(df,id,coords) {
    id = rlang::enquo(id)
    coords = rlang::enquo(coords)

    df_dist <-
        matrix({
            df %>%
                dplyr::select(id,coords={{coords}}) %>%
                tidyr::unnest(cols =c(coords)) %>%
                sf::st_as_sf() %>%
                sf::st_distance() %>%
                as.matrix()},
            nrow = length(df$id),
            ncol = length(df$id),
            dimnames = list(df$id,df$id)) %>%
        data.frame() %>%
        tibble::rownames_to_column(var = "from") %>%
        tidyr::gather(to,km,-from) %>%
        dplyr::mutate(km = km/1000) %>%  # original matrix is in meters
        dplyr::mutate(miles = km *0.621371 )

    df   %>%
        dplyr::mutate(from = {{id}}) %>%
        tidyr::nest(foo=c(from)) %>%
        dplyr::mutate(distances = purrr::map(foo,~(.x %>% dplyr::inner_join(df_dist,"from")))) %>%
        dplyr::select(-foo)

}
