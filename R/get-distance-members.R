#' Identify points within a great circle distance
#'
#' @param df The input data frame
#' @param id The identifying variable
#' @param coords Nested coordinate object with x,y coordinates for the point
#' @param dist The maximum distance allowed
#' @param unit The unit (miles or km)
#'
#' @return
#' @export

get_distance_members <- function(df, id, coords, dist, unit = c("miles","km")) {
    tmp <- distances <- NULL
    id = rlang::enquo(id)
    coords = rlang::enquo(coords)

    df_ <-
        df %>%
        dplyr::select(id = {{id}}, tmp = {{coords}}) %>%
        get_greatcircle_distances(id = id, coords = tmp)

    if (unit=="miles") {
        df_ <-
            df_ %>%
            dplyr::mutate(within = purrr::map(distances,~(.x %>% dplyr::filter(miles<dist) %>% dplyr::select(name=to,miles)))) %>%
            dplyr::select(id,within) %>%
            purrr::set_names(c(rlang::quo_name(id),glue::glue("within_{dist}{unit}")))
    } else if (unit=="km") {
        df_ <-
            df_ %>%
            dplyr::mutate(within = purrr::map(distances,~(.x %>% dplyr::filter(km<dist) %>% dplyr::select(name=to,km)))) %>%
            dplyr::select(id,within) %>%
            purrr::set_names(c(rlang::quo_name(id),glue::glue("within_{dist}{unit}")))
    }
    df %>%
        dplyr::left_join(df_,rlang::quo_name(id))
}



