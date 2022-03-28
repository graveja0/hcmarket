#' Identify which points within a data frame are within a given isochrone
#'
#' @param df The input data frame
#' @param id The ID variable
#' @param coords Nested sf coordinate object for the x,y of the point
#' @param iso The isochrone column
#'
#' @return
#' @export

get_isochrone_members <- function(df,id,coords,iso) {
    id = rlang::enquo(id)
    coords = rlang::enquo(coords)
    iso = rlang::enquo(iso)

    sf_coord <-
        df %>%
        dplyr::select(id= {{id}},coords = {{coords}}) %>%
        tidyr::unnest(cols = c(coords)) %>%
        sf::st_as_sf(crs = 4326)

    df_ <-
        df %>%
        dplyr::select(id = {{id}},
               iso = {{iso}}) %>%
        dplyr::mutate(within = purrr::map(iso,~({
            data.frame(name = sf_coord$id, within = sf::st_within(sf_coord,.x,sparse = TRUE) %>% as.numeric()) %>%
                dplyr::filter(within==1)
        }))) %>%
    dplyr::select({{id}},within) %>%
        purrr::set_names(c(rlang::quo_name(id),glue::glue("within_{rlang::quo_name(iso)}")))

    df %>%
        dplyr::left_join(df_,rlang::quo_name(id)) %>%
        tibble::as_tibble()
}
