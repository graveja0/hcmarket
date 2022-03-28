#' Obtain a drive time isochrone from mapbox.com
#'
#' @param df The data frame with id, latitude and longitude
#' @param id The ID variable name for the created isochrone
#' @param long The longitude variable name
#' @param lat The latitude variable name
#' @param contours_minutes Drive time minutes to define boundary of isochrone (must be 60 or less)
#' @param base_url The mapbox base url.
#' @param mapbox_token  Your mapbox API token
#'
#' @return
#' @export

get_mapbox_isochrone <- function(df, id, long, lat, contours_minutes, base_url = "https://api.mapbox.com/", mapbox_token = "") {
    request_url <- NULL
    id <- rlang::enquo(id)
    y <- rlang::enquo(lat)
    x <- rlang::enquo(long)

    ls_ <- df %>%
        dplyr::select(id = {{id}},
               x = {{x}},
               y = {{y}})  %>%
        dplyr::rowwise() %>%
        dplyr::mutate(request_url = httr::modify_url(base_url,
            path = glue::glue("isochrone/v1/mapbox/driving/{x},{y}?contours_minutes={contours_minutes}&polygons=true&access_token={mapbox_token}"))) %>%
        dplyr::select(id,request_url) %>%
        tibble::deframe() %>%
        as.list()

    res <-
        ls_ %>%
        purrr::map(~(
            tryCatch({
                temp <- suppressWarnings(httr::GET(.x, verbose = T))
                Sys.sleep(1)
            },
            error = function(cond) {
                message(paste("URL does not seem to exist:", url))
                message("Here's the original error message:")
                message(cond)
                # Choose a return value in case of error
                return(NA)
            },
            finally = {
                out <- suppressMessages(jsonlite::fromJSON(httr::content(temp, "text"),
                                                           simplifyVector = FALSE, simplifyDataFrame = TRUE, flatten = TRUE
                ))

                name_iso <- sort(unlist(stringr::str_split(contours_minutes, pattern = ", ", n = 4)), decreasing = T)
                # print(name_iso)
                coords <- out$features$geometry.coordinates

                # If isochrone is found, name the the subsets by their corresponding driving-times
                # for example, a 10 minute isochrones is named "10"
                if (!is.null(coords)) {

                    names(coords) <- name_iso
                    out <-
                        coords %>%
                        purrr::pluck(1) %>%
                        purrr::pluck(1) %>%
                        purrr::map(~(data.frame(.x) %>%
                                         dplyr::mutate(var = c("x","y")) %>%
                                         tidyr::spread(var,.x))) %>%
                        dplyr::bind_rows() %>%
                        unique() %>%
                        sf::st_as_sf(coords = c("x","y"), crs = 4326) %>%
                        dplyr::summarize(geometry = sf::st_combine(geometry)) %>%
                        sf::st_cast("POLYGON")

                    cant_simplify <- try(out %>% sf::st_simplify(dTolerance = 100), silent = TRUE)
                    if (grepl("Error|error",cant_simplify)) out <- out else out <- out %>% sf::st_simplify(dTolerance = 100)
                }
                return(out)

            }
        )))
}
