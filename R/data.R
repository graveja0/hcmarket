#' Example bipartite adjanccy matrix
#'
#' A matrix summarizing aggregate demand within a bipartite adjacency matrix.
#'
#' @usage data(geo_hosp_A)
#' @format A matrix with 12 rows and 10 columns
#' \describe{
#'   \item{rows}{Geographic micromarkets}
#'   \item{columns}{Hospitals}
#'   ...
#' }

"geo_hosp_A"

#' Example isochrone
#'
#' An sf list object with isochrones (60min) for four x,y locations.
#'
#' @usage data(iso30)
#' @format A list object with 5 elements, each of which is an isochrone sf geometry around a point.
#' \describe{
#'   \item{elements}{60-minute drive-time isochrone}
#'   ...
#' }

"iso30"
