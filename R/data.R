#' Map of municipality borders in the Friuli Venezia Giulia region, Italy
#'
#' An sf object containing the ISTAT municipality codes, geometry
#' and the municipality names in the Friuli Venezia Giulia region in northeastern
#' Italy, based on official ISTAT shapefiles.
#'
#' @format An sf object with 215 features and 6 fields:
#' \describe{
#'   \item{Cod_reg}{region code, always =6 (Friuli Venezia Giulia)}
#'   \item{Cod_pro}{province code (93=Pordenone, 30=Udine, 31=Gorizia, 32=Trieste)}
#'   \item{Pro_com}{municipality code, consists of province code + progressive
#'   code of the municipality within the province}
#'   \item{Shape_leng}{length of municipality perimeter}
#'   \item{Shape_area}{municipality area}
#'   \item{geometry}{a MULTIPOLYGON}
#' }
#' @source \url{https://www.istat.it/it/archivio/104317}
"fvgmap"
