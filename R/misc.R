#' Read Turner Johnson's 2021 deployment data
#' 
#' @export
#' @param filename character, the file to read
#' @param form character, either 'tibble' or 'sf'
#' @param use character, determines if sf output uses lonlat or rectangular coords
#' @return tibble, possible as sf POINT
read_deployment <- function(
    filename = system.file(file.path("extdata", "2021-10-12_deployment.csv.gz"), 
                           package = "cascophys"),
    form = c("tibble", "sf")[2],
    use = c("xy", "lonlat")[1]){
  
  x <- readr::read_csv(filename,
                       col_types = readr::cols(
                         time = readr::col_datetime(format = ""),
                         lat = readr::col_double(),
                         lon = readr::col_double(),
                         status = readr::col_character(),
                         Temp_A = readr::col_double(),
                         Temp_B = readr::col_double(),
                         Temp_C = readr::col_double(),
                         Conductivity = readr::col_double(),
                         x = readr::col_double(),
                         y = readr::col_double(),
                         z = readr::col_double()
                       )) 
  
  if (tolower(form[1]) == 'sf') {
    if (tolower(use[1]) == "lonlat"){
      x <- sf::st_as_sf(x, coords = c("lon", "lat", "z"), crs = 4326)
    } else {
      x <- sf::st_as_sf(x, coords = c("x", "y", "z"), 
        crs = "+proj=tmerc +datum=NAD83 +lon_0=-70d10 lat_0=42d50 k=.9999666666666667 x_0=900000 y_0=0")
    }
  }
  
  x  
}