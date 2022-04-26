#' Project [lon,lat] values to rectangular [x,y] coordinates
#' 
#' Converted from Ernie True's ll2xy.m matlab code
#' 
#' @export
#' @param x sf POINT object or a vector of longitudes in decimal degrees
#' @param y numeric vector of latitudes, ignored if x is of type POINT
#' @param reflon numeric, the reference longitude in decimal degrees
#' @param reflat numeric, the reference latitude in decimal degrees
#' @return a tibble with x and y
ll2xy <- function(x, y = NULL,
                  reflon = -71.03, 
                  reflat = 42.35){
  # %ll2xy::convert latitude and longitude to distance in meters
  # %
  # % [X,Y]=LL2XYGOM(LAT,LON,REFLAT,REFLON)
  # %
  # % Returns X and Y vectors (as distance from arbitrary 
  #                            % origin REFLAT and REFLON) for vectors of latitudes 
  # % and longitudes, LAT and LON.
  # % 
  # % REFLAT and REFLON default to Boston
  # %
  # % LAT and LON may be specified as LON+i*LAT
  # %
  # % Specifying only a single output yields X+i*Y
  # 
  # % CVL, 7-10-97
  # % Hacked from mercgom2 from C. Naimie.
  
  if (inherits(x, "sf")){
    ll <- sf::st_coordinates(x)
    lon <- ll[,1]
    lat <- ll[,2]
  } else {
    if (is.null(y)) stop("if x is not POINT, then y must have latitudes")
    lon <- x
    lat <- y
  }
  
  r = 6.3675E+6
  
  reflon <- reflon * pi / 180
  reflat <- reflat * pi / 180
  
  xo <- r * cos(reflat) * reflon
  yo <- r * cos(reflat) * log( (1.0 + sin(reflat))/cos(reflat) )
  
  rlong <- lon * pi/180;
  rlat <- lat * pi/180;
  x = r * cos(reflat) * rlong-xo;
  y = r * cos(reflat) * log((1.0+sin(rlat)) / cos(rlat)) - yo;
  
  dplyr::tibble(x = x, y = y)
}

#' Transform [x,y] values to lon,lat coordinates
#' 
#' Converted from Ernie True's xy2ll.m matlab code
#' 
#' @export
#' @param x sf POINT object or a vector of x coordinates in meters relative to the reflon
#' @param y numeric vector of y coordinates, ignored if x is of type POINT
#' @param reflon numeric, the reference longitude in decimal degrees
#' @param reflat numeric, the reference latitude in decimal degrees
#' @param tol numeric, the tolorence for defining convergence
#' @param n numeric, the maximum number of iterations permitted when seeking convergence
#' @return a tibble with lon and lat, some possibly with NaN values
xy2ll <- function(x, y = NULL,
                  reflon = -71.03, 
                  reflat = 42.35,
                  tol = 0.0001,
                  n = 1000){
  # %xy2ll::convert distance based coordinate system to latitude and longitude
  # % [LAT,LON]=XY2LLGOM(X,Y,REFLAT,REFLON)
  # %
  # % Returns vectors of latitudes and longitudes, LAT and LON, 
  # % for X and Y vectors (as distance in meters from arbitrary 
  #                        % origin REFLAT and REFLON).  Uses a Newton-Raphson Method
  # % to calculate latitude.  In situations where one or more
  # % values do not converge, that location is assigned the value
  # % NaN and a warning is issued.
  # %
  # % REFLAT and REFLON default to Boston
  # %
  # % X and Y may be specified as X+i*Y
  # %
  # % Specifying only a single output yields LON+i*LAT
  # 
  # % CVL, 7-10-97
  # % Hacked from mercgom2 from C. Naimie.

  if (inherits(x, "sf")){
    xy <- sf::st_coordinates(x)
    y <- xy[,2]
    x <- ll[,1]
  } else {
    if (is.null(y)) stop("if x is not POINT, then y must have latitudes")
  }
  
  reflon = reflon * pi/180
  reflat = reflat * pi/180

  r = 6.3675e+6

  xo = r * cos(reflat) * reflon
  yo = r * cos(reflat) * log( (1.0+sin(reflat))/cos(reflat) )
  
  rlon = (x + xo) / ( r * cos(reflat) )
  
  y = y + yo
  
  po = rep(reflat, length(x))
  
  ii <- 0
  for (i in seq_len(n)){
    ii <- i
    p1 <- po - ( (1.0+sin(po)) /cos(po) - exp(y/(r*cos(reflat)))) / ((1.0+sin(po)) / (cos(po)^2.0))
    if (max(abs(p1-po)) < tol) {
      break
    }
    po <- p1
  }
  
  if (ii == n) {
    ix <- which.max( (abs(p1-po)) < tol)
    p1[ix] <- NaN
    rlon[ix] <- NaN
    warning('Did not converge after 1000 iterations, some values NaN')
  }
  
  dplyr::tibble(lon = rlon * 180/pi, 
                lat = p1 * 180/pi)
}

