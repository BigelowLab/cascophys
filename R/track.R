#' Track a particle
#' 
#' @export
#' @param CB CB_Physics object
#' @param P0 starting point, see \code{CB_Physics$random_point}
#' @param tstep numeric, seconds between each iteration
#' @param tmax numeric, number of seconds to run for
#' @param show_progress logical, if TRUE then show a progress bar
#' @return sf object of type POINT
particle_track <- function(CB, P0 = CB$random_point(),
                           tstep = 60,
                           tmax = 3600 * 12,
                           show_progress = TRUE,
                           filename = c("particle_track.gpkg", NA)[2]){
  
  NMAX = tmax/tstep
  #preallocate the points list
  P <- vector(length = NMAX, mode = "list")
  N <- 1
  P[[N]] <- P0
  
  if (show_progress[1]){
    pb <- txtProgressBar(min = 0, max = NMAX, initial = 0, style = 3)
  }
  
  while (N <= NMAX){
    if (show_progress[1]) setTxtProgressBar(pb, N)
    # get the current point's u,v and w
    uvw <- fvcom::get_elem_var(CB$NC, var = c("u", "v", "ww"),
                               elem = P[[N]]$elem,
                               time = N) |>
      as.matrix()
    # and it's coordinates
    pn <- sf::st_coordinates(P[[N]])
    # translate (aka affine shift)
    d <- pn + (uvw[,2:4] * tstep)
    # convert to sfc 
    g <- sf::st_sfc(sf::st_point(d), crs = sf::st_crs(CB$M))
    # determine which element the pount belongs to
    ix <- lengths(sf::st_contains(CB$M, g)) > 0
    elem <- which(ix)
    if (length(elem) == 0){
      break
    }
    # add a new point
    P[[N+1]] <- sf::st_sf(dplyr::tibble(elem = elem, 
                                        time = P[[N]]$time[1] + tstep, 
                                        geometry = g)  )                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           )
    N <- N + 1
  } 
  
  if (show_progress) close(pb)
      
  P <- dplyr::bind_rows(P[seq_len(N)])
  if (nrow(P) > 0 && !is.na(filename[1])){
    P <- sf::write_sf(P, filename)
  }
  P
}