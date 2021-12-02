library(sf)
library(ncdf4)
library(dplyr)
COLORS <- RColorBrewer::brewer.pal(8, "Dark2")
fvcom <- "/mnt/ecocast/corecode/R/fvcom"
#devtools::load_all(fvcom)
library(fvcom)
cascophys <- "/mnt/ecocast/corecode/R/cascophys"
#devtools::load_all(cascophys)
library(cascophys)
CB <- CascoBayPhysics(verbose = TRUE)

P<- sf::read_sf("/mnt/ecocast/coredata/cascobay/runs/particle_60_10800.gpkg")


#(P0 <- CB$random_points(n=1))$elem
#P <- particle_track(CB, P0 = P0, tmax = 3600)

times <- 3600 * 1:3
pp <- lapply(times, function(tmax){
  ofile <- file.path("/mnt/ecocast/coredata/cascobay/runs", 
                     sprintf("particle_60_%i.gpkg", tmax))
  particle_track(CB, P0 = CB$random_points(), 
                 tmax = tmax,
                 show_progress = TRUE,
                 filename = ofile)
})


times <- 3600 * c(18, 24, 36, 48)
pp <- lapply(times, function(tmax){
  ofile <- file.path("/mnt/ecocast/coredata/cascobay/runs", 
                     sprintf("particle_60_%i.gpkg", tmax))
  particle_track(CB, P0 = CB$random_points(), 
                 tmax = tmax,
                 show_progress = TRUE,
                 filename = ofile)
})

P <- sf::read_sf("/mnt/ecocast/coredata/cascobay/runs/particle_60_10800.gpkg")
pt <- P |> 
  sf::st_transform(crs = 4326) |>
  sf::st_coordinates(P) |>
  sf::st_linestring() |>
  sf::st_sfc(crs = 4326)

p <- sf::st_sf(dplyr::tibble(geometry = pt), crs = 4326)



library(leaflet)
leaflet(data = pt) |>
  addTiles() |>
  addPolygons()

if (FALSE){
  
  clo = CB$M |> dplyr::filter(bound == "closed")
  opn = CB$M |> dplyr::filter(bound == "open")
  plot(st_geometry(CB$M))
  plot(st_geometry(opn), border = COLORS[1], col = COLORS[1], add = TRUE)
  plot(st_geometry(clo), border = COLORS[2], col = COLORS[2], add = TRUE)
}

if (FALSE) {
    #xy <- locator(20, type  ="l") 
    xy <- list(x = c(58763.814199408, 72042.9111933635, 91555.0537150939, 
                     106595.663575594, 112557.707123901, 113641.715041775, 109576.685349748, 
                     107544.170503734, 107273.168524266, 106595.663575594, 66622.8716039939, 
                     59712.3211275477, 58086.3092507368), y = c(130592.630344877, 
                                                                126121.097683647, 127340.606591255, 135199.663995841, 146988.25010272, 
                                                                156337.818394383, 162299.861942689, 162706.364911892, 160267.347096676, 
                                                                148614.261979531, 131812.139252485, 132896.147170359, 132896.147170359
                     )) 
    
    p <- do.call(cbind, xy)
    p <- rbind(p, p[1,])
    p <- sf::st_sfc(sf::st_polygon(list(p)), crs = sf::st_crs(CB$M))
    ix <- opn$elem[sf::st_contains(p, opn)[[1]]]
    
    b <- bind_rows(clo, opn)
    iy <- b$elem[!(b$elem %in% ix)]

    clsd <- CB$M %>% slice(iy)
}

