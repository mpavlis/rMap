library(sf)
library(ggplot2)
library(ggmap)
library(ggsci)

source("~/Dropbox/git/rMap/create_scale_bar.r")

get_basemap <-  function(location, ...){
  
  if (is(location, "sf") | is(location, "sfc")){
    
    sfc_tr <- st_transform(st_geometry(location), 4326)
    
    # this is the bounding box of the input dataset
    bbox_sfc <- as.numeric(st_bbox(sfc_tr))
    
    # use get_map to get the basemap
    basemap <- get_map(location = bbox_sfc, ...)
    
    # this is the bounding box of the basemap (xmin, ymin, xmax, ymax)
    bbox_basemap <- as.numeric(attributes(basemap)$bb)[c(2,1,4,3)]
    
    # for as long as the bbox of the basemap is smaller than the bbox of the input, zoom out 
    while (any(bbox_sfc[1:2] - bbox_basemap[1:2] < 0) | any(bbox_sfc[3:4] - bbox_basemap[3:4] > 0)){
      zoom <- attributes(basemap)$zoom - 1
      basemap <- get_map(location = bbox_sfc, zoom = zoom, ...)
      bbox_basemap <- as.numeric(attributes(basemap)$bb)[c(2,1,4,3)]
    }
    return(basemap)
  } else {
    basemap <- get_map(location = location, ...)
    return(basemap)
  }
}

get_basemap_elements <- function(basemap, orientation = T, arrow_north_size = 8, scalebar_pos="bottomleft"){
  
  bbox_basemap <- attributes(basemap)$bb
  # lon_dist <- distHaversine(c(bbox_background$ll.lon,bbox_background$ll.lat), c(bbox_background$ur.lon, bbox_background$ll.lat))
  # lat_dist <- distHaversine(c(bbox_background$ll.lon,bbox_background$ll.lat), c(bbox_background$ll.lon, bbox_background$ur.lat))
  
  lon_dif <- bbox_basemap$ur.lon - bbox_basemap$ll.lon
  lat_dif <- bbox_basemap$ur.lat - bbox_basemap$ll.lat
  if (scalebar_pos == "bottomleft"){
    lon <- bbox_basemap$ll.lon + lon_dif/20
    lat <- bbox_basemap$ll.lat + lat_dif/20
  } else if (scalebar_pos == "bottomright"){
    lon <- bbox_basemap$ur.lon - lon_dif/3
    lat <- bbox_basemap$ll.lat + lat_dif/10
  } else if (scalebar_pos == "topleft"){
    lon <- bbox_basemap$ll.lon + lon_dif/20
    lat <- bbox_basemap$ur.lat - lat_dif/5
  } else if (scalebar_pos == "topright"){
    lon <- bbox_basemap$ur.lon - lon_dif/3
    lat <- bbox_basemap$ur.lat - lat_dif/6
  }
  
  distance_lon <- lon_dif * 10
  distance_lat = distance_lon/5
  distance_legend <- distance_lon/3
  arrow_length <- distance_lon/3
  arrow.distance <- distance_lon/2
  
  if (orientation){
    if (arrow_north_size == 0){
      arrow_north_size <- ceiling(distance_lon * 10)
    }
  }
  scaleBar(lon = lon, lat = lat, distanceLon = distance_lon, distanceLat = distance_lat, distanceLegend = distance_legend,
           dist.unit = "km", arrow.length = arrow_length, arrow.distance = arrow.distance, arrow.North.size = arrow_north_size, orientation = orientation)
}

add_basemap_elements <- function(basemap, basemap_elements, extent = "device", ...){
  ggmap(basemap, extent = extent, ...) +
    basemap_elements
}


########################################################################################################################

add_points <- function(basemap, sf_df, basemap_elements = NULL, colour_by_fields = NULL, colour = NULL,
                       d3_palette = NULL, alpha_points = 0.8, legend_background = "grey", alpha_legend = 0.1,
                       size = 2, legend_text = "", ...){
  
  sfc_tr <- st_transform(st_geometry(sf_df), 4326)
  coords <- do.call(rbind, unclass(sfc_tr))
  sf_df$long <- coords[,1]
  sf_df$lat <- coords[,2]
  
  if (!is.null(colour_by_fields)){
    maps <- list()
    
    for (field_name in colour_by_fields){
      sf_df[[field_name]] <- as.factor(sf_df[[field_name]])
      # map_temp <- ggmap(basemap, extent = 'device', legend = "bottomright") +
      #   geom_point(data = sf_df, aes_string(x="long", y="lat", colour = field_name), size=size, alpha = alpha) +
      #   # theme(panel.background = element_rect(fill = NA, colour = NA)) +
      #   theme(legend.background = element_rect(fill=alpha('grey', 0.1))) + 
      #   scaleBar(lon = lon, lat = lat, distanceLon = distance_lon, distanceLat = distance_lat, distanceLegend = distance_legend,
      #            dist.unit = "km", arrow.length = arrow_length, arrow.distance = arrow.distance, arrow.North.size = arrow_north_size, orientation = orientation)
      map_temp <- ggmap(basemap, extent = 'device', legend = "bottomright", padding = 0.05) +
        geom_point(data = sf_df, aes_string(x="long", y="lat", colour = field_name), size=size, alpha = alpha_points) +
        theme(legend.background = element_rect(fill=alpha(legend_background, alpha_legend))) +
        basemap_elements
                 
      if (!is.null(d3_palette)){
        map_temp <- map_temp + scale_color_d3(palette = d3_palette)
      }
      maps[[field_name]] <- map_temp
    }
  } else if (!is.null(colour)){
    maps <- list(ggmap(basemap, extent = 'device', legend = "bottomright", padding = 0.05) +
                   geom_point(data = sf_df, aes(x=long, y=lat, colour = "location"), size=size, alpha = alpha, show.legend = T) +
                   scale_colour_manual(name=legend_text, values=c(location=colour)) + 
                   theme(legend.background = element_rect(fill=alpha('grey', 0.1)),
                         legend.text = element_text(size=rel(1.2)),
                         legend.title = element_text(size = rel(1.5))) + 
                   scaleBar(lon = lon, lat = lat, distanceLon = distance_lon, distanceLat = distance_lat, distanceLegend = distance_legend,
                            dist.unit = "km", arrow.length = arrow_length, arrow.distance = arrow.distance, arrow.North.size = arrow_north_size, orientation = orientation)
    )
  }
  
  maps
  
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
