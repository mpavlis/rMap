
source("~/Dropbox/git/rMap/rmap.r")
source("~/Dropbox/git/Route/sf_functions.r")

library(stringr)

# shp <- st_read("~/Dropbox/liverpool/graph_dbscan/all/kn15_150_60_cleaned.shp")

# shp <- st_read("~/Dropbox/liverpool/graph_dbscan/graph_ids/pnts_graph_id_kn15.shp", stringsAsFactors = F)

site_names <- c("abertillery", "bristol", "cardiff", "clapham", "glasgow", "inverurie", "winchester", "wolverhampton")
data_dir <- "~/Dropbox/liverpool/graph_dbscan/new"
setwd(data_dir)

sites <- vector("list")
for (s_name in site_names){
  site <- st_read(paste0(getwd(),"/",s_name, ".shp"), stringsAsFactors = F)
  site <- site[site$clustering != 0, ]
  sites[[s_name]] <- site
}

##### Figure 1: All sites #####################################################################################################################################

centr <- do.call(rbind, lapply(sites, function(s) centroids(coords_from_points(s$geometry))))
centr_sf <- data.frame(site = row.names(centr))
centr_sf$geometry <- st_sfc(lapply(1:nrow(centr), function(x) st_point(centr[x, ])))
centr_sf <- st_as_sf(centr_sf)
st_crs(centr_sf) <- 27700

centr_tr <- st_transform(centr_sf, 4326)
coords_tr <- coords_from_points(centr_tr$geometry)
centr_tr$x <- coords_tr[,1]
centr_tr$y <- coords_tr[,2]
centr_tr$site <- str_to_title(centr_tr$site)
centr_tr$site[4] <- "Clapham \nJunction"
centr_tr$y[1] <- centr_tr$y[1] + 0.2
centr_tr$x[2] <- centr_tr$x[2] + 0.7
centr_tr$x[3] <- centr_tr$x[3] - 0.2
centr_tr$y[3] <- centr_tr$y[3] + 0.1
centr_tr$y[4] <- centr_tr$y[4] + 0.3
centr_tr$x[5] <- centr_tr$x[5] + 0.2
centr_tr$y[5] <- centr_tr$y[5] - 0.2
centr_tr$x[6] <- centr_tr$x[6] - 0.4
centr_tr$y[6] <- centr_tr$y[6] - 0.2
centr_tr$x[7] <- centr_tr$x[7] + 1
centr_tr$x[8] <- centr_tr$x[8] + 0.3
centr_tr$y[8] <- centr_tr$y[8] + 0.2


basemap <- get_basemap(location = "Great Britain", zoom = 6, maptype = "toner-background")
basemap_elements <- get_basemap_elements(basemap = basemap, orientation = F, scalebar_pos = "bottomleft", round_scale = 0, rec.colour = "white", rec2.colour = "white",legend.colour = "white")
# basemap <- add_basemap_elements(basemap = basemap, basemap_elements = basemap_elements)
map <- add_points(basemap = basemap, basemap_elements = basemap_elements, sf_df = centr_sf, colour = "dark grey",
                  alpha_points = 0.8, legend_text = "Sites", size = 5)
map <- add_text(map_list = map, x = centr_tr$x, y = centr_tr$y, txt = centr_tr$site, size = 5, colour = "black", family = "ArialMT")

##### Figure 2: Retail units of sites #######################################################################################################################

# create_maps(basemap = basemap, sf_df = centr_sf, colour_by_fields = "site", scalebar_pos = "bottomleft",
#             alpha = 0.6, orientation = F, maptype = "toner")

maps_list <- vector("list", length = 8)
x <- 1
for (site in (sites)){
  count_cls <- sort(table(site$clustering), decreasing = T)
  if (length(count_cls) > 20){
    site <- site[! site$clustering %in% as.integer(names(count_cls)[21:length(count_cls)]), ]
    site$clustering <- as.integer(as.factor(site$clustering))
  }
  basemap <- get_basemap(location = site, maptype = "toner-background")
  basemap_elements <- get_basemap_elements(basemap = basemap, orientation = F, scalebar_pos = "bottomleft", round_scale = 0)
  map <- add_points(basemap = basemap, basemap_elements = basemap_elements, sf_df = site, colour_by_fields = "clustering",
                    alpha_points = 0.6)
  maps_list[[x]] <- 
  maps_list[[x]] <- create_maps(sf_df = site, colour_by_fields = "clustering", scalebar_pos = "bottomleft",
                                alpha = 0.6, orientation = F, maptype = "toner")
  x <- x+1
}


multiplot(maps_list[[1]][[1]],
          maps_list[[2]][[1]],
          maps_list[[3]][[1]],
          maps_list[[4]][[1]],
          maps_list[[5]][[1]],
          maps_list[[6]][[1]],
          maps_list[[7]][[1]],
          maps_list[[8]][[1]],
          cols = 4)


###################################### Plot points #####################################################

# bristol <- st_read(paste0(getwd(),"/", "bristol", ".shp"), stringsAsFactors = F)
# map_bristol <- create_maps(sf_df = bristol, colour_object = "dark red", scalebar_pos = "bottomleft",
#                            orientation = F, alpha = 0.8, maptype = "toner")

bristol <- st_read(paste0(getwd(),"/", "bristol", ".shp"), stringsAsFactors = F)
map_bristol <- create_maps(sf_df = bristol, colour_object = "dark red", scalebar_pos = "bottomleft",
                           orientation = F, alpha = 0.8, maptype = "toner")

##### Example of knn graphs
setwd("/home/michalis/Dropbox/liverpool/graph_dbscan/graph_ids")

shp1 <- st_read("pnts_graph_id_kn4.shp", stringsAsFactors = F)
shp2 <- st_read("pnts_graph_id_kn10.shp", stringsAsFactors = F)
shp3 <- st_read("pnts_graph_id_kn15.shp", stringsAsFactors = F)

maps_list <- vector("list", length = 3)
maps_list[[1]] <- create_maps(shp1[shp1$graph_id %in% c(2763, 2426, 1558, 1866, 881), ], colour_by_fields = "graph_id", scalebar_pos = "topleft")
maps_list[[2]] <- create_maps(shp2[shp2$graph_id %in% c(726, 1126), ], colour_by_fields = "graph_id", scalebar_pos = "topleft")
maps_list[[3]] <- create_maps(shp3[shp3$graph_id == 707, ], colour_by_fields = "graph_id", scalebar_pos = "topleft")

multiplot(maps_list[[1]][[1]],
          maps_list[[2]][[1]],
          maps_list[[3]][[1]],
          cols = 3)

##### Traditional national-scale DBSCAN
shp <- st_read("~/Dropbox/liverpool/graph_dbscan/graph_ids/pnts_graph_id_kn15.shp", stringsAsFactors = F)
eps <- calc_eps_clust(do.call(rbind, unclass(shp$geometry)))
setDT(shp)
shp[, clustering := dbscan::dbscan(do.call(rbind, unclass(geometry)), eps = eps, minPts = 10, borderPoints = T)]
shp[, clustering := dbscan::dbscan(do.call(rbind, unclass(geometry)), eps = eps, minPts = 10, borderPoints = T)$cluster]
shp[, eps := eps]
st_write(st_as_sf(as.data.frame(shp)), "/home/michalis/Dropbox/liverpool/graph_dbscan/all/trad_dbscan.shp", driver = "ESRI Shapefile")

data_dir <- "/home/michalis/Dropbox/liverpool/graph_dbscan/all/trad_dbscan_sele/"
site_names <- list.files(data_dir, ".shp")

maps_list <- vector("list", length = 8)

for (i in seq_along(site_names)){
  site <- st_read(paste0(data_dir,site_names[i]), stringsAsFactors = F)
  site <- site[site$clustering != 0, ]
  maps_list[[i]] <- create_maps(site, colour_by_fields = "clustering", scalebar_pos = "topleft")
}

multiplot(maps_list[[1]][[1]],
          maps_list[[2]][[1]],
          maps_list[[3]][[1]],
          maps_list[[4]][[1]],
          maps_list[[5]][[1]],
          maps_list[[6]][[1]],
          maps_list[[7]][[1]],
          maps_list[[8]][[1]],
          cols = 4)