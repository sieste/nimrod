source('nimrod.R')

# get coast lines for plotting
coast = read.csv('coast.csv')

# 
img = nimrod_read(from='2020-02-01', to='2020-02-05', by='60 min')
img = nimrod_apply_bbox(img, xlim=c(-2e5, 8e5), ylim=c(-1e5,12e5))

# animate the collection
nimrod_animate(img, coast=coast, fps=10)

# single image
nimrod_plot(img[[1]])
lines(coast)




# snippets

# bng_coords = 
#   crossing(easting = seq(from=meta$xllcenter, by=meta$cellsize, len=meta$ncols),
#            northing = seq(from=meta$yllcenter, by=meta$cellsize, len=meta$nrows)) %>%
#   st_as_sf(coords = c('easting', 'northing'), crs=27700) %>%
#   st_coordinates() %>% 
#   as_tibble()
# 
# latlon_coords = 
#   bng_coords %>%
#   st_as_sf(coords = c('X', 'Y'), crs=27700) %>%
#   st_transform(4326) %>%
#   st_coordinates() %>% 
#   as_tibble()


# KEEP THIS FOR THE SF AND TRANSFORM STUFF
# get_coast = function(xlim=c(-405000, 800000), 
#                      ylim=c(-100000, 1100000),
#                      as_list = FALSE) {
# 
#   coast = rnaturalearth::ne_coastline(scale='medium') %>% 
#           fortify %>%
#           st_as_sf(coords = c('long', 'lat'), crs=4326) %>%
#           st_transform(27700) %>%
#           bind_cols(as_tibble(st_coordinates(.))) %>%
#           as_tibble() %>%
#           select(-geometry) %>%
#           group_by(group) %>% 
#           mutate(xmin = min(X), xmax=max(X), 
#                  ymin=min(Y), ymax=max(Y)) %>% 
#           filter(xmin > xlim[1], xmax < xlim[2], 
#                  ymin > ylim[1], ymax < ylim[2]) %>%
#           select(-xmin, -xmax, -ymin, -ymax)
# 
#   if(as_list) {
#     coast = coast %>%
#       mutate(group=paste(group)) %>%
#       select(-order, -piece, -id) %>%
#       split(.$group)
#   }
# 
#   return(coast)
# 
# }


