# TODO: 
# - warn if a file doesn't exist
# - aggregation in time and space
# - turn into R package 
# - extract time series
# - consistent color scale 

nimrod_read_dat = function(file_name) {

  # open file connection
  con = file(file_name, open='rb')

  # file structure is:
  # <header length><header><header length><data length><data><data length>
  header_len = readBin(con, integer(), n=1, size=4, endian='big') 
  
  # general header entries, integers (byte 1-62)
  meta1         = readBin(con, integer(), n = 31, size = 2, endian = 'big')
  validity_time = meta1[1:5]
  data_time     = meta1[7:11]
  nbyte_data    = meta1[13]
  n_row         = meta1[16]
  n_col         = meta1[17]
  n_data        = n_row * n_col
  na_value      = meta1[25]
  
  # general header entries, reals (byte 63-174)
  meta2   = readBin(con, double(), n = 28, size = 4, endian = 'big')
  delta_y = meta2[4]
  delta_x = meta2[6]
  
  # data-specific header entries, reals (bytes 175-354)
  meta3 = readBin(con, double(), n=45, size=4, endian='big')
  y_lim = range(meta3[c(1,3,5,7)])
  x_lim = range(meta3[c(2,4,6,8)])
  
  # character header entries (bytes 355-410)
  meta4 = c(readBin(con, character(), n=1, size=8, endian='big'),
            readBin(con, character(), n=2, size=24, endian='big'))
  unit = meta4[1]
  name = meta4[2]
  
  # read data into integer vector
  seek(con, where=4+header_len+4+4, origin='start')
  data = readBin(con, integer(), n=n_data, size=nbyte_data, endian='big')

  # close connection
  close(con)
  
  # assemble output and return
  ans = list(
    header = list(
      validity_time = validity_time,
      data_time     = data_time,
      n_row         = n_row,
      n_col         = n_col,
      na_value      = na_value,
      x_lim         = x_lim,
      y_lim         = y_lim,
      unit          = unit,
      name          = name),
    data = data
  )

  return(ans)
}


nimrod_read = function(from, to = from, by='15 min', 
                       nimrod_dir='~/data/clim/metoffice-nimrod-radar/') {

  # FIXME: nimrod_read("2015-11-16 00:45:00 GMT", "2015-11-16 05:30:00 GMT")
  # returns an empty list without any warning
  # FIXME: nimrod_read('2016-01-03 06:30')
  # throws error "vector size cannot be NA"

  t0          =  as.POSIXct(from)
  t1          =  as.POSIXct(to)
  times       =  seq(from=t0, to=t1, by=by)
  dates       =  unique(format(times, '%Y%m%d'))
  datestimes  =  format(times, '%Y%m%d%H%M')

  imgs = list()
  for (d in dates) {
    yr = format(as.Date(d, format='%Y%m%d'), '%Y')
    yr_dir = file.path(nimrod_dir, yr)
    day_file = dir(yr_dir, pattern=paste(d, '.*.dat.gz.tar', sep=''), 
                   full.names=TRUE)[1]
    stopifnot(file.exists(day_file))
    tstamps = grep(pattern=d, x=datestimes, value=TRUE)
  
    gz_lst = untar(day_file, list=TRUE)
    time_files = grep(pattern=paste(tstamps, collapse='|'), 
                      x=gz_lst, value=TRUE)
    time_files = sort(time_files)
    if (length(time_files) == 0) {
      next
    }
    untar(tarfile=day_file, files=time_files, exdir=tempdir())
    for (tf in time_files) {
      tff = file.path(tempdir(), tf)
      R.utils::gunzip(tff, remove=TRUE, overwrite=TRUE)
      dff = gsub('\\.gz$', '', tff)
      if (file.size(dff) == 0) {
        next
      } 
      dat = nimrod_read_dat(file_name = dff)
      nam = format(as.POSIXct(paste(dat[['header']][['data_time']], collapse='-'),
                             format='%Y-%m-%d-%H-%M'),
                   format='%Y-%m-%dT%H:%M')
      imgs[[ nam ]] = dat
    }
  }
  return(imgs)
}

nimrod_files_exist = function(from, to = from, by='15 min', 
                              nimrod_dir='~/data/clim/metoffice-nimrod-radar/') {
  t0 = as.POSIXct(from)
  t1 = as.POSIXct(to)
  times = seq(from=t0, to=t1, by=by)
  dates = unique(format(times, '%Y%m%d'))
  datestimes = format(times, '%Y%m%d%H%M')
  for (d in dates) {
    yr = format(as.Date(d, format='%Y%m%d'), '%Y')
    yr_dir = paste(nimrod_dir, '/', yr, '/', sep='')
    day_file = dir(yr_dir, pattern=paste(d, '.*.dat.gz.tar', sep=''), 
                   full.names=TRUE)[1]
    if(!file.exists(day_file)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


nimrod_get_coast = function() {

  # coast = rnaturalearth::ne_coastline(scale='medium')
  # coast = raster::crop(coast, raster::extent(-12,10,40,65))
  # coast = sp::spTransform(coast, sp::CRS(SRS_string = 'EPSG:27700'))
  # # 
  # coast_xy = do.call(rbind, 
  #                    lapply(sp::coordinates(coast), 
  #                           function(x) 
  #                             do.call(rbind, 
  #                                     lapply(x, function(y) rbind(y,NA)))))
  # colnames(coast_xy) = c('x', 'y')
  # write.csv(coast_xy, 'nimrod_coast.csv', row.names=FALSE)

  coast = read.csv('nimrod_coast.csv')
  return(coast) # can now be added to existing plot with `lines(coast)`
}


nimrod_apply_bbox = function(img, xlim=c(-2e5, 8e5), ylim=c(-1e5,12e5)) {

  img1 = img[[1]]
  xy = expand.grid(x = seq(from=img1[['header']][['x_lim']][1], 
                           to=img1[['header']][['x_lim']][2], 
                           len=img1[['header']][['n_col']]),
                   y = rev(seq(from=img1[['header']][['y_lim']][1], 
                               to=img1[['header']][['y_lim']][2], 
                               len=img1[['header']][['n_row']])))
  mask = (xy[['x']] >= xlim[1] & xy[['x']] <= xlim[2] & 
          xy[['y']] >= ylim[1] & xy[['y']] <= ylim[2])

  xymask = xy[mask, ]
  n_col = length(unique(xymask[['x']]))
  n_row = length(unique(xymask[['y']]))
  x_lim = range(xymask[['x']])
  y_lim = range(xymask[['y']])

  for (ii in 1:length(img)) {
    img[[ii]][['data']] = img[[ii]][['data']][ mask ]
    img[[ii]][['header']][['n_col']] = n_col
    img[[ii]][['header']][['n_row']] = n_row
    img[[ii]][['header']][['x_lim']] = x_lim
    img[[ii]][['header']][['y_lim']] = y_lim
  }
  return(img)
}

nimrod_apply_bbox2 = function(img, center = c(294800, 92600), nx=40, ny=40) {

  stopifnot(ny <= img[[1]][['header']][['n_col']])
  stopifnot(ny <= img[[1]][['header']][['n_col']])

  img1 = img[[1]]
  xy = expand.grid(x = seq(from=img1[['header']][['x_lim']][1], 
                           to=img1[['header']][['x_lim']][2], 
                           len=img1[['header']][['n_col']]),
                   y = rev(seq(from=img1[['header']][['y_lim']][1], 
                               to=img1[['header']][['y_lim']][2], 
                               len=img1[['header']][['n_row']])))

  xx = sort(unique(xy$x))
  yy = sort(unique(xy$y))

  # calculate best window of size nx*ny for given center coord
  n_col = img[[1]][['header']][['n_col']]
  n_row = img[[1]][['header']][['n_row']]
  x_mids = sapply(1:(n_col - nx + 1), function(ii) 0.5 * (xx[ii] + xx[ii - 1 + nx]))
  y_mids = sapply(1:(n_row - ny + 1), function(ii) 0.5 * (yy[ii] + yy[ii - 1 + ny]))
  ix_opt = which.min((x_mids - center[1])^2)
  iy_opt = which.min((y_mids - center[2])^2)

  # specify xlim and ylim
  xlim = c(xx[ix_opt], xx[ix_opt + nx - 1])
  ylim = c(yy[iy_opt], yy[iy_opt + ny - 1])

  mask = (xy[['x']] >= xlim[1] & xy[['x']] <= xlim[2] & 
          xy[['y']] >= ylim[1] & xy[['y']] <= ylim[2])

  xymask = xy[mask, ]
  n_col = length(unique(xymask[['x']]))
  n_row = length(unique(xymask[['y']]))
  x_lim = range(xymask[['x']])
  y_lim = range(xymask[['y']])

  for (ii in 1:length(img)) {
    img[[ii]][['data']] = img[[ii]][['data']][ mask ]
    img[[ii]][['header']][['n_col']] = n_col
    img[[ii]][['header']][['n_row']] = n_row
    img[[ii]][['header']][['x_lim']] = x_lim
    img[[ii]][['header']][['y_lim']] = y_lim
  }
  return(img)
}


nimrod_plot = function(img, zlim=NULL) {

  xx = seq(from = img[['header']][['x_lim']][1], 
           to = img[['header']][['x_lim']][2], 
           len = img[['header']][['n_col']])
  yy = seq(from = img[['header']][['y_lim']][1], 
           to = img[['header']][['y_lim']][2], 
           len = img[['header']][['n_row']])
  imgmat = matrix(img[['data']], 
                  nrow=img[['header']][['n_row']], 
                  ncol=img[['header']][['n_col']], 
                  byrow=TRUE)

  imgmat[ imgmat == img[['header']][['na_value']] ] = NA

  if (is.null(zlim)) {
    zlim = range(img[['data']], na.rm=TRUE)
    zlim[1] = max(0, zlim[1])
  }
  zlim = sort(zlim)
  if (zlim[1] == zlim[2]) zlim[2] = zlim[1] + 1

  titl =  format(as.POSIXct(paste(img[['header']][['data_time']], collapse='-'), 
                            format='%Y-%m-%d-%H-%M'), '%Y-%m-%d %H:%M')
  image(xx, yy, t(apply(imgmat, 2, rev)), 
        main=titl, xlab='Easting', ylab='Northing',
        col = hcl.colors(12, 'Viridis', rev = FALSE), 
        zlim=zlim)
}


nimrod_animate = function(nimrod_list, coast=NULL, fps=10, locator=FALSE, zlim=NULL) {

  tstamps = as.POSIXct(names(nimrod_list), format='%Y-%m-%dT%H:%M')
  if (is.null(coast)) {
    coast = nimrod_get_coast()
  }
  
  x11()
  for (img in nimrod_list) {
    dev.hold()
    nimrod_plot(img, zlim=zlim)
    lines(coast, col='#ffffff77')
    dev.flush()
    if (locator) {
      loc = locator(1)
      if(!is.null(loc)) {
        cat(paste(img[['header']][['data_time']]), loc[['x']], loc[['y']], '\n')
      }
    }
    Sys.sleep(1/fps)
  }

}




# # test code
if (FALSE) {
  source('nimrod.R')
  t0 = as.POSIXct('2020-01-01 00:00')
  t1 = as.POSIXct('2020-01-03 23:00')
  dt = '30 min'
  nimrod_dir = '~/data/clim/metoffice-nimrod-radar/'
  imgs = nimrod_read(t0, t1, dt, nimrod_dir)
  imgs = nimrod_apply_bbox(imgs, xlim=c(-2e5, 8e5), ylim=c(-1e5,12e5)) 
  imgs = nimrod_apply_bbox2(imgs, center = c(294800, 92600), nx = 40, ny=50) 
  nimrod_animate(imgs)
}




