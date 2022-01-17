# https://khufkens.com/2017/01/18/r-polar-plots/
# https://gist.github.com/khufkens/2746ffc7eb5ed9769ef7b61130d3946f
  
  
maps2sp = function(xlim, ylim, l.out = 100, clip = TRUE) {
  stopifnot(require(maps))
  m = map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = CRS("+init=epsg:4326")
  bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"bb")), proj4string = LL)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  stopifnot(require(maptools))
  m <- map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  if (!clip)
    m
  else {
    stopifnot(require(rgeos))
    gIntersection(m, bb) # cut map slice in WGS84
  }
}
par(mar = c(0, 0, 1, 0))
m = maps2sp(c(-100,-20), c(10,55))
crs.longlat = CRS("+init=epsg:4326")

sp = SpatialPoints(rbind(c(-101,9), c(-101,55), c(-19,9), c(-19,55)), CRS("+init=epsg:4326"))
laea = CRS("+proj=laea +lat_0=30 +lon_0=-40")
m.laea = spTransform(m, laea)
sp.laea = spTransform(sp, laea)
plot(as(m.laea, "Spatial"), expandBB = c(.1, 0.05, .1, .1))
plot(m.laea, col = grey(.8), add = TRUE)
gl = gridlines(sp, easts = c(-100,-80,-60,-40,-20), norths = c(20,30,40,50))
gl.laea = spTransform(gl, laea)
plot(gl.laea, add = TRUE)
text(labels(gl.laea, crs.longlat))
text(labels(gl.laea, crs.longlat, side = 3:4), col = 'red')
title("curved text label demo")

# polar:
par(mar = c(0, 0, 1, 0))
pts=SpatialPoints(rbind(c(-180,-70),c(0,-70),c(180,-89),c(180,-70)), CRS("+init=epsg:4326"))
gl = gridlines(pts, easts = seq(-180,180,20), ndiscr = 100)
polar = CRS("+init=epsg:3031")
plot(spTransform(pts, polar), expandBB = c(.05, 0, .05, 0))
gl.polar = spTransform(gl, polar)
lines(gl.polar)
l = labels(gl.polar, crs.longlat, side = 3)
l$pos = NULL # pos is too simple, use adj:
text(l, adj = c(0.5, -0.5), cex = .8) 
l = labels(gl.polar, crs.longlat, side = 4)
l$srt = 0 # otherwise they end up upside-down
text(l, cex = .8)
title("grid line labels on polar projection, epsg 3031")


par(mar = c(0, 0, 1, 0))
m = maps2sp(xlim = c(-180,180), ylim = c(-90,-70), clip = FALSE)
gl = gridlines(m, easts = seq(-180,180,20))
polar = CRS("+init=epsg:3031")
gl.polar = spTransform(gl, polar)
plot(as(gl.polar, "Spatial"), expandBB = c(.05, 0, .05, 0))
plot(gl.polar, add = TRUE)
plot(spTransform(m, polar), add = TRUE, col = grey(0.8, 0.8))
l = labels(gl.polar, crs.longlat, side = 3)
# pos is too simple here, use adj:
l$pos = NULL 
text(l, adj = c(0.5, -0.3), cex = .8)
l = labels(gl.polar, crs.longlat, side = 2)
l$srt = 0 # otherwise they are upside-down
text(l, cex = .8)
title("grid line labels on polar projection, epsg 3031")
