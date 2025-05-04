library("lidR")
las <- readLAS("full_area_all.las")
plot(las)


# Khosravipour et al. pitfree algorithm
thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- rasterize_canopy(las, 1, pitfree(thr, edg))

plot(chm)