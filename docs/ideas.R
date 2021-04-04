
# Angus Watters
# Spring 2021
# Geography 176C project ideas


library(tidyverse)
library(sf)
library(dataRetrieval)
library(AOI)
library(climateR)
library(janitor)

superfund <- readxl::read_xlsx("data/superfund_sites.xlsx") %>%
  clean_names()
superfund <- superfund %>%
  mutate(across(where(is.character), str_trim))

superfund$zip_code <- as.numeric(superfund$zip_code)

mining <- superfund %>%
  filter(site_type == "Mining")

zips <- USAboundaries::us_zipcodes() %>%
  rename(zip_code = zipcode)

zips <- zips %>%
  mutate(lat = unlist(map(zips$geometry,1)),
         long = unlist(map(zips$geometry,2)))
zips$zip_code <- as.numeric(zips$zip_code)

superfund <- left_join(superfund, select(zips, zip_code, lat, long), by= "zip_code")
plot(superfund$geometry)

superfund <- st_as_sf(superfund)


mining <- superfund %>%
  filter(site_type == "Mining")

plot(mining$geometry)

ca <- superfund %>%
  filter(state == "CA")

iron_mt <- ca %>%
  filter(site_name == 'IRON MOUNTAIN MINE')
mapview::mapview(iron_mt)
### TEST POINT FOR CLIMATE DATA ####

lat = 35.6643
lng = -96.91935
pt <- data.frame(lat, lng)
pt <- sf::st_as_sf(pt,
                   coords = c("lng", "lat"),
                   crs = 4326)


evap <- climateR::getTerraClim(AOI = pt, param = "aet",
                                startDate = "1993-01-01",
                                endDate = "2015-01-01")

evap$date <- paste0(evap$date, "-01")
evap$date <- as.Date(evap$date)
rownames(evap) <- evap$date
evap <- select(evap, AET = aet)

dygraph(data = evap,
        ylab = "volume (mm)") %>%
  dyHighlight(highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = .4) %>%
  dyOptions(colors = c("navy"),
            fillGraph = TRUE)

# Deficit
solar <- climateR::getTerraClim(AOI = pt, param = "srad",
                               startDate = "1993-01-01",
                               endDate = "2015-01-01")

solar$date <- paste0(solar$date, "-01")
solar$date <- as.Date(solar$date)
rownames(solar) <- solar$date
solar <- select(solar, srad)

dygraph(data = solar) %>%
  dyHighlight(highlightCircleSize = 4,
              highlightSeriesBackgroundAlpha = .4) %>%
  dyOptions(colors = c("navy"),
            fillGraph = TRUE)

df <- nwmHistoric::readNWMdata(comid = nldi$comid)

buffer <- st_buffer(iron_mt, 100)
bb <- st_bbox(buffer$geometry)
nwis_sites <- whatNWISsites(bBox = bb)
dataRetrieval::readWQPqw()


# Geog 190 question 2.4
library(tidyverse)
  Plot_ConvexHull<-function(xcoord, ycoord, lcolor){
    hpts <- chull(x = xcoord, y = ycoord)
    hpts <- c(hpts, hpts[1])
    lines(xcoord[hpts], ycoord[hpts], col = lcolor)
  }
df <- data.frame(x = c(32, 29, 27, 29, 32, 26, 24, 30), y = c( 31, 32, 36, 29,29, 25, 33,35))


# df <- df %>%
#   mutate(pts = paste0(x, ", ", y))
x1 <- rnorm(100, 0.8, 0.3)
y1 <- rnorm(100, 0.8, 0.3)
rm(x1, y1)
xrange <- range(df$x)
yrange <- range(df$y)
xmin = min(df$x)
xmax = max(df$x)
ymin = min(df$y)
ymax = max(df$y)

plot(df$x, df$y, type = "p", pch = 1, color = "black", xlim = c(xrange), ylim = c(yrange))
Plot_ConvexHull(xcoord = df$x, ycoord = df$y, lcolor = "black")

ch


getBoundingBox <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), ncol(xy) == 2)
  x   <- range(xy[ , 1])
  y   <- range(xy[ , 2])
  pts <- c(xleft=x[1], ybottom=y[1], xright=x[2], ytop=y[2])
  return(list(pts=pts, width=abs(diff(x)), height=abs(diff(y))))
}

xy <- as.matrix(df)
bb <- getBoundingBox(xy)

plot(xy, xlab="x", ylab="y", asp=1, type="n")
rect(bb$pts[1], bb$pts[2], bb$pts[3], bb$pts[4], border="blue", lwd="2")
points(xy, pch=16, cex=1.5)






