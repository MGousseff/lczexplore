dirList<-list.dirs("/home/gousseff/Documents/3_data/data_article_LCZ_diff_algos/newDataTree")[-1]

 dirList<-list.dirs(paste0(
 system.file("extdata", package = "lczexplore"),"/multipleWfs"), recursive = FALSE)
 allLocIntersected<-concatIntersectedLocations(
 dirList = dirList, inLocations = c("Arville", "Redon"), columns = "lcz_primary")

test1<-fsubset(allLocIntersected, location == "Arville")
test2 <-subset(allLocIntersected, location == "Arville")
attributes(test1)[[1]]
attributes(test2)[[1]]

str(attributes(test1))
str(attributes(test2))

expect_identical(attributes(test1), attributes(test2))
expect_identical(test1,test2)

expect_identical(test1$geometry, test2$geometry)
attr(test1$geometry, "classes")
attr(test2$geometry, "classes")

ggplot(test1) + geom_sf(aes(fill = osm))
ggplot(test2) + geom_sf(aes(fill = osm))



attributes(test1) <- attributes(test1)[c("names", "row.names", "sf_column", "agr", "class")]
ggplot(test1) + geom_sf(aes(fill = osm))



# reprex
make_square <- function(cx, cy, size = 0.5) {
  st_polygon(list(rbind(
    c(cx - size, cy - size),
    c(cx + size, cy - size),
    c(cx + size, cy + size),
    c(cx - size, cy + size),
    c(cx - size, cy - size)  # close the ring
  )))
}

mixed_geom <- st_sfc(
  make_square(1, 1),
  make_square(2, 1),
  st_union(make_square(3, 1), make_square(3.5, 1)),  # creates MULTIPOLYGON
  make_square(1, 2),
  make_square(2, 2),
  make_square(3, 2),
  crs = 4326
)

exampleSf <- st_sf(
  var      = factor(c("A", "B", "C", "A", "B", "C")),
  group    = factor(c("g1", "g1", "g1", "g2", "g2", "g2")),
  geometry = mixed_geom
)


tetest1<-fsubset(exampleSf,group == "g1")
# st_geometry(tetest1)<-st_sfc(st_geometry(tetest1), crs = st_crs(exampleSf))
tetest2<-subset(exampleSf,group == "g1")

expect_identical(attributes(tetest1),attributes(tetest2))
expect_identical(attributes(tetest1$geometry),attributes(tetest2$geometry))

attributes(tetest1$geometry)$bbox
attributes(tetest2$geometry)$bbox

showLCZ(tetest2, column = "var", repr = "alter")

ggplot() +
  geom_sf(data= exampleSf, aes(color = group)) +
  scale_color_manual(breaks = c("g1","g2"), values = c("blue", "red")) +
  geom_sf(data = tetest1, aes(fill = var))

ggplot() +
  geom_sf(data= exampleSf, aes(color = group)) +
  geom_sf(data = tetest2, aes(fill = var))




# now try fsubset
test1 <- collapse::fsubset(exampleSf, group == "g1")
test2 <- exampleSf[exampleSf$group == "g1", ]

expect_identical(test1$geometry, test2$geometry)
attributes(test1)
attributes(test2)

attr(test2$geometry, "classes")

