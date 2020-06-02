library(diseasemapping)
library(spdep)
library(glmmBUGS)
library(rgdal)
library(rgeos)

# https://census.edina.ac.uk/easy_download_data.html?data=England_lad_2011    #data source

setwd("../../packages/STIecoPredict")

UKla <- readOGR(dsn = "data/maps", layer = "LAD_DEC_2011_GB_BGC")
Londonla <- readOGR(dsn = "data/maps", "london_sport")
Englandla <- readOGR(dsn = "data/maps", "england_lad_2011")

names(Londonla@data)[names(Londonla@data)=="name"] <- "LAD11NM"
names(Englandla@data)[names(Englandla@data)=="name"] <- "LAD11NM"

UKla@data$LAD11NM <- toupper(UKla@data$LAD11NM)
Londonla@data$LAD11NM <- toupper(Londonla@data$LAD11NM)
Englandla@data$LAD11NM <- toupper(Englandla@data$LAD11NM)

popDataAdjMat = poly2nb(UKla, row.names=as.character(UKla[["LAD11NM"]]) )
popDataAdjMat.england = poly2nb(Englandla, row.names=as.character(Englandla[["LAD11NM"]]) )


names(popDataAdjMat) <- UKla@data$LAD11NM
names(popDataAdjMat.england) <- Englandla@data$LAD11NM

head(popDataAdjMat)

adjacency_matrix <- list( num = unlist(lapply(popDataAdjMat, length)),
                          adj = unlist(popDataAdjMat),
                          sumNumNeigh = length(unlist(popDataAdjMat)))

adjacency_matrix.england <- list( num = unlist(lapply(popDataAdjMat.england, length)),
                                  adj = unlist(popDataAdjMat.england),
                                  sumNumNeigh = length(unlist(popDataAdjMat.england)))

## does same thing
# popDataAdjMat.england.WB <- nb2WB(popDataAdjMat.england)
# dput(popDataAdjMat.england.WB, control=NULL)

## where are the no neighbour places
# which(unlist(lapply(popDataAdjMat.england, function(x) any(x==0))))

save(adjacency_matrix, file="../../scripts/mrp/data/adjacency_matrix-list.RData")
save(adjacency_matrix.england, file="../../scripts/mrp/data/adjacency_matrix_england-list.RData")
