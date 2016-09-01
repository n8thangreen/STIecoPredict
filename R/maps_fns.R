#'  Choropleth Mapping for Natsal MRP Predictions
#'
#'  Plot UK choropleth maps
#'  developed using LA and ETS data but
#'  can be applied more generally
#'  Note that the legend title is also used for the file name
#'
#' @param map.data Map object loaded by readOGR()
#' @param g Number of groups for values; max(g)=9
#' @param brks Given break points to group values (vector)
#' @param title Plot title (string)
#' @param zoom Whether to zoom-in (for UK) (logical)
#' @param value Variable name of colouring value (string)
#' @param file Output to file (logical)
#'
#' @return plot

map.tb <- function(map.data, g=NA, brks=NA, title="", zoom=FALSE, value="tb", file=FALSE){

  ## save to file open device
  if(file){png(file=paste("figures/", title, ".png", sep=""), width = 600, height= 550)}

  ## reset plotting area
#   par(mar=c(0,0,0,0))
#   par(oma=c(0,0,0,0))
#   par(mfrow=c(1,2))

  zoom.coord <- c(5342,592031)

  if(!is.na(brks)){
    ## defined breakpoints
    cols <- brewer.pal(length(brks), "Greens")
    frac <- cut(map.data@data[,value], brks)
    gs <- cols[findInterval(map.data@data[,value], vec = brks)]
  }else if(!is.na(g)){
    ## defined number of groups
    cols <- brewer.pal(g, "Greens")
    frac <- cut2(map.data@data[,value], g=g)
    gs <- cols[as.integer(factor(frac))]
  }

  if(zoom){plot(map.data, col = gs, ylim=zoom.coord)}
  else{    plot(map.data, col = gs)}

  title(main=title)
  legend("topleft", legend = levels(frac), fill = cols, title=title, cex=0.8)

  if(file){dev.off()}
}


#' Multiple Maps Plot
#'
#' @param map.data
#' @param datatype
#' @param g
#' @param brks
#' @param tofile
#' @param op
#'
#' @return plot
#'
#' @seealso map.tb

multimap <- function(map.data, datatype="all", g=NA, brks=NA, tofile=FALSE, op=NA){

  ## denote if using user-defined colour break points
  if(!is.na(brks)){
    datatype2 <- paste(datatype, "_brks", sep="")
  }else{datatype2 <- datatype}

  ## save to file open device
  if(tofile){
    png(file=paste("figures/Grid_", datatype2, ".png", sep=""), width = 3000, height= 2000)  #, width = 1500, height= 900)
  }

  if(!is.na(op)){par(op)}

  ## global within function variables
  gmap.data <<- map.data
  gg <<- g
  gbrks <<- brks
  gtofile <<- tofile

  ## simplify the mapping function
  map.tb.tmp <- function(title, zoom=TRUE, value){
    map.tb(gmap.data, gg, gbrks, title, zoom, value, file=FALSE)
  }

  if(datatype!="all"){
#     par(mfrow=c(3,3))

    map.tb.tmp(title=paste("White.",datatype2,sep=""), value=paste("White.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-Caribbean.",datatype2,sep=""), value=paste("Black-Caribbean.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-African.",datatype2,sep=""), value=paste("Black-African.",datatype,sep=""))
    map.tb.tmp(title=paste("Black-other.",datatype2,sep=""), value=paste("Black-other.",datatype,sep=""))
    map.tb.tmp(title=paste("Indian.",datatype2,sep=""), value=paste("Indian.",datatype,sep=""))
    map.tb.tmp(title=paste("Pakistani.",datatype2,sep=""), value=paste("Pakistani.",datatype,sep=""))
    map.tb.tmp(title=paste("Bangladeshi.",datatype2,sep=""), value=paste("Bangladeshi.",datatype,sep=""))
    map.tb.tmp(title=paste("Chinese.",datatype2,sep=""), value=paste("Chinese.",datatype,sep=""))

    if(!is.na(brks)){
      ## empty plot
      plot(1, type="n", axes=F, xlab="", ylab="")
      legend("topleft", legend = levels(cut(1,brks)), fill = brewer.pal(length(brks), "Greens"), cex=1.0) #, cex=1.4)
    }
  }else if(datatype=="all"){
    ethgrp.names <- c("White", "Black-Caribbean", "Black-African", "Black-other", "Indian", "Pakistani", "Bangladeshi", "Chinese")

    for (i in ethgrp.names){
      map.tb.tmp(title=paste(i,".tb",sep=""), value=paste(i,".tb",sep=""))
      map.tb.tmp(title=paste(i,".prop",sep=""), value=paste(i,".prop",sep=""))
      map.tb.tmp(title=paste(i,".pop",sep=""), value=paste(i,".pop",sep=""))
    }

    ##TODO##
    ## different brks for tb, prop, pop

  }

  if(tofile){dev.off()}
}

