# ##
# ## chlamydia observed-estimated
# ## chloropleth maps
# ##
# ## N Green
# ## Oct 2014
# ##
# ## compare with http://fingertips.phe.org.uk/profile/sexualhealth/data#gid/8000057/pat/6/ati/101/page/8/are/E07000032
#
#
# rm(list=ls()) #clear workspace
#
# library(rgdal)
# library(rgeos)
# library(ggplot2)  #requires lots of other packages...
# library(gridExtra)
# library(RColorBrewer)
# library(plyr)
# library(Hmisc)
# library(mapproj)
#
# source("./R_code/Chlamydia_classifier/sim_logistic_regn/maps_fns.R")
#
# ## load outcome
# ## C:\Users\nathan.green\Documents\chlamydia\classifier\data\output
# ## pred.knn, pred.log
# load(file.choose())
#
# CTADGUM_pred <- joinAllOutcomeData(pred.knn, pred.log)
# names(CTADGUM_pred)<-sub("LA Name","LAD11NM",names(CTADGUM_pred))
#
# ## for case for support doc
# # CTADGUM_pred$temp <- as.numeric(CTADGUM_pred$LAD11NM%in%c("Chesterfield","Ealing","Exeter","Hackney","Haringey","Kensington and Chelsea"))
#
#
#
# ## load map data
# setwd("C:\\Users\\nathan.green\\Documents\\TB\\R\\TB_descriptive_Rcode\\maps")
#
# ## map polygon (outline, area names & codes)
# # ogrInfo("sdvwR-master/sdvwR-master/data", "london_sport")
# setwd("./input_data/LA_2011_boundaries_files")
# UKla.orig <- readOGR(dsn=".", "LAD_DEC_2011_GB_BGC")
#
# ## or London only
# # UKla.orig <- readOGR("sdvwR-master/sdvwR-master/data", "london_sport")
# # names(UKla.orig@data)[names(UKla.orig@data)=="name"] <- "LAD11NM"
#
# setwd("../..")
# UKla <- UKla.orig
#
# # http://spatial.ly/2013/12/introduction-spatial-data-ggplot2/
# # http://stackoverflow.com/questions/22198566/ggplot-map-plot-fails-when-limits-set-with-coord-map  #zoom-in
#
# # UKla@data <- merge(UKla@data, CTADGUM_pred, by="LAD11NM", all.x=TRUE)   # I don't know why but this doesn't match the correct places!
# UKla@data <- join(UKla@data, CTADGUM_pred, by="LAD11NM")
# # View(UKla@data)
#
# UKla.f <- fortify(UKla, region = "LAD11NM")
# # head(UKla.f,100)
#
# UKla.f <- merge(UKla.f, UKla@data, by.x = "id", by.y = "LAD11NM")
#
#
# ###############
# ## plot maps ##
# ###############
#
# # x11()
#
# ## estimates
# ## ---------
# # map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = pred.knn)) + geom_polygon() +
# # map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = pred.log)) + geom_polygon() +
#
# ## exceedance
# ## ----------
# # map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predknnBINmean2011.1524 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predknnBINmean2012.1634 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predlogBINmean2011.1524 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predlogBINmean2012.1634 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predknnBIN0.2 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predlogBIN0.2 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predknnBIN0.25 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = predlogBIN0.25 )) + geom_polygon() +
#
# ## surveillance
# ## ------------
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = surv2012.1634 )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = surv2011.1524 )) + geom_polygon() +
#
# ## Indirect standardised ratios
# ## ----------------------------
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = ISR_surv2012.1634_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = ISR_surv2011.1524_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = ISR_surv2012.1634_knn )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = ISR_surv2011.1524_knn )) + geom_polygon() +
#
# ## quadrants
# ## ---------
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = quadrant_surv2012.1634_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = quadrant_surv2011.1524_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = quadrant_surv2012.1634_knn )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = quadrant_surv2011.1524_knn )) + geom_polygon() +
#
# ## difference between surveillance and expected
# ## --------------------------------------------
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = resid_surv2012.1634_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = resid_surv2011.1524_log )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = resid_surv2012.1634_knn )) + geom_polygon() +
# #   map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = resid_surv2011.1524_knn )) + geom_polygon() +
#
#
#
# ## save
# postscript("./docs/sim_logistic_regn/figures/map_TEMP.eps")
# dev.off()
#
# #   coord_equal() +
#   labs(x = "", y = "", fill = "Outcome") +
#   ggtitle("") +
#   theme(axis.ticks=element_blank(), axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
#
# ## contiuous outcomes
# map5 + scale_fill_continuous(low = "yellow", high = "blue") +
#   coord_equal(xlim=c(0, 670000), ylim=c(0, 650000)) # England zoom
#
#
#
# ## discrete outomes
# map5 + scale_fill_manual(values=c("1"="red","2"="green", "3"="blue", "4"="yellow")) #+
# #   coord_equal(xlim=c(0, 670000), ylim=c(0, 650000))
#
# # map5 + scale_fill_gradient(low="white", high="black")
# # map5 + scale_fill_gradientn(colours= brewer.pal(5, "Blues"))#, limits=c(0, 1))
# # map5 + scale_fill_brewer(palette = "Set3")
# # map5 + scale_fill_gradientn(colours = brewer.pal(7, "YlGn"))
#
#
# # map.tb(UKla, value="Combined Coverage", zoom=T, brks = c(0,0.00001,0.1,0.2,1))
#
# ##TODO##
# ## could include some logistic binary over-dispersion adjustment?
# # https://www.statistics.ma.tum.de/fileadmin/w00bdb/www/czado/lec5.pdf
#
#
#
# #  for case for support doc ----------------------------------------------------
# ## define temp above
# map5 <- ggplot(UKla.f, aes(long, lat, group = group, fill = temp)) +
# labs(x = "", y = "", fill = "Outcome") +
#   ggtitle("") +
#   theme(axis.ticks=element_blank(), axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
#
# map5 + geom_polygon(colour="light grey") + scale_fill_gradient(low="white", high="red",na.value="white") + coord_equal(xlim=c(0, 670000), ylim=c(0, 650000))
#
#
#
#
#
#
#
