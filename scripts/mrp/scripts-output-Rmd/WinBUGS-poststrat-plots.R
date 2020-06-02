##
## chlamydia MRP
## N Green
##
## plots
## using posterior distributions samples
##


library(ggplot2)
library(modeest)
library(dplyr)
library(modeest)
library(gridExtra)
library(grid)
library(lattice)


load(file = "C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/CTADGUM_pred-with-LA.RData")
# data("CTADGUM_pred_LA") ##TODO: create
# load(file = "data/CTADGUM_pred-with-LA.RData")
CTADGUM_pred_LA <- CTADGUM_pred

load(file = "C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/CTADGUM_pred-WinBUGS.RData")
# data("CTADGUM_pred_WinBUGS") ##TODO: create
# load(file = "data/CTADGUM_pred-WinBUGS.RData")

##TODO## need to debug why there are LAs missing in WinBUGS model, then remove this chunk
## match LA numbers
CTADGUM_pred <-
    CTADGUM_pred %>%
    subset(select = -laname.num)

CTADGUM_pred <- merge(x = CTADGUM_pred,
                      y = CTADGUM_pred_LA[,c("LA Name", "laname.num")],
                      by = "LA Name", all.x = TRUE)

MCMCcols <- grepl(pattern = "^V[1234567890]+",
                  x = names(CTADGUM_pred))

# redo Woodhall() repeat testing, bias adjustment
CTADGUM_pred$surv2011.1624 <- CTADGUM_pred$surv2011.1624/0.87*0.95

df <- data.frame(`LA.Name` = CTADGUM_pred$`LA Name`,
                 laname.num = CTADGUM_pred$laname.num,
                 NatsalLA = CTADGUM_pred$NatsalLA,
                 NatsalLAsize = CTADGUM_pred$NatsalLAsize,
                 wtmean = CTADGUM_pred$wtmean,
                 `surv2011.1624` = CTADGUM_pred$surv2011.1624,
                 `surv2012.1634` = CTADGUM_pred$surv2012.1634,
                 `surv2012.1524` = CTADGUM_pred$surv2012.1524,
                 `surv2013.1524` = CTADGUM_pred$surv2013.1524,
                 `surv2011.1524` = CTADGUM_pred$surv2011.1524,
                 x = 1:nrow(CTADGUM_pred),
                 ymean = apply(CTADGUM_pred[,MCMCcols], 1, mean),
                 ymode = apply(CTADGUM_pred[,MCMCcols], 1, function(x) mlv(x, method = "mfv")[["M"]]), #mode
                 ymedian = apply(CTADGUM_pred[,MCMCcols], 1, median),
                 ymin95 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.025),
                 ymax95 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.975),
                 ymin50 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.25),
                 ymax50 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.75),
                 ymin99 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.005),
                 ymax99 = apply(CTADGUM_pred[ ,MCMCcols], 1, quantile, probs = 0.995),
                 p.surveil_greaterthan_mrp = apply(sweep(CTADGUM_pred[ ,MCMCcols], 1, CTADGUM_pred$surv2011.1624, "<"), 1, sum, na.rm = TRUE)/sum(MCMCcols)
                 )

df <-
    within(df, {
        MRPabove = df$ymin95 > df$surv2011.1624
        MRPbelow = ymax95 < surv2011.1624
        diff50 = ymin50 > surv2011.1624 | ymax50 < surv2011.1624
        diff95 = ymin95 > surv2011.1624 | ymax95 < surv2011.1624
        diff99 = ymin99 > surv2011.1624 | ymax99 < surv2011.1624
    })


#proportion above/below
df[df$diff95 & df$surv2011.1624 > df$ymedian, ] %>% nrow()/nrow(df)
df[df$diff95 & df$surv2011.1624 < df$ymedian, ] %>% nrow()/nrow(df)

df[df$diff99 & df$surv2011.1624 > df$ymedian, ] %>% nrow()/nrow(df)
df[df$diff99 & df$surv2011.1624 < df$ymedian, ] %>% nrow()/nrow(df)


#########
# plots #
#########

## Natsal vs mrp
### bars
ggplot(data = df, aes(x = wtmean, y = ymedian)) +
  geom_errorbar(aes(ymin = ymin95, ymax = ymax95), colour = "grey") +
  geom_errorbar(aes(ymin = ymin50, ymax = ymax50), colour = "grey", size = 1.2) +
  theme(legend.position = "none") + theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
  ylab("MRP estimates") + xlab("Natsal") +
  geom_point(colour = "black") +
  geom_abline(intercept = 0, slope = 1, colour = "black") +
  xlim(0,1) + ylim(0,1)

### bubbles
ggplot(data = df, aes(x = wtmean, y = ymedian)) +
  geom_point(aes(size = NatsalLAsize), shape = 1) + scale_size_area() +
  theme(legend.position = "none") + theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
  ylab("MRP estimates") + xlab("Natsal") +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  xlim(0,1) + ylim(0,1)




## Recorded vs mrp
### bars
a <- ggplot(data = df, aes(y = `surv2011.1624`, x = ymode)) +
    geom_errorbarh(aes(xmin = ymin95, xmax = ymax95), colour = "grey") +
    geom_errorbarh(aes(xmin = ymin50, xmax = ymax50), colour = "grey", size = 1.2) +
    theme(legend.position = "none") + theme_bw() +
    xlab("MRP estimates") + ylab("Recorded data") +
    geom_point(colour = "black") +
    geom_abline(intercept = 0, slope = 1, colour = "black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
  theme(text = element_text(size = 20)) +
    ggtitle('(a)')

# zoomed-in
b <- ggplot(data = df, aes(y = `surv2011.1624`, x = ymode)) +
  geom_errorbarh(aes(xmin = ymin95, xmax = ymax95), colour = "grey") +
  geom_errorbarh(aes(xmin = ymin50, xmax = ymax50), colour = "grey", size = 1.2) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_point(colour = "black") +
  geom_abline(intercept = 0, slope = 1, colour = "black") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # xlim(0.1,0.7) + ylim(0,1) +
  scale_y_continuous(limits = c(0.15,0.5), breaks = seq(0,1,0.2)) +
  scale_x_continuous(limits = c(0.15,0.5), breaks = seq(0,1,0.2)) +
  theme(text = element_text(size = 20)) +
    ggtitle('(b)')


### label numbers
c <- ggplot(data = df, aes(y = `surv2011.1624`, x = ymode, label = laname.num)) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_text(size=3) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
  xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  theme(text = element_text(size=20)) +
    ggtitle('(c)')

# zoomed-in
d <- ggplot(data = df, aes(y = `surv2011.1624`, x = ymode, label = laname.num)) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_text(size=3) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # xlim(0.1,0.7) + ylim(0,1) +
  scale_y_continuous(limits=c(0.15,0.5), breaks=seq(0,1,0.2)) +
  scale_x_continuous(limits=c(0.15,0.5), breaks=seq(0,1,0.2)) +
  theme(text = element_text(size=20)) +
    ggtitle('(d)')


grid.arrange(a, b, c, d, ncol = 2)




## bubbles
ggplot(data = df, aes(y = `surv2011.1624`, x = ymedian)) +
  geom_point(aes(size = NatsalLAsize), shape=1) + scale_size_area() +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="red") +
  xlim(0,1) + ylim(0,1)

# zoomed-in
ggplot(data = df, aes(y = `surv2011.1624`, x = ymedian)) +
    geom_point(aes(size = NatsalLAsize), shape = 1) + scale_size_area(max_size = 10) +
    theme(legend.position = "none") + theme_bw() +
    xlab("MRP estimates") + ylab("Recorded data") +
    geom_abline(intercept = 0, slope = 1, colour="black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    # xlim(0.1,0.7) + ylim(0,1) +
    scale_y_continuous(limits=c(0.15,0.5), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0.15,0.5), breaks=seq(0,1,0.2)) +
    theme(text = element_text(size=20))


## Recorded vs mrp- subset
### bars 95%
bar <- ggplot(data = df[df$diff95,], aes(y = `surv2011.1624`, x = ymedian)) +
    geom_errorbarh(aes(xmin = ymin95, xmax = ymax95), colour = "grey") +
    geom_errorbarh(aes(xmin = ymin50, xmax = ymax50), colour = "grey", size = 1.2) +
    theme(legend.position = "none") + theme_bw() +
    xlab("MRP estimates") + ylab("Recorded data") +
    geom_abline(intercept = 0, slope = 1, colour = "black") +
    geom_point(colour = "black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    theme(text = element_text(size=20))

## Recorded vs mrp- subset bars
### label numbers 95%
num <- ggplot(data = df[df$diff95,], aes(y = `surv2011.1624`, x = ymode, label = laname.num)) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour = "black") +
  geom_text(size = 3) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    theme(text = element_text(size=20))

gridExtra::grid.arrange(bar, num, ncol = 2)


### bars 50%
bar <- ggplot(data = df[df$diff50,], aes(y = `surv2011.1624`, x = ymedian)) +
  geom_errorbarh(aes(xmin = ymin50, xmax = ymax50), colour = "grey") +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_point(colour="black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    theme(text = element_text(size=20))

### label numbers 50%
num <- ggplot(data = df[df$diff50,], aes(y = `surv2011.1624`, x = ymedian, label = laname.num)) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_text(size=3) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    theme(text = element_text(size=20))

gridExtra::grid.arrange(bar, num, ncol=2)


### bars 99%
bar <- ggplot(data = df[df$diff99, ], aes(y = `surv2011.1624`, x = ymedian)) +
  geom_errorbarh(aes(xmin = ymin99, xmax = ymax99), colour="grey") +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_point(colour="black") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    theme(text = element_text(size=20))

### label numbers 99%
num <- ggplot(data = df[df$diff99,], aes(y = `surv2011.1624`, x = ymedian, label = laname.num)) +
  theme(legend.position = "none") + theme_bw() +
  xlab("MRP estimates") + ylab("Recorded data") +
  geom_abline(intercept = 0, slope = 1, colour="black") +
  geom_text(size=3) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    xlim(0,1) + ylim(0,1) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    scale_x_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    theme(text = element_text(size=20))

gridExtra::grid.arrange(bar, num, ncol=2)


## compare blmer and WinBUGS results
### numbered scatter plot
par(mfrow=c(1,2))
plot(CTADGUM_pred_LA$LApred, CTADGUM_pred_LA$surv2011.1624,
     xlab="MRP estimates", ylab="Recorded data",
     xlim=c(0,1), ylim=c(0,1), type="n")
abline(a=0, b=1)
text(CTADGUM_pred_LA$LApred, CTADGUM_pred_LA$surv2011.1624,
     CTADGUM_pred_LA$laname.num,
     col="black", cex = 0.5)
plot(df$ymode, df$`surv2011.1624`,
     xlab="MRP estimates", ylab="Recorded data",
     xlim=c(0,1), ylim=c(0,1), type="n")
abline(a=0, b=1)
text(df$ymode, df$`surv2011.1624`,
     df$laname.num,
     col="black", cex = 0.5)



## predictions with Recorded points
# ggplot(data = df[order(df$ymedian),],aes(x = 1:nrow(df), y = ymedian)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ymin95, ymax = ymax95)) +
#   geom_errorbar(aes(ymin = ymin50, ymax = ymax50), size=1.2) +
#   geom_point(data = df[order(df$ymedian),], aes(x= 1:nrow(df), y = `surv2011.1624`, colour="#CC0000"), colour="#CC0000") +
#   theme(legend.position = "none") + theme_bw() +
#     ylab("Probability testing for chlamydia") + xlab("Local Authority")


##TODO##
## Recorded vs mrp- subset nearest point
# ggplot(data = df[df$diff,], aes(x = `surv2011.1624`, y = y)) +
#   geom_errorbar(aes(ymin = ymin95, ymax = ymax95)) +
#   geom_errorbar(aes(ymin = ymin50, ymax = ymax50), size=1.2) +
#   theme(legend.position = "none") + theme_bw() +
#   xlab("MRP estimates") + ylab("Recorded data") +
#   geom_abline(intercept = 0, slope = 1, colour="red") +
#   geom_point(colour="grey") +
#   xlim(0,1) + ylim(0,1)

# which(df[df$diff,][1,], )

# df$LA.Name[df$MRPabove]
# df$LA.Name[df$MRPbelow]

## order by variability
# 9 LAs with smaller variability than the rest
# these are the missing LAs (in the Natsal data)
# ggplot(data = df[order(df$ymax95-df$ymin95),],aes(x = 1:nrow(df),y = y)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ymin95, ymax = ymax95)) +
#   geom_errorbar(aes(ymin = ymin50, ymax = ymax50), size=1.2) + theme_bw()

## order by Recorded values
# ggplot(data = df[order(df$`surv2011.1624`),],aes(x = 1:nrow(df),y = y)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ymin95, ymax = ymax95)) +
#   geom_errorbar(aes(ymin = ymin50, ymax = ymax50), size=1.2) +
#   geom_point(data = df[order(df$`surv2011.1624`),], aes(x= 1:nrow(df), y = `surv2011.1624`, colour="red")) +
#   theme(legend.position = "none") + theme_bw()


