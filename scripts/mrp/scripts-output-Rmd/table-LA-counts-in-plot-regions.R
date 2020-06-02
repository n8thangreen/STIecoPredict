##
## table of number of LAs in various plot regions
##


load(file = "C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/CTADGUM_pred-with-LA.RData")
load("C:/Users/ngreen1/Dropbox/small_area_chlamydia/R_code/scripts/mrp/data/cleaned-regn-input-mrpNatsal.RData")

CTADGUM_pred$surv2011.1624 <- CTADGUM_pred$surv2011.1624/0.87*0.95

## mean values
##TDOD: why isnt this the same??
# mean.Natsal <- weighted.mean(Natsal2$cttestly, w = Natsal2$total_wt, na.rm = TRUE)
mean.Natsal <- 0.35

mean.surveil <- mean(CTADGUM_pred$surv2011.1624, na.rm = TRUE)

diff.NCSPmrp <- mean.surveil - mean.Natsal

attach(CTADGUM_pred)
(surveil_above_mean.surveil <-
        sum(surv2011.1624 > mean.surveil, na.rm = T) / sum(!is.na(surv2011.1624 >
                                                                      mean.surveil)))
(surveil_below_mean.surveil <-
        sum(mean.surveil > surv2011.1624, na.rm = T) / sum(!is.na(surv2011.1624 <
                                                                      mean.surveil)))
(surveil_above_mean.Natsal  <-
        sum(surv2011.1624 > mean.Natsal, na.rm = T) / sum(!is.na(surv2011.1624 >
                                                                     mean.Natsal)))
(surveil_below_mean.Natsal  <-
        sum(mean.Natsal > surv2011.1624, na.rm = T) / sum(!is.na(surv2011.1624 <
                                                                     mean.Natsal)))
(surveil_above_mrp <-
        sum(surv2011.1624 > LApred, na.rm = T) / sum(!is.na(surv2011.1624 > LApred)))
(surveil_below_mrp <-
        sum(LApred > surv2011.1624, na.rm = T) / sum(!is.na(surv2011.1624 < LApred)))
(surveil_above_mrpadj <-
        sum(surv2011.1624 > (LApred + diff.NCSPmrp), na.rm = T) / sum(!is.na(surv2011.1624 >
                                                                                 (
                                                                                     LApred + diff.NCSPmrp
                                                                                 ))))
(surveil_below_mrpadj <-
        sum((LApred + diff.NCSPmrp) > surv2011.1624, na.rm = T) / sum(!is.na(surv2011.1624 <
                                                                                 (
                                                                                     LApred + diff.NCSPmrp
                                                                                 ))))
detach(CTADGUM_pred)

round(
    prop.table(
        table(above_mean.surveil = CTADGUM_pred$surv2011.1624 < mean.surveil,
              above_mrp = CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred))*100)

round(prop.table(table(above_mean.surveil = CTADGUM_pred$surv2011.1624 < mean.surveil,
                       above_mrp = CTADGUM_pred$surv2011.1624 < (CTADGUM_pred$LApred + diff.NCSPmrp)))*100)


plot(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred],
     CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred],
     xlim = c(0,0.6), ylim = c(0,0.6),
     col = "red",
     xlab = "", ylab = "")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred],
     CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred],
     xlim = c(0,0.6), ylim = c(0,0.6),
     col = "green")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred],
     CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred],
     xlim = c(0,0.6), ylim = c(0,0.6),
     col = "blue")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred],
     CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred],
     xlim = c(0,0.6), ylim = c(0,0.6))

# adjusted

plot(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred + diff.NCSPmrp],
     CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred + diff.NCSPmrp],
     xlim = c(0,0.6), ylim = c(0,0.6),
     col = "red",
     xlab = "", ylab = "")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred + diff.NCSPmrp],
       CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 > CTADGUM_pred$LApred + diff.NCSPmrp],
       xlim = c(0,0.6), ylim = c(0,0.6),
       col = "green")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred + diff.NCSPmrp],
       CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 > mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred + diff.NCSPmrp],
       xlim = c(0,0.6), ylim = c(0,0.6),
       col = "blue")

points(CTADGUM_pred$LApred[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred + diff.NCSPmrp],
       CTADGUM_pred$surv2011.1624[CTADGUM_pred$surv2011.1624 < mean.surveil & CTADGUM_pred$surv2011.1624 < CTADGUM_pred$LApred + diff.NCSPmrp],
       xlim = c(0,0.6), ylim = c(0,0.6))

