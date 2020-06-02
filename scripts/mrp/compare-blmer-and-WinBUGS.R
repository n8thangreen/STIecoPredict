
## stack list of random effects in to one long dataframe
df <- ldply(ranef(fit, condVar=TRUE), .fun=function(y) data.frame(rownames(y), y))

FIXEF <- fixef(fit)
names(FIXEF) <- c("ALPHA","B.WOMAN","B.STUDENT","B.IMD")
FIXEF["B.MALE"] <- -FIXEF["B.WOMAN"]

df <- rbind(data.frame(".id"="fixef","rownames.y."=names(FIXEF), "X.Intercept."=FIXEF), df)

df$rownames.y. <- as.character(df$rownames.y.)

## map values to original factor level names
df$rownames.y.[df$.id=="gor"] <- as.character(unique(merge(data.frame(df$rownames.y.[df$.id=="gor"]), LA_factor_lookup[,c("gor","region_name")],
                                                   by.x="df.rownames.y..df..id.....gor..", by.y="gor", all.x=T, all=F, sort = F))$region_name)
df$rownames.y.[df$.id=="Numerical classification"]  <- as.character(unique(merge(data.frame(df$rownames.y.[df$.id=="Numerical classification"]),
                                                                                 LA_factor_lookup[,c("Classification","Numerical classification")],
                                          by.x="df.rownames.y..df..id.....Numerical.classification..", by.y="Numerical classification", all.x=T, all=F, sort = F))$Classification)

posterior.summary$variable <- toupper(posterior.summary$variable)
df <- merge(posterior.summary, df, by.x="variable", by.y="rownames.y.", all.x = T, all.y = T)

plot(df$mode, df$X.Intercept., xlim=c(-4,4), ylim=c(-4,4))
text(df$mode, df$X.Intercept., labels = df$variable)
abline(a = 0,b = 1)
abline(h=0, lty=2)
abline(v=0, lty=2)

plot(df$mode, df$X.Intercept., xlim=c(-0.5,0.5), ylim=c(-0.5,0.5))
text(df$mode, df$X.Intercept., labels = df$variable)
abline(a = 0,b = 1)
abline(h=0, lty=2)
abline(v=0, lty=2)

# plot(df$mode+df$mode[df$variable=="ALPHA"], df$X.Intercept.+df$X.Intercept.[df$variable=="ALPHA"], xlim=c(-4,4), ylim=c(-4,4))
# text(df$mode+df$mode[df$variable=="ALPHA"], df$X.Intercept.+df$X.Intercept.[df$variable=="ALPHA"], labels = df$variable)
# abline(a = 0,b = 1)