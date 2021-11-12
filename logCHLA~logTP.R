library(tidyverse)
library(mgcv)
library(visreg)

#get data from database
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts")
source("retrieve database stuff.R")

setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend")

#arrange data
df <- temp
df <- df[df$`CHLOROPHYLL A_OW_T` > 0,]
df$logCHLA <- log(df$`CHLOROPHYLL A_OW_T`)
df <- df[df$PHOSPHORUS_OW_T > 0,]
df$logTP <- log(df$PHOSPHORUS_OW_T)
LAKE_IDS <- unique(df$LAKE_ID) #list lakes
df <- na.omit(df)
#df$TN <- df$PHOS*df$NPratio

# #GAM
# gam1 <- gam(logCHLA~s(logTP), data=df)
# #gam2 <- gam(logCHLA~s(logTP,k=4), data=df)
# #gam3 <- gam(logCHLA~s(logTP,k=150), data=df)
# gam1.res <- resid(gam1) #residuals
# df$res.gam <- gam1.res
# summary(gam1)
# plot(gam1)

#linear model
lm1 <- lm(logCHLA~logTP, data=df)
summary(lm1)
df$res.lm <- resid(lm1)
plot.lm1 <- ggplot(df,aes(x=logTP,y=logCHLA))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title="Linear log-log model",x="log TP",y="log CHLA")
print(plot.lm1)

# GAM
# stats <- data.frame(characteristic=character(),
#                     intercept=double(),
#                     slope=double(),
#                     "p-value"=double(),
#                     rsquared=double()) #blank stat table
# cols <- c(2,8,10,14)
# for(i in cols){
#   lm.loop <- lm(res.gam ~ matrix(unlist(df[,i])), data=df)
#   pl <- ggplot(df,aes(x=matrix(unlist(df[,i])),y=res.gam))+
#     geom_point()+
#     geom_smooth(method='lm')+
#     labs(title=paste("a=",coef(lm.loop)[2],", b=",coef(lm.loop[1])),
#          x=paste(colnames(df[i])),
#          y='residuals of GAM(chl~s(TP))')
#   print(pl)
#   ggsave(paste(colnames(df[i]),"_GAM.png"))
#   stats[nrow(stats)+1,] <- c(colnames(df[i]),
#                              coef(lm.loop)[1],
#                              coef(lm.loop)[2],
#                              summary(lm.loop)$coefficients[,4],
#                              summary(lm.loop)$r.squared)
# }

#outliers
library(MASS)
df$studres <- studres(lm1) #generating studentized residuals
plot.studres <- ggplot(df,aes(y=studres,x=as.numeric(row.names(df))))+
  geom_point()+
  geom_abline(slope=0,intercept=3,color="red")+
  geom_abline(slope=0,intercept=-3,color="red")+
  labs(title="Studentized residuals") #plotting studentized residuals
print(plot.studres)
df2 <- df %>% 
  filter(between(studres,-3,3)) #remove studentized residuals outside of range -3 to 3
df.outs <- df %>% 
  filter(!between(studres,-3,3)) #outliers
plot.outs <- ggplot(data=NULL,aes(x=logCHLA,y=logTP))+
  geom_point(data=df)+
  geom_point(data=df.outs,color="red",shape="cross",size=3)+ #cross out outliers
  labs(title="Outliers removed")
print(plot.outs) #plot showing data points removed as outliers

#model without outliers
lm2 <- lm(logCHLA~logTP, data=df2)
df2$res.lm <- resid(lm1)
summary(lm2)
anova(lm2)
hist(resid(lm2)) #histogram without outliers much more normal

# variable residuals
stats <- data.frame(characteristic=character(),
                    intercept=double(),
                    slope=double(),
                    "p-value"=double(),
                    rsquared=double()) #blank stat table
cols <- c(2,8,10,14)
for(i in cols){
  lm.loop <- lm(res.lm ~ matrix(unlist(df2[,i])), data=df2) #linear model of residuals against variable
  plot <- ggplot(df2,aes(x=matrix(unlist(df2[,i])),y=res.lm))+
    geom_point()+
    geom_smooth(method='lm')+
    labs(title=paste("Equation: logCHLA =",coef(lm.loop)[2],"x logTP +",coef(lm.loop[1])),
         x=paste(colnames(df2[i])),
         y='residuals of LM(logCHLA~logTP)')
  print(plot)
  stats[nrow(stats)+1,] <- c(colnames(df2[i]),
                             coef(lm.loop),
                             summary(lm.loop)$coefficients[2,4],
                             summary(lm.loop)$r.squared)
}

#depth
depth <- read.csv("junk.sabrina.chl.predictors.csv")
temp<-depth %>%
  select(LAKE_ID,year) %>%
  arrange(desc(year)) %>%
  distinct(LAKE_ID,.keep_all = TRUE) %>%
  arrange(year,LAKE_ID)
depth<-merge(depth,temp,by=c('LAKE_ID','year'),all.x=TRUE)
lm.dep <- lm(log(CHLA)~log(PHOS),data=depth) #linear model
depth$studres <- studres(lm.dep) #studentized residuals
depth <- depth %>% 
  filter(between(studres,-3,3)) #remove outliers
lm.dep <- lm(log(CHLA)~log(PHOS),data=depth) #linear model again
depth$res <- resid(lm.dep) #residuals
depth <- na.omit(depth)
depth <- depth[depth$MAXFT>0,]
lm.dep <- lm(res~MAXFT, data=depth)
stats[nrow(stats)+1,] <- c("LAKE DEPTH",
                           coef(lm.dep),
                           summary(lm.dep)$coefficients[2,4],
                           summary(lm.dep)$r.squared) #coefficients
library(segmented)
seg.lm <- segmented(lm.dep,seg.Z=~MAXFT)
summary(seg.lm)
my.fitted <- fitted(seg.lm)
my.model <- data.frame(DEPTH = depth$MAXFT, RESIDUALS = my.fitted)
my.lines <- seg.lm$psi[, 2]
depth.plot + 
  geom_line(data = my.model, aes(x = DEPTH, y = RESIDUALS), colour = "tomato") +
  geom_vline(xintercept = my.lines, linetype = "dashed")

ggplot(data=depth,aes(x=MAXFT,y=res,group=MAXFT>16.4))+
  geom_point()+
  geom_smooth(method="lm")

#acid deposition
acid <- read.csv("junk.sabrina.chl.predictors.csv")
lm.acid <- lm(log(CHLA)~log(PHOS),data=acid) #linear model
acid$studres <- studres(lm.acid) #studentized residuals
acid <- acid %>% 
  filter(between(studres,-3,3)) #remove outliers
lm.acid <- lm(log(CHLA)~log(PHOS),data=acid) #linear model again
acid$res <- resid(lm.acid) #residuals
acid <- na.omit(acid)
lm.acid <- lm(res~acid, data=acid)
acid.plot <- ggplot(data=acid, aes(x=acid,y=res))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title=paste("Equation: logCHLA =",coef(lm.acid)[2],"x logTP +",coef(lm.acid[1])),
       x="ACID DEPOSITION",
       y='residuals of LM(logCHLA~logTP)') #plot
print(acid.plot)
stats[nrow(stats)+1,] <- c("ACID DEPOSITION",
                           coef(lm.acid),
                           summary(lm.acid)$coefficients[2,4],
                           summary(lm.acid)$r.squared) #coefficients

#precip
library(readxl)
rain <- read_xlsx("Trend_Lakes_AVG_Annual_Precip_cm-yr.xlsx")
rain <- pivot_longer(rain,2:33)
rain$name <- substr(rain$name,1,4)
colnames(rain) <- c("LAKE_ID","year","precip")
merge <- merge(rain, df2, by.y = c("year","LAKE_ID"), by.x = c("year","LAKE_ID"), all.x = TRUE)
merge <- na.omit(merge)
lm.rain <- lm(res.lm ~ precip,data=merge)
rain.plot <- ggplot(data=merge, aes(x=precip,y=res.lm))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title=paste("Equation: logCHLA =",coef(lm.rain)[2],"x logTP +",coef(lm.rain[1])),
       x="PRECIPITATION",
       y='residuals of LM(logCHLA~logTP)') #plot
print(rain.plot)
stats[nrow(stats)+1,] <- c("PRECIPITATION",
                           coef(lm.rain),
                           summary(lm.rain)$coefficients[2,4],
                           summary(lm.rain)$r.squared) #coefficients

#each lake
#loop over each lake
coeffs <- data.frame(lake=character(),
                     intercept=double(),
                     slope=double(),
                     'p-value'=double(),
                     rsquared=double()) #blank coefficient table

for (i in 1:length(LAKE_IDS)) {
  lake <- LAKE_IDS[[i]] #pick a lake
  dat <- df2[df2$LAKE_ID == lake,] #create new data with that lake
  resid.loop <- lm(logCHLA~logTP,data=dat) #linear model of chl vs tp
  dat$res.loop <- resid(resid.loop) #residuals for that lake
  lm.loop <- lm(res.loop~SAMPLE_DATE,data=dat) #linear model of residuals vs year
  co <- coef(lm.loop)
  plot <- ggplot(dat,aes(x=SAMPLE_DATE,y=res.loop))+
    geom_point()+
    geom_smooth(method='lm')+
    labs(title=paste(lake),x='year',y='residuals of LM(logCHLA~logTP)')
  print(plot) #save image
  coeffs[nrow(coeffs)+1,] <- c(lake,
                               co,
                               summary(lm.loop)$coefficients[,4],
                               summary(lm.loop)$r.squared)
}

coeffs$slope <- as.numeric(coeffs$slope)
coeffs$x <- "lakes"
slope.plot <- ggplot(coeffs,aes(x=x,y=slope)) +
  geom_boxplot(width=0.1) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5, show.legend = FALSE, aes(fill=sign,color=sign)) +
  geom_abline(slope=0,intercept=0)
print(slope.plot)

sub <- acid[acid$NPratio<1.5,]
sub2 <- acid[acid$NPratio>1.5,]
ggplot(sub,aes(x=acid,y=res,colour=year))+geom_point()+geom_smooth(method="lm")+labs(x="acid N:P<1.5")
ggplot(sub2,aes(x=acid,y=res,colour=year))+geom_point()+geom_smooth(method="lm")+labs(x="acid N:P>1.5")
summary(lm(res~acid,data=sub))
summary(lm(res~acid,data=sub2))



a<-df2[df2$`TRUE COLOR_OW_T`<=20,]
b<-df2[df2$`TRUE COLOR_OW_T`>20,]
p1 <- ggplot(data=b, aes(x=`TRUE COLOR_OW_T`,y=res.lm))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x="TRUE COLOR (high color)",
       y='residuals of LM(logCHLA~logTP)') #plot
p2 <- ggplot(data=b, aes(x=`TRUE COLOR_OW_T`)) +
  geom_density(fill="grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank(),
        axis.line=element_blank(),
        panel.background = element_blank())
require(gridExtra)
grid.arrange(p2, p1, ncol = 1, heights = c(1, 4))
