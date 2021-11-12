setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts")
source("retrieve database stuff.R")
# data$year <- as.numeric(substr(data$SAMPLE_DATE,1,4))
# temp<-data %>%
#   select(LAKE_ID,year) %>%
#   arrange(desc(year)) %>%
#   distinct(LAKE_ID,.keep_all = TRUE) %>%
#   arrange(year,LAKE_ID)
# new<-merge(data,temp,by=c('LAKE_ID','year'),all.x=TRUE)
# new<-new %>%
#   filter(!is.na(SAMPLE_DATE),
#          SAMPLE_DATE!='1899-12-31',
#          !Characteristic.Name %in% c("DEPTH","TEMPERATURE, AIR")) %>%
#   mutate(combined=paste(Characteristic.Name,INFO_TYPE,Result.Sample.Fraction,sep = "_"))  %>%
#   select(LAKE_ID,SAMPLE_DATE,combined,Result.Value,LAB_QUALIFIERS) %>%
#   mutate(Result.Value=ifelse(!is.na(LAB_QUALIFIERS)&(LAB_QUALIFIERS=="U"|LAB_QUALIFIERS=="UE"),"0",Result.Value),
#          Result.Value=trimws(Result.Value),
#          Result.Value=as.numeric(Result.Value),
#          #remove negative values
#          Result.Value>=0) %>%
#   filter(!is.na(Result.Value)) %>%
#   select(LAKE_ID,SAMPLE_DATE,combined,Result.Value) %>%
#   distinct(LAKE_ID,SAMPLE_DATE,combined,.keep_all = TRUE) %>%
#   mutate(year = format(SAMPLE_DATE, "%Y"),
#          year = as.numeric(year),
#          decade = year - year %% 10,
#          dyear=decimal_date(SAMPLE_DATE),
#          month=substr(SAMPLE_DATE,6,7),
#          year=as.integer(dyear)) %>%
#   distinct()
# #removes rows where even one value is NA
# new<-new[rowSums(is.na(new))==0,]
# new<-new %>%
#   select(LAKE_ID,SAMPLE_DATE,year,month,decade,combined,Result.Value)
# #make all fields numeric
# new<-new %>%
#   mutate(year=as.numeric(year),
#          month=as.numeric(month))
# #pivot wide
# new <- new %>%
#   pivot_wider(names_from = combined, values_from = Result.Value)
#   
df<-temp %>%
  select(LAKE_ID,year,`CHLOROPHYLL A_OW_T`,PHOSPHORUS_OW_T,`DEPTH, BOTTOM_OW_NA`)

setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend")
# depth <- read.csv("junk.max.depth.csv")

#arrange data
df <- df[df$`CHLOROPHYLL A_OW_T` > 0,]
df$logCHLA <- log(as.numeric(df$`CHLOROPHYLL A_OW_T`))
df <- df[df$PHOSPHORUS_OW_T > 0,]
df$logTP <- log(as.numeric(df$PHOSPHORUS_OW_T))
df <- df[!is.na(df$LAKE_ID),]

#linear model
lm1 <- lm(logCHLA~logTP, data=df)
summary(lm1)
df$res.lm <- resid(lm1)
plot.lm1 <- ggplot(df,aes(x=logTP,y=logCHLA))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title="Linear log-log model",x="log TP",y="log CHLA")
print(plot.lm1)


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
detach(package:MASS, unload=TRUE) #to prevent confusion

lm2 <- lm(logCHLA~logTP, data=df2)
df2$res.lm <- resid(lm2)


recent <- data.frame(LAKE_ID=as.character(),
                     year=as.numeric(),
                     depth=as.numeric(),
                     res=as.numeric())

lakes <- unique(df2$LAKE_ID)
for(i in 1:length(lakes)){
  loop <- df %>%
    select(LAKE_ID,year,`DEPTH, BOTTOM_OW_NA`,res.lm) %>%
    filter(LAKE_ID==lakes[i]) %>%
    arrange(desc(year)) 
  y <- as.numeric(loop[1,2])
  d <- mean(loop$`DEPTH, BOTTOM_OW_NA`,na.rm=TRUE)
  # d <- depth[depth$LAKE_ID==lakes[i],2]
  if(length(d)>0){
    loop$`DEPTH, BOTTOM_OW_NA` <- d
    loop <- loop[loop$year==y,]
    recent[(nrow(recent)+1):(nrow(recent)+nrow(loop)),] <- loop[1:nrow(loop),]
    }
}

lm.dep <- lm(res~depth,data=recent)
ggplot(data=recent,aes(x=depth,y=res))+
  geom_point(aes(colour=LAKE_ID))+
  geom_smooth(method="lm")
summary(lm.dep)


p1 <- ggplot(data=recent,aes(x=depth,y=res))+
  geom_point()+
  geom_smooth(method="lm") +
  labs(x='MAX DEPTH (most recent year)',
       y='residuals of LM(logCHLA~logTP)')
p2 <- ggplot(data=recent, aes(x=depth)) +
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
