library(tidyverse)
library(mgcv)
library(visreg)

#get data from database
setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts")
source("Retrieve Lake Info.R")

setwd("~/OneDrive - New York State Office of Information Technology Services/Rscripts/Trend")

df<-newdata %>%
  mutate(combined=paste(CHARACTERISTIC_NAME,
                        INFORMATION_TYPE,
                        RSLT_RESULT_SAMPLE_FRACTION,
                        sep = "_"))  %>%
  select(LAKE_HISTORY_ID,
         SAMPLE_DATE,
         combined,
         RSLT_RESULT_VALUE,
         RSLT_LABORATORY_QUALIFIER,
         RSLT_VALIDATOR_QUALIFIER,
         RSLT_PROFILE_DEPTH) %>%
  mutate(RSLT_RESULT_VALUE=ifelse(!is.na(RSLT_LABORATORY_QUALIFIER)&(RSLT_LABORATORY_QUALIFIER=="U"|RSLT_LABORATORY_QUALIFIER=="UE"),"0",RSLT_RESULT_VALUE),
         RSLT_RESULT_VALUE=as.numeric(RSLT_RESULT_VALUE)) %>%
  filter(!is.na(RSLT_RESULT_VALUE),
         is.na(RSLT_VALIDATOR_QUALIFIER)|(RSLT_VALIDATOR_QUALIFIER!="R"),
         combined %in% c('CHLOROPHYLL A_OW_TOTAL',
                         'PHOSPHORUS, TOTAL_OW_TOTAL',
                         "NITROGEN, NITRATE-NITRITE_OW_TOTAL",
                         "NITROGEN, KJELDAHL, TOTAL_OW_TOTAL",
                         "NITROGEN, TOTAL_OW_TOTAL")) %>%
  select(LAKE_HISTORY_ID,SAMPLE_DATE,combined,RSLT_RESULT_VALUE) %>%
  distinct(LAKE_HISTORY_ID,SAMPLE_DATE,combined,.keep_all = TRUE) %>%
  rename(LAKE_ID=LAKE_HISTORY_ID,
         chemical_name=combined,
         result_value=RSLT_RESULT_VALUE)

df<-df %>% 
  select(LAKE_ID,SAMPLE_DATE,chemical_name,result_value) %>% 
  # pivot wide
  pivot_wider(names_from=chemical_name,
              values_from=result_value) %>% 
  mutate(`NITROGEN, TOTAL`=case_when(
    !is.na(`NITROGEN, KJELDAHL, TOTAL_OW_TOTAL`) ~
      (`NITROGEN, NITRATE-NITRITE_OW_TOTAL`+`NITROGEN, KJELDAHL, TOTAL_OW_TOTAL`),
    is.na(`NITROGEN, KJELDAHL, TOTAL_OW_TOTAL`) ~ `NITROGEN, TOTAL_OW_TOTAL`)) %>% 
  mutate(year=substr(SAMPLE_DATE, start = 1, stop = 4))

acid <- read.csv("junk.sabrina.chl.predictors.csv")

ggplot(acid,aes(x=acid,y=log(NPratio)))+
  geom_point()

df2<-merge(df,acid,by=c("LAKE_ID","year"),all.x=TRUE) %>% 
  mutate(`NPratio`=(`NITROGEN, TOTAL`/`PHOSPHORUS, TOTAL_OW_TOTAL`)) %>% 
  distinct()

ggplot(df2,aes(x=acid,y=log(NPratio)))+
  geom_point()