#!/usr/bin/env Rscript
#Above line allows code to be run using ./DataCleaner.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(insight)#print_color function

#Collect data
fcidf <- read.csv(paste0('rawdata/FCI-ARK.csv'))
fcitbbl <- as_tibble(fcidf)

fmcedf <- read.csv(paste0('rawdata/FMCE-WVU.csv'))
fmcetbbl <- as_tibble(fmcedf)

#Define columns of interest
fcipost <- paste0('AQ',1:30,'.y')
fcipre <- paste0('AQ',1:30,'.x')
fmcepre <- paste0('FMCEPre',1:47) 
fmcepost <- paste0('FMCEPost',1:47) 

#New column names
fciItem <- paste0('Item',1:30)
fmceItem <- paste0('Item',1:47)

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('==============================Cleaned Data Set==============================\n','bold')
print_color('============================================================================\n','bold')
fcipostdata <- fcitbbl %>%
	select(all_of(fcipost))
setnames(fcipostdata, old = fcipost, new = fciItem)
print(fcipostdata)
write.csv(fcipostdata, 'FCI-post.csv', row.names = FALSE)

fcipredata <- fcitbbl %>%
	select(all_of(fcipre))
setnames(fcipredata, old = fcipre, new = fciItem)
print(fcipredata)
write.csv(fcipredata, 'FCI-pre.csv', row.names = FALSE)

#Clean FMCE data
temp <- fmcetbbl %>%
	select(all_of(c(fmcepre,fmcepost))) %>%
	na.omit(.) %>%
	filter(if_all(-c(1), ~ . != 'BLANK')) %>%
	filter(if_all(where(is.character), ~ nchar(.x) == 1)) #%>%#filter out double selections

fmcekey <- c('B','D','F','F','D','B','B','A','A','A','A','A','A','E','E','A','E','B','B','G','E','A','B','C','B','C','A','A','A','E','E','E','E','E','A','A','A','A','E','A','F','B','D','B','B','A','A')

#Score and output pretest data
pretemp <- temp %>%
	select(all_of(fmcepre))
print(pretemp)

for (i in 1:47){
	pretemp[fmceItem[i]] <- ifelse(pretemp[[i]] == fmcekey[i],1,0)
}
fmcepredata <- pretemp[,fmceItem]
print(fmcepredata)
write.csv(fmcepredata, 'FMCE-pre.csv', row.names = FALSE)

#Score and output posttest data
posttemp <- temp %>%
	select(all_of(fmcepost))
print(posttemp)

for (i in 1:47){
	posttemp[fmceItem[i]] <- ifelse(posttemp[[i]] == fmcekey[i],1,0)
}
fmcepostdata <- posttemp[,fmceItem]
print(fmcepostdata)
write.csv(fmcepostdata, 'FMCE-post.csv', row.names = FALSE)

#Implement Thornton Scoring for FMCE
fmceTh <- paste0('Item',c(1:4,7,'8.10',11.13,14,16:26,27.29,30:32,34,36,38,40:43))

fmcepredata['Item8.10'] <- ifelse((fmcepredata$Item8 == 1 & fmcepredata$Item9 == 1 & fmcepredata$Item10 == 1),1,0)
fmcepredata['Item11.13'] <- ifelse((fmcepredata$Item11 == 1 & fmcepredata$Item12 == 1 & fmcepredata$Item13 == 1),1,0)
fmcepredata['Item27.29'] <- ifelse((fmcepredata$Item27 == 1 & fmcepredata$Item28 == 1 & fmcepredata$Item29 == 1),1,0)
fmcepredataTh <- fmcepredata %>%
	select(all_of(fmceTh))
print(fmcepredataTh)
write.csv(fmcepredataTh, 'FMCETh-pre.csv', row.names = FALSE)

fmcepostdata['Item8.10'] <- ifelse((fmcepostdata$Item8 == 1 & fmcepostdata$Item9 == 1 & fmcepostdata$Item10 == 1),1,0)
fmcepostdata['Item11.13'] <- ifelse((fmcepostdata$Item11 == 1 & fmcepostdata$Item12 == 1 & fmcepostdata$Item13 == 1),1,0)
fmcepostdata['Item27.29'] <- ifelse((fmcepostdata$Item27 == 1 & fmcepostdata$Item28 == 1 & fmcepostdata$Item29 == 1),1,0)
fmcepostdataTh <- fmcepostdata %>%
	select(all_of(fmceTh))
print(fmcepostdata)#TEMP
print(fmcepostdataTh)
write.csv(fmcepostdataTh, 'FMCETh-post.csv', row.names = FALSE)



