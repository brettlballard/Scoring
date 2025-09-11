#!/usr/bin/env Rscript
#Above line allows code to be run using ./DataCleaner.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(insight)#print_color function

#Collect data
fcidf <- read.csv(paste0('realdata/FCI-ARK.csv'))
fcitbbl <- as_tibble(fcidf)

fmcedf <- read.csv(paste0('realdata/FMCE-WVU.csv'))
fmcetbbl <- as_tibble(fmcedf)

kin1dpdv1df <- read.csv(paste0('realdata/Kin1D-PD-Ver1.csv'))
kin1dpdv1tbbl <- as_tibble(kin1dpdv1df)

#Define columns of interest
fcipost <- paste0('AQ',1:30,'.y')
fcipre <- paste0('AQ',1:30,'.x')
fmcepre <- paste0('FMCEPre',1:47) 
fmcepost <- paste0('FMCEPost',1:47) 
kin1dpdv1items <- c('KD1.1.V3s','KD1.10.V7ECs','KD1.8.V5ECs','KD1.11.V7JSs','KD1.12.V3ECs','KD1.14.V3s','KD1.15.V3ECs','KD1.17.V4JSs','KD1.18.V1s','KD1.48.V2JSs','KD1.19.V4ECs','KD1.20.V5JSs','KD1.23.V4JSs','KD1.32.V8ECs','KD1.40.V7JSs','KD1.38.V8ECs','KD1.18.V5ECs','KD1.52.V5JSs','KD1.45.V3ECs','KD1.43.V8ECs')

#New column names
fciItem <- paste0('Item',1:30)
fmceItem <- paste0('Item',1:47)
kin1dpdv1Item <- paste0('Item',1:20)

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('==============================Cleaned Data Set==============================\n','bold')
print_color('============================================================================\n','bold')

print_color('====================================FCI=====================================\n','bgreen')
fcipostdata <- fcitbbl %>%
	select(all_of(fcipost))
setnames(fcipostdata, old = fcipost, new = fciItem)
print(fcipostdata)
write.csv(fcipostdata, 'realdata/FCI-post.csv', row.names = FALSE)

fcipredata <- fcitbbl %>%
	select(all_of(fcipre))
setnames(fcipredata, old = fcipre, new = fciItem)
print(fcipredata)
write.csv(fcipredata, 'realdata/FCI-pre.csv', row.names = FALSE)

print_color('====================================FMCE====================================\n','bgreen')
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
write.csv(fmcepredata, 'realdata/FMCE-pre.csv', row.names = FALSE)

#Score and output posttest data
posttemp <- temp %>%
	select(all_of(fmcepost))
print(posttemp)

for (i in 1:47){
	posttemp[fmceItem[i]] <- ifelse(posttemp[[i]] == fmcekey[i],1,0)
}
fmcepostdata <- posttemp[,fmceItem]
print(fmcepostdata)
write.csv(fmcepostdata, 'realdata/FMCE-post.csv', row.names = FALSE)

#Implement Thornton Scoring for FMCE
fmceTh <- paste0('Item',c(1:4,7,'8.10',11.13,14,16:26,27.29,30:32,34,36,38,40:43))

fmcepredata['Item8.10'] <- ifelse((fmcepredata$Item8 == 1 & fmcepredata$Item9 == 1 & fmcepredata$Item10 == 1),1,0)
fmcepredata['Item11.13'] <- ifelse((fmcepredata$Item11 == 1 & fmcepredata$Item12 == 1 & fmcepredata$Item13 == 1),1,0)
fmcepredata['Item27.29'] <- ifelse((fmcepredata$Item27 == 1 & fmcepredata$Item28 == 1 & fmcepredata$Item29 == 1),1,0)
fmcepredataTh <- fmcepredata %>%
	select(all_of(fmceTh))
print(fmcepredataTh)
write.csv(fmcepredataTh, 'realdata/FMCETh-pre.csv', row.names = FALSE)

fmcepostdata['Item8.10'] <- ifelse((fmcepostdata$Item8 == 1 & fmcepostdata$Item9 == 1 & fmcepostdata$Item10 == 1),1,0)
fmcepostdata['Item11.13'] <- ifelse((fmcepostdata$Item11 == 1 & fmcepostdata$Item12 == 1 & fmcepostdata$Item13 == 1),1,0)
fmcepostdata['Item27.29'] <- ifelse((fmcepostdata$Item27 == 1 & fmcepostdata$Item28 == 1 & fmcepostdata$Item29 == 1),1,0)
fmcepostdataTh <- fmcepostdata %>%
	select(all_of(fmceTh))
print(fmcepostdata)#TEMP
print(fmcepostdataTh)
write.csv(fmcepostdataTh, 'realdata/FMCETh-post.csv', row.names = FALSE)

print_color('===============================Kin1D-PD-Ver1================================\n','bgreen')
print(kin1dpdv1tbbl)
kin1dpdv1data <- kin1dpdv1tbbl %>%
	select(all_of(c(kin1dpdv1items,'Test.Time')))
setnames(kin1dpdv1data, old = kin1dpdv1items, new = kin1dpdv1Item)
print(kin1dpdv1data)

kin1dpdv1post <- kin1dpdv1data %>%
	filter(grepl('Post',Test.Time)) %>%
	select(all_of(kin1dpdv1Item))
print(kin1dpdv1post)
write.csv(kin1dpdv1post, 'realdata/Kin1D-PD-Ver1-post.csv', row.names = FALSE)

kin1dpdv1pre <- kin1dpdv1data %>%
	filter(grepl('Pre',Test.Time)) %>%
	select(all_of(kin1dpdv1Item))
print(kin1dpdv1pre)
write.csv(kin1dpdv1pre, 'realdata/Kin1D-PD-Ver1-pre.csv', row.names = FALSE)

itemcodes <- data.frame(Old.Item.Names = gsub('s','',kin1dpdv1items), New.Item.Names = kin1dpdv1Item)
write.csv(itemcodes, 'realdata/Kin1D-PD-Ver1-ItemCodes.csv', row.names = FALSE)

