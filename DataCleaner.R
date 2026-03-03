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

k1df <- read.csv(paste0('realdata/K1.csv'))
k1tbbl <- as_tibble(k1df)

csemwvudf <- read.csv(paste0('realdata/CSEM-WVU-PrePost-Aggregated-Scored.csv'))
csemwvutbbl <- as_tibble(csemwvudf)

csemarkpredf <- read.csv(paste0('realdata/CSEM-ARK-Pretest-Aggregated-Scored.csv'))
csemarkpretbbl <- as_tibble(csemarkpredf)

csemarkpostdf <- read.csv(paste0('realdata/CSEM-ARK-Posttest-Aggregated-Scored.csv'))
csemarkposttbbl <- as_tibble(csemarkpostdf)

#Define columns of interest
fcipost <- paste0('AQ',1:30,'.y')
fcipre <- paste0('AQ',1:30,'.x')
fmcepre <- paste0('FMCEPre',1:43) 
fmcepost <- paste0('FMCEPost',1:43) 
k1items <- c('KD1.1.V3s','KD1.10.V7ECs','KD1.8.V5ECs','KD1.11.V7JSs','KD1.12.V3ECs','KD1.14.V3s','KD1.15.V3ECs','KD1.17.V4JSs','KD1.18.V1s','KD1.48.V2JSs','KD1.19.V4ECs','KD1.20.V5JSs','KD1.23.V4JSs','KD1.32.V8ECs','KD1.40.V7JSs','KD1.38.V8ECs','KD1.18.V5ECs','KD1.52.V5JSs','KD1.45.V3ECs','KD1.43.V8ECs')
k1ciitems <- c('KD1.14.V3s','KD1.17.V4JSs','KD1.48.V2JSs','KD1.23.V4JSs','KD1.32.V8ECs','KD1.38.V8ECs','KD1.18.V5ECs')
csemwvupre <- paste0('CSEMPre',1:32)
csemwvupost <- paste0('CSEMPost',1:32)
csemark <- paste0('P',1:32)

#New column names
fciItem <- paste0('Item',1:30)
fmceItem <- paste0('Item',1:43)
k1Item <- paste0('Item',1:20)
k1ciItem <- paste0('Item',1:7)
csemItem <- paste0('Item',1:32)

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

fmcekey <- c('B','D','F','F','D','B','B','A','A','A','A','A','A','E','E','A','E','B','B','G','E','A','B','C','B','C','A','A','A','E','E','E','E','E','A','A','A','A','E','A','F','B','D')

#Score and output pretest data
pretemp <- temp %>%
	select(all_of(fmcepre))
print(pretemp)

for (i in 1:43){
	pretemp[fmceItem[i]] <- ifelse(pretemp[[i]] == fmcekey[i],1,0)
}
fmcepredata <- pretemp[,fmceItem]
print(fmcepredata)
write.csv(fmcepredata, 'realdata/FMCE-pre.csv', row.names = FALSE)

#Score and output posttest data
posttemp <- temp %>%
	select(all_of(fmcepost))
print(posttemp)

for (i in 1:43){
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

print_color('===================================K1-20====================================\n','bgreen')
print(k1tbbl)
k1data <- k1tbbl %>%
	select(all_of(c(k1items,'Test.Time')))
setnames(k1data, old = k1items, new = k1Item)
print(k1data)

k1post <- k1data %>%
	filter(grepl('Post',Test.Time)) %>%
	select(all_of(k1Item))
print(k1post)
write.csv(k1post, 'realdata/K1-20-post.csv', row.names = FALSE)

k1pre <- k1data %>%
	filter(grepl('Pre',Test.Time)) %>%
	select(all_of(k1Item))
print(k1pre)
write.csv(k1pre, 'realdata/K1-20-pre.csv', row.names = FALSE)

itemcodes <- data.frame(Old.Item.Names = gsub('s','',k1items), New.Item.Names = k1Item)
write.csv(itemcodes, 'realdata/K1-20-ItemCodes.csv', row.names = FALSE)

print_color('===================================K1-7=====================================\n','bgreen')
print(k1tbbl)
k1cidata <- k1tbbl %>%
	select(all_of(c(k1ciitems,'Test.Time')))
setnames(k1cidata, old = k1ciitems, new = k1ciItem)
print(k1cidata)

k1cipost <- k1cidata %>%
	filter(grepl('Post',Test.Time)) %>%
	select(all_of(k1ciItem))
print(k1cipost)
write.csv(k1cipost, 'realdata/K1-7-post.csv', row.names = FALSE)

k1cipre <- k1cidata %>%
	filter(grepl('Pre',Test.Time)) %>%
	select(all_of(k1ciItem))
print(k1cipre)
write.csv(k1cipre, 'realdata/K1-7-pre.csv', row.names = FALSE)

itemcodes <- data.frame(Old.Item.Names = gsub('s','',k1ciitems), New.Item.Names = k1ciItem)
write.csv(itemcodes, 'realdata/K1-7-ItemCodes.csv', row.names = FALSE)

print_color('==================================CSEM WVU==================================\n','bgreen')
#Separate and score WVU data
temp <- csemwvutbbl %>%
	select(all_of(c(csemwvupre,csemwvupost))) %>%
	na.omit(.) %>%
	filter(if_all(-c(1), ~ . != 'BLANK')) %>%
	filter(if_all(where(is.character), ~ nchar(.x) == 1)) #%>%#filter out double selections

csemkey <- c('B','A','B','B','C','E','B','B','B','C','E','D','E','D','A','E','E','D','A','D','E','D','A','C','D','A','E','C','C','A','E','D')

#Score and output pretest data
pretemp <- temp %>%
	select(all_of(csemwvupre))
print(pretemp)

for (i in 1:32){
	pretemp[csemItem[i]] <- ifelse(pretemp[[i]] == csemkey[i],1,0)
}
csemwvupredata <- pretemp[,csemItem]
print(csemwvupredata)
write.csv(csemwvupredata, 'realdata/CSEMsam2-pre.csv', row.names = FALSE)

#Score and output posttest data
posttemp <- temp %>%
	select(all_of(csemwvupost))
print(posttemp)

for (i in 1:32){
	posttemp[csemItem[i]] <- ifelse(posttemp[[i]] == csemkey[i],1,0)
}
csemwvupostdata <- posttemp[,csemItem]
print(csemwvupostdata)
write.csv(csemwvupostdata, 'realdata/CSEMsam2-post.csv', row.names = FALSE)

print_color('==================================CSEM ARK==================================\n','bgreen')
#Get pre and post from arkansas then keep only complete matched records
pretemp <- csemarkpretbbl %>%
	select(all_of(c('StudentID',csemark))) %>%
	na.omit(.) %>%
	print()
#Investigating duplicate IDs
nID <- data.frame(table(pretemp$StudentID))
if (any(nID$Freq > 1)){
	print(nID[nID$Freq > 1,])
	print(as.data.frame(pretemp[pretemp$StudentID %in% nID$Var1[nID$Freq > 1],]))
	rmID <- c()
	for (dup in nID$Var1[nID$Freq > 1]){
		testdf <- pretemp[pretemp$StudentID == dup,]
		dupl <- duplicated(testdf)
		if (any(dupl == TRUE)){
			duplog <- TRUE
		}else {
			duplog <- FALSE
			rmID <- c(rmID, dup)
		}
		print(paste0(dup, ' is identically duplicated: ',duplog))
	}
	pretemp <- pretemp[!(pretemp$StudentID %in% rmID),]
	pretemp <- pretemp[!duplicated(pretemp),]
	print(as_tibble(pretemp))
}
posttemp <- csemarkposttbbl %>%
	select(all_of(c('StudentID',csemark))) %>%
	na.omit(.) %>%
	print()
#Investigating duplicate IDs
nID <- data.frame(table(posttemp$StudentID))
if (any(nID$Freq > 1)){
	print(nID[nID$Freq > 1,])
	print(as.data.frame(posttemp[posttemp$StudentID %in% nID$Var1[nID$Freq > 1],]))
	rmID <- c()
	for (dup in nID$Var1[nID$Freq > 1]){
		testdf <- posttemp[posttemp$StudentID == dup,]
		dupl <- duplicated(testdf)
		if (any(dupl == TRUE)){
			duplog <- TRUE
		}else {
			duplog <- FALSE
			rmID <- c(rmID, dup)
		}
		print(paste0(dup, ' is identically duplicated: ',duplog))
	}
	posttemp <- posttemp[!(posttemp$StudentID %in% rmID),]
	posttemp <- posttemp[!duplicated(posttemp),]
	print(as_tibble(posttemp))
}

commID <- intersect(pretemp$StudentID,posttemp$StudentID)
pretemp <- pretemp %>%
	filter(StudentID %in% commID) %>%
	print()

csemarkpredata <- pretemp %>%
	select(all_of(csemark))
setnames(csemarkpredata, old = csemark, new = csemItem)
print(csemarkpredata)
write.csv(csemarkpredata, 'realdata/CSEMsam1-pre.csv', row.names = FALSE)

posttemp <- posttemp %>%
	filter(StudentID %in% commID) %>%
	print()

csemarkpostdata <- posttemp %>%
	select(all_of(csemark))
setnames(csemarkpostdata, old = csemark, new = csemItem)
print(csemarkpostdata)
write.csv(csemarkpostdata, 'realdata/CSEMsam1-post.csv', row.names = FALSE)
