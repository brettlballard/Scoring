#!/usr/bin/env Rscript
#Above line allows code to be run using ./Analysis.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the scoring analysis')
parser <- add_argument(parser, "--data", help = 'data being used; options are in the code',nargs='*',default='IRTflex')
#
#NOISE fixed & flex
#CTT fixed & flex
#IRT fixed & flex
#FCI pre & post
#FMCE pre & post
#FMCE Thornton pre & post
#
parser <- add_argument(parser, "--flexname", help = 'name for sim flex dataset',nargs='*',default='TEMP')
parser <- add_argument(parser, "--items", help = 'items being used',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Checking arguments and setting parameters based on them
if (arg$data == 'NOISEfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING NOISE FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'NOISE'
	tt <- 'fixed'
}else if (arg$data == 'NOISEflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING NOISE FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'NOISE'
	tt <- 'flex'
	name <- arg$flexname
}else if (arg$data == 'CTTfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING CTT FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CTT'
	tt <- 'fixed'
}else if (arg$data == 'CTTflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING CTT FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CTT'
	tt <- 'flex'
	name <- arg$flexname
}else if (arg$data == 'IRTfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'IRT'
	tt <- 'fixed'
}else if (arg$data == 'IRTflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'IRT'
	tt <- 'flex'
	name <- arg$flexname
}else if (arg$data == 'FCIpre'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FCI PRE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FCI'
	tt <- 'pre'
}else if (arg$data == 'FCIpost'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FCI POST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FCI'
	tt <- 'post'
}else if (arg$data == 'FMCEpre'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCE PRE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCE'
	tt <- 'pre'
}else if (arg$data == 'FMCEpost'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCE POST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCE'
	tt <- 'post'
}else if (arg$data == 'FMCEThpre'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCETh PRE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCETh'
	tt <- 'pre'
}else if (arg$data == 'FMCEThpost'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCETh POST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCETh'
	tt <- 'post'
}else {
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!INVALID DATA ARGUMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
        break
}

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

#Function to calculate classical item discrimination 
disc <- function(data, item, total, perc){
	d <- data
	upper <- quantile(d[[total]],probs = perc)
	upperscores <- d[d[[total]] >= upper,]
	lower <- quantile(d[[total]],probs = (1-perc))
	lowerscores <- d[d[[total]] <= lower,]
	itemdiscrimination <- mean(upperscores[[item]]) - mean(lowerscores[[item]])
	return(itemdiscrimination)
}

#Function to define item information function 
IIF <- function(item, parameters, theta, max = FALSE){
	a <- parameters[parameters$Items == item,]$Est.Discrimination.2PL  
	b <- parameters[parameters$Items == item,]$Est.Difficulty.2PL
	P <- exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	iif <- (a**2)*P*(1-P) 
        if (max){
		return(a*iif)
	}else {
		return(iif)
	}
}

#Function to define test information function
TIF <- function(items, parameters, theta, max = FALSE){
        sumvec <- c()
        for (i in items){
                iif <- IIF(item = i, parameters = parameters, theta = theta, max = max)
                sumvec <- c(sumvec, iif)
        }
        tif <- sum(sumvec)
        return(tif)
}

#Function to calculate item probability
EIS <- function(item, parameters, theta, max = FALSE){
	a <- parameters[parameters$Items == item,]$Est.Discrimination.2PL  
	b <- parameters[parameters$Items == item,]$Est.Difficulty.2PL
	P <- exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	if (max){
		return(a*P)
	}else{
		return(P)
	}
}

#Function to calculate expected test score
ETS <- function(items, parameters, theta, max = FALSE){
	sumvec <- c()
	for (i in items){
		p <- EIS(item = i, parameters = parameters, theta = theta, max = max)
		sumvec <- c(sumvec, p)
	}
	ets <- sum(sumvec)
	return(ets)
}

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Using these to help with data retrieval
realdata <- c('FCIpre','FCIpost','FMCEpre','FMCEpost','FMCEThpre','FMCEThpost')
simdata <- c('NOISEfixed','NOISEflex','CTTfixed','CTTflex','IRTfixed','IRTflex')

#Collect response data
if (arg$data %in% realdata){
	df <- read.csv(paste0('rawdata/',test,'-',tt,'.csv'))
}else if (arg$data %in% simdata){
	if (tt == 'fixed'){
		df <- read.csv(paste0('simdata/',tt,'/',test,'-Data.csv'))
	}else if (tt == 'flex'){
		df <- read.csv(paste0('simdata/',tt,'/',name,'-',test,'-Data.csv'))
	}
}

data <- as_tibble(df)

#Defining Items based on argparser
arg$items <- strsplit(arg$items,',')[[1]]
if ('All' %in% arg$items){
	Item <- colnames(data)
	Item <- Item[grepl('Item',Item)]
}else {
	Item <- paste0('Item',arg$items)
}
nitems <- length(Item)
print(Item)

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('========================Data Set With Selected Items========================\n','bold')
print_color('============================================================================\n','bold')
data <- data %>%
	select(all_of(Item))
data$Raw.Score <- apply(data[,Item],1,sum)
print(data)
npart <- nrow(data)

#Collect true parameter values
print_color('============================================================================\n','bold')
print_color('=========================True Item Parameter Values=========================\n','bold')
print_color('============================================================================\n','bold')
if (arg$data %in% simdata){
	if (tt == 'fixed'){
		df <- read.csv(paste0('simdata/',tt,'/',test,'-Items.csv'))
	}else if (tt == 'flex'){
		df <- read.csv(paste0('simdata/',tt,'/',name,'-',test,'-Items.csv'))
	}
	pardf <- df
}
pardf <- pardf %>%
	filter(Items %in% Item)
print(pardf)

##############################################################################################################
###############################################CTT ANALYSIS###################################################
##############################################################################################################

#Making dataframe to save classical parameters
diffvec <- c()
discvec <- c()
for (i in Item){
#disc <- function(data, item, total, perc){
	diffvec <- c(diffvec, mean(data[[i]]))
	discvec <- c(discvec, disc(data=data, item=i, total='Raw.Score', perc=.75))
}
est.parcttdf <- data.frame(Items = Item, Est.Difficulty.CTT = diffvec, Est.Discrimination.CTT = discvec)

#Print estimated CTT parameter values
print_color('============================================================================\n','bold')
print_color('=====================Estimated CTT Item Parameter Values====================\n','bold')
print_color('============================================================================\n','bold')
print(est.parcttdf)

##############################################################################################################
###############################################IRT ANALYSIS###################################################
##############################################################################################################

#Retrieve 2pl parameters for each item and theta estimates from data
print_color('============================================================================\n','bgreen')
print_color('=====================2PL Parameter Values For Each Item=====================\n','bgreen')
print_color('============================================================================\n','bgreen')

#2PL Model
irt2plmodel <- mirt(data=data[,Item], model=1, itemtype='2PL')
coeff <- coef(irt2plmodel, IRTpars=TRUE, simplify=TRUE)
print(coeff)

#Save estimated thetas
scores2pl <- fscores(irt2plmodel, method = 'ML', full.scores = TRUE, full.scores.SE = TRUE) 
est.theta <- scores2pl[,1]
data$Est.Theta <- est.theta

#Making dataframe to save IRT parameters
parameters <- unlist(coeff[1])
parcount <- 1
avec <- c()
bvec <- c()
for (i in Item){
	avec <- c(avec,parameters[parcount])
	bvec <- c(bvec,parameters[parcount+nitems])
	parcount <- parcount + 1        
}
est.par2pldf <- data.frame(Items = Item, Est.Discrimination.2PL = avec, Est.Difficulty.2PL = bvec)
print(est.par2pldf)
if (tt == 'flex'){
	write.csv(est.par2pldf, paste0('analysisout/summary/2PLpar-',name,'-',test,tt,npart,'.csv'), row.names = FALSE)
}else {
	write.csv(est.par2pldf, paste0('analysisout/summary/2PLpar-',test,tt,npart,'.csv'), row.names = FALSE)
}

#Print estimated IRT parameter values
print_color('============================================================================\n','bold')
print_color('=====================Estimated IRT Item Parameter Values====================\n','bold')
print_color('============================================================================\n','bold')
print(est.par2pldf)

#Calculate 2pl expected totals for each student theta to compare with raw scores
print_color('============================================================================\n','bgreen')
print_color('====================2PL Expected Totals For Each Student====================\n','bgreen')
print_color('============================================================================\n','bgreen')

score2plvec <- c()
for (th in data$Est.Theta){
	#Expected Test Score
	ets <- ETS(items = Item, parameters = est.par2pldf, theta = th)
	score2plvec <- c(score2plvec, ets)
}
data$Est.ExpScore <- score2plvec

#Converting students with perfect scores and inf thetas into perfect expected scores
data$Est.ExpScore <- ifelse(data$Raw.Score == nitems,nitems,data$Est.ExpScore)
print(data[,c('Raw.Score','Est.ExpScore')])

#Calculating residual sum of squares for 2PL model
resid2pl <- data$Raw.Score - data$Est.ExpScore
SSR2pl <- sum(resid2pl**2)
print_color(paste0('The sum of squared residuals of the 2PL expected totals: ',SSR2pl,'\n'),'bold')
print_color(paste0('The mean square error of the 2PL expected totals: ',(SSR2pl/npart),'\n'),'bold')
print_color(paste0('The root mean square error of the 2PL expected totals: ',sqrt((SSR2pl/npart)),'\n'),'bold')

#Make score ICCs 
print_color('============================================================================\n','bgreen')
print_color('==================================Data ICCs=================================\n','bgreen')
print_color('============================================================================\n','bgreen')

#Raw Score
print_color('==================================Raw Score=================================\n','bcyan')
tempraw <- data[,c(Item,'Raw.Score')]
tempfreq <- tempraw %>%
	group_by(Raw.Score) %>%
	summarize(n()) %>%
	as.data.frame() %>%
	print()
tempplraw <- tempraw %>%
	group_by(Raw.Score) %>%
	summarize_at(Item, mean) %>%
	as.data.frame() %>%
	rename(Score = Raw.Score) %>%
	print()
tempplraw$Score.Type <- rep('Raw.Total.Score',nrow(tempplraw))

#2PL Expected Score
print_color('==============================2PL Expected Score============================\n','bcyan')
temp2pl <- data[,c(Item,'Est.ExpScore')]
temp2pl$Est.ExpScore <- round(temp2pl$Est.ExpScore,0)
tempfreq <- temp2pl %>%
	group_by(Est.ExpScore) %>%
	summarize(n()) %>%
	as.data.frame() %>%
	print()
temppl2pl <- temp2pl %>%
	group_by(Est.ExpScore) %>%
	summarize_at(Item, mean) %>%
	as.data.frame() %>%
	rename(Score = Est.ExpScore) %>%
	print()
temppl2pl$Score.Type <- rep('Est.Exp.Score',nrow(temppl2pl))

#Merging Data
pldata <- Reduce(function(x,y) merge(x,y,all = TRUE), list(list(tempplraw),list(temppl2pl)))

#Plotting score distribution
if (tt == 'flex'){
	pdf(paste0('analysisout/plots/ItemICCs-',name,'-',test,tt,npart,'.pdf'))
}else {
	pdf(paste0('analysisout/plots/ItemICCs-',test,tt,npart,'.pdf'))
}
print(ggplot(data=data, aes(x=Raw.Score))+geom_histogram(alpha=.5)+labs(title='Raw Score Distribution'))
print(ggplot(data=data, aes(x=round(Est.ExpScore,0)))+geom_histogram(alpha=.5)+labs(title='2PL Expected Score Distribution'))
for (i in Item){
	print(ggplot(data=pldata, mapping=aes(x=Score,y=.data[[i]],color=Score.Type))+geom_point(size=2)+labs(title=paste0('Score vs Percentage Correct for\n',i))+scale_x_continuous(name='Score', limits=c(0,nitems))+scale_y_continuous(name='Percentage Correct', n.breaks=10, limits=c(0,1)))
}


