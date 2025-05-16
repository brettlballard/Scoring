#!/usr/bin/env Rscript
#Above line allows code to be run using ./IRTscorer.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the IRT scoring analysis')
parser <- add_argument(parser, "--test", help = 'test being used, options are FCI or FMCE: default is FCI',nargs='*',default='FMCE')
parser <- add_argument(parser, "--post", help = 'TRUE if posttest data is being used for FCI: default is TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--items", help = 'items chosen for subscale in single run mode if subsetting',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Redefining variable from argparser
if (arg$post){
	tt <- 'post'
}else {
	tt <- 'pre'
}

#Collect data
print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!Running ',arg$test,' ',toupper(tt),'TEST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
df <- read.csv(paste0('../rawdata/',arg$test,'-',tt,'.csv'))
data <- as_tibble(df)

#Defining Items based on argparser
arg$items <- strsplit(arg$items,',')[[1]]
if ('All' %in% arg$items){
	Item <- colnames(data)
}else {
	Item <- paste0('Item',arg$items)
}
nitems <- length(Item)

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('========================Data Set With Selected Items========================\n','bold')
print_color('============================================================================\n','bold')
data <- data %>%
	select(all_of(Item))
data$Raw.Score <- apply(data[,Item],1,sum)
print(data)
npart <- nrow(data)

##############################################################################################################
###############################################IRT ANALYSIS###################################################
##############################################################################################################

#Function to calculate item probability
EIS <- function(item, parameters, model, theta){
	if (model == '2PL'){
		a <- parameters[parameters$Items == item,]$Discrimination.2PL  
		b <- parameters[parameters$Items == item,]$Difficulty.2PL
		P <- exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	}else if (model == '3PL'){
		a <- parameters[parameters$Items == item,]$Discrimination.3PL
		b <- parameters[parameters$Items == item,]$Difficulty.3PL
		g <- parameters[parameters$Items == item,]$Guessing.3PL
		P <- g + (1 - g)*exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	}else {
		print_color('!!!!!!!!!!!!!!!!!!!!!!MODEL SELECTION NOT ALLOWED!!!!!!!!!!!!!!!!!!!!!\n','red')
		break
	}
	return(P)
}

#Function to calculate expected test score
ETS <- function(items, parameters, model, theta){
	sumvec <- c()
	for (i in items){
		p <- EIS(item = i, parameters = parameters, model = model, theta = theta)
		sumvec <- c(sumvec, p)
	}
	ets <- sum(sumvec)
	return(ets)
}

#Retrieve 2pl and 3pl parameters for each item and theta estimates
print_color('============================================================================\n','bgreen')
print_color('=================2PL and 3PL Parameter Values For Each Item=================\n','bgreen')
print_color('============================================================================\n','bgreen')

#2PL Model
irt2plmodel <- mirt(data=data[,Item], model=1, itemtype='2PL')
coeff <- coef(irt2plmodel, IRTpars=TRUE, simplify=TRUE)
print(coeff)
scores2pl <- fscores(irt2plmodel, method = 'ML', full.scores = TRUE, full.scores.SE = TRUE) 
theta2pl <- scores2pl[,1]

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
par2pldf <- data.frame(Items = Item, Discrimination.2PL = avec, Difficulty.2PL = bvec)
print(par2pldf)
write.csv(par2pldf, paste0('2PLparameters',arg$test,tt,npart,'.csv'), row.names = FALSE)

#3PL Model
irt3plmodel <- mirt(data=data[,Item], model=1, itemtype='3PL')
coeff <- coef(irt3plmodel, IRTpars=TRUE, simplify=TRUE)
print(coeff)
scores3pl <- fscores(irt3plmodel, method = 'ML', full.scores = TRUE, full.scores.SE = TRUE) 
theta3pl <- scores3pl[,1]

#Making dataframe to save IRT parameters
parameters <- unlist(coeff[1])
parcount <- 1
avec <- c()
bvec <- c()
gvec <- c()
for (i in Item){
	avec <- c(avec,parameters[parcount])
	bvec <- c(bvec,parameters[parcount+nitems])
	gvec <- c(gvec,parameters[parcount+2*nitems])
	parcount <- parcount + 1        
}
par3pldf <- data.frame(Items = Item, Discrimination.3PL = avec, Difficulty.3PL = bvec, Guessing.3PL = gvec)
print(par3pldf)
write.csv(par3pldf, paste0('3PLparameters',arg$test,tt,npart,'.csv'), row.names = FALSE)

#Calculate 2pl and 3pl expected totals for each student theta to compare with raw scores
print_color('============================================================================\n','bgreen')
print_color('================2PL and 3PL Expected Totals For Each Student================\n','bgreen')
print_color('============================================================================\n','bgreen')

score2plvec <- c()
for (th in theta2pl){
	#Expected Test Score
	ets <- ETS(items = Item, parameters = par2pldf, model = '2PL', theta = th)
	score2plvec <- c(score2plvec, ets)
}
data$ExpScore.2PL <- score2plvec

score3plvec <- c()
for (th in theta3pl){
	#Expected Test Score
	ets <- ETS(items = Item, parameters = par3pldf, model = '3PL', theta = th)
	score3plvec <- c(score3plvec, ets)
}
data$ExpScore.3PL <- score3plvec

#Converting students with perfect scores and inf thetas into perfect expected scores
data$ExpScore.2PL <- ifelse(data$Raw.Score == nitems,nitems,data$ExpScore.2PL)
data$ExpScore.3PL <- ifelse(data$Raw.Score == nitems,nitems,data$ExpScore.3PL)
print(data[,c('Raw.Score','ExpScore.2PL','ExpScore.3PL')])

#Calculating residual sum of squares for 2PL and 3PL models
resid2pl <- data$Raw.Score - data$ExpScore.2PL
resid3pl <- data$Raw.Score - data$ExpScore.3PL
SSR2pl <- sum(resid2pl**2)
SSR3pl <- sum(resid3pl**2)
print_color(paste0('The sum of squared residuals of the 2PL expected totals: ',SSR2pl,'\n'),'bold')
print_color(paste0('The sum of squared residuals of the 3PL expected totals: ',SSR3pl,'\n'),'bold')
print_color(paste0('The mean square error of the 2PL expected totals: ',(SSR2pl/npart),'\n'),'bold')
print_color(paste0('The mean square error of the 3PL expected totals: ',(SSR3pl/npart),'\n'),'bold')
print_color(paste0('The root mean square error of the 2PL expected totals: ',sqrt((SSR2pl/npart)),'\n'),'bold')
print_color(paste0('The root mean square error of the 3PL expected totals: ',sqrt((SSR3pl/npart)),'\n'),'bold')

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
temp2pl <- data[,c(Item,'ExpScore.2PL')]
temp2pl$ExpScore.2PL <- round(temp2pl$ExpScore.2PL,0)
tempfreq <- temp2pl %>%
	group_by(ExpScore.2PL) %>%
	summarize(n()) %>%
	as.data.frame() %>%
	print()
temppl2pl <- temp2pl %>%
	group_by(ExpScore.2PL) %>%
	summarize_at(Item, mean) %>%
	as.data.frame() %>%
	rename(Score = ExpScore.2PL) %>%
	print()
temppl2pl$Score.Type <- rep('2PL.Exp.Score',nrow(temppl2pl))

#3PL Expected Score
print_color('==============================3PL Expected Score============================\n','bcyan')
temp3pl <- data[,c(Item,'ExpScore.3PL')]
temp3pl$ExpScore.3PL <- round(temp3pl$ExpScore.3PL,0)
tempfreq <- temp3pl %>%
	group_by(ExpScore.3PL) %>%
	summarize(n()) %>%
	as.data.frame() %>%
	print()
temppl3pl <- temp3pl %>%
	group_by(ExpScore.3PL) %>%
	summarize_at(Item, mean) %>%
	as.data.frame() %>%
	rename(Score = ExpScore.3PL) %>%
	print()
temppl3pl$Score.Type <- rep('3PL.Exp.Score',nrow(temppl3pl))

#Merging Data
pldata <- Reduce(function(x,y) merge(x,y,all = TRUE), list(list(tempplraw),list(temppl2pl),list(temppl3pl)))

#Plotting score distribution
pdf(paste0('ItemICCs',arg$test,tt,npart,'.pdf'))
print(ggplot(data=data, aes(x=Raw.Score))+geom_histogram(alpha=.5)+labs(title='Raw Score Distribution'))
print(ggplot(data=data, aes(x=round(ExpScore.2PL,0)))+geom_histogram(alpha=.5)+labs(title='2PL Expected Score Distribution'))
print(ggplot(data=data, aes(x=round(ExpScore.3PL,0)))+geom_histogram(alpha=.5)+labs(title='3PL Expected Score Distribution'))
for (i in Item){
	print(ggplot(data=pldata, mapping=aes(x=Score,y=.data[[i]],color=Score.Type))+geom_point(size=2)+labs(title=paste0('Score vs Percentage Correct for\n',i))+scale_x_continuous(name='Score', limits=c(0,nitems))+scale_y_continuous(name='Percentage Correct', n.breaks=10, limits=c(0,1)))
}


