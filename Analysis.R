#!/usr/bin/env Rscript
#Above line allows code to be run using ./Analysis.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related
library(geomtextpath)#geom_text_segment
library(ggrepel)#geom_text_repel

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
#Kin1D-PD-Ver1 pre & post
#
parser <- add_argument(parser, "--name", help = 'name for sim flex dataset',nargs='*',default='TEST')
parser <- add_argument(parser, "--nitems", help = 'number of items being investigated: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students being investigated: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
parser <- add_argument(parser, "--run", help = 'running in run mode when TRUE',nargs='*',default=FALSE)
parser <- add_argument(parser, "--nrun", help = 'number of runs when in run mode',nargs='*',default=10)
parser <- add_argument(parser, "--rmitems", help = 'items being removed',nargs='*',default=c('NULL'))
parser <- add_argument(parser, "--useitems", help = 'items being used',nargs='*',default=c('NULL'))
parser <- add_argument(parser, "--postweights", help = 'posttest weights being used for weighted score in real data when TRUE',nargs='*',default=TRUE)
arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])

#Checking arguments and setting parameters based on them
if (arg$data == 'NOISEfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING NOISE FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'NOISE'
	tt <- 'fixed'
}else if (arg$data == 'NOISEflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING NOISE FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'NOISE'
	tt <- 'flex'
	name <- arg$name
}else if (arg$data == 'CTTfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING CTT FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CTT'
	tt <- 'fixed'
}else if (arg$data == 'CTTflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING CTT FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CTT'
	tt <- 'flex'
	name <- arg$name
}else if (arg$data == 'IRTfixed'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT FIXED ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'IRT'
	tt <- 'fixed'
}else if (arg$data == 'IRTflex'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT FLEX ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'IRT'
	tt <- 'flex'
	name <- arg$name
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
}else if (arg$data == 'Kin1D-PD-Ver1pre'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!RUNNING KIN-1D-VER1 PRE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'Kin1D-PD-Ver1'
	tt <- 'pre'
}else if (arg$data == 'Kin1D-PD-Ver1post'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!RUNNING KIN-1D-VER1 POST ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'Kin1D-PD-Ver1'
	tt <- 'post'
}else {
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!INVALID DATA ARGUMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
        break
}

if (arg$run){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING RUN ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else {
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!RUNNING SINGULAR ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
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
IIF <- function(item, parameters, theta, max = FALSE, est.par = TRUE){
	if (est.par){
		a <- parameters[parameters$Items == item,]$Est.Discrimination.2PL  
		b <- parameters[parameters$Items == item,]$Est.Difficulty.2PL
	}else {
		a <- parameters[parameters$Items == item,]$Discrimination  
		b <- parameters[parameters$Items == item,]$Difficulty
	}
	P <- exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	iif <- (a**2)*P*(1-P) 
        if (max){
		return(a*iif)
	}else {
		return(iif)
	}
}

#Function to define test information function
TIF <- function(items, parameters, theta, max = FALSE, est.par = TRUE){
        sumvec <- c()
        for (i in items){
                iif <- IIF(item = i, parameters = parameters, theta = theta, max = max, est.par = est.par)
                sumvec <- c(sumvec, iif)
        }
        tif <- sum(sumvec)
        return(tif)
}

#Function to calculate item probability
EIS <- function(item, parameters, theta, max = FALSE, est.par = TRUE){
	if (est.par){
		a <- parameters[parameters$Items == item,]$Est.Discrimination.2PL  
		b <- parameters[parameters$Items == item,]$Est.Difficulty.2PL
	}else {
		a <- parameters[parameters$Items == item,]$Discrimination  
		b <- parameters[parameters$Items == item,]$Difficulty
	}
	P <- exp(a*(theta - b))/(1 + exp(a*(theta - b)))
	if (max){
		return(a*P)
	}else{
		return(P)
	}
}

#Function to calculate expected test score
ETS <- function(items, parameters, theta, max = FALSE, est.par = TRUE){
	sumvec <- c()
	for (i in items){
		p <- EIS(item = i, parameters = parameters, theta = theta, max = max, est.par = est.par)
		sumvec <- c(sumvec, p)
	}
	ets <- sum(sumvec)
	return(ets)
}

#Function to calculate weighted test score
WSCORE <- function(itemscores, weights){
	wsvec <- itemscores * weights
	ws <- sum(wsvec)
	return(ws)
}

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Using these to help with data retrieval
realdata <- c('FCIpre','FCIpost','FMCEpre','FMCEpost','FMCEThpre','FMCEThpost','Kin1D-PD-Ver1pre','Kin1D-PD-Ver1post')
simdata <- c('NOISEfixed','NOISEflex','CTTfixed','CTTflex','IRTfixed','IRTflex')

if (arg$run){
	nrun <- arg$nrun
}else {
	nrun <- 1
}

#Data to store over many potential runs
nItems <- c()
nStud <- c()
nStudRem <- c()
RunNum <- c()
alpha <- c()
modelRMSEA <- c()
modelSRMSR <- c()
modelTLI <- c()
modelCFI <- c()
RMSEEstExpScvSimSumSc <- c()
RMSEEstExpScvWSc <- c()
RMSEWScvSimSumSc <- c()
R2EstThbySimSumSc <- c()	
R2EstThbyWSc <- c()	
R2EstThdeladdSimSumSc <- c()
R2EstThdeladdWSc <- c()
R2EstExpScbySimSumSc <- c()	
R2EstExpScbyWSc <- c()	
R2EstExpScdeladdSimSumSc <- c()
R2EstExpScdeladdWSc <- c()
CorrEstThvSimSumSc <- c()
CorrEstThvWSc <- c()
CorrEstThvEstExpSc <- c()
CorrSimSumScvWSc <- c()
CorrSimSumScvEstExpSc <- c()
CorrWScvEstExpSc <- c()

#Data stored only for IRT simulated data
if (test == 'IRT'){
	RMSETrExpScvSimSumSc <- c()
	RMSETrExpScvWSc <- c()
	RMSETrExpScvEstExpSc <- c()
	R2TrThbySimSumSc <- c()
	R2TrThbyWSc <- c()
	R2TrThdeladdSimSumSc <- c()
	R2TrThdeladdWSc <- c()
	R2TrExpScbySimSumSc <- c()
	R2TrExpScbyWSc <- c()
	R2TrExpScdeladdSimSumSc <- c()
	R2TrExpScdeladdWSc <- c()
	RMSEb <- c()
	RMSEa <- c()
	RMSEth <- c()
	CorrTrThvTrExpSc <- c()
	CorrTrThvEstTh <- c()
	CorrTrThvSimSumSc <- c()
	CorrTrThvWSc <- c()
	CorrTrThvEstExpSc <- c()
	CorrTrExpScvEstTh <- c()
	CorrTrExpScvSimSumSc <- c()
	CorrTrExpScvWSc <- c()
	CorrTrExpScvEstExpSc <- c()
}


#Collect response data
if (arg$data %in% realdata){
	df <- read.csv(paste0('realdata/',test,'-',tt,'.csv'))
}
if (arg$data %in% simdata & tt == 'fixed'){
	df <- read.csv(paste0('simdata/',tt,'/',test,'-Data.csv'))
}

if ((arg$data %in% realdata) | (arg$data %in% simdata & tt == 'fixed')){
	#Defining Items based on argparser
	Item <- colnames(df)
	Item <- Item[grepl('Item',Item)]
	arg$rmitems <- strsplit(arg$rmitems,',')[[1]]
	arg$useitems <- strsplit(arg$useitems,',')[[1]]
	if (!('NULL' %in% arg$rmitems)){
		rmitems <- paste0('Item',arg$rmitems)
		Item <- Item[!(Item %in% rmitems)]
	}
	if (!('NULL' %in% arg$useitems)){
		useitems <- paste0('Item',arg$useitems)
		Item <- Item[Item %in% useitems]
	}
	print(Item)
	numitems <- length(Item)
	numst <- nrow(df)
}

for (nit in numitems){
	for (nst in numst){
		for (r in 1:nrun){
			print_color(paste0('==============================================================================\n'),'bviolet')
                        print_color(paste0('===============================RUN NUMBER ',r,'===================================\n'),'bviolet')
                        print_color(paste0('==============================================================================\n'),'bviolet')

			#Save data
			nItems <- c(nItems, nit)
			nStud <- c(nStud, nst)
			RunNum <- c(RunNum, r)
			
			#Collect response data
			if (arg$data %in% simdata & tt == 'flex'){
				df <- read.csv(paste0('simdata/',tt,'/',test,'/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Data.csv'))
				
				#Defining Item
				Item <- paste0('Item',1:nit)
				print(Item)
			}
			
			data <- as_tibble(df)

			#Printing out the full tibble so one can see column names and data types
			print_color('============================================================================\n','bold')
			print_color('========================Data Set With Selected Items========================\n','bold')
			print_color('============================================================================\n','bold')
			data <- data %>%
				select(all_of(Item))
			data$SimSum.Score <- apply(data[,Item],1,sum)
			print(data)
			npart <- nrow(data)

			#Collect true parameter values
			if (arg$data %in% simdata){
				print_color('============================================================================\n','bold')
				print_color('=========================True Item Parameter Values=========================\n','bold')
				print_color('============================================================================\n','bold')
				if (tt == 'fixed'){
					tempdf <- read.csv(paste0('simdata/',tt,'/',test,'-Items.csv'))
				}else if (tt == 'flex'){
					tempdf <- read.csv(paste0('simdata/',tt,'/',test,'/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Items.csv'))
				}
				pardf <- tempdf
				pardf <- pardf %>%
					filter(Items %in% Item)
				print(pardf)
			}

			##############################################################################################################
			###############################################CTT ANALYSIS###################################################
			##############################################################################################################

			#Calculating alpha for this sample
			temp <- data[,Item]
			p_i <- apply(temp, 2, mean)
			itemvar <- sapply(p_i, function(x) x * (1 - x))
			kr20 <- (nit/(nit - 1)) * (1 - sum(itemvar)/var(data$SimSum.Score))
			alpha <- c(alpha, kr20)


			#Making dataframe to save classical parameters
			diffvec <- c()
			discvec <- c()
			for (i in Item){
				diffvec <- c(diffvec, mean(data[[i]]))
				discvec <- c(discvec, disc(data=data, item=i, total='SimSum.Score', perc=.75))
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
			
			#Model fit
			modfit <- M2(irt2plmodel)
			print(modfit)
			
			#Save data
			modelRMSEA <- c(modelRMSEA, modfit$RMSEA)
			modelSRMSR <- c(modelSRMSR, modfit$SRMSR)
			modelTLI <- c(modelTLI, modfit$TLI)
			modelCFI <- c(modelCFI, modfit$CFI)
			
			print(itemfit(irt2plmodel, fit_stats = c('S_X2')))
			print(coeff)

			#Save estimated thetas
			scores2pl <- fscores(irt2plmodel, method = 'ML', full.scores = TRUE, full.scores.SE = TRUE) 
			est.theta <- scores2pl[,1]
			data$Est.Theta <- est.theta
			if (test == 'IRT'){
				data$True.Theta <- df$Theta
			}

			#Removing students with Inf or -Inf from the dataset
			data <- data %>%
				filter(Est.Theta != Inf) %>%
				filter(Est.Theta != -Inf)
			npart <- nrow(data)	
			
			#Save data
			nStudRem <- c(nStudRem, (nst-nrow(data)))

			#Making dataframe to save IRT parameters
			parameters <- unlist(coeff[1])
			parcount <- 1
			avec <- c()
			bvec <- c()
			for (i in Item){
				avec <- c(avec,parameters[parcount])
				bvec <- c(bvec,parameters[parcount+nit])
				parcount <- parcount + 1        
			}
			est.par2pldf <- data.frame(Items = Item, Est.Discrimination.2PL = avec, Est.Difficulty.2PL = bvec)
			if (tt == 'flex'){
				if (!dir.exists(paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'))){dir.create(paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'), recursive = TRUE)}
				write.csv(est.par2pldf, paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/2PLpar-',paste0(name,r),'.csv'), row.names = FALSE)
			}else {
				if (!dir.exists(paste0('analysisout/summary/',test,'/',tt))){dir.create(paste0('analysisout/summary/',test,'/',tt), recursive = TRUE)}
				write.csv(est.par2pldf, paste0('analysisout/summary/',test,'/',tt,'/2PLpar-',nst,'.csv'), row.names = FALSE)
			}

			#Plotting 2PL difficulty vs 2PL discrimination
			ggplot(data=est.par2pldf, mapping=aes(x=Est.Difficulty.2PL,y=Est.Discrimination.2PL))+geom_point(size=2)+geom_text_repel(label=est.par2pldf$Items, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+labs(title='2PL Item Difficulty vs 2PL Item Discrimination')+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10, limits=c(min(est.par2pldf$Est.Difficulty.2PL),max(est.par2pldf$Est.Difficulty.2PL)))+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10, limits=c(min(est.par2pldf$Est.Discrimination.2PL),max(est.par2pldf$Est.Discrimination.2PL)))#+geom_smooth(method = lm, se = TRUE)
			if (tt == 'flex'){
				if (!dir.exists(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'))){dir.create(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'), recursive = TRUE)}
				ggsave(file=paste0('2PLDiffvDisc-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				if (!dir.exists(paste0('analysisout/plots/',test,'/',tt))){dir.create(paste0('analysisout/plots/',test,'/',tt), recursive = TRUE)}
				ggsave(file=paste0('2PLDiffvDisc-',nst,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}

			#Plotting CTT difficulty vs CTT discrimination
			ggplot(data=est.parcttdf, mapping=aes(x=Est.Difficulty.CTT,y=Est.Discrimination.CTT))+geom_point(size=2)+geom_text_repel(label=est.parcttdf$Items, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+labs(title='CTT Item Difficulty vs CTT Item Discrimination')+scale_x_continuous(name='CTT Item Difficulty', n.breaks=10, limits=c(min(est.parcttdf$Est.Difficulty.CTT),max(est.parcttdf$Est.Difficulty.CTT)))+scale_y_continuous(name='CTT Item Discrimination', n.breaks=10, limits=c(min(est.parcttdf$Est.Discrimination.CTT),max(est.parcttdf$Est.Discrimination.CTT)))#+geom_smooth(method = lm, se = TRUE)
			if (tt == 'flex'){
				ggsave(file=paste0('CTTDiffvDisc-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				ggsave(file=paste0('CTTDiffvDisc-',nst,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}

			#Print estimated IRT parameter values
			print_color('============================================================================\n','bold')
			print_color('=====================Estimated IRT Item Parameter Values====================\n','bold')
			print_color('============================================================================\n','bold')
			print(est.par2pldf)

			parlinmodel <- lm(data = est.par2pldf, Est.Discrimination.2PL ~ Est.Difficulty.2PL)
			print(summary(parlinmodel))

			#Calculating a weighted score from the 2PL item discrimination
			print_color('============================================================================\n','bgreen')
			print_color('=========================Weighted Score Calculations========================\n','bgreen')
			print_color('============================================================================\n','bgreen')
			
			#Assigning weights to the scores
			if (arg$data %in% realdata){
				if (arg$postweights){
					wdf <- read.csv(paste0('analysisout/summary/',test,'/post/2PLpar-',nst,'.csv'))
					weights <- wdf$Est.Discrimination.2PL
				}else {
					wdf <- read.csv(paste0('analysisout/summary/',test,'/pre/2PLpar-',nst,'.csv'))
					weights <- wdf$Est.Discrimination.2PL
				}
			}else {
				weights <- est.par2pldf$Est.Discrimination.2PL
			}
			
			wscorevec <- c()
			for (part in 1:npart){
				scores <- data[part,Item]
				wscore <- WSCORE(itemscores = scores, weights = weights)
				wscorevec <- c(wscorevec,wscore)
			}
			data$Weighted.Score <- wscorevec
			data$Scaled.Weighted.Score <- data$Weighted.Score *(nit / sum(weights))
			data$Diff.SWS.SimSum <- data$Scaled.Weighted.Score - data$SimSum.Score
			data$SimSum.Score.Z <- scale(data$SimSum.Score)
			data$Scaled.Weighted.Score.Z <- scale(data$Scaled.Weighted.Score)
			data$Diff.SWS.SimSum.Z <- data$Scaled.Weighted.Score.Z - data$SimSum.Score.Z
			data$SimSum.Perc <- (data$SimSum.Score / nit) * 100
			data$SWS.Perc <- (data$Scaled.Weighted.Score / nit) * 100
			data$Diff.SWS.SimSum.Perc <- data$SWS.Perc - data$SimSum.Perc

			print(head(as.data.frame(data[,c('SimSum.Perc','SWS.Perc','Diff.SWS.SimSum.Perc','SimSum.Score','Weighted.Score','Scaled.Weighted.Score','Diff.SWS.SimSum','SimSum.Score.Z','Scaled.Weighted.Score.Z','Diff.SWS.SimSum.Z')])))
			print_color(paste0('The mean of difference between simsum percentage and scaled weighted percentage: ',round(mean(data$Diff.SWS.SimSum.Perc),4),'\n'),'bold')
			print_color(paste0('The sd of difference between simsum percentage and scaled weighted percentage: ',round(sd(data$Diff.SWS.SimSum.Perc),4),'\n'),'bold')
			print_color(paste0('The mean of difference between simsum score and scaled weighted score: ',round(mean(data$Diff.SWS.SimSum),4),'\n'),'bold')
			print_color(paste0('The sd of difference between simsum score and scaled weighted score: ',round(sd(data$Diff.SWS.SimSum),4),'\n'),'bold')
			print_color(paste0('The mean of difference between simsum score-z and scaled weighted score-z: ',round(mean(data$Diff.SWS.SimSum.Z),4),'\n'),'bold')
			print_color(paste0('The sd of difference between simsum score-z and scaled weighted score-z: ',round(sd(data$Diff.SWS.SimSum.Z),4),'\n'),'bold')
			
			#Calculate 2pl expected totals for each student theta 
			print_color('============================================================================\n','bgreen')
			print_color('=============================2PL Expected Totals============================\n','bgreen')
			print_color('============================================================================\n','bgreen')

			estscore2plvec <- c()
			for (th in data$Est.Theta){
				#Expected Test Score
				ets <- ETS(items = Item, parameters = est.par2pldf, theta = th)
				estscore2plvec <- c(estscore2plvec, ets)
			}
			data$Est.ExpScore <- estscore2plvec
			
			#Comparing the three scoring methods 
			print_color('============================================================================\n','bgreen')
			print_color('===================Comparison of Differing Scoring Methods==================\n','bgreen')
			print_color('============================================================================\n','bgreen')
			scoredata <- data[,c('Est.Theta','Est.ExpScore','SimSum.Score','Scaled.Weighted.Score')]
			print(scoredata)

			print_color('===================Estimated Expected Total v SimSum Sum Score=================\n','bcyan')
			#Calculating residual sum of squares 
			resid <- scoredata$SimSum.Score - scoredata$Est.ExpScore
			SSR <- sum(resid**2)
			print_color(paste0('The root mean square error of the estimated expected total vs simsum sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')

			#Save data
			RMSEEstExpScvSimSumSc <- c(RMSEEstExpScvSimSumSc, sqrt((SSR/npart)))

			print_color('==============Estimated Expected Total v Scaled Weighted Sum Score==========\n','bcyan')
			#Calculating residual sum of squares 
			resid <- scoredata$Scaled.Weighted.Score - scoredata$Est.ExpScore
			SSR <- sum(resid**2)
			print_color(paste0('The root mean square error of the estimated expected total vs scaled weighted sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')
			
			#Save data
			RMSEEstExpScvWSc <- c(RMSEEstExpScvWSc, sqrt((SSR/npart)))

			print_color('===================Scaled Weighted Sum Score v SimSum Sum Score================\n','bcyan')
			#Calculating residual sum of squares 
			resid <- scoredata$SimSum.Score - scoredata$Scaled.Weighted.Score
			SSR <- sum(resid**2)
			print_color(paste0('The root mean square error of the scaled weighted score vs simsum sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')
			
			#Save data
			RMSEWScvSimSumSc <- c(RMSEWScvSimSumSc, sqrt((SSR/npart)))

			#Plotting true estimated expected total vs the other two above
			if (test == 'IRT'){
				#Retrieving True expected test scores
				score2plvec <- c()
				for (th in data$True.Theta){
					#True Expected Test Score
					ets <- ETS(items = Item, parameters = pardf, theta = th, est.par = FALSE)
					score2plvec <- c(score2plvec, ets)
				}
				scoredata$True.Theta <- data$True.Theta
				data$True.ExpScore <- score2plvec
				scoredata$True.ExpScore <- score2plvec

				print_color('======================True Expected Total v SimSum Sum Score===================\n','bcyan')
				#Calculating residual sum of squares 
				resid <- scoredata$SimSum.Score - scoredata$True.ExpScore
				SSR <- sum(resid**2)
				print_color(paste0('The root mean square error of the true expected total vs simsum sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')
				
				#Save data 
				RMSETrExpScvSimSumSc <- c(RMSETrExpScvSimSumSc, sqrt(SSR/npart))

				print_color('================True Expected Total v Estimated Expected Total==============\n','bcyan')
				#Calculating residual sum of squares 
				resid <- scoredata$Est.ExpScore - scoredata$True.ExpScore
				SSR <- sum(resid**2)
				print_color(paste0('The root mean square error of the true expected total vs estimated expected total: ',round(sqrt((SSR/npart)),4),'\n'),'bold')
				
				#Save data 
				RMSETrExpScvEstExpSc <- c(RMSETrExpScvEstExpSc, sqrt(SSR/npart))

				print_color('===================True Expected Total v Scaled Weighted Sum Score=================\n','bcyan')
				#Calculating residual sum of squares 
				resid <- scoredata$Scaled.Weighted.Score - scoredata$True.ExpScore
				SSR <- sum(resid**2)
				print_color(paste0('The root mean square error of the true expected total vs scaled weighted score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')
				
				#Save data 
				RMSETrExpScvWSc <- c(RMSETrExpScvWSc, sqrt(SSR/npart))

			}#end of IRT only loop

			#Saving different scores and removing rows with infite estimated theta
			scoreout <- scoredata 
			if (tt == 'flex'){
				write.csv(scoreout, paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'), row.names = FALSE)
			}else {
				write.csv(scoreout, paste0('analysisout/summary/',test,'/',tt,'/Scores-',nst,'.csv'), row.names = FALSE)
			}

			#Looking at R2 change between adding the different scores
			print_color('=====Comparing Weighted Score and SimSum Score at Predicting Estimated Theta===\n','bcyan')
			mod1 <- lm(Est.Theta ~ SimSum.Score, data = scoreout)
			mod2 <- lm(Est.Theta ~ Scaled.Weighted.Score, data = scoreout)
			mod3 <- lm(Est.Theta ~ SimSum.Score + Scaled.Weighted.Score, data = scoreout)
			addSWS1 <- summary(mod3)$r.squared - summary(mod1)$r.squared
			addSimSum1 <- summary(mod3)$r.squared - summary(mod2)$r.squared
			print_color(paste0('R^2 for simsum score only model: ',round(summary(mod1)$r.squared,4),'\n'),'bviolet')
			print_color(paste0('R^2 for scaled weighted score only model: ',round(summary(mod2)$r.squared,4),'\n'),'bviolet')
			print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS1,4),'\n'),'bviolet')
			print_color(paste0('R^2 change from adding simsum score: ',round(addSimSum1,4),'\n'),'bviolet')
			
			#Save data
		       	R2EstThbySimSumSc <- c(R2EstThbySimSumSc, summary(mod1)$r.squared)	
		       	R2EstThbyWSc <- c(R2EstThbyWSc, summary(mod2)$r.squared)	
			R2EstThdeladdSimSumSc <- c(R2EstThdeladdSimSumSc, addSimSum1)
			R2EstThdeladdWSc <- c(R2EstThdeladdWSc, addSWS1)
			
			print_color('Comparing Weighted Score and SimSum Score at Predicting Estimated Expected Score\n','bcyan')
			mod1 <- lm(Est.ExpScore ~ SimSum.Score, data = scoreout)
			mod2 <- lm(Est.ExpScore ~ Scaled.Weighted.Score, data = scoreout)
			mod3 <- lm(Est.ExpScore ~ SimSum.Score + Scaled.Weighted.Score, data = scoreout)
			addSWS2 <- summary(mod3)$r.squared - summary(mod1)$r.squared
			addSimSum2 <- summary(mod3)$r.squared - summary(mod2)$r.squared
			print_color(paste0('R^2 for simsum score only model: ',round(summary(mod1)$r.squared,4),'\n'),'bviolet')
			print_color(paste0('R^2 for scaled weighted score only model: ',round(summary(mod2)$r.squared,4),'\n'),'bviolet')
			print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS2,4),'\n'),'bviolet')
			print_color(paste0('R^2 change from adding simsum score: ',round(addSimSum2,4),'\n'),'bviolet')
			
			#Save data
		       	R2EstExpScbySimSumSc <- c(R2EstExpScbySimSumSc, summary(mod1)$r.squared)	
		       	R2EstExpScbyWSc <- c(R2EstExpScbyWSc, summary(mod2)$r.squared)	
			R2EstExpScdeladdSimSumSc <- c(R2EstExpScdeladdSimSumSc, addSimSum2)
			R2EstExpScdeladdWSc <- c(R2EstExpScdeladdWSc, addSWS2)
		

			if (test == 'IRT'){
				print_color('=======Comparing Weighted Score and SimSum Score at Predicting True Theta======\n','bcyan')
				mod1 <- lm(True.Theta ~ SimSum.Score, data = scoreout)
				mod2 <- lm(True.Theta ~ Scaled.Weighted.Score, data = scoreout)
				mod3 <- lm(True.Theta ~ SimSum.Score + Scaled.Weighted.Score, data = scoreout)
				addSWS3 <- summary(mod3)$r.squared - summary(mod1)$r.squared
				addSimSum3 <- summary(mod3)$r.squared - summary(mod2)$r.squared
				print_color(paste0('R^2 for simsum score only model: ',round(summary(mod1)$r.squared,4),'\n'),'bviolet')
				print_color(paste0('R^2 for scaled weighted score only model: ',round(summary(mod2)$r.squared,4),'\n'),'bviolet')
				print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS3,4),'\n'),'bviolet')
				print_color(paste0('R^2 change from adding simsum score: ',round(addSimSum3,4),'\n'),'bviolet')
			
				#Save data
				R2TrThbySimSumSc <- c(R2TrThbySimSumSc, summary(mod1)$r.squared)	
				R2TrThbyWSc <- c(R2TrThbyWSc, summary(mod2)$r.squared)	
				R2TrThdeladdSimSumSc <- c(R2TrThdeladdSimSumSc, addSimSum3)
				R2TrThdeladdWSc <- c(R2TrThdeladdWSc, addSWS3)
				
				print_color('==Comparing Weighted Score and SimSum Score at Predicting True Expected Score==\n','bcyan')
				mod1 <- lm(True.ExpScore ~ SimSum.Score, data = scoreout)
				mod2 <- lm(True.ExpScore ~ Scaled.Weighted.Score, data = scoreout)
				mod3 <- lm(True.ExpScore ~ SimSum.Score + Scaled.Weighted.Score, data = scoreout)
				addSWS4 <- summary(mod3)$r.squared - summary(mod1)$r.squared
				addSimSum4 <- summary(mod3)$r.squared - summary(mod2)$r.squared
				print_color(paste0('R^2 for simsum score only model: ',round(summary(mod1)$r.squared,4),'\n'),'bviolet')
				print_color(paste0('R^2 for scaled weighted score only model: ',round(summary(mod2)$r.squared,4),'\n'),'bviolet')
				print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS4,4),'\n'),'bviolet')
				print_color(paste0('R^2 change from adding simsum score: ',round(addSimSum4,4),'\n'),'bviolet')
				
				#Save data
				R2TrExpScbySimSumSc <- c(R2TrExpScbySimSumSc, summary(mod1)$r.squared)	
				R2TrExpScbyWSc <- c(R2TrExpScbyWSc, summary(mod2)$r.squared)	
				R2TrExpScdeladdSimSumSc <- c(R2TrExpScdeladdSimSumSc, addSimSum4)
				R2TrExpScdeladdWSc <- c(R2TrExpScdeladdWSc, addSWS4)
			}	
			
			#Make score ICCs 
			print_color('============================================================================\n','bgreen')
			print_color('==================================Data ICCs=================================\n','bgreen')
			print_color('============================================================================\n','bgreen')

			#SimSum Score
			print_color('==================================SimSum Score=================================\n','bcyan')
			tempsimsum <- data[,c(Item,'SimSum.Score')]
			tempfreq <- tempsimsum %>%
				group_by(SimSum.Score) %>%
				summarize(n()) %>%
				as.data.frame() %>%
				print()
			tempplsimsum <- tempsimsum %>%
				group_by(SimSum.Score) %>%
				summarize_at(Item, mean) %>%
				as.data.frame() %>%
				rename(Score = SimSum.Score) %>%
				print()
			tempplsimsum$Score.Type <- rep('SimSum.Total.Score',nrow(tempplsimsum))

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
			pldata <- Reduce(function(x,y) merge(x,y,all = TRUE), list(list(tempplsimsum),list(temppl2pl)))

			#Plotting score distribution
			if (tt == 'flex'){
				pdf(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/ItemICCs-',paste0(name,r),'.pdf'))
			}else {
				pdf(paste0('analysisout/plots/',test,'/',tt,'/ItemICCs-',nst,'.pdf'))
			}
			print(ggplot(data=data, aes(x=SimSum.Score))+geom_histogram(alpha=.5)+labs(title='SimSum Score Distribution'))
			print(ggplot(data=data, aes(x=round(Est.ExpScore,0)))+geom_histogram(alpha=.5)+labs(title='2PL Expected Score Distribution'))
			for (i in Item){
				print(ggplot(data=pldata, mapping=aes(x=Score,y=.data[[i]],color=Score.Type))+geom_point(size=2)+labs(title=paste0('Score vs Percentage Correct for\n',i))+scale_x_continuous(name='Score', limits=c(min(data$Est.ExpScore),max(data$Est.ExpScore)))+scale_y_continuous(name='Percentage Correct', n.breaks=10, limits=c(0,1)))
			}
			dev.off()

			##############################################################################################################
			##############################CORRELATIONS BETWEEN DIFFERING SCORING METHODS##################################
			##############################################################################################################
			
			#Comparing the different scores
			print_color('============================================================================\n','bgreen')
			print_color('========================Differing Scoring Procedures========================\n','bgreen')
			print_color('============================================================================\n','bgreen')

			#Theta parameter
			estth <- data$Est.Theta
			simsumscore <- data$SimSum.Score
			wscore <- data$Scaled.Weighted.Score
			estexp <- data$Est.ExpScore
			
			#Outputting correlations
			estthvsimsum <- cor(simsumscore, estth, method = 'pearson')
			estthvwscore <- cor(wscore, estth, method = 'pearson')
			estthvestexp <- cor(estexp, estth, method = 'pearson')
			simsumvwscore <- cor(simsumscore, wscore, method = 'pearson')
			simsumvestexp <- cor(simsumscore, estexp, method = 'pearson')
			wscorevestexp <- cor(estexp, wscore, method = 'pearson')
			print_color(paste0('The pearson correlation between estimated theta and simsum score: ',round(estthvsimsum,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between estimated theta and scaled weighted score: ',round(estthvwscore,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between estimated theta and estimated expected score: ',round(estthvestexp,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between simsum score and scaled weighted score: ',round(simsumvwscore,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between simsum score and estimated expected score: ',round(simsumvestexp,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between scaled weighted score and estimated expected score: ',round(wscorevestexp,4),'\n'),'bold')
		
			#Save data
			CorrEstThvSimSumSc <- c(CorrEstThvSimSumSc, estthvsimsum)
			CorrEstThvWSc <- c(CorrEstThvWSc, estthvwscore)
			CorrEstThvEstExpSc <- c(CorrEstThvEstExpSc, estthvestexp)
			CorrSimSumScvWSc <- c(CorrSimSumScvWSc, simsumvwscore)
			CorrSimSumScvEstExpSc <- c(CorrSimSumScvEstExpSc, simsumvestexp)
			CorrWScvEstExpSc <- c(CorrWScvEstExpSc, wscorevestexp)

			##############################################################################################################
			############################CORRELATIONS BETWEEN ESTIMATED AND TRUE PARAMETERS################################
			##############################################################################################################

			if (arg$data %in% simdata){
				#Compare estimated parameters and the true parameters used in data generation 
				print_color('============================================================================\n','bgreen')
				print_color('=========================Estimated v True Parameters========================\n','bgreen')
				print_color('============================================================================\n','bgreen')
				
				if (test == 'IRT'){
					#b parameter
					bresid <- pardf$Difficulty - est.par2pldf$Est.Difficulty.2PL
					bSSR <- sum(bresid**2)
					print_color(paste0('The root mean square error of the IRT difficulty parameter, b: ',round(sqrt((bSSR/npart)),4),'\n'),'bold')
					
					#Save data 
					RMSEb <- c(RMSEb, sqrt(bSSR/npart))
					
					#a parameter
					aresid <- pardf$Discrimination - est.par2pldf$Est.Discrimination.2PL
					aSSR <- sum(aresid**2)
					print_color(paste0('The root mean square error of the IRT discrimination parameter, a: ',round(sqrt((aSSR/npart)),4),'\n'),'bold')
					
					#Save data 
					RMSEa <- c(RMSEa, sqrt(aSSR/npart))

					#Theta parameter
					th <- data$True.Theta
					trexp <- data$True.ExpScore
					estth <- data$Est.Theta
					simsumscore <- data$SimSum.Score
					wscore <- data$Scaled.Weighted.Score
					estexp <- data$Est.ExpScore
					
					thresid <- th - estth
					thSSR <- sum(thresid**2)
					print_color(paste0('The root mean square error of the student theta: ',round(sqrt((thSSR/npart)),4),'\n'),'bold')
					
					#Save data 
					RMSEth <- c(RMSEth, sqrt(thSSR/npart))
					
					#Outputting correlations
					trthvtrexp <- cor(th, trexp, method = 'pearson')
					trthvestth <- cor(th, estth, method = 'pearson')
					trthvsimsum <- cor(th, simsumscore, method = 'pearson')
					trthvwscore <- cor(th, wscore, method = 'pearson')
					trthvestexp <- cor(th, estexp, method = 'pearson')
					trexpvestth <- cor(trexp, estth, method = 'pearson')
					trexpvsimsum <- cor(trexp, simsumscore, method = 'pearson')
					trexpvwscore <- cor(trexp, wscore, method = 'pearson')
					trexpvestexp <- cor(trexp, estexp, method = 'pearson')
					print_color(paste0('The pearson correlation between true theta and true expected score: ',round(trthvtrexp,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and estimated theta: ',round(trthvestth,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and simsum score: ',round(trthvsimsum,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and scaled weighted score: ',round(trthvwscore,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and expected score: ',round(trthvestexp,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and estimated theta: ',round(trexpvestth,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and simsum score: ',round(trexpvsimsum,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and scaled weighted score: ',round(trexpvwscore,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and expected score: ',round(trexpvestexp,4),'\n'),'bold')

					#Save data
					CorrTrThvTrExpSc <- c(CorrTrThvTrExpSc, trthvtrexp)
					CorrTrThvEstTh <- c(CorrTrThvEstTh, trthvestth)
					CorrTrThvSimSumSc <- c(CorrTrThvSimSumSc, trthvsimsum)
					CorrTrThvWSc <- c(CorrTrThvWSc, trthvwscore)
					CorrTrThvEstExpSc <- c(CorrTrThvEstExpSc, trthvestexp)
					CorrTrExpScvEstTh <- c(CorrTrExpScvEstTh, trexpvestth)
					CorrTrExpScvSimSumSc <- c(CorrTrExpScvSimSumSc, trexpvsimsum)
					CorrTrExpScvWSc <- c(CorrTrExpScvWSc, trexpvwscore)
					CorrTrExpScvEstExpSc <- c(CorrTrExpScvEstExpSc, trexpvestexp)
					
				}else if (test == 'CTT'){
					#Difficulty parameter
					diffresid <- pardf$Difficulty - est.parcttdf$Est.Difficulty.CTT
					diffSSR <- sum(diffresid**2)
					print_color(paste0('The root mean square error of the CTT difficulty parameter: ',round(sqrt((diffSSR/npart)),4),'\n'),'bold')
					
				}
			}#end of estimated vs true analysis
		}#end of run analysis 
	}#end of number of students loop
}#end of number of items loop

#Analysis Output
print_color('============================================================================\n','bviolet')
print_color('===============================Analysis Output==============================\n','bviolet')
print_color('============================================================================\n','bviolet')
#Outputting saved data
if (test == 'IRT'){
	out <- data.frame('Number.Items' = nItems, 'Number.Students.Original' = nStud, 'Number.Students.Removed' = nStudRem, 'Number.Run' = RunNum, 'Alpha' = alpha, 'Model.RMSEA' = modelRMSEA, 'Model.SRMSR' = modelSRMSR, 'Model.TLI' = modelTLI, 'Model.CFI' = modelCFI, 'RMSE.Item.Discrimination' = RMSEa, 'RMSE.Item.Difficulty' = RMSEb, 'RMSE.Theta' = RMSEth, 'RMSE.EstExpSc.SimSumSc' = RMSEEstExpScvSimSumSc, 'RMSE.EstExpSc.WSc' = RMSEEstExpScvWSc, 'RMSE.WSc.SimSumSc' = RMSEWScvSimSumSc, 'RMSE.TrExpSc.SimSumSc' = RMSETrExpScvSimSumSc, 'RMSE.TrExpSc.WSc' = RMSETrExpScvWSc, 'RMSE.TrExpSc.EstExpSc' = RMSETrExpScvEstExpSc, 'R2.EstTh.SimSumSc' = R2EstThbySimSumSc, 'R2.EstTh.WSc' = R2EstThbyWSc, 'R2Del.EstTh.add.SimSumSc' = R2EstThdeladdSimSumSc, 'R2Del.EstTh.add.WSc' = R2EstThdeladdWSc, 'R2.EstExpSc.SimSumSc' = R2EstExpScbySimSumSc, 'R2.EstExpSc.WSc' = R2EstExpScbyWSc, 'R2Del.EstExpSc.add.SimSumSc' = R2EstExpScdeladdSimSumSc, 'R2Del.EstExpSc.add.WSc' = R2EstExpScdeladdWSc, 'R2.TrTh.SimSumSc' = R2TrThbySimSumSc, 'R2.TrTh.WSc' = R2TrThbyWSc, 'R2Del.TrTh.add.SimSumSc' = R2TrThdeladdSimSumSc, 'R2Del.TrTh.add.WSc' = R2TrThdeladdWSc, 'R2.TrExpSc.SimSumSc' = R2TrExpScbySimSumSc, 'R2.TrExpSc.WSc' = R2TrExpScbyWSc, 'R2Del.TrExpSc.add.SimSumSc' = R2TrExpScdeladdSimSumSc, 'R2Del.TrExpSc.add.WSc' = R2TrExpScdeladdWSc, 'CORR.EstTh.SimSumSc' = CorrEstThvSimSumSc, 'CORR.EstTh.WSc' = CorrEstThvWSc, 'CORR.EstTh.EstExpSc' = CorrEstThvEstExpSc, 'CORR.SimSumSc.WSc' = CorrSimSumScvWSc, 'CORR.SimSumSc.EstExpSc' = CorrSimSumScvEstExpSc, 'CORR.WScvEstExpSc' = CorrWScvEstExpSc, 'CORR.TrTh.TrExpSc' = CorrTrThvTrExpSc, 'CORR.TrTh.EstTh' = CorrTrThvEstTh, 'CORR.TrTh.SimSumSc' = CorrTrThvSimSumSc, 'CORR.TrTh.WSc' = CorrTrThvWSc, 'CORR.TrTh.EstExpSc' = CorrTrThvEstExpSc, 'CORR.TrExpSc.EstTh' = CorrTrExpScvEstTh, 'CORR.TrExpSc.SimSumSc' = CorrTrExpScvSimSumSc, 'CORR.TrExpSc.WSc' = CorrTrExpScvWSc, 'CORR.TrExpSc.EstExpSc' = CorrTrExpScvEstExpSc)
}else {
	out <- data.frame('Number.Items' = nItems, 'Number.Students.Original' = nStud, 'Number.Students.Removed' = nStudRem, 'Number.Run' = RunNum, 'Alpha' = alpha, 'Model.RMSEA' = modelRMSEA, 'Model.SRMSR' = modelSRMSR, 'Model.TLI' = modelTLI, 'Model.CFI' = modelCFI, 'RMSE.EstExpSc.SimSumSc' = RMSEEstExpScvSimSumSc, 'RMSE.EstExpSc.WSc' = RMSEEstExpScvWSc, 'RMSE.WSc.SimSumSc' = RMSEWScvSimSumSc, 'R2.EstTh.SimSumSc' = R2EstThbySimSumSc, 'R2.EstTh.WSc' = R2EstThbyWSc, 'R2Del.EstTh.add.SimSumSc' = R2EstThdeladdSimSumSc, 'R2Del.EstTh.add.WSc' = R2EstThdeladdWSc, 'R2.EstExpSc.SimSumSc' = R2EstExpScbySimSumSc, 'R2.EstExpSc.WSc' = R2EstExpScbyWSc, 'R2Del.EstExpSc.add.SimSumSc' = R2EstExpScdeladdSimSumSc, 'R2Del.EstExpSc.add.WSc' = R2EstExpScdeladdWSc, 'CORR.EstTh.SimSumSc' = CorrEstThvSimSumSc, 'CORR.EstTh.WSc' = CorrEstThvWSc, 'CORR.EstTh.EstExpSc' = CorrEstThvEstExpSc, 'CORR.SimSumSc.WSc' = CorrSimSumScvWSc, 'CORR.SimSumSc.EstExpSc' = CorrSimSumScvEstExpSc, 'CORR.WScvEstExpSc' = CorrWScvEstExpSc)
}

#Outputting to a file for later analyses
print(as_tibble(out))
print(colnames(out))
niter <- nrow(out) 
if (tt == 'flex'){
	write.csv(out, paste0('analysisout/summary/',test,'/',tt,'/',name,'/AnalysisOutput',niter,'.csv'), row.names = FALSE)
}else {
	write.csv(out, paste0('analysisout/summary/',test,'/',tt,'/AnalysisOutput',niter,'.csv'), row.names = FALSE)
}

#Summarizing data collected over many runs
if (niter > 1){
	summcols <- c('Number.Students.Removed', 'Alpha', 'Model.RMSEA', 'Model.SRMSR', 'Model.TLI', 'Model.CFI', 'RMSE.Item.Discrimination', 'RMSE.Item.Difficulty', 'RMSE.Theta', 'RMSE.EstExpSc.SimSumSc', 'RMSE.EstExpSc.WSc', 'RMSE.WSc.SimSumSc', 'RMSE.TrExpSc.SimSumSc', 'RMSE.TrExpSc.WSc', 'RMSE.TrExpSc.EstExpSc', 'R2.EstTh.SimSumSc', 'R2.EstTh.WSc', 'R2Del.EstTh.add.SimSumSc', 'R2Del.EstTh.add.WSc', 'R2.EstExpSc.SimSumSc', 'R2.EstExpSc.WSc', 'R2Del.EstExpSc.add.SimSumSc', 'R2Del.EstExpSc.add.WSc', 'R2.TrTh.SimSumSc', 'R2.TrTh.WSc', 'R2Del.TrTh.add.SimSumSc', 'R2Del.TrTh.add.WSc', 'R2.TrExpSc.SimSumSc', 'R2.TrExpSc.WSc', 'R2Del.TrExpSc.add.SimSumSc', 'R2Del.TrExpSc.add.WSc', 'CORR.EstTh.SimSumSc', 'CORR.EstTh.WSc', 'CORR.EstTh.EstExpSc', 'CORR.SimSumSc.WSc', 'CORR.SimSumSc.EstExpSc', 'CORR.WScvEstExpSc', 'CORR.TrTh.TrExpSc', 'CORR.TrTh.EstTh', 'CORR.TrTh.SimSumSc', 'CORR.TrTh.WSc', 'CORR.TrTh.EstExpSc', 'CORR.TrExpSc.EstTh', 'CORR.TrExpSc.SimSumSc', 'CORR.TrExpSc.WSc', 'CORR.TrExpSc.EstExpSc')
	means <- c()
	stderr <- c()
	for (col in summcols){
		mn <- mean(out[[col]])
		means <- c(means, round(mn,4))
		ster <- sd(out[[col]])/sqrt(niter)
		stderr <- c(stderr, round(ster,4))
	}
	summout <- data.frame(Variable = summcols, Mean = means, Std.Error = stderr)
	print(summout)
}

#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
