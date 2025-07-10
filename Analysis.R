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
#
parser <- add_argument(parser, "--name", help = 'name for sim flex dataset',nargs='*',default='TEMP')
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
realdata <- c('FCIpre','FCIpost','FMCEpre','FMCEpost','FMCEThpre','FMCEThpost')
simdata <- c('NOISEfixed','NOISEflex','CTTfixed','CTTflex','IRTfixed','IRTflex')

if (arg$run){
	nrun <- arg$nrun
}else {
	nrun <- 1
}

for (nit in numitems){
	for (nst in numst){
		for (r in 1:nrun){
			#Collect response data
			if (arg$data %in% realdata){
				df <- read.csv(paste0('realdata/',test,'-',tt,'.csv'))
			}else if (arg$data %in% simdata){
				if (tt == 'fixed'){
					df <- read.csv(paste0('simdata/',tt,'/',test,'-Data.csv'))
				}else if (tt == 'flex'){
					df <- read.csv(paste0('simdata/',tt,'/',test,'/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Data.csv'))
				}
			}
			
			data <- as_tibble(df)

			#Defining Items based on argparser
			Item <- colnames(data)
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

			#Making dataframe to save classical parameters
			diffvec <- c()
			discvec <- c()
			for (i in Item){
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
			print(M2(irt2plmodel))
			print(itemfit(irt2plmodel, fit_stats = c('S_X2')))
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
			if (tt == 'flex'){
				if (!dir.exists(paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'))){dir.create(paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'), recursive = TRUE)}
				write.csv(est.par2pldf, paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/2PLpar-',paste0(name,r),'.csv'), row.names = FALSE)
			}else {
				if (!dir.exists(paste0('analysisout/summary/',test,'/',tt))){dir.create(paste0('analysisout/summary/',test,'/',tt), recursive = TRUE)}
				write.csv(est.par2pldf, paste0('analysisout/summary/',test,'/',tt,'/2PLpar-',npart,'.csv'), row.names = FALSE)
			}

			#Plotting 2PL difficulty vs 2PL discrimination
			ggplot(data=est.par2pldf, mapping=aes(x=Est.Difficulty.2PL,y=Est.Discrimination.2PL))+geom_point(size=2)+geom_text_repel(label=est.par2pldf$Items, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+labs(title='2PL Item Difficulty vs 2PL Item Discrimination')+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10, limits=c(min(est.par2pldf$Est.Difficulty.2PL),max(est.par2pldf$Est.Difficulty.2PL)))+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10, limits=c(min(est.par2pldf$Est.Discrimination.2PL),max(est.par2pldf$Est.Discrimination.2PL)))
			if (tt == 'flex'){
				if (!dir.exists(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'))){dir.create(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students'), recursive = TRUE)}
				ggsave(file=paste0('2PLDiffvDisc-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				if (!dir.exists(paste0('analysisout/plots/',test,'/',tt))){dir.create(paste0('analysisout/plots/',test,'/',tt), recursive = TRUE)}
				ggsave(file=paste0('2PLDiffvDisc-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}


			#Print estimated IRT parameter values
			print_color('============================================================================\n','bold')
			print_color('=====================Estimated IRT Item Parameter Values====================\n','bold')
			print_color('============================================================================\n','bold')
			print(est.par2pldf)

			#Calculating a weighted score from the 2PL item discrimination
			print_color('============================================================================\n','bgreen')
			print_color('=========================Weighted Score Calculations========================\n','bgreen')
			print_color('============================================================================\n','bgreen')
			
			#Assigning weights to the scores
			if (arg$data %in% realdata){
				if (arg$postweights){
					wdf <- read.csv(paste0('analysisout/summary/',test,'/post/2PLpar-',npart,'.csv'))
					weights <- wdf$Est.Discrimination.2PL
				}else {
					wdf <- read.csv(paste0('analysisout/summary/',test,'/pre/2PLpar-',npart,'.csv'))
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
			data$Scaled.Weighted.Score <- data$Weighted.Score *(nitems / sum(weights))
			data$Diff.SWS.Raw <- data$Scaled.Weighted.Score - data$Raw.Score
			data$Raw.Score.Z <- scale(data$Raw.Score)
			data$Scaled.Weighted.Score.Z <- scale(data$Scaled.Weighted.Score)
			data$Diff.SWS.Raw.Z <- data$Scaled.Weighted.Score.Z - data$Raw.Score.Z
			data$Raw.Perc <- (data$Raw.Score / nitems) * 100
			data$SWS.Perc <- (data$Scaled.Weighted.Score / nitems) * 100
			data$Diff.SWS.Raw.Perc <- data$SWS.Perc - data$Raw.Perc

			print(head(as.data.frame(data[,c('Raw.Perc','SWS.Perc','Diff.SWS.Raw.Perc','Raw.Score','Weighted.Score','Scaled.Weighted.Score','Diff.SWS.Raw','Raw.Score.Z','Scaled.Weighted.Score.Z','Diff.SWS.Raw.Z')])))
			print_color(paste0('The mean of difference between raw percentage and scaled weighted percentage: ',round(mean(data$Diff.SWS.Raw.Perc),4),'\n'),'bold')
			print_color(paste0('The sd of difference between raw percentage and scaled weighted percentage: ',round(sd(data$Diff.SWS.Raw.Perc),4),'\n'),'bold')
			print_color(paste0('The mean of difference between raw score and scaled weighted score: ',round(mean(data$Diff.SWS.Raw),4),'\n'),'bold')
			print_color(paste0('The sd of difference between raw score and scaled weighted score: ',round(sd(data$Diff.SWS.Raw),4),'\n'),'bold')
			print_color(paste0('The mean of difference between raw score-z and scaled weighted score-z: ',round(mean(data$Diff.SWS.Raw.Z),4),'\n'),'bold')
			print_color(paste0('The sd of difference between raw score-z and scaled weighted score-z: ',round(sd(data$Diff.SWS.Raw.Z),4),'\n'),'bold')
			
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
			
			#Converting students with perfect scores and inf thetas into perfect expected scores
			data$Est.ExpScore <- ifelse(data$Raw.Score == nitems,nitems,data$Est.ExpScore)
			print(data[,c('Raw.Score','Est.ExpScore')])

			#Comparing the three scoring methods 
			print_color('============================================================================\n','bgreen')
			print_color('===================Comparison of Differing Scoring Methods==================\n','bgreen')
			print_color('============================================================================\n','bgreen')
			scoredata <- data[,c('Est.Theta','Est.ExpScore','Raw.Score','Scaled.Weighted.Score')]
			print(scoredata)

			print_color('===================Estimated Expected Total v Raw Sum Score=================\n','bcyan')
			#Calculating residual sum of squares 
			resid <- scoredata$Raw.Score - scoredata$Est.ExpScore
			SSR <- sum(resid**2)
			print_color(paste0('The root mean square error of the estimated expected total vs raw sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')

			#Plotting estimated expected total v raw sum score
			ggplot(data=scoredata, mapping=aes(x=Raw.Score,y=Est.ExpScore))+geom_point(size=2)+labs(title=paste0('Estimated Expected Score vs Raw Score'))+scale_x_continuous(name='Raw Score')+scale_y_continuous(name='Estimated Expected Score')
			if (tt == 'flex'){
				ggsave(file=paste0('EstExpvRawSum-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				ggsave(file=paste0('EstExpvRawSum-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}

			print_color('==============Estimated Expected Total v Scaled Weighted Sum Score==========\n','bcyan')
			#Plotting estimated expected total v scaled weighted sum score
			ggplot(data=scoredata, mapping=aes(x=Scaled.Weighted.Score,y=Est.ExpScore))+geom_point(size=2)+labs(title=paste0('Estimated Expected Score vs Scaled Weighted Score'))+scale_x_continuous(name='Scaled Weighted Score')+scale_y_continuous(name='Estimated Expected Score')
			if (tt == 'flex'){
				ggsave(file=paste0('EstExpvScaledWeightSum-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				ggsave(file=paste0('EstExpvScaledWeightSum-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}

			print_color('===================Scaled Weighted Sum Score v Raw Sum Score================\n','bcyan')
			#Plotting scaled weighted sum score v raw sum score
			ggplot(data=scoredata, mapping=aes(x=Raw.Score,y=Scaled.Weighted.Score))+geom_point(size=2)+labs(title=paste0('Scaled Weighted Score vs Raw Score'))+scale_x_continuous(name='Raw Score')+scale_y_continuous(name='Scaled Weighted Score')
			if (tt == 'flex'){
				ggsave(file=paste0('ScaledWeightSumvRawSum-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
			}else {
				ggsave(file=paste0('ScaledWeightSumvRawSum-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
			}

			#Plotting true estimated expected total vs the other two above
			if ((arg$data %in% simdata) & (test == 'IRT')){
				#Retrieving True expected test scores
				score2plvec <- c()
				for (th in df$Theta){
					#Expected Test Score
					ets <- ETS(items = Item, parameters = pardf, theta = th, est.par = FALSE)
					score2plvec <- c(score2plvec, ets)
				}
				scoredata$True.Theta <- df$Theta
				data$True.ExpScore <- score2plvec
				scoredata$True.ExpScore <- score2plvec

				print_color('======================True Expected Total v Raw Sum Score===================\n','bcyan')
				#Calculating residual sum of squares 
				resid <- scoredata$Raw.Score - scoredata$True.ExpScore
				SSR <- sum(resid**2)
				print_color(paste0('The root mean square error of the true expected total vs raw sum score: ',round(sqrt((SSR/npart)),4),'\n'),'bold')

				#Plotting true expected total v raw sum score
				ggplot(data=scoredata, mapping=aes(x=Raw.Score,y=True.ExpScore))+geom_point(size=2)+labs(title=paste0('True Expected Score vs Raw Score'))+scale_x_continuous(name='Raw Score')+scale_y_continuous(name='True Expected Score')
				if (tt == 'flex'){
					ggsave(file=paste0('TrueExpvRawSum-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
				}else {
					ggsave(file=paste0('TrueExpvRawSum-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
				}

				print_color('================True Expected Total v Estimated Expected Total==============\n','bcyan')
				#Calculating residual sum of squares 
				resid <- scoredata$Est.ExpScore - scoredata$True.ExpScore
				SSR <- sum(resid**2)
				print_color(paste0('The root mean square error of the true expected total vs estimated expected total: ',round(sqrt((SSR/npart)),4),'\n'),'bold')

				#Plotting true expected total v estimated expected total
				ggplot(data=scoredata, mapping=aes(x=Est.ExpScore,y=True.ExpScore))+geom_point(size=2)+labs(title=paste0('True Expected Score vs Estimated Expected Score'))+scale_x_continuous(name='Estimated Expected Score')+scale_y_continuous(name='True Expected Score')
				if (tt == 'flex'){
					ggsave(file=paste0('TrueExpvEstExp-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
				}else {
					ggsave(file=paste0('TrueExpvEstExp-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
				}

				print_color('===================True Expected Total v Scaled Weighted Sum Score=================\n','bcyan')
				#Plotting true expected total v scaled weighted sum score
				ggplot(data=scoredata, mapping=aes(x=Est.ExpScore,y=Scaled.Weighted.Score))+geom_point(size=2)+labs(title=paste0('True Expected Score vs Scaled Weighted Score'))+scale_x_continuous(name='Scaled Weighted Score')+scale_y_continuous(name='True Expected Score')
				if (tt == 'flex'){
					ggsave(file=paste0('TrueExpvScaledWeightSum-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
				}else {
					ggsave(file=paste0('TrueExpvScaledWeightSum-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
				}
			}#end of IRT only loop

			#Saving different scores and removing rows with infite estimated theta
			scoreout <- scoredata %>%
				filter(Est.Theta != Inf) %>%
				filter(Est.Theta != -Inf)			
			if (tt == 'flex'){
				write.csv(scoreout, paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'), row.names = FALSE)
			}else {
				write.csv(scoreout, paste0('analysisout/summary/',test,'/',tt,'/Scores-',npart,'.csv'), row.names = FALSE)
			}

			#Looking at R2 change between adding the different scores
			print_color('=====Comparing Weighted Score and Raw Score at Predicting Estimated Theta===\n','bcyan')
			mod1 <- lm(Est.Theta ~ Raw.Score, data = scoreout)
			mod2 <- lm(Est.Theta ~ Scaled.Weighted.Score, data = scoreout)
			mod3 <- lm(Est.Theta ~ Raw.Score + Scaled.Weighted.Score, data = scoreout)
			addSWS <- summary(mod3)$r.squared - summary(mod1)$r.squared
			addRaw <- summary(mod3)$r.squared - summary(mod2)$r.squared
			print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS,4),'\n'),'bviolet')
			print_color(paste0('R^2 change from adding raw score: ',round(addRaw,4),'\n'),'bviolet')
			
			if ((arg$data %in% simdata) & (test == 'IRT')){
				print_color('=======Comparing Weighted Score and Raw Score at Predicting True Theta======\n','bcyan')
				mod4 <- lm(True.Theta ~ Raw.Score, data = scoreout)
				mod5 <- lm(True.Theta ~ Scaled.Weighted.Score, data = scoreout)
				mod6 <- lm(True.Theta ~ Raw.Score + Scaled.Weighted.Score, data = scoreout)
				addSWS1 <- summary(mod6)$r.squared - summary(mod4)$r.squared
				addRaw1 <- summary(mod6)$r.squared - summary(mod5)$r.squared
				print_color(paste0('R^2 change from adding scaled weighted score: ',round(addSWS1,4),'\n'),'bviolet')
				print_color(paste0('R^2 change from adding raw score: ',round(addRaw1,4),'\n'),'bviolet')
			}	

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
				pdf(paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/ItemICCs-',paste0(name,r),'.pdf'))
			}else {
				pdf(paste0('analysisout/plots/',test,'/',tt,'/ItemICCs-',npart,'.pdf'))
			}
			print(ggplot(data=data, aes(x=Raw.Score))+geom_histogram(alpha=.5)+labs(title='Raw Score Distribution'))
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
			rawscore <- data$Raw.Score
			wscore <- data$Scaled.Weighted.Score
			estexp <- data$Est.ExpScore
			infindex <- which(grepl('Inf',estth))
			
			if (length(infindex) != 0){
				newestth <- estth[-infindex]
				newrawscore <- rawscore[-infindex]
				newwscore <- wscore[-infindex]
				newestexp <- estexp[-infindex]
			}else {
				newestth <- estth
				newrawscore <- rawscore
				newwscore <- wscore
				newestexp <- estexp
			}
			
			#Outputting correlations
			estthvraw <- cor(newrawscore, newestth, method = 'pearson')
			estthvwscore <- cor(newwscore, newestth, method = 'pearson')
			estthvestexp <- cor(newestexp, newestth, method = 'pearson')
			rawvwscore <- cor(newrawscore, newwscore, method = 'pearson')
			rawvestexp <- cor(newrawscore, newestexp, method = 'pearson')
			wscorevestexp <- cor(newestexp, newwscore, method = 'pearson')
			print_color(paste0('The pearson correlation between estimated theta and raw score: ',round(estthvraw,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between estimated theta and scaled weighted score: ',round(estthvwscore,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between estimated theta and estimated expected score: ',round(estthvestexp,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between raw score and scaled weighted score: ',round(rawvwscore,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between raw score and estimated expected score: ',round(rawvestexp,4),'\n'),'bold')
			print_color(paste0('The pearson correlation between scaled weighted score and estimated expected score: ',round(wscorevestexp,4),'\n'),'bold')
			
			##############################################################################################################
			############################CORRELATIONS BETWEEN ESTIMATED AND TRUE PARAMETERS################################
			##############################################################################################################

			if (arg$data %in% simdata){
				#Compare estimated parameters and the true parameters used in data generation 
				print_color('============================================================================\n','bgreen')
				print_color('=========================Estimated v True Parameters========================\n','bgreen')
				print_color('============================================================================\n','bgreen')
				
				#PARAMETERS
				#pardf
				#est.parcttdf
				#est.par2pldf
				
				#THETAS
				#data$Est.Theta
				#df$Theta

				if (test == 'IRT'){
					#b parameter
					bresid <- pardf$Difficulty - est.par2pldf$Est.Difficulty.2PL
					bSSR <- sum(bresid**2)
					print_color(paste0('The root mean square error of the IRT difficulty parameter, b: ',round(sqrt((bSSR/npart)),4),'\n'),'bold')
					
					#a parameter
					aresid <- pardf$Discrimination - est.par2pldf$Est.Discrimination.2PL
					aSSR <- sum(aresid**2)
					print_color(paste0('The root mean square error of the IRT discrimination parameter, a: ',round(sqrt((aSSR/npart)),4),'\n'),'bold')

					#Theta parameter
					th <- df$Theta
					trexp <- data$True.ExpScore
					estth <- data$Est.Theta
					rawscore <- data$Raw.Score
					wscore <- data$Scaled.Weighted.Score
					estexp <- data$Est.ExpScore
					infindex <- which(grepl('Inf',estth))
					
					if (length(infindex) != 0){
						newth <- th[-infindex]
						newtrexp <- trexp[-infindex]
						newestth <- estth[-infindex]
						newrawscore <- rawscore[-infindex]
						newwscore <- wscore[-infindex]
						newestexp <- estexp[-infindex]
						
						#Interested in values of generated theta that led to infinities in the estimation 
						neginfindex <- which(grepl('-Inf',estth))
						posinfindex <- setdiff(infindex,neginfindex)
						print_color(paste0('The mean true student thetas that resulted in positive infinities: ',round(mean(th[posinfindex]),4),'\n'),'bgreen')
						print_color(paste0('The true student thetas that resulted in positive infinities:\n'),'bgreen')
						print(th[posinfindex])
						print_color(paste0('The mean true student thetas that resulted in negative infinities: ',round(mean(th[neginfindex]),4),'\n'),'bred')
						print_color(paste0('The true student thetas that resulted in negative infinities:\n'),'bred')
						print(th[neginfindex])
					}else {
						newth <- th
						newtrexp <- trexp
						newestth <- estth
						newrawscore <- rawscore
						newwscore <- wscore
						newestexp <- estexp
					}

					thresid <- newth - newestth
					thSSR <- sum(thresid**2)
					print_color(paste0('The root mean square error of the student theta: ',round(sqrt((thSSR/npart)),4),'\n'),'bold')
					
					#Outputting correlations
					trthvestth <- cor(newth, newestth, method = 'pearson')
					trthvraw <- cor(newth, newrawscore, method = 'pearson')
					trthvwscore <- cor(newth, newwscore, method = 'pearson')
					trthvestexp <- cor(newth, newestexp, method = 'pearson')
					trexpvestth <- cor(newtrexp, newestth, method = 'pearson')
					trexpvraw <- cor(newtrexp, newrawscore, method = 'pearson')
					trexpvwscore <- cor(newtrexp, newwscore, method = 'pearson')
					trexpvestexp <- cor(newtrexp, newestexp, method = 'pearson')
					estthvraw <- cor(newrawscore, newestth, method = 'pearson')
					print_color(paste0('The pearson correlation between true theta and estimated theta: ',round(trthvestth,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and raw score: ',round(trthvraw,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and scaled weighted score: ',round(trthvwscore,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true theta and expected score: ',round(trthvestexp,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and estimated theta: ',round(trexpvestth,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and raw score: ',round(trexpvraw,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and scaled weighted score: ',round(trexpvwscore,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between true expected score and expected score: ',round(trexpvestexp,4),'\n'),'bold')
					print_color(paste0('The pearson correlation between estimated theta and raw score: ',round(estthvraw,4),'\n'),'bold')

					corrout <- data.frame(Variable1 = c('True Theta','True Theta','True Theta','True Theta','True Expected Score','True Expected Score','True Expected Score','True Expected Score','Estimated Theta'), Variable2 = c('Estimated Theta','Raw Score','Weighted Score','Estimated Expected Score','Estimated Theta','Raw Score','Weighted Score','Estimated Expected Score','Raw Score'), Correlation = c(trthvestth,trthvraw,trthvwscore,trthvestexp,trexpvestth,trexpvraw,trexpvwscore,trexpvestexp,estthvraw))
					if (tt == 'flex'){
						write.csv(corrout, paste0('analysisout/summary/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/Correlations-',paste0(name,r),'.csv'), row.names = FALSE)
					}else {
						write.csv(corrout, paste0('analysisout/summary/',test,'/',tt,'/Correlations-',npart,'.csv'), row.names = FALSE)
					}

					#Plotting true vs estimated thetas
					plotdf <- data.frame(True.Theta = newth, Est.Theta = newestth)
					ggplot(data=plotdf, mapping=aes(x=True.Theta,y=Est.Theta))+geom_point(size=2)+labs(title=paste0('Estimated Theta vs True Theta'))+scale_x_continuous(name='True.Theta')+scale_y_continuous(name='Estimated Theta')
					if (tt == 'flex'){
						ggsave(file=paste0('TrueThvEstTh-',paste0(name,r),'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/',name,'/',nit,'items','/',nst,'students','/'))
					}else {
						ggsave(file=paste0('TrueThvEstTh-',npart,'.pdf'),path = paste0('analysisout/plots/',test,'/',tt,'/'))
					}
						

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

#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
