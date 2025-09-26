#!/usr/bin/env Rscript
#Above line allows code to be run using ./QuickUpdater.R in terminal

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
library(directlabels)#used to add labels on plots when lots of lines are used together

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying which analyses get updated with the additional calculation')
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob','fcipost','fmcethpost','kin1dpdv1post')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'noshape'=350, 'normalb'=350, 'zerob'=350, 'fcipost'=1, 'fmcethpost'=1,'kin1dpdv1'=1)
sim <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Collecting information of interest
meansets <- list()
for (name in names){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	#Collecting analysis outputs
	if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
		if (grepl('fci', name) & grepl('post', name)){
			df <- read.csv(paste0('analysisout/summary/FCI/post/AnalysisOutput1.csv'))
		}else if (grepl('fmceth', name) & grepl('post', name)){
			df <- read.csv(paste0('analysisout/summary/FMCETh/post/AnalysisOutput1.csv'))
		}else if (grepl('kin1dpdv1', name) & grepl('post', name)){
			df <- read.csv(paste0('analysisout/summary/Kin1D-PD-Ver1/post/AnalysisOutput1.csv'))
		}
	}else {
		df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'))
	}

	#Add variables of interest to output without needing to rerun all of Analysis.R
	addvec <- c()
	for (row in 1:nrow(df)){
		nit <- df[row, 'Number.Items']
		nst <- df[row, 'Number.Students.Original']
		r <- df[row, 'Number.Run']

		if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
			if (grepl('fci', name) & grepl('post', name)){
				pardf <- read.csv(paste0('analysisout/summary/FCI/post/2PLpar-',nst,'.csv'))
			}else if (grepl('fmceth', name) & grepl('post', name)){
				pardf <- read.csv(paste0('analysisout/summary/FMCETh/post/2PLpar-',nst,'.csv'))
			}else if (grepl('kin1dpdv1',name) & grepl('post', name)){
				pardf <- read.csv(paste0('analysisout/summary/Kin1D-PD-Ver1/post/2PLpar-',nst,'.csv'))
			}
		}else {
			pardf <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/2PLpar-',paste0(name,r),'.csv'))
		}
		
		corr <- cor(pardf$Est.Difficulty.2PL, pardf$Est.Discrimination.2PL, method = 'pearson')
		addvec <- c(addvec, corr)
	}

	df$CORR.2PLDiff.2PLDisc <- addvec
	if (name %in% sim){
		df <- df[c('Number.Items', 'Number.Students.Original', 'Number.Students.Removed', 'Number.Run', 'Alpha', 'Model.RMSEA', 'Model.SRMSR', 'Model.TLI', 'Model.CFI', 'RMSE.Item.Discrimination', 'RMSE.Item.Difficulty', 'RMSE.Theta', 'RMSE.EstExpSc.SimSumSc', 'RMSE.EstExpSc.WSc', 'RMSE.WSc.SimSumSc', 'RMSE.TrExpSc.SimSumSc', 'RMSE.TrExpSc.WSc', 'RMSE.TrExpSc.EstExpSc', 'R2.EstTh.SimSumSc', 'R2.EstTh.WSc', 'R2.EstExpSc.SimSumSc', 'R2.EstExpSc.WSc', 'R2.TrTh.SimSumSc', 'R2.TrTh.WSc', 'R2.TrExpSc.SimSumSc', 'R2.TrExpSc.WSc', 'CORR.2PLDiff.2PLDisc', 'CORR.EstTh.SimSumSc', 'CORR.EstTh.WSc', 'CORR.EstTh.EstExpSc', 'CORR.SimSumSc.WSc', 'CORR.SimSumSc.EstExpSc', 'CORR.WScvEstExpSc', 'CORR.TrTh.TrExpSc', 'CORR.TrTh.EstTh', 'CORR.TrTh.SimSumSc', 'CORR.TrTh.WSc', 'CORR.TrTh.EstExpSc', 'CORR.TrExpSc.EstTh', 'CORR.TrExpSc.SimSumSc', 'CORR.TrExpSc.WSc', 'CORR.TrExpSc.EstExpSc')]
	}else {
		df <- df[c('Number.Items', 'Number.Students.Original', 'Number.Students.Removed', 'Number.Run', 'Alpha', 'Model.RMSEA', 'Model.SRMSR', 'Model.TLI', 'Model.CFI', 'RMSE.EstExpSc.SimSumSc', 'RMSE.EstExpSc.WSc', 'RMSE.WSc.SimSumSc', 'R2.EstTh.SimSumSc', 'R2.EstTh.WSc', 'R2.EstExpSc.SimSumSc', 'R2.EstExpSc.WSc', 'CORR.2PLDiff.2PLDisc', 'CORR.EstTh.SimSumSc', 'CORR.EstTh.WSc', 'CORR.EstTh.EstExpSc', 'CORR.SimSumSc.WSc', 'CORR.SimSumSc.EstExpSc', 'CORR.WScvEstExpSc')]
	}
	
	#Rewriting Outputs with added column 
	if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
		if (grepl('fci', name) & grepl('post', name)){
			write.csv(df, paste0('analysisout/summary/FCI/post/AnalysisOutput1.csv'), row.names = FALSE)
		}else if (grepl('fmceth', name) & grepl('post', name)){
			write.csv(df, paste0('analysisout/summary/FMCETh/post/AnalysisOutput1.csv'), row.names = FALSE)
		}else if (grepl('kin1dpdv1', name) & grepl('post', name)){
			write.csv(df, paste0('analysisout/summary/Kin1D-PD-Ver1/post/AnalysisOutput1.csv'), row.names = FALSE)
		}
	}else {
		write.csv(df, paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'), row.names = FALSE)
	}

}



#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
