#!/usr/bin/env Rscript
#Above line allows code to be run using ./ZScores.R in terminal

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
parser <- arg_parser('Options for varying the correlational analysis for a run of scoring analyses')
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'noshape'=350, 'normalb'=350, 'zerob'=350)
itemiter <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
sim <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
ggshapes <- c(0:14,32:127)
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
	df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'))
	nitems <- unique(df$Number.Items)
	nstud <- unique(df$Number.Students.Original)
	nrun <- unique(df$Number.Run)

	#Will do more runs later, but using just thirty for testing	
	nitems <- 30

	#Collect specific run data of interest
	for (nit in nitems){
		for (nst in nstud){
			for (r in nrun){
			
				scoredf <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'))
			
				#Calculate z-scores of each score
				scoredf <- scoredf %>%
					mutate(True.Theta.Z = scale(True.Theta)) %>% 
					mutate(Weighted.Score.Z = scale(Scaled.Weighted.Score)) %>% 
					mutate(SimSum.Score.Z = scale(SimSum.Score))  
				
				zscoredf <- scoredf[c('True.Theta.Z','Weighted.Score.Z','SimSum.Score.Z')]
				mod <- lm(True.Theta.Z ~ Weighted.Score.Z + SimSum.Score.Z, data = zscoredf)
				modsumm <- summary(mod) 
				modcoeff <- modsumm$coefficients[-1,1]
				print(modcoeff)
				
				break#TEMP
			}
		}
	}
}




#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
