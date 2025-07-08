#!/usr/bin/env Rscript
#Above line allows code to be run using ./CorrAnalysis.R in terminal

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

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the correlational analysis for a run of scoring analyses')
parser <- add_argument(parser, "--name", help = 'name for set of runs being investigated',nargs='*',default='TEMP')
parser <- add_argument(parser, "--nrun", help = 'how many files in run length',nargs='*',default=10)
parser <- add_argument(parser, "--nitems", help = 'number of items being investigated: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students being investigated: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
arg <- parse_args(parser)

#Resetting argument parameters
name <- arg$name
nrun <- arg$nrun

numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
nums <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])

print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Collecting information of interest
nsvec <- c()
nitemsvec <- c() 
truethvestth <- c()
truethvrawscore <- c()
truethvwscore <- c()
truethvestexp <- c()
trexpvestth <- c()
trexpvrawscore <- c()
trexpvwscore <- c()
trexpvestexp <- c()
estthvrawscore <- c()
propgvec <- c()
for (nit in numitems){
	for (nst in nums){
		for (r in 1:nrun){
			nitemsvec <- c(nitemsvec,nit)
			nsvec <- c(nsvec,nst)
			
			#Collecting correlations
			corr <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/','Correlations-',paste0(name,r),'.csv'))
			truethvestth <- c(truethvestth,corr[(corr$Variable1 == 'True Theta' & corr$Variable2 == 'Estimated Theta'),]$Correlation)
			truethvrawscore <- c(truethvrawscore,corr[(corr$Variable1 == 'True Theta' & corr$Variable2 == 'Raw Score'),]$Correlation)
			truethvwscore <- c(truethvwscore,corr[(corr$Variable1 == 'True Theta' & corr$Variable2 == 'Weighted Score'),]$Correlation)
			truethvestexp <- c(truethvestexp,corr[(corr$Variable1 == 'True Theta' & corr$Variable2 == 'Estimated Expected Score'),]$Correlation)
			trexpvestth <- c(trexpvestth,corr[(corr$Variable1 == 'True Expected Score' & corr$Variable2 == 'Estimated Theta'),]$Correlation)
			trexpvrawscore <- c(trexpvrawscore,corr[(corr$Variable1 == 'True Expected Score' & corr$Variable2 == 'Raw Score'),]$Correlation)
			trexpvwscore <- c(trexpvwscore,corr[(corr$Variable1 == 'True Expected Score' & corr$Variable2 == 'Weighted Score'),]$Correlation)
			trexpvestexp <- c(trexpvestexp,corr[(corr$Variable1 == 'True Expected Score' & corr$Variable2 == 'Estimated Expected Score'),]$Correlation)
			estthvrawscore <- c(estthvrawscore,corr[(corr$Variable1 == 'Estimated Theta' & corr$Variable2 == 'Raw Score'),]$Correlation)
			
			#Collecting the proportion of good items 
			itemdf <- read.csv(paste0('simdata/flex/IRT/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Items.csv'))
			propg <- nrow(itemdf[itemdf$Discrimination == 3,])/nrow(itemdf)
			propgvec <- c(propgvec,propg)
		}
	}
}

#TODO: Add weighted score graphs as well

#Plotting correlation differences for things of interest
pldf <- data.frame(Number.Students = nsvec, Number.Items = nitemsvec, Proportion.Good.Items = propgvec, TrueTheta.v.EstTheta = truethvestth, TrueTheta.v.RawScore = truethvrawscore, TrueTheta.v.WScore = truethvwscore, TrueTheta.v.EstExpScore = truethvestexp, TrueExpScore.v.EstTheta = trexpvestth, TrueExpScore.v.RawScore = trexpvrawscore, TrueExpScore.v.WScore = trexpvwscore, TrueExpScore.v.EstExpScore = trexpvestexp, EstTheta.v.RawScore = estthvrawscore)

pldf$Proportion.Good.Items <- sapply(pldf$Proportion.Good.Items, function(x) round(x,2))

#Make columns to plot
pldf <- pldf %>%
	mutate(Abs.Difference1 = TrueTheta.v.EstExpScore - TrueTheta.v.RawScore) %>%
	mutate(Abs.Difference2 = TrueExpScore.v.EstExpScore - TrueExpScore.v.RawScore)

print(pldf)

if (arg$ns[3] == 0 & arg$nitems[3] != 0){
	if (!dir.exists(paste0('corrout/flex/IRT/',name,'/IncItems/',nst,'students','/'))){dir.create(paste0('corrout/flex/IRT/',name,'/IncItems/',nst,'students','/'), recursive = TRUE)}
	if (arg$nitems[3] != 0){
		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Proportion.Good.Items))+geom_point(size=1,aes(colour=Abs.Difference1))+scale_colour_gradient2()+labs(title=paste0('Absolute Difference in Correlations Between True Theta\n and Raw Score or 2PL Expected Score'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Proportion of Good Items', n.breaks=10)
		ggsave(file=paste0('AbsDiff-TrueThetavRawExpScore.pdf'), path=paste0('corrout/flex/IRT/',name,'/IncItems/',nst,'students','/'))

		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Abs.Difference2))+geom_point(size=1,aes(colour = Proportion.Good.Items))+labs(title=paste0('Absolute Difference in Correlations Between True Expected Score\n and Raw Score or 2PL Expected Score'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Abosulte Difference', n.breaks=10)
		ggsave(file=paste0('AbsDiff-TrueExpvRawExpScore.pdf'), path=paste0('corrout/flex/IRT/',name,'/IncItems/',nst,'students','/'))
	}
}

#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
