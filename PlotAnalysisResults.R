#!/usr/bin/env Rscript
#Above line allows code to be run using ./PlotAnalysisResults.R in terminal

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
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- list('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob','normalblowainc','zeroblowainc')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'noshape'=350, 'normalb'=350, 'zerob'=350, 'normalblowainc'=950, 'zeroblowainc'=950)
itemiter <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
disciter <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalblowainc','zeroblowainc')
sim <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
ggshapes <- c(0:14,32:127)

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################
stderr <- function(x){
	stderror <- sd(x)/sqrt(length(x))
	return(stderror)
}
##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Collecting information of interest
datasets <- list()
meansets <- list()
for (name in names){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	#Collecting analysis outputs
	df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'))
	nitems <- unique(df$Number.Items)
	nstud <- unique(df$Number.Students.Original)
	nrun <- unique(df$Number.Run)
	
	#Collect specific run data of interest
	prophvec <- c()
	deldiscvec <- c()
	for (nit in nitems){
		for (nst in nstud){
			for (r in nrun){
				#Collecting the proportion of high discriminating items 
				itemdf <- read.csv(paste0('simdata/flex/IRT/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Items.csv'))
				proph <- nrow(itemdf[itemdf$Discrimination >= 3,])/nrow(itemdf)
				prophvec <- c(prophvec,proph)

				#Collecting the range of discriminations
				discrange <- max(itemdf$Discrimination) - min(itemdf$Discrimination)
				deldiscvec <- c(deldiscvec, discrange)
			}
		}
	}
	df$Analysis.Name <- rep(name, times = outputs[name])
	df$Proportion.High.Disc.Items <- prophvec
	df$Proportion.High.Disc.Items <- sapply(df$Proportion.High.Disc.Items, function(x) round(x,2))
	df$Discrimination.Range <- deldiscvec
	df$Discrimination.Range <- sapply(df$Discrimination.Range, function(x) round(x,2))
	df <- df %>%
		mutate(R2Del.TrTh.Diff = R2Del.TrTh.add.WSc - R2Del.TrTh.add.RawSc) %>%
		mutate(R2Del.TrExpSc.Diff = R2Del.TrExpSc.add.WSc - R2Del.TrExpSc.add.RawSc) %>%
		mutate(R2Del.EstTh.Diff = R2Del.EstTh.add.WSc - R2Del.EstTh.add.RawSc) %>%
		mutate(R2Del.EstExpSc.Diff = R2Del.EstExpSc.add.WSc - R2Del.EstExpSc.add.RawSc) %>%
		mutate(CORR.TrTh.Diff = CORR.TrTh.WSc - CORR.TrTh.RawSc) %>%
		mutate(CORR.TrExpSc.Diff = CORR.TrExpSc.WSc - CORR.TrExpSc.RawSc) %>%
		mutate(CORR.TrTh.DiffEst = CORR.TrTh.EstExpSc - CORR.TrTh.EstTh) %>%
		as_tibble() %>%
		print()

	#Plotting things for each analysis individually	
	if (!dir.exists(paste0('plotanalysisout/flex/IRT/',name,'/'))){dir.create(paste0('plotanalysisout/flex/IRT/',name,'/'), recursive = TRUE)}
	
	#Plotting item iteration x-axis 
	if (name %in% itemiter){
		
		meandf <- df %>%
			group_by(Number.Items) %>%
			summarize(Mean.CORR.TrTh.Diff = mean(CORR.TrTh.Diff), Mean.CORR.TrTh.DiffEst = mean(CORR.TrTh.DiffEst), Mean.R2Del.TrTh.Diff = mean(R2Del.TrTh.Diff)) %>%
			mutate(Analysis.Name = rep(name,times = length(nitems))) %>%
			as_tibble() %>%
			print()
		meansets <- append(meansets, list(meandf))

		ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.Diff))
		ggsave(file=paste0('DiffCorr-TrTh-WScvsRawSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.DiffEst))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta: Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.DiffEst))
		ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))
		
		ggplot(data=df, mapping=aes(x=Number.Items,y=R2Del.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in R-Squared Added'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Add Weighted Score - True Theta:Add Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.R2Del.TrTh.Diff))
		ggsave(file=paste0('DiffR2Del-TrTh-WScvsRawSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		if (name %in% disciter){
			ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.Diff))+geom_point(size=1,aes(color=Discrimination.Range))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.Diff))
			ggsave(file=paste0('DiffCorr-TrTh-WScvsRawSc-IterItems-DR.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

			ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.DiffEst))+geom_point(size=1,aes(color=Discrimination.Range))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta: Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.DiffEst))
			ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterItems-DR.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))
			
			ggplot(data=df, mapping=aes(x=Number.Items,y=R2Del.TrTh.Diff))+geom_point(size=1,aes(color=Discrimination.Range))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in R-Squared Added'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Add Weighted Score - True Theta:Add Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.R2Del.TrTh.Diff))
			ggsave(file=paste0('DiffR2Del-TrTh-WScvsRawSc-IterItems-DR.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))
		}
	}
	if (name %in% disciter){
		ggplot(data=df, mapping=aes(x=Discrimination.Range,y=CORR.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Discrimination Range', n.breaks=10, limits=c(min(df$Discrimination.Range)-.25,max(df$Discrimination.Range)+.25))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffCorr-TrTh-WScvsRawSc-IterDisc-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Discrimination.Range,y=CORR.TrTh.DiffEst))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Discrimination Range', n.breaks=10, limits=c(min(df$Discrimination.Range)-.25,max(df$Discrimination.Range)+.25))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta: Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterDisc-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))
		
		ggplot(data=df, mapping=aes(x=Discrimination.Range,y=R2Del.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in R-Squared Added'))+scale_x_continuous(name='Discrimination Range', n.breaks=10, limits=c(min(df$Discrimination.Range)-.25,max(df$Discrimination.Range)+.25))+scale_y_continuous(name='True Theta:Add Weighted Score - True Theta:Add Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2Del-TrTh-WScvsRawSc-IterDisc-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))
	}
	
	datasets <- append(datasets, list(df))
}

print_color(paste0('=======================================================================\n'),'bcyan')
print_color(paste0('=======================PLOTTING ALL ANALYSES===========================\n'),'bcyan')
print_color(paste0('=======================================================================\n'),'bcyan')
#Merging different analysis name runs if applicable
data <- do.call(rbind, datasets)
print(as_tibble(data))
print(colnames(data))

meandata <- do.call(rbind, meansets)
print(as_tibble(meandata))
print(colnames(meandata))

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.TrTh.Diff,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffCorr-TrTh-WScvsRawSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.TrTh.DiffEst,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta:Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))
		
ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.R2Del.TrTh.Diff,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in R-Squared Added'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Add Weighted Score - True Theta:Add Raw Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffR2Del-TrTh-WScvsRawSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))



#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
