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
library(cowplot)#combining plots

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the correlational analysis for a run of scoring analyses')
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- list('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'noshape'=350, 'normalb'=350, 'zerob'=350)
itemiter <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob')
disciter <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape')
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
		mutate(CORR.TrTh.Diff = CORR.TrTh.WSc - CORR.TrTh.SimSumSc) %>%
		mutate(CORR.TrExpSc.Diff = CORR.TrExpSc.WSc - CORR.TrExpSc.SimSumSc) %>%
		mutate(CORR.TrTh.DiffEst = CORR.TrTh.EstExpSc - CORR.TrTh.EstTh) %>%
		mutate(R2.TrTh.Diff = R2.TrTh.WSc - R2.TrTh.SimSumSc) %>%
		as_tibble() %>%
		print()

	#Plotting things for each analysis individually	
	if (!dir.exists(paste0('plotanalysisout/flex/IRT/',name,'/'))){dir.create(paste0('plotanalysisout/flex/IRT/',name,'/'), recursive = TRUE)}
	
	#Plotting item iteration x-axis 
	if (name %in% itemiter){
		
		meandf <- df %>%
			group_by(Number.Items) %>%
			summarize(Mean.CORR.TrTh.Diff = mean(CORR.TrTh.Diff), Mean.CORR.TrTh.WSc = mean(CORR.TrTh.WSc), Mean.CORR.TrTh.SimSumSc = mean(CORR.TrTh.SimSumSc), Mean.CORR.TrTh.DiffEst = mean(CORR.TrTh.DiffEst), Mean.R2.TrTh.Diff = mean(R2.TrTh.Diff), Mean.R2.TrTh.WSc = mean(R2.TrTh.WSc), Mean.R2.TrTh.SimSumSc = mean(R2.TrTh.SimSumSc), Mean.CORR.2PLDiff.2PLDisc = mean(CORR.2PLDiff.2PLDisc), Mean.Alpha = mean(Alpha)) %>%
			mutate(Analysis.Name = rep(name,times = length(nitems))) %>%
			as_tibble() %>%
			print()
		meansets <- append(meansets, list(meandf))

		ggplot(data=df, mapping=aes(x=Number.Items,y=Alpha))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Alpha'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Alpha', n.breaks=10)+geom_line(data=meandf, aes(x=Number.Items,y=Mean.Alpha))
		ggsave(file=paste0('Alpha-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.2PLDiff.2PLDisc))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Correlation Between 2PL Difficulty And 2PL Discrimination'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Correlation Between 2PL Difficulty And 2PL Discrimination', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.2PLDiff.2PLDisc))
		ggsave(file=paste0('Corr-2PLDiff-2PLDisc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:SimSum Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.Diff))
		ggsave(file=paste0('DiffCorr-TrTh-WScvsSimSumSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.DiffEst))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta: Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.DiffEst))
		ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		ggplot(data=df, mapping=aes(x=Number.Items,y=R2.TrTh.Diff))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in R-Squared'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:SimSum Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')+geom_line(data=meandf, aes(x=Number.Items,y=Mean.R2.TrTh.Diff))
		ggsave(file=paste0('DiffR2-TrTh-WScvsSimSumSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'))

		#Will combine the two below
		CORRWSc <- ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.WSc))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Correlation Between True Theta And Weighted Score', n.breaks=10)+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.WSc))

		CORRSimSumSc <- ggplot(data=df, mapping=aes(x=Number.Items,y=CORR.TrTh.SimSumSc))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Correlation Between True Theta And SimSum Score', n.breaks=10)+geom_line(data=meandf, aes(x=Number.Items,y=Mean.CORR.TrTh.SimSumSc))
		
		corr <- plot_grid(CORRWSc+theme(legend.position='none'), CORRSimSumSc+theme(legend.position='none'), labels = c('A','B'))
		legend <- get_legend(CORRWSc+guides(color = guide_legend(nrow=1))+theme(legend.position = 'bottom'))
		corr <- plot_grid(corr, legend, ncol = 1, rel_heights=c(1, .1))
		ggsave(file=paste0('Corr-TrTh-WScvSimSumSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'), corr)

		#Will combine the two below
		R2WSc <- ggplot(data=df, mapping=aes(x=Number.Items,y=R2.TrTh.WSc))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Variance in True Theta Explained By Weighted Score', n.breaks=10)+geom_line(data=meandf, aes(x=Number.Items,y=Mean.R2.TrTh.WSc))

		R2SimSumSc <- ggplot(data=df, mapping=aes(x=Number.Items,y=R2.TrTh.SimSumSc))+geom_point(size=1,aes(color=Proportion.High.Disc.Items))+scale_colour_gradient(low='red',high='blue')+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(df$Number.Items)-5,max(df$Number.Items)+5))+scale_y_continuous(name='Variance in True Theta Explained by SimSum Score', n.breaks=10)+geom_line(data=meandf, aes(x=Number.Items,y=Mean.R2.TrTh.SimSumSc))
		
		r2 <- plot_grid(R2WSc+theme(legend.position='none'), R2SimSumSc+theme(legend.position='none'), labels = c('A','B'))
		legend <- get_legend(R2WSc+guides(color = guide_legend(nrow=1))+theme(legend.position = 'bottom'))
		r2 <- plot_grid(r2, legend, ncol = 1, rel_heights=c(1, .1))
		ggsave(file=paste0('R2-TrTh-WScvSimSumSc-IterItems-PHDI.pdf'), path=paste0('plotanalysisout/flex/IRT/',name,'/'), r2)
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

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.Alpha,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Alpha'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Alpha', n.breaks=10)
ggsave(file=paste0('Alpha-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.2PLDiff.2PLDisc,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Correlation Between 2PL Difficulty and 2PL Discrimination'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Correlation Between 2PL Difficulty and 2PL Discrimination', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('Corr-2PLDiff-2PLDisc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.TrTh.Diff,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:SimSum Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffCorr-TrTh-WScvsSimSumSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))

ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.TrTh.DiffEst,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in Correlations'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Est Expected Score - True Theta:Estimated Theta', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffCorr-TrTh-EstExpScvsEstTh-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))
		
ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.R2.TrTh.Diff,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+labs(title=paste0('Difference in R-Squared'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='True Theta:Weighted Score - True Theta:SimSum Score', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
ggsave(file=paste0('DiffR2-TrTh-WScvsSimSumSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'))

#Will combine the two below
mnCORRWSc <- ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.CORR.TrTh.WSc,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Correlation Between True Theta And Weighted Score', n.breaks=10)

mnCORRSimSumSc <- ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.R2.TrTh.SimSumSc,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Correlation Between True Theta And SimSum Score', n.breaks=10)

mncorr <- plot_grid(mnCORRWSc+theme(legend.position='none'), mnCORRSimSumSc+theme(legend.position='none'), labels = c('A','B'))
legend <- get_legend(mnCORRWSc+guides(color = guide_legend(nrow=1))+theme(legend.position = 'bottom'))
mncorr <- plot_grid(mncorr, legend, ncol = 1, rel_heights=c(.8, .2))
save_plot(file=paste0('Corr-TrTh-WScvSimSumSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'),mncorr, ncol = 1, nrow = 2)


#Will combine the two below
mnR2WSc <- ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.R2.TrTh.WSc,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Variance in True Theta Explained by Weighted Score', n.breaks=10)

mnR2SimSumSc <- ggplot(data=meandata, mapping=aes(x=Number.Items,y=Mean.R2.TrTh.SimSumSc,group=Analysis.Name,color=Analysis.Name,shape=Analysis.Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(meandata$Analysis.Name))])+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(min(meandata$Number.Items)-5,max(meandata$Number.Items)+5))+scale_y_continuous(name='Variance in True Theta Explained by SimSum Score', n.breaks=10)

mnr2 <- plot_grid(mnR2WSc+theme(legend.position='none'), mnR2SimSumSc+theme(legend.position='none'), labels = c('A','B'))
legend <- get_legend(mnR2WSc+guides(color = guide_legend(nrow=1))+theme(legend.position = 'bottom'))
mnr2 <- plot_grid(mnr2, legend, ncol = 1, rel_heights=c(.8, .2))
save_plot(file=paste0('R2-TrTh-WScvSimSumSc-IterItems.pdf'), path=paste0('plotanalysisout/flex/IRT/'),mnr2, ncol = 1, nrow = 2)


#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
