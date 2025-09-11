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
library(directlabels)#used to add labels on plots when lots of lines are used together

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the correlational analysis for a run of scoring analyses')
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- c('expgrow','expdecay','log','logrev','gaussian','invgaussian','poslinear','neglinear','noshape','normalb','zerob','normalblowainc','zeroblowainc','fcipost','fmcethpost','kin1dpdv1post')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'noshape'=350, 'normalb'=350, 'zerob'=350, 'normalblowainc'=950, 'zeroblowainc'=950, 'fcipost'=1, 'fmcethpost'=1,'kin1dpdv1'=1)
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
	nitems <- unique(df$Number.Items)
	nstud <- unique(df$Number.Students.Original)
	nrun <- unique(df$Number.Run)
	
	#Will use more runs later but reducing to just the thirty item ones now
	if (grepl('ainc',name)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!',name,' ANALYSIS NOT RUN!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
		next
	}
	if (grepl('fci',name)){
		nitems <- 29
	}else if (grepl('kin1dpdv1',name)){
		nitems <- 20 
	}else {
		nitems <- 30
	}

	#Collect specific run data of interest
	for (nit in nitems){
		for (nst in nstud){
			for (r in nrun){
			
				if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
					if (grepl('fci', name) & grepl('post', name)){
						scoredf <- read.csv(paste0('analysisout/summary/FCI/post/Scores-',nst,'.csv'))
					}else if (grepl('fmceth', name) & grepl('post', name)){
						scoredf <- read.csv(paste0('analysisout/summary/FMCETh/post/Scores-',nst,'.csv'))
					}else if (grepl('kin1dpdv1',name) & grepl('post', name)){
						scoredf <- read.csv(paste0('analysisout/summary/Kin1D-PD-Ver1/post/Scores-',nst,'.csv'))
					}
				}else {
					scoredf <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'))
				}
			
				#Turn scores into percentiles
				scoredf <- scoredf %>%
					mutate(SimSum.Perc = SimSum.Score / nit) %>%
					mutate(WS.Perc = Scaled.Weighted.Score / nit) %>%
					mutate(Percent.Difference = WS.Perc - SimSum.Perc) %>%
					mutate(Abs.Percent.Difference = abs(Percent.Difference)) 

				meandf <- scoredf %>%
					group_by(SimSum.Perc) %>%
					summarize(Mean.WS.Perc = mean(WS.Perc), Mean.Percent.Difference = mean(Percent.Difference), Mean.Abs.Percent.Difference = mean(Abs.Percent.Difference)) %>%
					mutate(Analysis.Name = name) %>%
					mutate(Number.Items = nit) %>%
					mutate(Run.Number = r) %>%
					as_tibble()
				meansets <- append(meansets, list(meandf))

				#Plotting things for each individually
				if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
					if (!dir.exists(paste0('comparescoresout/',name,'/'))){dir.create(paste0('comparescoresout/',name,'/'), recursive = TRUE)}
				}else {	
					if (!dir.exists(paste0('comparescoresout/',name,'/',nit,'items','/',nst,'students','/'))){dir.create(paste0('comparescoresout/',name,'/',nit,'items','/',nst,'students','/'), recursive = TRUE)}
				}
		
				ggplot(data=scoredf, mapping=aes(x=SimSum.Perc,y=WS.Perc))+geom_point(size=1)+labs(title=paste0('Weighted Score vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Weighted Score', n.breaks=10, limits=c(0,1))+annotate('segment', x=0, y=0, xend=1, yend=1, colour='blue', linetype='dashed')+geom_line(data=meandf, aes(x=SimSum.Perc,y=Mean.WS.Perc), color='red')
				if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
					ggsave(file=paste0('WeightedScvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/'))
				}else {
					ggsave(file=paste0('WeightedScvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/',nit,'items','/',nst,'students','/'))
				}
				
				ggplot(data=scoredf, mapping=aes(x=SimSum.Perc,y=Percent.Difference))+geom_point(size=1)+labs(title=paste0('Percent Difference vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Percent Difference: Weighted Score - SimSum Score', n.breaks=10)+annotate('segment', x = 0, xend = 1, y=0, colour='blue', linetype='dashed')+geom_line(data=meandf, aes(x=SimSum.Perc,y=Mean.Percent.Difference), color='red')
				if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
					ggsave(file=paste0('PercDiffvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/'))
				}else {
					ggsave(file=paste0('PercDiffvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/',nit,'items','/',nst,'students','/'))
				}
				
				ggplot(data=scoredf, mapping=aes(x=SimSum.Perc,y=Abs.Percent.Difference))+geom_point(size=1)+labs(title=paste0('Absolute Percent Difference vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Absolute Percent Difference: Weighted Score - SimSum Score', n.breaks=10)+annotate('segment', x = 0, xend = 1, y=0, colour='blue', linetype='dashed')+geom_line(data=meandf, aes(x=SimSum.Perc,y=Mean.Abs.Percent.Difference), color='red')
				if (grepl('fci', name) | grepl('fmce', name) | grepl('kin1dpdv1',name)){
					ggsave(file=paste0('AbsPercDiffvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/'))
				}else {
					ggsave(file=paste0('AbsPercDiffvSimSumSc-',paste0(name,r),'.pdf'), path=paste0('comparescoresout/',name,'/',nit,'items','/',nst,'students','/'))
				}
			}
		}
	}
}

#Plotting mean lines of interest
meandata <- do.call(rbind, meansets)
print(as_tibble(meandata))
print(colnames(meandata))

plotdf <- meandata %>%
	group_by(SimSum.Perc, Analysis.Name, Number.Items) %>%
	summarize(Mean.Mean.WS.Perc = mean(Mean.WS.Perc), Mean.Mean.Percent.Difference = mean(Mean.Percent.Difference), Mean.Mean.Abs.Percent.Difference = mean(Mean.Abs.Percent.Difference)) %>%
	mutate(Name = paste0(Analysis.Name,' ',Number.Items)) %>%
	as_tibble() %>%
	print()

ggplot(data=plotdf, mapping=aes(x=SimSum.Perc,y=Mean.Mean.WS.Perc,group=Name,color=Name,shape=Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(plotdf$Name))])+labs(title=paste0('Mean Weighted Score vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Mean Weighted Score', n.breaks=10, limits=c(0,1))+annotate('segment', x=0, y=0, xend=1, yend=1, colour='blue', linetype='dashed')
ggsave(file=paste0('MeanWeightedScvSimSumSc.pdf'), path=paste0('comparescoresout/'))

ggplot(data=plotdf, mapping=aes(x=SimSum.Perc,y=Mean.Mean.Percent.Difference,group=Name,color=Name,shape=Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(plotdf$Name))])+labs(title=paste0('Mean Percent Difference vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Mean Percent Difference: Weighted Score - SimSum Score', n.breaks=10)+annotate('segment', x=0, xend=1, y=0, colour='blue', linetype='dashed')
ggsave(file=paste0('MeanPercDiffvSimSumSc.pdf'), path=paste0('comparescoresout/'))

ggplot(data=plotdf, mapping=aes(x=SimSum.Perc,y=Mean.Mean.Abs.Percent.Difference,group=Name,color=Name,shape=Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(plotdf$Name))])+labs(title=paste0('Mean Absolute Percent Difference vs SimSum Score'))+scale_x_continuous(name='SimSum Score', n.breaks=10, limits=c(0,1))+scale_y_continuous(name='Mean Absolute Percent Difference: Weighted Score - SimSum Score', n.breaks=10)+annotate('segment', x=0, xend=1, y=0, colour='blue', linetype='dashed')
ggsave(file=paste0('MeanAbsPercDiffvSimSumSc.pdf'), path=paste0('comparescoresout/'))



#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
