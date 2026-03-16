#!/usr/bin/env Rscript
#Above line allows code to be run using ./RankScores.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(tidyr)#drop_na function
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
	names <- c('expgrow','expdecay','logist','reflogist','gaussian','invgaussian','poslinear','neglinear','leftasym','rightasym','mixednorm','split','restricunif','uniform','FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'logist'=350, 'reflogist'=350, 'gaussian'=350, 'invgaussian'=350, 'poslinear'=350, 'neglinear'=350, 'leftasym'=350, 'rightasym'=350, 'mixednorm'=350, 'split'=350, 'restricunif'=350, 'uniform'=350, 'FCIpostsim'=50,'FMCEpostsim'=50,'FMCEThpostsim'=50,'K1-20postsim'=50,'CSEMsam1postsim'=50,'CSEMsam2postsim'=50)
itemiter <- c('expgrow','expdecay','logist','reflogist','gaussian','invgaussian','poslinear','neglinear','leftasym','rightasym','mixednorm','split','restricunif','uniform','FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')
sim <- c('expgrow','expdecay','logist','reflogist','gaussian','invgaussian','poslinear','neglinear','leftasym','rightasym','mixednorm','split','restricunif','uniform','FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')
ggshapes <- c(0:14,32:127)
##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Collecting information of interest
meansets <- list()
RMSEvalues <- data.frame('Name'=c(NA), 'Number.Items'=c(NA), 'Mean.RMSE.TrTh.Est.Th'=c(NA), 'Mean.RMSE.TrTh.SimSumSc'=c(NA), 'Mean.RMSE.TrTh.WSc'=c(NA), 'Mean.RMSE.TrTh.RoundedWSc'=c(NA))
for (name in names){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	#Collecting analysis outputs
	df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'))
	nitems <- unique(df$Number.Items)
	nstud <- unique(df$Number.Students.Original)
	nrun <- unique(df$Number.Run)

	#Collect specific run data of interest
	RMSE.EstTh <- c()
	RMSE.SimSumSc <- c()
	RMSE.WSc <- c()
	RMSE.RoundedWSc <- c()
	for (nit in nitems){
		for (nst in nstud){
			for (r in nrun){
			
				scoredf <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'))
			
				scoredf <- scoredf %>%
					rename(SimSumSc = SimSum.Score, WSc = Scaled.Weighted.Score) 

				#Round WSc to test integer effect
				scoredf$RoundedWSc <- round(scoredf$WSc,0)

				#Calculate ranks for each score	
				rankdf <- scoredf %>%
					select(True.Theta,Est.Theta,SimSumSc,WSc,RoundedWSc) %>%
					mutate(True.Theta.Rank = rank(True.Theta)) %>%
					mutate(Est.Theta.Rank = rank(Est.Theta)) %>%
					mutate(SimSumSc.Rank = rank(SimSumSc)) %>%
					mutate(WSc.Rank = rank(WSc)) %>% 
					mutate(RoundedWSc.Rank = rank(RoundedWSc))  

				RMSErankdiff.TrTh.Est.Theta <- sqrt(mean((rankdf$True.Theta.Rank - rankdf$Est.Theta.Rank)**2))
				RMSErankdiff.TrTh.SimSumSc <- sqrt(mean((rankdf$True.Theta.Rank - rankdf$SimSumSc.Rank)**2))
				RMSErankdiff.TrTh.WSc <- sqrt(mean((rankdf$True.Theta.Rank - rankdf$WSc.Rank)**2))
				RMSErankdiff.TrTh.RoundedWSc <- sqrt(mean((rankdf$True.Theta.Rank - rankdf$RoundedWSc.Rank)**2))
				RMSE.EstTh <- c(RMSE.EstTh, RMSErankdiff.TrTh.Est.Theta)	
				RMSE.SimSumSc <- c(RMSE.SimSumSc, RMSErankdiff.TrTh.SimSumSc)	
				RMSE.WSc <- c(RMSE.WSc, RMSErankdiff.TrTh.WSc)	
				RMSE.RoundedWSc <- c(RMSE.RoundedWSc, RMSErankdiff.TrTh.RoundedWSc)	

				if (r == 1){
					#Plot scores against one another to investigate rank differences
					if (!dir.exists(paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))){dir.create(paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'), recursive = TRUE)}
					
					#Tr.Th vs Est.Th
					ggplot(data=scoredf, mapping=aes(x=Est.Theta,y=True.Theta))+geom_point()+scale_x_continuous(name='Estimated Latent', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('TrThvsEstTh-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					#Tr.Th vs WSc
					ggplot(data=scoredf, mapping=aes(x=WSc,y=True.Theta))+geom_point()+scale_x_continuous(name='Weighted Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('TrThvsWSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					#Tr.Th vs SimSumSc
					ggplot(data=scoredf, mapping=aes(x=SimSumSc,y=True.Theta))+geom_point()+scale_x_continuous(name='Simple Sum Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('TrThvsSimSumSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					#Tr.Th vs RoundedWSc
					ggplot(data=scoredf, mapping=aes(x=RoundedWSc,y=True.Theta))+geom_point()+scale_x_continuous(name='Rounded Weighted Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('TrThvsRoundedWSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					
					#Est.Th vs WSc
					ggplot(data=scoredf, mapping=aes(x=WSc,y=Est.Theta))+geom_point()+scale_x_continuous(name='Weighted Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('EstThvsWSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					#Est.Th vs SimSumSc
					ggplot(data=scoredf, mapping=aes(x=SimSumSc,y=Est.Theta))+geom_point()+scale_x_continuous(name='Simple Sum Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('EstThvsSimSumSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
					#Est.Th vs RoundedWSc
					ggplot(data=scoredf, mapping=aes(x=RoundedWSc,y=Est.Theta))+geom_point()+scale_x_continuous(name='Rounded Weighted Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()
					ggsave(file=paste0('EstThvsRoundedWSc-',paste0(name,r),'.pdf'), path=paste0('rankscoresout/',name,'/',nit,'items/',nst,'students/'))
				}
			}
		}
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bcyan')
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!AVERAGES OVER ALL RUNS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bcyan')
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bcyan')
		print_color(paste0('Mean RMSE of True Theta Rank and Estimated Theta Rank: ',mean(RMSE.EstTh),'\n'),'bviolet')
		print_color(paste0('Mean RMSE of True Theta Rank and SimSumSc Rank: ',mean(RMSE.SimSumSc),'\n'),'bviolet')
		print_color(paste0('Mean RMSE of True Theta Rank and WSc Rank: ',mean(RMSE.WSc),'\n'),'bviolet')
		print_color(paste0('Mean RMSE of True Theta Rank and Rounded WSc Rank: ',mean(RMSE.RoundedWSc),'\n'),'bviolet')
		RMSEvalues <- rbind(RMSEvalues, c(name, nit, mean(RMSE.EstTh), mean(RMSE.SimSumSc), mean(RMSE.WSc), mean(RMSE.RoundedWSc))) 
	}
}

RMSEvalues <- RMSEvalues %>% 
		drop_na() %>%
		mutate_at(c('Number.Items','Mean.RMSE.TrTh.Est.Th','Mean.RMSE.TrTh.SimSumSc','Mean.RMSE.TrTh.WSc','Mean.RMSE.TrTh.RoundedWSc'),as.numeric)
print(RMSEvalues)

pdf('rankscoresout/Plots.pdf')
for (name in names){
	temp <- RMSEvalues %>% 
		filter(Name == name) %>%
		select(Number.Items, Mean.RMSE.TrTh.SimSumSc, Mean.RMSE.TrTh.WSc, Mean.RMSE.TrTh.RoundedWSc) %>%
		as_tibble()
	plotdf <- melt(temp, id = 'Number.Items')
	print(plotdf)
	
	print(ggplot(data=plotdf, mapping=aes(x=Number.Items,y=value,group=variable,color=variable,shape=variable))+geom_point()+scale_shape_manual(values=ggshapes[1:length(unique(plotdf$variable))])+geom_smooth()+labs(title=paste0('Rank RMSE for ',name))+scale_x_continuous(name='Number of Items', n.breaks=10)+scale_y_continuous(name='Rank RMSE', n.breaks=10)+coord_cartesian(ylim=c(min(RMSEvalues$Mean.RMSE.TrTh.SimSumSc,RMSEvalues$Mean.RMSE.TrTh.WSc,RMSEvalues$Mean.RMSE.TrTh.RoundedWSc),max(RMSEvalues$Mean.RMSE.TrTh.SimSumSc,RMSEvalues$Mean.RMSE.TrTh.WSc,RMSEvalues$Mean.RMSE.TrTh.RoundedWSc))))
}
dev.off()

#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
