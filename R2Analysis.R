#!/usr/bin/env Rscript
#Above line allows code to be run using ./R2Analysis.R in terminal

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
parser <- arg_parser('Options for varying the R^2 analysis for a run of scoring analyses')
parser <- add_argument(parser, "--name", help = 'name for set of runs being investigated',nargs='*',default='TEST')
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
estthaddraw <- c()
estthaddw <- c()
estexpaddraw <- c()
estexpaddw <- c()
trthaddraw <- c()
trthaddw <- c()
trexpaddraw <- c()
trexpaddw <- c()
prophvec <- c()
deldiscvec <- c()
for (nit in numitems){
	for (nst in nums){
		for (r in 1:nrun){
			nitemsvec <- c(nitemsvec,nit)
			nsvec <- c(nsvec,nst)
			
			#Collecting correlations
			r2 <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/','DeltaR2-',paste0(name,r),'.csv'))
			estthaddraw <- c(estthaddraw,r2[(r2$DV == 'Estimated Theta' & r2$AddVariable == 'Raw Score'),]$Delta.R2)
			estthaddw <- c(estthaddw,r2[(r2$DV == 'Estimated Theta' & r2$AddVariable == 'Weighted Score'),]$Delta.R2)
			estexpaddraw <- c(estexpaddraw,r2[(r2$DV == 'Estimated Expected Score' & r2$AddVariable == 'Raw Score'),]$Delta.R2)
			estexpaddw <- c(estexpaddw,r2[(r2$DV == 'Estimated Expected Score' & r2$AddVariable == 'Weighted Score'),]$Delta.R2)
			trthaddraw <- c(trthaddraw,r2[(r2$DV == 'True Theta' & r2$AddVariable == 'Raw Score'),]$Delta.R2)
			trthaddw <- c(trthaddw,r2[(r2$DV == 'True Theta' & r2$AddVariable == 'Weighted Score'),]$Delta.R2)
			trexpaddraw <- c(trexpaddraw,r2[(r2$DV == 'True Expected Score' & r2$AddVariable == 'Raw Score'),]$Delta.R2)
			trexpaddw <- c(trexpaddw,r2[(r2$DV == 'True Expected Score' & r2$AddVariable == 'Weighted Score'),]$Delta.R2)
			
			#Collecting differences in discriminations
			fl <- readLines(paste0('simdata/flex/IRT/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Generators.txt'))
			discmn <- fl[grepl('Discrimination Mean:',fl)]
			discmn <- gsub('Discrimination Mean: ','',discmn)
			discmn <- sapply(strsplit(discmn,',')[[1]], function(x) as.numeric(x))
			deldiscvec <- c(deldiscvec,discmn[2]-discmn[1])

			#Collecting the proportion of good items 
			itemdf <- read.csv(paste0('simdata/flex/IRT/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Items.csv'))
			proph <- nrow(itemdf[itemdf$Discrimination == 3,])/nrow(itemdf)
			prophvec <- c(prophvec,proph)
		}
	}
}

#Plotting correlation differences for things of interest
pldf <- data.frame(Number.Students = nsvec, Number.Items = nitemsvec, Proportion.High.Items = prophvec, Delta.Discrimination = deldiscvec, EstTheta.AddRaw = estthaddraw, EstTheta.AddWeighted = estthaddw, EstExpScore.AddRaw = estexpaddraw, EstExpScore.AddWeighted = estexpaddw, TrueTheta.AddRaw = trthaddraw, TrueTheta.AddWeighted = trthaddw, TrueExpScore.AddRaw = trexpaddraw, TrueExpScore.AddWeighted = trexpaddw)

pldf$Proportion.High.Items <- sapply(pldf$Proportion.High.Items, function(x) round(x,2))

#Make columns to plot
pldf <- pldf %>%
	mutate(Abs.Difference1 = EstTheta.AddWeighted - EstTheta.AddRaw) %>%
	mutate(Abs.Difference2 = EstExpScore.AddWeighted - EstExpScore.AddRaw) %>%
	mutate(Abs.Difference3 = TrueTheta.AddWeighted - TrueTheta.AddRaw) %>%
	mutate(Abs.Difference4 = TrueExpScore.AddWeighted - TrueExpScore.AddRaw) %>%
	print()

#Plotting correlational differences as a function of number of items
if (arg$ns[3] == 0 & arg$nitems[3] != 0){
	if (!dir.exists(paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'))){dir.create(paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'), recursive = TRUE)}
	if (arg$nitems[3] != 0){
		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Abs.Difference1))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of Estimated Theta'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-EstTheta.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'))

		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Abs.Difference2))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of Estimated Expected Score'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-EstExpScore.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'))
		
		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Abs.Difference3))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of True Theta'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-TrueTheta.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'))

		ggplot(data=pldf, mapping=aes(x=Number.Items,y=Abs.Difference4))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of True Expected Score'))+scale_x_continuous(name='Number of Items', n.breaks=10, limits=c(arg$nitems[1]-arg$nitems[3],arg$nitems[2]+arg$nitems[3]))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-TrueExpScore.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncItems/',nst,'students','/'))
	}
}

#Plotting correlational differences as a function of discrimination difference for different number of items
if (length(unique(pldf$Delta.Discrimination)) > 1){
	for (nit in unique(pldf$Number.Items)){
		if (!dir.exists(paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'))){dir.create(paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'), recursive = TRUE)}

		ggplot(data=pldf, mapping=aes(x=Delta.Discrimination,y=Abs.Difference1))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of Estimated Theta For ',nit,' Items'))+scale_x_continuous(name='Difference Between High and Low Discrimination', n.breaks=10, limits=c(min(pldf$Delta.Discrimination)-.1,max(pldf$Delta.Discrimination)+.1))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-EstTheta.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'))

		ggplot(data=pldf, mapping=aes(x=Delta.Discrimination,y=Abs.Difference2))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of Estimated Expected Score For ',nit,' Items'))+scale_x_continuous(name='Difference Between High and Low Discrimination', n.breaks=10, limits=c(min(pldf$Delta.Discrimination)-.1,max(pldf$Delta.Discrimination)+.1))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-EstExpScore.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'))
		
		ggplot(data=pldf, mapping=aes(x=Delta.Discrimination,y=Abs.Difference3))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of True Theta For ',nit,' Items'))+scale_x_continuous(name='Difference Between High and Low Discrimination', n.breaks=10, limits=c(min(pldf$Delta.Discrimination)-.1,max(pldf$Delta.Discrimination)+.1))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-TrueTheta.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'))

		ggplot(data=pldf, mapping=aes(x=Delta.Discrimination,y=Abs.Difference4))+geom_point(size=1,aes(colour=Proportion.High.Items))+scale_colour_gradient(low='red',high='blue')+labs(title=paste0('Difference in Added Explained Variance of True Expected Score For ',nit,' Items'))+scale_x_continuous(name='Difference Between High and Low Discrimination', n.breaks=10, limits=c(min(pldf$Delta.Discrimination)-.1,max(pldf$Delta.Discrimination)+.1))+scale_y_continuous(name='Weighted Score Additional Variance - Raw Score Additional Variance', n.breaks=10)+geom_hline(yintercept=0,linetype='dashed',color='black')
		ggsave(file=paste0('DiffR2-TrueExpScore.pdf'), path=paste0('r2out/flex/IRT/',name,'/IncDisc/',nit,'items','/',nst,'students','/'))
	}
}

#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
