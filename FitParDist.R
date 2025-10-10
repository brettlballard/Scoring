#!/usr/bin/env Rscript
#Above line allows code to be run using ./FitParDist.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(insight)#print_color function
library(argparser)#anything parser related
library(ggplot2)#plot related

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the correlational analysis for a run of scoring analyses')
parser <- add_argument(parser, "--names", help = 'name for set of runs being investigated',nargs='*',default=c('All'))
arg <- parse_args(parser)

#Resetting argument parameters
if ('All' %in% arg$names){
	names <- list('expgrow','expdecay','log','logrev','gaussian','invgaussian')
}else {
	names <- strsplit(arg$names,',')[[1]]
}

#Splitting datasets for stuff below
outputs <- list('expgrow'=350, 'expdecay'=350, 'log'=350, 'logrev'=350, 'gaussian'=350, 'invgaussian'=350)

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

#Collecting information of interest
for (name in names){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	#Collecting analysis outputs
	df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput',paste0(outputs[name]),'.csv'))
	nitems <- unique(df$Number.Items)
	nstud <- unique(df$Number.Students.Original)
	nrun <- unique(df$Number.Run)
	
	#Collect specific run data of interest
	TrPar <- list()
	EstPar <- list()
	for (nit in nitems){
		for (nst in nstud){
			for (r in nrun){
				#Collecting the true item parameters 
				Trdf <- read.csv(paste0('simdata/flex/IRT/',name,'/',nit,'items','/',nst,'students','/',paste0(name,r),'-Items.csv'))
				TrPar <- append(TrPar, list(Trdf))

				#Collecting the estimated item parameters 
				Estdf <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/2PLpar-',paste0(name,r),'.csv'))
				EstPar <- append(EstPar, list(Estdf))
			}
		}
	}
	print_color(paste0('=======================================================================\n'),'bcyan')
	print_color(paste0('=======================TRUE ITEM PARAMETERS============================\n'),'bcyan')
	print_color(paste0('=======================================================================\n'),'bcyan')
	#Merging true item parameters	
	Trdata <- do.call(rbind, TrPar)
	setnames(Trdata, old=c('Difficulty','Discrimination'), new=c('Diff','Disc'))
	Trdata <- Trdata %>%
		filter(abs(Diff) < 5) %>%#filter out outliers from distributions
		filter(abs(Disc) < 5)
	print(as_tibble(Trdata))
	
	print_color(paste0('=======================================================================\n'),'bcyan')
	print_color(paste0('====================ESTIMATED ITEM PARAMETERS==========================\n'),'bcyan')
	print_color(paste0('=======================================================================\n'),'bcyan')
	#Merging estimated item parameters	
	Estdata <- do.call(rbind, EstPar)
	setnames(Estdata, old=c('Est.Difficulty.2PL','Est.Discrimination.2PL'), new=c('Est.Diff','Est.Disc'))
	Estdata <- Estdata %>%
		filter(abs(Est.Diff) < 5) %>%#filter out outliers from estimation
		filter(abs(Est.Disc) < 5)
	print(as_tibble(Estdata))
	
	x <- seq(from = -3, to = 3, length.out = 1000)
	fitdf <- data.frame(X = x)
	names <- list('expgrow','expdecay','log','logrev','gaussian','invgaussian')
	if (name == 'expgrow'){
		#Exponential Growth
		#formula: y = a*exp(b*(x-c))
		fitdf$Y <- 1.2 * exp(.4 * (fitdf$X - 0))
	}else if (name == 'expdecay'){
		#Exponential Decay
		#formula: y = a*exp(b*(x-c))
		fitdf$Y <- 1.2 * exp(-.4 * (fitdf$X - 0))
	}else if (name == 'log'){
		#Left-Asym
		#formula: y = a-(a-b)*exp(-c*(x-d))
		fitdf$Y <- 3.25 - (3.25 - 1)*exp(-.7 * (fitdf$X + 2.5))
	}else if (name == 'logrev'){
		#Right-Asym
		#formula: y = a-(a-b)*exp(-c*(-1*x-d))
		fitdf$Y <- 3.25 - (3.25 - 1)*exp(-.7 * (-1 * fitdf$X + 2.5))
	}else if (name == 'gaussian'){
		#Gaussian
		#formula: y = a*exp(-b*(x-c)**2)+d
		fitdf$Y <- 2.25 * exp(-.5 * (fitdf$X - 0)**2) + .75
	}else if(name == 'invgaussian'){
		#Inverted Gaussian
		#formula: y = -a*exp(-b*(x-c)**2)+d
		fitdf$Y <- -2.5 * exp(-.5 * (fitdf$X - 0)**2) + 3.5
	}

	
	#Plotting things for each analysis individually	
	print_color(paste0('=======================================================================\n'),'bviolet')
	print_color(paste0('=====================PLOTTING ITEM PARAMETERS==========================\n'),'bviolet')
	print_color(paste0('=======================================================================\n'),'bviolet')
	if (!dir.exists(paste0('fitpardistout/flex/IRT/',name,'/'))){dir.create(paste0('fitpardistout/flex/IRT/',name,'/'), recursive = TRUE)}
	
	ggplot(data=Trdata, mapping=aes(x=Diff,y=Disc))+geom_point()+labs(title=paste0('True Item Discrimination vs True Item Difficulty'))+scale_x_continuous(name='True Item Difficulty', n.breaks=10)+scale_y_continuous(name='True Item Discrimination', n.breaks=10)+geom_line(data = fitdf, aes(x=X,y=Y),color='blue')
	ggsave(file=paste0('TrueItemParDist.pdf'), path=paste0('fitpardistout/flex/IRT/',name,'/'))

	ggplot(data=Estdata, mapping=aes(x=Est.Diff,y=Est.Disc))+geom_point()+labs(title=paste0('Estimated Item Discrimination vs Estimated Item Difficulty'))+scale_x_continuous(name='Estimated Item Difficulty', n.breaks=10)+scale_y_continuous(name='Estimated Item Discrimination', n.breaks=10)+geom_line(data = fitdf, aes(x=X,y=Y),color='blue')
	ggsave(file=paste0('EstItemParDist.pdf'), path=paste0('fitpardistout/flex/IRT/',name,'/'))

}

#Getting FMCEThpost fit  
print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCEThpost ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
print_color(paste0('=======================================================================\n'),'bcyan')
print_color(paste0('====================ESTIMATED ITEM PARAMETERS==========================\n'),'bcyan')
print_color(paste0('=======================================================================\n'),'bcyan')
#Collecting the estimated item parameters 
df <- read.csv(paste0('analysisout/summary/FMCETh/post/2PLpar-4505.csv'))
setnames(df, old=c('Est.Difficulty.2PL','Est.Discrimination.2PL'), new=c('Est.Diff','Est.Disc'))
df <- df %>%
	filter(abs(Est.Diff) < 5) %>%#filter out outliers from estimation
	filter(abs(Est.Disc) < 5)
print(as_tibble(df))


print_color(paste0('=======================================================================\n'),'bcyan')
print_color(paste0('=======================NONLINEAR MODEL FIT=============================\n'),'bcyan')
print_color(paste0('=======================================================================\n'),'bcyan')
#NLS fitting capabilities are limited currently until more research into self starting fxns is done
#mod <- nlsLM(Est.Disc ~ a * exp(b * (Est.Diff - c)) + d, data = df, start = list(a=.2,b=1,c=0,d=1.2), control = nls.lm.control(maxiter = 100))
#print(mod)
#print(summary(mod))
#print(coeff(mod))

x <- seq(from = min(df$Est.Diff), to = max(df$Est.Diff), length.out = 1000)
fitdf <- data.frame(X = x)
#Exponential Growth
#formula: y = a*exp(b*(x-c))
fitdf$Y <- 2.5 * exp(.75 * (fitdf$X - 0))

#Plotting things for each analysis individually	
print_color(paste0('=======================================================================\n'),'bviolet')
print_color(paste0('=====================PLOTTING ITEM PARAMETERS==========================\n'),'bviolet')
print_color(paste0('=======================================================================\n'),'bviolet')
if (!dir.exists(paste0('fitpardistout/FMCETh/post/'))){dir.create(paste0('fitpardistout/FMCETh/post/'), recursive = TRUE)}
ggplot(data=df, mapping=aes(x=Est.Diff,y=Est.Disc))+geom_point()+labs(title=paste0('Estimated Item Discrimination vs Estimated Item Difficulty'))+scale_x_continuous(name='Estimated Item Difficulty', n.breaks=10)+scale_y_continuous(name='Estimated Item Discrimination', n.breaks=10)+geom_line(data = fitdf, aes(x=X,y=Y),color='blue')
ggsave(file=paste0('EstItemParDist.pdf'), path=paste0('fitpardistout/FMCETh/post/'))

	





#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
