#!/usr/bin/env Rscript
#Above line allows code to be run using ./PaperStuff.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(plyr)#mapvalues function
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related
library(geomtextpath)#geom_text_segment
library(ggrepel)#geom_text_repel
library(ggExtra)#ggMarginal stuff
library(cowplot)#combining plots
library(directlabels)#used to add labels on plots when lots of lines are used together
source('src/MultiPDF.R')#use of multiple random pdfs
source('src/DataframeToLaTeX.R')#converts a dataframe to a LaTeX table format

run <- c('PLOTSCORES')
ggshapes <- c(0:14,32:127)

if ('DEMO' %in% run){
	#Retrieving demographics for the real data
	print_color('============================================================================\n','bcyan')
	print_color('==========================Demographics Information==========================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	names <- c('FCI-post','FMCE-post','FMCETh-post','K1-20-post','K1-7-post','CSEMsam1-post','CSEMsam2-post')
	#Collecting information of interest
	testvec <- c()
	nitemsvec <- c()
	nstudvec <- c()
	alphavec <- c()
	covvarratiovec <- c()
	scoremnvec <- c()
	scoresdvec <- c()
	for (name in names){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		#Collecting data
		df <- read.csv(paste0('realdata/',name,'.csv'))
		if (name == 'FCI-post'){
			df <- df %>% select(-all_of(c('Item29')))
		}
		print(as_tibble(df))
		simsumsc <- apply(df,1,sum)

		#Getting item variances and covariances to calculate alpha
		covvar <- cov(df)
		avgvar <- mean(diag(covvar))
		avgcov <- mean(covvar[lower.tri(covvar)])
		print(avgcov/avgvar)
		alpha <- (ncol(df)*avgcov)/(avgvar + (ncol(df) - 1)*avgcov)
		print(alpha)

		#Saving things for a full table
		testvec <- c(testvec,sub('-post','',name))
		nitemsvec <- c(nitemsvec,ncol(df))
		nstudvec <- c(nstudvec,nrow(df))
		alphavec <- c(alphavec,alpha)
		covvarratiovec <- c(covvarratiovec,avgcov/avgvar) 
		scoremnvec <- c(scoremnvec,mean(simsumsc))
		scoresdvec <- c(scoresdvec,sd(simsumsc))
	}

	testdemo <- data.frame('Test'=testvec, 'Number.Items'=nitemsvec, 'Number.Students'=nstudvec, 'Alpha'=alphavec, 'Cov/Var'=covvarratiovec, 'Score.Mean'=scoremnvec, 'Score.Std'=scoresdvec)
	print(testdemo)
	dftoLaTeX(data=testdemo, filename='paperstuffout/TestDemo')
}

if ('DISCPROP' %in% run){
	#Retrieving fit statistics for the samples
	print_color('============================================================================\n','bcyan')
	print_color('========================2PL Discrimination Information======================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	simnames <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')
	legnames <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')

	discdata <- data.frame(Name = c(NA), Number.Items = c(NA), Number.Run = c(NA), Prop.High.Disc = c(NA), Prop.Low.Disc = c(NA))
	for (name in c(simnames,legnames)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		if (name %in% legnames){
			df <- read.csv(paste0('analysisout/summary/',name,'/post/AnalysisOutput1.csv'))
		}else if (name %in% simnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput350.csv'))
		}

		nitems <- unique(df$Number.Items)
		nstud <- unique(df$Number.Students.Original)
		nrun <- unique(df$Number.Run)

		#Collect specific run data of interest
		for (nit in nitems){	
			for (nst in nstud){
				for (r in nrun){
					if (name %in% legnames){
						pars <- read.csv(paste0('analysisout/summary/',name,'/post/2PLpar-',nst,'.csv'))
					}else {
						pars <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items/',nst,'students/2PLpar-',paste0(name,r),'.csv'))
					}
					#Collect discriminations	
					disc <- pars$Est.Discrimination.2PL
					#High disc > 2 and low disc < 1
					hdisc <- length(disc[disc > 2])
					ldisc <- length(disc[disc < 1])
					prophdisc <- hdisc / nit
					propldisc <- ldisc / nit

					temp <- data.frame(Name = name, Number.Items = nit, Number.Run = r, Prop.High.Disc = prophdisc, Prop.Low.Disc = propldisc)
					discdata <- rbind(discdata, temp)
				}
			}
		}
	}
	discdata1 <- discdata %>%
		na.omit(discdata) %>%
		group_by(Name,Number.Items) %>%
		summarize(Prop.High.Disc = mean(Prop.High.Disc), Prop.Low.Disc = mean(Prop.Low.Disc)) %>%
		as.data.frame() %>%
		print()
	dftoLaTeX(data=discdata1, filename='paperstuffout/DiscriminationDataByItems')
	discdata2 <- discdata %>%
		na.omit(discdata) %>%
		group_by(Name) %>%
		summarize(Prop.High.Disc = mean(Prop.High.Disc), Prop.Low.Disc = mean(Prop.Low.Disc)) %>%
		as.data.frame() %>%
		print()
	dftoLaTeX(data=discdata2, filename='paperstuffout/DiscriminationDataByNames')
}

if ('DIFFPROP' %in% run){
	#Retrieving fit statistics for the samples
	print_color('============================================================================\n','bcyan')
	print_color('==========================2PL Difficulty Information========================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	simnames <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')
	legnames <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')

	diffdata <- data.frame(Name = c(NA), Number.Items = c(NA), Number.Run = c(NA), Prop.High.Diff = c(NA), Prop.Low.Diff = c(NA))
	for (name in c(simnames,legnames)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		if (name %in% legnames){
			df <- read.csv(paste0('analysisout/summary/',name,'/post/AnalysisOutput1.csv'))
		}else if (name %in% simnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput350.csv'))
		}

		nitems <- unique(df$Number.Items)
		nstud <- unique(df$Number.Students.Original)
		nrun <- unique(df$Number.Run)

		#Collect specific run data of interest
		for (nit in nitems){	
			for (nst in nstud){
				for (r in nrun){
					if (name %in% legnames){
						pars <- read.csv(paste0('analysisout/summary/',name,'/post/2PLpar-',nst,'.csv'))
					}else {
						pars <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items/',nst,'students/2PLpar-',paste0(name,r),'.csv'))
					}
					#Collect difficulties	
					diff <- pars$Est.Difficulty.2PL
					#High diff > 1.5 and low diff < -1.5
					hdiff <- length(diff[diff > 1.5])
					ldiff <- length(diff[diff < -1.5])
					prophdiff <- hdiff / nit
					propldiff <- ldiff / nit

					temp <- data.frame(Name = name, Number.Items = nit, Number.Run = r, Prop.High.Diff = prophdiff, Prop.Low.Diff = propldiff)
					diffdata <- rbind(diffdata, temp)
				}
			}
		}
	}
	diffdata1 <- diffdata %>%
		na.omit(diffdata) %>%
		group_by(Name,Number.Items) %>%
		summarize(Prop.High.Diff = mean(Prop.High.Diff), Prop.Low.Diff = mean(Prop.Low.Diff)) %>%
		as.data.frame() %>%
		print()
	dftoLaTeX(data=diffdata1, filename='paperstuffout/DifficultyDataByItems')
	diffdata2 <- diffdata %>%
		na.omit(diffdata) %>%
		group_by(Name) %>%
		summarize(Prop.High.Diff = mean(Prop.High.Diff), Prop.Low.Diff = mean(Prop.Low.Diff)) %>%
		as.data.frame() %>%
		print()
	dftoLaTeX(data=diffdata2, filename='paperstuffout/DifficultyDataByNames')
}

if ('FITS' %in% run){
	#Retrieving fit statistics for the samples
	print_color('============================================================================\n','bcyan')
	print_color('==========================2PL Model Fit Information=========================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	simnames <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')
	legnames <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')
	legsimnames <- c('FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')

	fitdata <- list()
	for (name in c(simnames,legnames,legsimnames)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		if (name %in% legnames){
			df <- read.csv(paste0('analysisout/summary/',name,'/post/AnalysisOutput1.csv'))
		}else if (name %in% legsimnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput50.csv'))
		}else if (name %in% simnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput350.csv'))
		}

		tempdf <- df %>%
			select(c(Number.Items,Number.Run,Model.RMSEA,Model.SRMSR,Model.TLI,Model.CFI)) %>%
			mutate(Name = rep(name, times = nrow(df)))
		fitdata <- append(fitdata, list(tempdf))
	}
	
	print_color('============================================================================\n','bviolet')
	print_color('=======================Summarizing Model Fit Statistics=====================\n','bviolet')
	print_color('============================================================================\n','bviolet')
	#Merging and summarizing fit data for all samples
	data <- Reduce(function(x,y) merge(x,y,all = TRUE), fitdata)
	print(head(data,20))

	#Summarize fit statistics for real instruments
	temp <- data %>%
		filter(Name %in% c(legnames,legsimnames))
	tabdata <- temp %>%
		group_by(Name) %>%
		summarize(RMSEA = mean(Model.RMSEA), SRMSR = mean(Model.SRMSR), TLI = mean(Model.TLI), CFI = mean(Model.CFI)) %>%
		as.data.frame() 
	dftoLaTeX(data=tabdata, filename='paperstuffout/RealFits')

	#Summarize fit statistics for simulated instruments
	temp <- data %>%
		filter(Name %in% c(simnames))
	tabdata <- temp %>%
		group_by(Name,Number.Items) %>%
		summarize(RMSEA = mean(Model.RMSEA), SRMSR = mean(Model.SRMSR), TLI = mean(Model.TLI), CFI = mean(Model.CFI)) %>%
		as.data.frame() 
	dftoLaTeX(data=tabdata, filename='paperstuffout/SimFits')
}

if ('REAL' %in% run){
	#Plotting IRT parameter dependencies for the real tests
	print_color('============================================================================\n','bcyan')
	print_color('================Plotting Real Test Dependencies For All Items===============\n','bcyan')
	print_color('============================================================================\n','bcyan')
	names <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')
	coeffsets <- list()
	pdf('paperstuffout/Real-Test-Plots.pdf')
	#Collecting fit information for each instrument
	rmsea <- c() 
	srmsr <- c() 
	tli <- c() 
	cfi <- c() 

	for (name in names){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		data <- read.csv(paste0('realdata/',name,'-post.csv'))
		model <- mirt(data=data, model=1, itemtype='2PL')
		print(M2(model))
		print(coef(model, IRTpars=TRUE, simplify=TRUE))
		print(itemfit(model, fit_stats = c('S_X2')))

		#Toggle to remove items or not
		remove <- TRUE
		if (remove){
			#Removing items with abnormal parameters
			if (name == 'FCI'){
				rmitems <- c('Item29')
				#Item 29: b = -10.67
				data <- data %>% select(-all_of(rmitems))
			
			}else if (name == 'FMCE'){
				rmitems <- c()
				data <- data %>% select(-all_of(rmitems))
			
			}else if (name == 'FMCETh'){
				rmitems <- c()
				data <- data %>% select(-all_of(rmitems))
			
			}else if (name == 'K1-20'){
				rmitems <- c()
				data <- data %>% select(-all_of(rmitems))
		
			}else if (name == 'CSEMsam1'){
				rmitems <- c()
				data <- data %>% select(-all_of(rmitems))
			
			}else if (name == 'CSEMsam2'){
				rmitems <- c()
				data <- data %>% select(-all_of(rmitems))
			}
		}

		print_color('============================================================================\n','bviolet')
		print_color('==============================Post Item Removal=============================\n','bviolet')
		print_color('============================================================================\n','bviolet')
		model <- mirt(data=data, model=1, itemtype='2PL')
		print(M2(model))
		rmsea <- c(rmsea, M2(model)$RMSEA)
		srmsr <- c(srmsr, M2(model)$SRMSR)
		tli <- c(tli, M2(model)$TLI)
		cfi <- c(cfi, M2(model)$CFI)
		coeff <- coef(model, IRTpars=TRUE, simplify=TRUE)
		print(coeff)
		print(itemfit(model, fit_stats = c('S_X2')))
		coeff <- as.data.frame(coef(model, IRTpars=TRUE, simplify=TRUE))
		coeff$Label <- rownames(coeff) 
		coeff <- coeff[,c('Label','items.a','items.b')]
		coeff$Instrument <- rep(name, nrow(coeff))
		coeffsets <- append(coeffsets, list(coeff))		
	
		#Plot things
		print(ggplot(data=coeff, mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+geom_text_repel(label=coeff$Label, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10)+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10)+geom_smooth(method='lm', se=FALSE)+theme_bw())#For plotting smooth after items removed
		
		#print(ggplot(data=coeff, mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+geom_text_repel(label=coeff$Label, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10)+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10)+theme_bw())#For plotting all items
		
	}
	dev.off()
	
	#Merge all plots into one figure
	coeffdata <- Reduce(function(x,y) merge(x,y,all = TRUE), coeffsets)
	print(as_tibble(coeffdata))

	fcipl <- ggplot(data=coeffdata[coeffdata$Instrument == 'FCI',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(a) FCI'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	fmcepl <- ggplot(data=coeffdata[coeffdata$Instrument == 'FMCE',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(b) FMCE'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	fmcethpl <- ggplot(data=coeffdata[coeffdata$Instrument == 'FMCETh',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(c) FMCE Thornton'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	k120pl <- ggplot(data=coeffdata[coeffdata$Instrument == 'K1-20',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(d) K1-20'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	csemsam1pl <- ggplot(data=coeffdata[coeffdata$Instrument == 'CSEMsam1',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(e) CSEM 1'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	csemsam2pl <- ggplot(data=coeffdata[coeffdata$Instrument == 'CSEMsam2',], mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+labs(title=paste0('(f) CSEM 2'))+scale_x_continuous(name='Difficulty', limits=c(min(coeffdata$items.b),max(coeffdata$items.b)), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(min(coeffdata$items.a),max(coeffdata$items.b)), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))

	allplot <- plot_grid(fcipl, fmcepl, fmcethpl, k120pl, csemsam1pl, csemsam2pl, ncol=2)
	ggsave(file=paste0('All-Real-Par-Plots.pdf'), path=paste0('paperstuffout/'), allplot, width=7.5, height=10.5, units='in') 

	#Display all fit information
	fit <- data.frame(Test=names, RMSEA=rmsea, SRMSR=srmsr, TLI=tli, CFI=cfi)
	print(fit)
}

if ('SIM' %in% run){
	print_color('============================================================================\n','bcyan')
	print_color('=====================Plotting Simulated Test Dependencies===================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	names <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')

	nitems <- 1000
	allpar <- list()
	for (name in names){
		if (name == 'expgrow'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			itemdisc <- c()
			for (diff in itemdiff){
				amn <- 1.2 * exp(.8 * (diff - 1))
				disc <- multirnorm(1, mean=amn, sd=c(.1), w=c('eq'))
				itemdisc <- c(itemdisc,disc)
			}
		}else if (name == 'gaussian'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			itemdisc <- c()
			for (diff in itemdiff){
				amn <- 3.5 * exp(-.75 * (diff - 0)**2) + .25
				disc <- multirnorm(1, mean=amn, sd=c(.1), w=c('eq'))
				itemdisc <- c(itemdisc,disc)
			}
		}else if (name == 'logist'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			itemdisc <- c()
			for (diff in itemdiff){
				amn <- .25 + (3.75 - .25) * (1 / (1 + exp(-2.5 * (diff - 0)))) 
				disc <- multirnorm(1, mean=amn, sd=c(.1), w=c('eq'))
				itemdisc <- c(itemdisc,disc)
			}
		}else if (name == 'poslinear'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			itemdisc <- c()
			for (diff in itemdiff){
				amn <- .75 * diff + 2
				disc <- multirnorm(1, mean=amn, sd=c(.1), w=c('eq'))
				itemdisc <- c(itemdisc,disc)
			}
		}else if (name == 'mixednorm'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			aw <- c(.6,.3,.1)
			itemdisc <- multirnorm(nitems, mean=c(1,2,3), sd=c(.5,.5,.5), w=aw)
		}else if (name == 'split'){
			itemdiff <- multirnorm(nitems, mean=c(-1.5,0,1.5), sd=c(.5,1,.5), w=c(.25,.5,.25))
			itemdisc <- multirnorm(nitems, mean=c(.5,3), sd=c(.1,.1), w=c(.7,.3))
		}else if (name == 'restricunif'){
			itemdiff <- multirunif(nitems, min=c(-.5), max=c(.5), w=c('eq'))
			itemdisc <- multirunif(nitems, min=c(1.5), max=c(3.5), w=c('eq'))
		}else if (name == 'uniform'){
			itemdiff <- multirunif(nitems, min=c(-2.5), max=c(2.5), w=c('eq'))
			itemdisc <- multirunif(nitems, min=c(0), max=c(4), w=c('eq'))
		}
		par <- data.frame(Analysis = rep(name, nitems), Difficulty = itemdiff, Discrimination = itemdisc)
		allpar <- append(allpar, list(par))
	}
	pardata <- Reduce(function(x,y) merge(x,y,all = TRUE), allpar)
	print(as_tibble(pardata))

	#Plot all separately
	pdf('paperstuffout/Sim-Par-Plots.pdf')

	for (name in names){
		pldf <- pardata %>% filter(Analysis == name)
		print(ggplot(data=pldf, mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10)+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10)+coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,4))+theme_bw())
	}
	dev.off()

	#Plot all on the same figure
	expgrowpl <- ggplot(data=pardata[pardata$Analysis == 'expgrow',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(a) Exponential Growth'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	expgrowpl <- ggMarginal(expgrowpl, margins='y', type='density', color='blue', size=15)

	gaussianpl <- ggplot(data=pardata[pardata$Analysis == 'gaussian',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(b) Gaussian'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	gaussianpl <- ggMarginal(gaussianpl, margins='y', type='density', color='blue', size=15)

	logistpl <- ggplot(data=pardata[pardata$Analysis == 'logist',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(c) Logistic'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	logistpl <- ggMarginal(logistpl, margins='y', type='density', color='blue', size=15)

	poslinearpl <- ggplot(data=pardata[pardata$Analysis == 'poslinear',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(d) Positive Linear'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	poslinearpl <- ggMarginal(poslinearpl, margins='y', type='density', color='blue', size=15)

	mixednormpl <- ggplot(data=pardata[pardata$Analysis == 'mixednorm',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(e) Mixed Normal'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	mixednormpl <- ggMarginal(mixednormpl, margins='y', type='density', color='blue', size=15)

	splitpl <- ggplot(data=pardata[pardata$Analysis == 'split',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(f) Split'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	splitpl <- ggMarginal(splitpl, margins='y', type='density', color='blue', size=15)

	restricunifpl <- ggplot(data=pardata[pardata$Analysis == 'restricunif',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(g) Restricted Uniform'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	restricunifpl <- ggMarginal(restricunifpl, margins='y', type='density', color='blue', size=15)

	uniformpl <- ggplot(data=pardata[pardata$Analysis == 'uniform',], mapping=aes(x=Difficulty,y=Discrimination))+geom_point(size=2)+labs(title=paste0('(h) Uniform'))+scale_x_continuous(name='Difficulty', limits=c(-2.5,2.5), n.breaks=5)+scale_y_continuous(name='Discrimination', limits=c(0,4), n.breaks=5)+theme_bw()+theme(text=element_text(family='serif'))
	uniformpl <- ggMarginal(uniformpl, margins='y', type='density', color='blue', size=15)

	allplot <- plot_grid(expgrowpl, gaussianpl, logistpl, poslinearpl, mixednormpl, splitpl, restricunifpl, uniformpl, ncol=2) 
	ggsave(file=paste0('All-Sim-Par-Plots.pdf'), path=paste0('paperstuffout/'), allplot, width=7.5, height=10.5, units='in') 

}

if ('SCORES' %in% run){
	print_color('============================================================================\n','bcyan')
	print_color('======================Collecting Score Comparison Output====================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	simnames <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')
	legnames <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')
	legsimnames <- c('FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')

	plotdata <- list()
	alphadata <- list()
	for (name in c(simnames,legnames,legsimnames)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		if (name %in% legnames){
			df <- read.csv(paste0('analysisout/summary/',name,'/post/AnalysisOutput1.csv'))
		}else if (name %in% legsimnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput50.csv'))
		}else if (name %in% simnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput350.csv'))
		}
		nitems <- unique(df$Number.Items)
		nstud <- unique(df$Number.Students.Original)
		nrun <- unique(df$Number.Run)

		tempdf <- df %>%
			select(c(Number.Items,Number.Run,Alpha)) %>%
			mutate(Name = rep(name, times = nrow(df)))
		alphadata <- append(alphadata, list(tempdf))
		
		#Collect specific run data of interest
		for (nit in nitems){	
			for (nst in nstud){
				for (r in nrun){
					if (name %in% legnames){
						scores <- read.csv(paste0('analysisout/summary/',name,'/post/Scores-',nst,'.csv'))
					}else {
						scores <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items/',nst,'students/Scores-',paste0(name,r),'.csv'))
					}
			
					#Turn scores into percentiles	
					scoredf <- scores %>%
						mutate(SimSum.Perc = 100*(SimSum.Score / nit)) %>%
						mutate(WS.Perc = 100*(Scaled.Weighted.Score / nit)) %>%
						mutate(Percent.Difference = WS.Perc - SimSum.Perc) 

					appenddf <- scoredf %>%
                                        group_by(SimSum.Perc) %>%
                                        summarize(Mean.WS.Perc = mean(WS.Perc), Mean.Percent.Difference = mean(Percent.Difference)) %>%
                                        mutate(Name = name) %>%
                                        mutate(Items = nit) %>%
                                        mutate(Run.Number = r) %>%
                                        as_tibble()

					if (name %in% c(simnames,legsimnames)){
						RoundWSc <- round(scoredf$Scaled.Weighted.Score,0)
						#Collect R2
						mod1 <- lm(True.Theta ~ Est.Theta, data = scoredf)
						mod2 <- lm(True.Theta ~ Scaled.Weighted.Score, data = scoredf)
						mod3 <- lm(True.Theta ~ SimSum.Score, data = scoredf)
						mod4 <- lm(True.Theta ~ RoundWSc, data = scoredf)
						appenddf$R2TrThEstTh <- summary(mod1)$r.squared
						appenddf$R2TrThWSc <- summary(mod2)$r.squared
						appenddf$R2TrThSimSumSc <- summary(mod3)$r.squared
						appenddf$R2TrThRoundWSc <- summary(mod4)$r.squared
						#Collect Correlations
						appenddf$CORRTrThEstTh <- cor(scoredf$True.Theta, scoredf$Est.Theta, method='pearson')
						appenddf$CORRTrThWSc <- cor(scoredf$True.Theta, scoredf$Scaled.Weighted.Score, method='pearson')
						appenddf$CORRTrThSimSumSc <- cor(scoredf$True.Theta, scoredf$SimSum.Score, method='pearson')
						appenddf$CORRTrThRoundWSc <- cor(scoredf$True.Theta, RoundWSc, method='pearson')
						#Collect Rank RMSE
						TrThRank <- rank(scoredf$True.Theta)
						EstThRank <- rank(scoredf$Est.Theta)
						WScRank <- rank(scoredf$Scaled.Weighted.Score)
						SimSumScRank <- rank(scoredf$SimSum.Score)
						RoundWScRank <- rank(RoundWSc)
						appenddf$RankRMSE.TrTh.EstTh <- sqrt(mean((TrThRank - EstThRank)**2))
						appenddf$RankRMSE.TrTh.WSc <- sqrt(mean((TrThRank - WScRank)**2))
						appenddf$RankRMSE.TrTh.RoundWSc <- sqrt(mean((TrThRank - RoundWScRank)**2))
						appenddf$RankRMSE.TrTh.SimSumSc <- sqrt(mean((TrThRank - SimSumScRank)**2))
					}

					print(as_tibble(appenddf))
					plotdata <- append(plotdata, list(appenddf))
					#break #used for testing plots below
				}
			}
		}
	}#end of name loop
	
	print_color('============================================================================\n','bviolet')
	print_color('=======================Plotting Score Comparison Output=====================\n','bviolet')
	print_color('============================================================================\n','bviolet')
	#Collecting all of the plot data together
	pldf <- Reduce(function(x,y) merge(x,y,all = TRUE), plotdata)
	print(head(as.data.frame(pldf),20))

	#Change names from internal codes to external codes
	old <- c('FMCETh','CSEMsam1','CSEMsam2','FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim','expgrow','gaussian','logist','mixednorm','poslinear','restricunif','split','uniform')
	new <- c('FMCE Thornton','CSEM 1','CSEM 2','FCI(S)','FMCE(S)','FMCE Thornton(S)','K1-20(S)','CSEM 1(S)','CSEM 2(S)','Exponential Growth','Gaussian','Logistic','Mixed Normal', 'Positive Linear','Restricted Uniform','Split','Uniform')
	pldf$Name <- mapvalues(pldf$Name, from = old, to = new)
	legnames <- mapvalues(legnames, from = old, to = new)
	legsimnames <- mapvalues(legsimnames, from = old, to = new)
	simnames <- mapvalues(simnames, from = old, to = new)

	print(head(as.data.frame(pldf),20))

	#Average across runs
	percdiffpl <- pldf %>%
		group_by(SimSum.Perc,Name,Items) %>%
		summarize(Mean.Percent.Difference = mean(Mean.Percent.Difference)) %>%
		mutate(Items = factor(Items, levels=c(10,15,20,25,30,35,40,29,32,43))) %>%
		as_tibble() %>%
		print()

	###################################
	#Plotting Score Percent Differences
	###################################
	limitvec <- c(percdiffpl[percdiffpl$Name %in% c(legnames,legsimnames),]$Mean.Percent.Difference)
	#Real Test Plots
	real <- ggplot(data=percdiffpl[percdiffpl$Name %in% legnames,], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Name,color=Name,shape=Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Name))])+labs(title=paste0('(a) Instruments'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	
	#Real Test Simulation Plots
	simreal <- ggplot(data=percdiffpl[percdiffpl$Name %in% legsimnames,], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Name,color=Name,shape=Name))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Name))])+labs(title=paste0('(b) Simulated Instruments'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	
	simrealplot <- plot_grid(real,simreal, nrow=2) 
	ggsave(file=paste0('RealTests-ScorePercentage-Plots.pdf'), path=paste0('paperstuffout/'), simrealplot, width=7.5, height=10.5, units='in') 

	#Simulation Plots
	limitvec <- c(percdiffpl[percdiffpl$Name %in% c(simnames),]$Mean.Percent.Difference)
	expgrow <- ggplot(data=percdiffpl[percdiffpl$Name == 'Exponential Growth',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(a) Exponential Growth'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	gaussian <- ggplot(data=percdiffpl[percdiffpl$Name == 'Gaussian',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(b) Gaussian'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	logist <- ggplot(data=percdiffpl[percdiffpl$Name == 'Logistic',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(c) Logistic'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	poslinear <- ggplot(data=percdiffpl[percdiffpl$Name == 'Positive Linear',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(d) Positive Linear'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	mixednorm <- ggplot(data=percdiffpl[percdiffpl$Name == 'Mixed Normal',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(e) Mixed Normal'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	split <- ggplot(data=percdiffpl[percdiffpl$Name == 'Split',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(f) Split'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	restricunif <- ggplot(data=percdiffpl[percdiffpl$Name == 'Restricted Uniform',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(g) Restricted Uniform'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	uniform <- ggplot(data=percdiffpl[percdiffpl$Name == 'Uniform',], mapping=aes(x=SimSum.Perc,y=Mean.Percent.Difference,group=Items,color=Items,shape=Items))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(percdiffpl$Items))], breaks=c(10,15,20,25,30,35,40))+labs(title=paste0('(h) Uniform'))+scale_x_continuous(name='Simple Sum Score(%)', n.breaks=5, limits=c(0,100))+scale_y_continuous(name='Mean: WS(%) - SS(%)', n.breaks=5, limits=c(min(limitvec),max(limitvec)))+annotate('segment', x=0, y=0, xend=100, yend=0, colour='black', linetype='dashed')+theme_bw()+theme(text=element_text(family='serif'))
	
	#Combine
	simplot <- plot_grid(expgrow+theme(legend.position='none'),gaussian+theme(legend.position='none'),logist+theme(legend.position='none'),poslinear+theme(legend.position='none'),mixednorm+theme(legend.position='none'),split+theme(legend.position='none'),restricunif+theme(legend.position='none'),uniform+theme(legend.position='none'), ncol=2)
	legend <- get_legend(expgrow+guides(color = guide_legend(nrow=1))+theme(legend.position='bottom'))
	simplot <- plot_grid(simplot, legend, ncol=1, rel_heights=c(.9,.1))
	ggsave(file=paste0('SimTests-ScorePercentage-Plots.pdf'), path=paste0('paperstuffout/'), simplot, width=7.5, height=10.5, units='in') 

	#########################
	#Plotting R^2 Differences
	#########################
	trthpl <- pldf %>%
		filter(Name %in% c(simnames,legsimnames)) %>%
		group_by(Name,Items) %>%
		summarize(R2TrThEstTh = mean(R2TrThEstTh), R2TrThWSc = mean(R2TrThWSc), R2TrThRoundWSc = mean(R2TrThRoundWSc), R2TrThSimSumSc = mean(R2TrThSimSumSc)) %>%
		rename('Estimated Latent' = R2TrThEstTh, WS = R2TrThWSc, 'Rounded WS' = R2TrThRoundWSc, SS = R2TrThSimSumSc) %>%
		as_tibble() 
	trthpl <- melt(trthpl, id = c('Name','Items'))
	trthpl <- trthpl %>%
		rename(Score = variable)
	print(trthpl)

	#Legacy Simulation Plots
	limitvec <- c(trthpl[trthpl$Name %in% c(legsimnames),]$value)
	legsimplot <- ggplot()+geom_point(data=trthpl[trthpl$Name %in% legsimnames,], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score), size=3)+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+scale_x_continuous(name='Number of Items')+scale_y_continuous(name='Mean R-Squared')+geom_text_repel(data=trthpl[trthpl$Name %in% legsimnames,], mapping=aes(x=Items,y=value,label=gsub('postsim','',Name)), size=4, max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+coord_cartesian(xlim=c(15,45), ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'), legend.position='none')
	ggsave(file=paste0('LegSimTests-TrThR2-Plots.pdf'), path=paste0('paperstuffout/'), legsimplot, width=7.5, height=7.5, units='in') 

	#Simulation Plots
	limitvec <- c(trthpl[trthpl$Name %in% c(simnames),]$value)
	expgrow <- ggplot(data=trthpl[trthpl$Name == 'Exponential Growth',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(a) Exponential Growth'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	gaussian <- ggplot(data=trthpl[trthpl$Name == 'Gaussian',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(b) Gaussian'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	logist <- ggplot(data=trthpl[trthpl$Name == 'Logistic',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(c) Logistic'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	poslinear <- ggplot(data=trthpl[trthpl$Name == 'Positive Linear',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(d) Positive Linear'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	mixednorm <- ggplot(data=trthpl[trthpl$Name == 'Mixed Normal',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(e) Mixed Normal'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	split <- ggplot(data=trthpl[trthpl$Name == 'Split',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(f) Split'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	restricunif <- ggplot(data=trthpl[trthpl$Name == 'Restricted Uniform',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(g) Restricted Uniform'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	uniform <- ggplot(data=trthpl[trthpl$Name == 'Uniform',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(h) Uniform'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Mean R-Squared', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))

	#Combine
	r2plot <- plot_grid(expgrow+theme(legend.position='none'),gaussian+theme(legend.position='none'),logist+theme(legend.position='none'),poslinear+theme(legend.position='none'),mixednorm+theme(legend.position='none'),split+theme(legend.position='none'),restricunif+theme(legend.position='none'),uniform+theme(legend.position='none'), ncol=2)
	legend <- get_legend(expgrow+guides(color = guide_legend(nrow=1))+theme(legend.position='bottom'))
	r2plot <- plot_grid(r2plot, legend, ncol=1, rel_heights=c(.9,.1))
	ggsave(file=paste0('SimTests-TrThR2-Plots.pdf'), path=paste0('paperstuffout/'), r2plot, width=7.5, height=10.5, units='in') 
	
	###############################
	#Plotting Rank RMSE Differences
	###############################
	trthpl <- pldf %>%
		filter(Name %in% c(simnames,legsimnames)) %>%
		group_by(Name,Items) %>%
		summarize(RankRMSE.TrTh.EstTh = mean(RankRMSE.TrTh.EstTh), RankRMSE.TrTh.WSc = mean(RankRMSE.TrTh.WSc), RankRMSE.TrTh.RoundWSc = mean(RankRMSE.TrTh.RoundWSc), RankRMSE.TrTh.SimSumSc = mean(RankRMSE.TrTh.SimSumSc)) %>%
		rename('Estimated Latent' = RankRMSE.TrTh.EstTh, WS = RankRMSE.TrTh.WSc, 'Rounded WS' = RankRMSE.TrTh.RoundWSc, SS = RankRMSE.TrTh.SimSumSc) %>%
		as_tibble() 
	trthpl <- melt(trthpl, id = c('Name','Items'))
	trthpl <- trthpl %>%
		rename(Score = variable)
	print(trthpl)
	
	#Legacy Simulation Plots
	limitvec <- c(trthpl[trthpl$Name %in% c(legsimnames),]$value)
	legsimplot <- ggplot()+geom_point(data=trthpl[trthpl$Name %in% legsimnames,], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score), size=3)+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+scale_x_continuous(name='Number of Items')+scale_y_continuous(name='Rank Order RMSE')+geom_text_repel(data=trthpl[trthpl$Name %in% legsimnames,], mapping=aes(x=Items,y=value,label=gsub('postsim','',Name)), size=3, max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+coord_cartesian(xlim=c(15,45), ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'), legend.position='none')
	ggsave(file=paste0('LegSimTests-TrThRankRMSE-Plots.pdf'), path=paste0('paperstuffout/'), legsimplot, width=7.5, height=7.5, units='in') 

	#Simulation Plots
	limitvec <- c(trthpl[trthpl$Name %in% c(simnames),]$value)
	expgrow <- ggplot(data=trthpl[trthpl$Name == 'Exponential Growth',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(a) Exponential Growth'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank ORder RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	gaussian <- ggplot(data=trthpl[trthpl$Name == 'Gaussian',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(b) Gaussian'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	logist <- ggplot(data=trthpl[trthpl$Name == 'Logistic',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(c) Logistic'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	poslinear <- ggplot(data=trthpl[trthpl$Name == 'Positive Linear',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(d) Positive Linear'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	mixednorm <- ggplot(data=trthpl[trthpl$Name == 'Mixed Normal',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(e) Mixed Normal'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	split <- ggplot(data=trthpl[trthpl$Name == 'Split',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(f) Split'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	restricunif <- ggplot(data=trthpl[trthpl$Name == 'Restricted Uniform',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(g) Restricted Uniform'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))
	uniform <- ggplot(data=trthpl[trthpl$Name == 'Uniform',], mapping=aes(x=Items,y=value,group=Score,color=Score,shape=Score))+geom_point()+geom_line()+scale_shape_manual(values=ggshapes[1:length(unique(trthpl$Score))])+labs(title=paste0('(h) Uniform'))+scale_x_continuous(name='Number of Items', n.breaks=5)+scale_y_continuous(name='Rank Order RMSE', n.breaks=5)+coord_cartesian(ylim=c(min(limitvec),max(limitvec)))+theme_bw()+theme(text=element_text(family='serif'))

	#Combine
	rankplot <- plot_grid(expgrow+theme(legend.position='none'),gaussian+theme(legend.position='none'),logist+theme(legend.position='none'),poslinear+theme(legend.position='none'),mixednorm+theme(legend.position='none'),split+theme(legend.position='none'),restricunif+theme(legend.position='none'),uniform+theme(legend.position='none'), ncol=2)
	legend <- get_legend(expgrow+guides(color = guide_legend(nrow=1))+theme(legend.position='bottom'))
	rankplot <- plot_grid(rankplot, legend, ncol=1, rel_heights=c(.9,.1))
	ggsave(file=paste0('SimTests-TrThRankRMSE-Plots.pdf'), path=paste0('paperstuffout/'), rankplot, width=7.5, height=10.5, units='in') 
	
	###########################
	#Plotting Alpha Differences
	###########################
	alphadf <- do.call(rbind, alphadata)
	alphapl <- alphadf %>%
		group_by(Name,Number.Items) %>%
		summarize(Alpha = mean(Alpha)) %>%
		as_tibble() %>%
		print()
	alphapl$Name <- mapvalues(alphapl$Name, from = old, to = new)

	alphaplot <- ggplot()+geom_point(data=alphapl[alphapl$Name %in% simnames,], mapping=aes(x=Number.Items,y=Alpha,group=Name,color=Name,shape=Name))+geom_line(data=alphapl[alphapl$Name %in% simnames,], mapping=aes(x=Number.Items,y=Alpha,group=Name,color=Name))+scale_shape_manual(values=ggshapes[1:length(unique(alphapl[alphapl$Name %in% simnames,]$Name))])+scale_x_continuous(name='Number of Items', limits=c(10,45))+scale_y_continuous(name='Alpha', n.breaks=10)+geom_point(data=alphapl[alphapl$Name %in% c(legnames,legsimnames),], mapping=aes(x=Number.Items,y=Alpha), size=3)+geom_text_repel(data=alphapl[alphapl$Name %in% c(legnames,legsimnames),], mapping=aes(x=Number.Items,y=Alpha,label=Name), size=3, max.overlaps=getOption('ggrepel.max.overlaps', default=Inf))+theme_bw()+theme(text=element_text(family='serif'), legend.position='none')	
	ggsave(file=paste0('AllTests-Alpha-Plots.pdf'), path=paste0('paperstuffout/'), alphaplot, width=7.5, height=7.5, units='in') 
}

if ('PLOTSCORES' %in% run){
	print_color('============================================================================\n','bcyan')
	print_color('======================Collecting Score Comparison Output====================\n','bcyan')
	print_color('============================================================================\n','bcyan')
	simnames <- c('expgrow','gaussian','logist','poslinear','mixednorm','split','restricunif','uniform')
	legnames <- c('FCI','FMCE','FMCETh','K1-20','CSEMsam1','CSEMsam2')
	legsimnames <- c('FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim')

	#Collect scores for the first run to plot against
	for (name in c(simnames,legnames,legsimnames)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		if (name %in% legnames){
			df <- read.csv(paste0('analysisout/summary/',name,'/post/AnalysisOutput1.csv'))
		}else if (name %in% legsimnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput50.csv'))
		}else if (name %in% simnames){
			df <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/AnalysisOutput350.csv'))
		}
	
		#So as not to produce too many plots, will look at specific simulations 
		r <- 1
		if (name %in% legnames){
			nit <- unique(df$Number.Items)
			nst <- unique(df$Number.Students.Original)
		}else if (name %in% legsimnames){
			nst <- 5000
			if (name == 'FMCEpostsim'){
				nit <- 43
			}else if (name == 'K1-20postsim'){
				nit <- 20
			}else if (name == 'FCIpostsim'){
				nit <- 29
			}else if (name == 'CSEMsam1postsim' | name == 'CSEMsam2postsim'){
				nit <- 32
			}else if (name == 'FMCEThpostsim'){
				nit <- 30
			}
		}else if (name %in% simnames){
			nit <- 30
			nst <- 5000
		}

		#Collect specific run data of interest
		if (name %in% legnames){
			scores <- read.csv(paste0('analysisout/summary/',name,'/post/Scores-',nst,'.csv'))
		}else {
			scores <- read.csv(paste0('analysisout/summary/IRT/flex/',name,'/',nit,'items','/',nst,'students','/Scores-',paste0(name,r),'.csv'))
		}

		scoredf <- scores %>%
			rename(SimSumSc = SimSum.Score, WSc = Scaled.Weighted.Score)

		#Round WSc to test integer effect
		scoredf$RoundedWSc <- round(scoredf$WSc,0)

		if (name %in% c(legsimnames,simnames)){
			#Plot scores against true latent
			#Tr.Th vs Est.Th
			trthvestth <- ggplot(data=scoredf, mapping=aes(x=Est.Theta,y=True.Theta))+geom_point()+labs(title=paste0('(e) True Latent vs Estimated Latent'))+scale_x_continuous(name='Estimated Latent', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
			#Tr.Th vs WSc
			trthvws <- ggplot(data=scoredf, mapping=aes(x=WSc,y=True.Theta))+geom_point()+labs(title=paste0('(f) True Latent vs Weighted Score'))+scale_x_continuous(name='Weighted Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
			#Tr.Th vs RoundedWSc
			trthvrws <- ggplot(data=scoredf, mapping=aes(x=RoundedWSc,y=True.Theta))+geom_point()+labs(title=paste0('(g) True Latent vs Rounded Weighted Score'))+scale_x_continuous(name='Rounded Weighted Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
			#Tr.Th vs SimSumSc
			trthvss <- ggplot(data=scoredf, mapping=aes(x=SimSumSc,y=True.Theta))+geom_point()+labs(title=paste0('(h) True Latent vs Simple Sum Score'))+scale_x_continuous(name='Simple Sum Score', n.breaks=10)+scale_y_continuous(name='True Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
		}

		#Plot scores against the estimated latent
		#Est.Th vs WSc
		estthvws <- ggplot(data=scoredf, mapping=aes(x=WSc,y=Est.Theta))+geom_point()+labs(title=paste0('(a) Estimated Latent vs Weighted Score'))+scale_x_continuous(name='Weighted Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
		#Est.Th vs RoundedWSc
		estthvrws <- ggplot(data=scoredf, mapping=aes(x=RoundedWSc,y=Est.Theta))+geom_point()+labs(title=paste0('(b) Estimated Latent vs Rounded Weighted Score'))+scale_x_continuous(name='Rounded Weighted Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
		#Est.Th vs SimSumSc
		estthvss <- ggplot(data=scoredf, mapping=aes(x=SimSumSc,y=Est.Theta))+geom_point()+labs(title=paste0('(c) Estimated Latent vs Simple Sum Score'))+scale_x_continuous(name='Simple Sum Score', n.breaks=10)+scale_y_continuous(name='Estimated Latent', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))
		
		#Plot simple sum score against the weighted score
		#WSc vs SimSumSc
		wsvss <- ggplot(data=scoredf, mapping=aes(x=SimSumSc,y=WSc))+geom_point()+labs(title=paste0('(d) Weighted Score vs Simple Sum Score'))+scale_x_continuous(name='Simple Sum Score', n.breaks=10)+scale_y_continuous(name='Weighted Score', n.breaks=10)+theme_bw()+theme(text=element_text(family='serif'), plot.title=element_text(size=10))

		#Change names from internal codes to external codes
		old <- c('FMCETh','CSEMsam1','CSEMsam2','FCIpostsim','FMCEpostsim','FMCEThpostsim','K1-20postsim','CSEMsam1postsim','CSEMsam2postsim','expgrow','gaussian','logist','mixednorm','poslinear','restricunif','split','uniform')
		new <- c('FMCE Thornton','CSEM 1','CSEM 2','FCI(S)','FMCE(S)','FMCE Thornton(S)','K1-20(S)','CSEM 1(S)','CSEM 2(S)','Exponential Growth','Gaussian','Logistic','Mixed Normal', 'Positive Linear','Restricted Uniform','Split','Uniform')
		name <- mapvalues(name, from = old, to = new)
		legnames <- mapvalues(legnames, from = old, to = new)
		legsimnames <- mapvalues(legsimnames, from = old, to = new)
		simnames <- mapvalues(simnames, from = old, to = new)

		#Combine score plots for each sample
		if (name %in% legnames){
			scoreplot <- plot_grid(estthvws,estthvrws,estthvss,wsvss, ncol=2)
			title <- ggdraw()+draw_label(name,fontface='bold')+theme_bw()+theme(text=element_text(family='serif'), panel.border=element_blank(), plot.background=element_blank(), panel.background=element_blank())
			scoreplot <- plot_grid(title, scoreplot, ncol=1, rel_heights=c(.05,1))
			ggsave(file=paste0('ScorePlots-',name,'.pdf'), path=paste0('paperstuffout/'), scoreplot, width=7.5, height=7.5, units='in') 
		}else {
			scoreplot <- plot_grid(estthvws,estthvrws,estthvss,wsvss,trthvestth,trthvws,trthvrws,trthvss, ncol=2)
			title <- ggdraw()+draw_label(name,fontface='bold')+theme_bw()+theme(text=element_text(family='serif'), panel.border=element_blank(), plot.background=element_blank(), panel.background=element_blank())
			scoreplot <- plot_grid(title, scoreplot, ncol=1, rel_heights=c(.05,1))
			ggsave(file=paste0('ScorePlots-',name,'.pdf'), path=paste0('paperstuffout/'), scoreplot, width=7.5, height=10.5, units='in') 
		}

		#Reset these
		legnames <- mapvalues(legnames, from = new, to = old)
		legsimnames <- mapvalues(legsimnames, from = new, to = old)
		simnames <- mapvalues(simnames, from = new, to = old)
	}
}







#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
