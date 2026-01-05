#!/usr/bin/env Rscript
#Above line allows code to be run using ./PaperStuff.R in terminal

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
library(geomtextpath)#geom_text_segment
library(ggrepel)#geom_text_repel
library(cowplot)#combining plots
library(directlabels)#used to add labels on plots when lots of lines are used together

run <- c('REAL')

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
}

if ('REAL' %in% run){
	#Plotting IRT parameter dependencies for the real tests
	print_color('============================================================================\n','bcyan')
	print_color('================Plotting Real Test Dependencies For All Items===============\n','bcyan')
	print_color('============================================================================\n','bcyan')
	names <- c('FCI','FMCE','FMCETh','K1-20','K1-7','CSEMsam1','CSEMsam2')

	names <- c('FMCETh')

	pdf('paperstuffout/Real-Test-Plots.pdf')
	for (name in names){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',name,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		data <- read.csv(paste0('realdata/',name,'-post.csv'))
		model <- mirt(data=data, model=1, itemtype='2PL')
		print(M2(model))
		print(coef(model, IRTpars=TRUE, simplify=TRUE))
		print(itemfit(model, fit_stats = c('S_X2')))

		#Removing items with poor parameters: |b| > 3
		#Removing the items sequentially with the highest RMSEA S_Chi^2 if tied then take highest S_Chi^2
		#Repeat item removal until TLI & CFI > .9, RMSEA < .06, & SRMSR < .05 or number of items is less than 10 
		if (name == 'FCI'){
			rmitems <- c('Item29','Item15','Item21','Item22','Item11','Item9','Item27','Item26','Item16','Item2','Item5','Item25','Item14','Item28')
			#Item 29: b = -10.67
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'FMCE'){
			rmitems <- c('Item15','Item33')
			#Item 15: b = -4.85 
			#Item 33: b = -3.57
			#fit indices got substantially worse during the trimming process based on item fit stats
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'FMCETh'){
			rmitems <- c('Item43','Item16','Item19','Item1','Item18','Item4')
			#Item 43: b = -3.07 
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'K1-20'){
			rmitems <- c()
			#Item : Full test S_Chi^2 over 100
			#Fit indices fine already
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'K1-7'){
			rmitems <- c()
			#Item : Full test S_Chi^2 over 100
			#Fit indices fine already
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'CSEMsam1'){
			rmitems <- c('Item4')
			#Item 4: Full test S_Chi^2 over 100
			#Item 5,10,11,23,25,29: Reduced test S_Chi^2 over 30
			data <- data %>% select(-all_of(rmitems))
		
		}else if (name == 'CSEMsam2'){
			rmitems <- c()
			#Item : Full test S_Chi^2 over 100
			#Item : Reduced test S_Chi^2 over 50
			data <- data %>% select(-all_of(rmitems))
		
		}

		print_color('============================================================================\n','bviolet')
		print_color('==============================Post Item Removal=============================\n','bviolet')
		print_color('============================================================================\n','bviolet')
		model <- mirt(data=data, model=1, itemtype='2PL')
		print(M2(model))
		coeff <- coef(model, IRTpars=TRUE, simplify=TRUE)
		print(coeff)
		print(itemfit(model, fit_stats = c('S_X2')))
		coeff <- as.data.frame(coef(model, IRTpars=TRUE, simplify=TRUE))
		coeff$Label <- rownames(coeff) 
		coeff <- coeff[,c('Label','items.a','items.b')]
		
		#Plot things
		print(ggplot(data=coeff, mapping=aes(x=items.b,y=items.a))+geom_point(size=2)+geom_text_repel(label=coeff$Label, size=2,max.overlaps=getOption('ggrepel.max.overlaps',default=Inf))+scale_x_continuous(name='2PL Item Difficulty', n.breaks=10, limits=c(-3,3))+scale_y_continuous(name='2PL Item Discrimination', n.breaks=10)+geom_smooth(method = lm, se = TRUE))
	}
	dev.off()
}

if ('SIMS' %in% run){
	print_color('============================================================================\n','bcyan')
	print_color('=====================Plotting Simulated Test Dependencies===================\n','bcyan')
	print_color('============================================================================\n','bcyan')

}



#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
