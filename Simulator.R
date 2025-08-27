#!/usr/bin/env Rscript
#Above line allows code to be run using ./Simulator.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(insight)#print_color function
library(argparser)#anything parser related
source('src/MultiPDF.R')#use of multiple random pdfs 

#Adding argument parsers so that I can vary the simulated data from the command line
parser <- arg_parser('Options for varying the simulated data generated')
parser <- add_argument(parser, "--flex", help = 'running in flex mode when TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--run", help = 'running in run mode when TRUE',nargs='*',default=FALSE)
parser <- add_argument(parser, "--nrun", help = 'number of runs when in run mode',nargs='*',default=10)
parser <- add_argument(parser, "--name", help = 'name of output when in flex/run mode if other name desired',nargs='*',default='TEST')

#flex mode arguments
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
parser <- add_argument(parser, "--pardist", help = 'parameter distribution type when in flex mode, options are norm and unif: default is norm',nargs='*',default='norm')

#run mode arguments
parser <- add_argument(parser, "--bmn", help = 'IRT difficulty mean when in flex mode',nargs='*',default=c(0,0,0))
parser <- add_argument(parser, "--bsd", help = 'IRT difficulty sd when in flex mode',nargs='*',default=c(1,1,0))
parser <- add_argument(parser, "--bw", help = 'IRT difficulty weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--amn", help = 'IRT discrimination mean when in flex mode',nargs='*',default=c(1.5,1.5,0))
parser <- add_argument(parser, "--asd", help = 'IRT discrimination sd when in flex mode',nargs='*',default=c(.5,.5,0))
parser <- add_argument(parser, "--aw", help = 'IRT discrimination weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--thmn", help = 'IRT theta mean when in flex mode',nargs='*',default=c(0,0,0))
parser <- add_argument(parser, "--thsd", help = 'IRT theta sd when in flex mode',nargs='*',default=c(1,1,0))
parser <- add_argument(parser, "--thw", help = 'IRT theta weights when in flex mode: default is eq',nargs='*',default=c('eq'))
arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])
if (!('eq' %in% arg$aw)){
	arg$aw <- sapply(strsplit(arg$aw,',')[[1]], function(x) as.numeric(x))
}else {
	arg$aw <- 'eq'
}
if (!('eq' %in% arg$bw)){
	arg$bw <- sapply(strsplit(arg$bw,',')[[1]], function(x) as.numeric(x))
}else {
	arg$bw <- 'eq'
}
if (!('eq' %in% arg$thw)){
	arg$thw <- sapply(strsplit(arg$thw,',')[[1]], function(x) as.numeric(x))
}else {
	arg$thw <- 'eq'
}



#Check inputs for correct formatting
if (length(arg$amn) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!DISCRIMINATION MEAN INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if (length(arg$asd) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!DISCRIMINATION SD INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if ((length(arg$aw) %% 3 != 0) & !('eq' %in% arg$aw)){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!DISCRIMINATION WEIGHTS INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if (length(arg$bmn) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!DIFFICULTY MEAN INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if (length(arg$bsd) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!DIFFICULTY SD INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if ((length(arg$bw) %% 3 != 0) & !('eq' %in% arg$bw)){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!DIFFICULTY WEIGHTS INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if (length(arg$thmn) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!THETA MEAN INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if (length(arg$thsd) %% 3 != 0){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!THETA SD INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}
if ((length(arg$thw) %% 3 != 0) & !('eq' %in% arg$thw)){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!THETA WEIGHTS INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}

#Checking inputs for consistency
if ((length(arg$amn) %/% 3) != (length(arg$asd) %/% 3)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!DISCRIMINATION INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
		break
}else {
	if (!('eq' %in% arg$aw)){
		if ((length(arg$amn) %/% 3) != (length(arg$aw) %/% 3)){
			print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!DISCRIMINATION INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
			break
		}
	}
}
if ((length(arg$bmn) %/% 3) != (length(arg$bsd) %/% 3)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!DIFFICULTY INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
		break
}else {
	if (!('eq' %in% arg$bw)){
		if ((length(arg$bmn) %/% 3) != (length(arg$bw) %/% 3)){
			print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!DIFFICULTY INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
			break
		}
	}
}
if ((length(arg$thmn) %/% 3) != (length(arg$thsd) %/% 3)){
		print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!THETA INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
		break
}else {
	if (!('eq' %in% arg$thw)){
		if ((length(arg$thmn) %/% 3) != (length(arg$thw) %/% 3)){
			print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!THETA INPUT ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
			break
		}
	}
}

#Running checks on user input
if (arg$flex){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FLEX MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else {
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FIXED MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}

#Running checks on user input
if (arg$run){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING RUN MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}

print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')

###############################################################################################################
##################################################FUNCTIONS####################################################
###############################################################################################################
#Function to generate student repsonse
response <- function(g,itempar,th=0){
	set.seed(NULL)#used to unset the seed so the responses will differ for each item
	st <- runif(1, min=0, max=1)
	
	#Set probabilities based on model
	a <- itempar$Discrimination
	b <- itempar$Difficulty
	p <- exp(a*(th - b))/(1 + exp(a*(th - b)))
	
	#Set student response based on random uniform dice roll and probability
	if (st < p){
		r <- 1
	}else {
		r <- 0
	}	

	return(r)
}

#Function to increment parameters
parincr <- function(par){
	templist <- list()
	npar <- length(par) %/% 3
	for (i in 1:npar){
		temp <- list(seq(from = par[(3*(i-1)+1)], to = par[(3*(i-1)+2)], by = par[(3*(i-1)+3)]))
		templist <- append(templist,temp)
	}
	#Create df of all possible combinations 
	parout <- expand.grid(templist)
	return(parout)
}

#Function to increment weights
wincr <- function(weights){
	tempdf <- data.frame(Var1 = seq(from = weights[1], to = weights[2], by = weights[3]))
	nw <- length(weights) %/% 3
	for (i in 2:nw){
		temp <- seq(from = weights[(3*(i-1)+1)], to = weights[(3*(i-1)+2)], by = weights[(3*(i-1)+3)])
		tempdf[paste0('Var',i)] <- temp
	}
	wout <- tempdf
	#Create df of all possible combinations 
	return(wout)
}
###############################################################################################################
##################################################SIMULATION###################################################
###############################################################################################################
#Simulating data 
#Flexible simulations based on user input
if (arg$flex){
	#If multiple runs of the same set of items or students, but with potentially varying parameters
	if (arg$run){
		nrun <- arg$nrun

		#Incrementing values	
		print_color(paste0('==============================================================================\n'),'bviolet')
		print_color(paste0('===========================Incrementing Parameters============================\n'),'bviolet')
		print_color(paste0('==============================================================================\n'),'bviolet')
		amndf <- parincr(par = arg$amn)
		asddf <- parincr(par = arg$asd)
		if (!('eq' %in% arg$aw)){
			awdf <- wincr(weights = arg$aw)
		}else {
			awdf <- data.frame(Var1 = arg$aw)
		}

		bmndf <- parincr(par = arg$bmn)
		bsddf <- parincr(par = arg$bsd)
		if (!('eq' %in% arg$bw)){
			bwdf <- wincr(weights = arg$bw)
		}else {
			bwdf <- data.frame(Var1 = arg$bw)
		}

		thmndf <- parincr(par = arg$thmn)
		thsddf <- parincr(par = arg$thsd)
		if (!('eq' %in% arg$thw)){
			thwdf <- wincr(weights = arg$thw)
		}else {
			thwdf <- data.frame(Var1 = arg$thw)
		}

	}else {
		nrun <- 1
	}
	
	#Build datasets
	for (nit in numitems){
		for (nst in numst){

			#Keep track of different files
			filecount <- 1
	
			#Loop over different parameter spaces
			for (ai in 1:nrow(amndf)){
				amn <- amndf[ai,]
				for (aj in 1:nrow(asddf)){
					asd <- asddf[aj,]
					for (ak in 1:nrow(awdf)){
						aw <- awdf[ak,]
						for (bi in 1:nrow(bmndf)){
							bmn <- bmndf[bi,]
							for (bj in 1:nrow(bsddf)){
								bsd <- bsddf[bj,]
								for (bk in 1:nrow(bwdf)){
									bw <- bwdf[bk,]
									for (thi in 1:nrow(thmndf)){
										thmn <- thmndf[thi,]
										for (thj in 1:nrow(thsddf)){
											thsd <- thsddf[thj,]
											for (thk in 1:nrow(thwdf)){
												thw <- thwdf[thk,]

												#Generate data
												for (r in 1:nrun){
													#Setting incremented values
													nitems <- nit
													Item <- paste0('Item',1:nitems)
													ns <- nst
													
													#Saving item generators
													if (!dir.exists(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'))){dir.create(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'), recursive = TRUE)}
													gen <- file(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,filecount),'-Generators.txt'), 'w')
													writeLines(paste0('Number of Items: ',nitems), con = gen)
													writeLines(paste0('Number of Students: ',ns), con = gen)
													writeLines(paste0('Parameter Distribution Type: ',arg$pardist), con = gen)
													writeLines(paste0('Difficulty Mean: ',paste0(bmn,collapse=',')), con = gen)
													writeLines(paste0('Difficulty Standard Deviation: ',paste0(bsd,collapse=',')), con = gen)
													writeLines(paste0('Difficulty Weighting: ',paste0(bw,collapse=',')), con = gen)
													writeLines(paste0('Discrimination Mean: ',paste0(amn,collapse=',')), con = gen)
													writeLines(paste0('Discrimination Standard Deviation: ',paste0(asd,collapse=',')), con = gen)
													writeLines(paste0('Discrimination Weighting: ',paste0(aw,collapse=',')), con = gen)
													writeLines(paste0('Theta Mean: ',paste0(thmn,collapse=',')), con = gen)
													writeLines(paste0('Theta Standard Deviation: ',paste0(thsd,collapse=',')), con = gen)
													writeLines(paste0('Theta Weighting: ',paste0(thw,collapse=',')), con = gen)
													close(gen)

													#True item parameters that will be used in the generated data
													par <- data.frame(Items = Item, Difficulty = multirnorm(nitems, mean=bmn, sd=bsd, w=bw), Discrimination = multirnorm(nitems, mean=amn, sd=asd, w=aw))
													print_color(paste0('==============================================================================\n'),'bold')
													print_color(paste0('==============================Item Parameters=================================\n'),'bold')
													print_color(paste0('==============================================================================\n'),'bold')
													print(par)
													write.csv(par, paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,filecount),'-Items.csv'), row.names = FALSE)	

													#Setting true proficiencies
													df <- data.frame(ID = 1:ns, Theta = multirnorm(ns, mean=thmn, sd=thsd, w=thw))

													#Fill in student responses 
													print_color(paste0('==============================================================================\n'),'bcyan')
													print_color(paste0('========================Generating Student Responses==========================\n'),'bcyan')
													print_color(paste0('==============================================================================\n'),'bcyan')
													for (j in Item){
														temp <- c()
														
														for (i in 1:ns){
															resp <- response(g='IRT', itempar=par[par$Items == j,], th=df[df$ID == i,]$Theta)
															temp <- c(temp, resp)
														}
														df[[j]] <- temp
													}
													print(as_tibble(df))
													
													#Saving flex datasets
													write.csv(df, paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,filecount),'-Data.csv'), row.names = FALSE)
													filecount <- filecount + 1
												}#end of nrun loop
											}#end of thw loop
										}#end of thsd loop
									}#end of thmn loop
								}#end of bw loop
							}#end of bsd loop
						}#end of bmn loop	
					}#end of aw loop
				}#end of asd loop
			}#end of amn loop
		}#end of ns loop
	}#end of nitems loop

}else {
	#Generating massive datasets that are fixed
	nitems <- 100
	Item <- paste0('Item',1:nitems)
	ns <- 10000

	#True item parameters that will be used in the generated data
	set.seed(328)#consistent item parameters when run
	irtdiff <- rnorm(nitems, mean=0, sd=1)
	irtdisc <- rnorm(nitems, mean=1.5, sd=.5)
	
	print_color(paste0('==============================================================================\n'),'bold')
	print_color(paste0('============================IRT Item Parameters===============================\n'),'bold')
	print_color(paste0('==============================================================================\n'),'bold')
	irtpar <- data.frame(Items = Item, Difficulty = irtdiff, Discrimination = irtdisc)
	print(irtpar)	
	write.csv(irtpar, paste0('simdata/fixed/IRT-Items.csv'), row.names = FALSE)	

	#Setting true proficiencies 
	set.seed(326)#consistent proficiencies when run
	irtdf <- data.frame(ID = 1:ns, Theta = rnorm(ns, mean=0, sd=1))

	#Because there are a lot of responses, then I want to output progress
	itprogress <- seq(1, nitems, by = nitems/10)
	itprogress <- paste0('Item',itprogress)

	#Fill in student responses 
	print_color(paste0('==============================================================================\n'),'bcyan')
	print_color(paste0('========================Generating Student Responses==========================\n'),'bcyan')
	print_color(paste0('==============================================================================\n'),'bcyan')
	for (j in Item){
		if (j %in% itprogress){
			prog <- which(itprogress == j)
			print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!',(prog*10),'% DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
		}

		irttemp <- c()
		
		for (i in 1:ns){
			irtr <- response(g='IRT', itempar=irtpar[irtpar$Items == j,], mod=irtdf[irtdf$ID == i,]$Theta)
			irttemp <- c(irttemp, irtr)
		}
		irtdf[[j]] <- irttemp
	}
	print(as_tibble(irtdf))
	
	#Saving fixed datasets 
	write.csv(irtdf, paste0('simdata/fixed/IRT-Data.csv'), row.names = FALSE)	
}

#Curious about runtime 
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
