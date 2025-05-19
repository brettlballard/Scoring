#!/usr/bin/env Rscript
#Above line allows code to be run using ./Simulator.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(insight)#print_color function
library(argparser)#anything parser related

#Adding argument parsers so that I can vary the simulated data from the command line
parser <- arg_parser('Options for varying the simulated data generated')
parser <- add_argument(parser, "--flex", help = 'running in flex mode when TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--name", help = 'name of output when in flex mode if other name desired',nargs='*',default='TEMP')
parser <- add_argument(parser, "--g", help = 'which model for simulated data to use when running in flex mode',nargs='*',default='IRT')
#
#NOISE: uses classical difficulty to generate the probabilty for each student
#CTT: uses classical difficulty and a simple student modifier to generate the probability for each student
#IRT: uses a 2PL model to generate the probability for each student
#
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode',nargs='*',default=20)
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode',nargs='*',default=1000)
parser <- add_argument(parser, "--diffmn", help = 'CTT difficulty mean when in flex mode',nargs='*',default=.5)
parser <- add_argument(parser, "--diffsd", help = 'CTT difficulty sd when in flex mode',nargs='*',default=.15)
parser <- add_argument(parser, "--modmn", help = 'CTT modifier mean when in flex mode',nargs='*',default=0)
parser <- add_argument(parser, "--modsd", help = 'CTT modifier sd when in flex mode',nargs='*',default=.1)
parser <- add_argument(parser, "--bmn", help = 'IRT difficulty mean when in flex mode',nargs='*',default=0)
parser <- add_argument(parser, "--bsd", help = 'IRT difficulty sd when in flex mode',nargs='*',default=1)
parser <- add_argument(parser, "--amn", help = 'IRT discrimination mean when in flex mode',nargs='*',default=1.5)
parser <- add_argument(parser, "--asd", help = 'IRT discrimination sd when in flex mode',nargs='*',default=.5)
parser <- add_argument(parser, "--thmn", help = 'IRT theta mean when in flex mode',nargs='*',default=0)
parser <- add_argument(parser, "--thsd", help = 'IRT theta sd when in flex mode',nargs='*',default=1)
arg <- parse_args(parser)

#Running checks on user input
if (arg$flex){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FLEX MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else {
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FIXED MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}

if (arg$g == 'NOISE'){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING NOISE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else if (arg$g == 'CTT'){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING CTT ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else if (arg$g == 'IRT'){
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING IRT ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
}else {
	print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!INVALID ARGUMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
	break
}

#Function to generate student repsonse
response <- function(g,itempar,mod=0,th=0){
	
	set.seed(NULL)#used to unset the seed so the responses will differ for each item
	st <- runif(1, min=0, max=1)
	
	#Set probabilities based on model
	if (g == 'IRT'){
		a <- itempar$Discrimination
		b <- itempar$Difficulty
		p <- exp(a*(th - b))/(1 + exp(a*(th - b)))
	}else if (g == 'CTT'){
		p <- itempar$Difficulty
		st <- st + mod
	}else if (g == 'NOISE'){
		p <- itempar$Difficulty
	}
	
	#Set student response based on random uniform dice roll and probability
	if (st < p){
		r <- 1
	}else {
		r <- 0
	}	

	return(r)
}

#Simulating data 
if (arg$flex){
	#Flexible simulations based on user input
	nitems <- arg$nitems
	Item <- paste0('Item',1:nitems)
	ns <- arg$ns
	
	#True item parameters that will be used in the generated data
	if (arg$g == 'NOISE' | arg$g == 'CTT'){
		par <- data.frame(Items = Item, Difficulty = rnorm(nitems, mean=arg$diffmn, sd=arg$diffsd))
	}else if (arg$g == 'IRT'){
		par <- data.frame(Items = Item, Difficulty = rnorm(nitems, mean=arg$bmn, sd=arg$bsd), Discrimination = rnorm(nitems, mean=arg$amn, sd=arg$asd))
	}
	print_color(paste0('==============================================================================\n'),'bold')
	print_color(paste0('==============================Item Parameters=================================\n'),'bold')
	print_color(paste0('==============================================================================\n'),'bold')
	print(par)
	write.csv(par, paste0('simdata/flex/',arg$name,'-',arg$g,'-Items.csv'), row.names = FALSE)	

	#Setting true proficiencies
       	if (arg$g == 'NOISE'){
		df <- data.frame(ID = 1:ns)
	}else if (arg$g == 'CTT'){
		df <- data.frame(ID = 1:ns, Modifier = rnorm(ns, mean=arg$modmn, sd=arg$modsd))
	}else if (arg$g == 'IRT'){
		df <- data.frame(ID = 1:ns, Theta = rnorm(ns, mean=arg$thmn, sd=arg$thsd))
	}	

	#Fill in student responses 
	print_color(paste0('==============================================================================\n'),'bcyan')
	print_color(paste0('========================Generating Student Responses==========================\n'),'bcyan')
	print_color(paste0('==============================================================================\n'),'bcyan')
	for (j in Item){
		temp <- c()
		
		for (i in 1:ns){
			if (arg$g == 'NOISE'){
				r <- response(g='NOISE', itempar=par[par$Items == j,])
				temp <- c(temp, r)
			}else if (arg$g == 'CTT'){
				r <- response(g='CTT', itempar=par[par$Items == j,], mod=df[df$ID == i,]$Modifier)
				temp <- c(temp, r)
			}else if (arg$g == 'IRT'){
				r <- response(g='IRT', itempar=par[par$Items == j,], th=df[df$ID == i,]$Theta)
				temp <- c(temp, r)
			}
		}
		df[[j]] <- temp
	}
	print(as_tibble(df))
	
	#Saving fixed datasets 
	write.csv(df, paste0('simdata/flex/',arg$name,'-',arg$g,'-Data.csv'), row.names = FALSE)	

}else {
	#Generating massive datasets that are fixed
	nitems <- 100
	Item <- paste0('Item',1:nitems)
	ns <- 10000

	#True item parameters that will be used in the generated data
	set.seed(328)#consistent item parameters when run
	cttdiff <- rnorm(nitems, mean=.5, sd=.15)
	irtdiff <- rnorm(nitems, mean=0, sd=1)
	irtdisc <- rnorm(nitems, mean=1.5, sd=.5)
	
	#Saving true item parameters for reference
	print_color(paste0('==============================================================================\n'),'bold')
	print_color(paste0('============================CTT Item Parameters===============================\n'),'bold')
	print_color(paste0('==============================================================================\n'),'bold')
	cttpar <- data.frame(Items = Item, Difficulty = cttdiff)
	print(cttpar)
	write.csv(cttpar, paste0('simdata/fixed/CTT-Items.csv'), row.names = FALSE)	
	
	print_color(paste0('==============================================================================\n'),'bold')
	print_color(paste0('============================IRT Item Parameters===============================\n'),'bold')
	print_color(paste0('==============================================================================\n'),'bold')
	irtpar <- data.frame(Items = Item, Difficulty = irtdiff, Discrimination = irtdisc)
	print(irtpar)	
	write.csv(irtpar, paste0('simdata/fixed/IRT-Items.csv'), row.names = FALSE)	

	#Setting true proficiencies 
	set.seed(326)#consistent proficiencies when run
	noisedf <- data.frame(ID = 1:ns)
	cttdf <- data.frame(ID = 1:ns, Modifier = rnorm(ns, mean=0, sd=.1))
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

		noisetemp <- c()
		ctttemp <- c()
		irttemp <- c()
		
		for (i in 1:ns){
			noiser <- response(g='NOISE', itempar=cttpar[cttpar$Items == j,])
			noisetemp <- c(noisetemp, noiser)

			cttr <- response(g='CTT', itempar=cttpar[cttpar$Items == j,], mod=cttdf[cttdf$ID == i,]$Modifier)
			ctttemp <- c(ctttemp, cttr)
			
			irtr <- response(g='IRT', itempar=irtpar[irtpar$Items == j,], mod=irtdf[irtdf$ID == i,]$Theta)
			irttemp <- c(irttemp, irtr)
		}
		noisedf[[j]] <- noisetemp
		cttdf[[j]] <- ctttemp
		irtdf[[j]] <- irttemp
	}
	print(as_tibble(noisedf))
	print(as_tibble(cttdf))
	print(as_tibble(irtdf))
	
	#Saving fixed datasets 
	write.csv(noisedf, paste0('simdata/fixed/NOISE-Data.csv'), row.names = FALSE)	
	write.csv(cttdf, paste0('simdata/fixed/CTT-Data.csv'), row.names = FALSE)	
	write.csv(irtdf, paste0('simdata/fixed/IRT-Data.csv'), row.names = FALSE)	
}
