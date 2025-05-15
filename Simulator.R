#!/usr/bin/env Rscript
#Above line allows code to be run using ./Simulator.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related

#Adding argument parsers so that I can vary the simulated data from the command line
parser <- arg_parser('Options for varying the simulated data generated')
parser <- add_argument(parser, "--flex", help = 'running in flex mode when TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--g", help = 'which model for simulated data to use when running in flex mode',nargs='*',default='NOISE')
#
#Noise: uses classical difficulty to generate the probabilty for each student
#CTT: uses classical difficulty and a simple student modifier to generate the probability for each student
#IRT: uses a 2PL model to generate the probability for each student
#
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode',nargs='*',default=20)
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode',nargs='*',default=100)
arg <- parse_args(parser)

#Function to generate student repsonse
response <- function(g,itempar,mod=0,th=0){
	
	set.seed(NULL)#used to unset the seed for the item parameters
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
	cttpar <- data.frame(Items = Item, Difficulty = cttdiff)
	print(cttpar)
	write.csv(cttpar, paste0('simdata/fixed/CTT-Items-Fixed.csv'), row.names = FALSE)	
	
	irtpar <- data.frame(Items = Item, Difficulty = irtdiff, Discrimination = irtdisc)
	print(irtpar)	
	write.csv(irtpar, paste0('simdata/fixed/IRT-Items-Fixed.csv'), row.names = FALSE)	

	#Setting true proficiencies 
	noisedf <- data.frame(ID = 1:ns)
	cttdf <- data.frame(ID = 1:ns, Modifier = rnorm(ns, mean=0, sd=.1))
	irtdf <- data.frame(ID = 1:ns, Theta = rnorm(ns, mean=0, sd=1))

	#Because there are a lot of responses, then I want to output progress
	itprogress <- seq(1, nitems, by = nitems/10)
	itprogress <- paste0('Item',itprogress)

	#Fill in student responses 
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
	write.csv(noisedf, paste0('simdata/fixed/Noise-Data-Fixed.csv'), row.names = FALSE)	
	write.csv(cttdf, paste0('simdata/fixed/CTT-Data-Fixed.csv'), row.names = FALSE)	
	write.csv(irtdf, paste0('simdata/fixed/IRT-Data-Fixed.csv'), row.names = FALSE)	
}
