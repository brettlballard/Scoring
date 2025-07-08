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
parser <- add_argument(parser, "--name", help = 'name of output when in flex/run mode if other name desired',nargs='*',default='TEMP')
parser <- add_argument(parser, "--g", help = 'which model for simulated data to use when running in flex/run mode',nargs='*',default='IRT')
#
#NOISE: uses classical difficulty to generate the probabilty for each student
#CTT: uses classical difficulty and a simple student modifier to generate the probability for each student
#IRT: uses a 2PL model to generate the probability for each student
#

#flex mode arguments
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
parser <- add_argument(parser, "--pardist", help = 'parameter distribution type when in flex mode, options are norm and unif: default is norm',nargs='*',default='norm')
parser <- add_argument(parser, "--diffmn", help = 'CTT difficulty mean when in flex mode',nargs='*',default=c('.5'))
parser <- add_argument(parser, "--diffsd", help = 'CTT difficulty sd when in flex mode',nargs='*',default=c('.15'))
parser <- add_argument(parser, "--diffw", help = 'CTT difficulty weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--modmn", help = 'CTT modifier mean when in flex mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--modsd", help = 'CTT modifier sd when in flex mode',nargs='*',default=c('.1'))
parser <- add_argument(parser, "--modw", help = 'CTT modifier weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--bmn", help = 'IRT difficulty mean when in flex mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--bsd", help = 'IRT difficulty sd when in flex mode',nargs='*',default=c('1'))
parser <- add_argument(parser, "--bw", help = 'IRT difficulty weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--amn", help = 'IRT discrimination mean when in flex mode',nargs='*',default=c('1.5'))
parser <- add_argument(parser, "--asd", help = 'IRT discrimination sd when in flex mode',nargs='*',default=c('.5'))
parser <- add_argument(parser, "--aw", help = 'IRT discrimination weights when in flex mode: default is eq',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--thmn", help = 'IRT theta mean when in flex mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--thsd", help = 'IRT theta sd when in flex mode',nargs='*',default=c('1'))
parser <- add_argument(parser, "--thw", help = 'IRT theta weights when in flex mode: default is eq',nargs='*',default=c('eq'))

#run mode arguments
parser <- add_argument(parser, "--diffmninc", help = 'change in CTT difficulty mean when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--diffsdinc", help = 'change in CTT difficulty sd when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--diffwinc", help = 'change in CTT difficulty weights when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--modmninc", help = 'change in CTT modifier mean when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--modsdinc", help = 'change in CTT modifier sd when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--modwinc", help = 'change in CTT modifier weights when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--bmninc", help = 'change in IRT difficulty mean when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--bsdinc", help = 'change in IRT difficulty sd when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--bwinc", help = 'change in IRT difficulty weights when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--amninc", help = 'change in IRT discrimination mean when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--asdinc", help = 'change in IRT discrimination sd when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--awinc", help = 'change in IRT discrimination weights when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--thmninc", help = 'change in IRT theta mean when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--thsdinc", help = 'change in IRT theta sd when in run mode',nargs='*',default=c('0'))
parser <- add_argument(parser, "--thwinc", help = 'change in IRT theta weights when in run mode',nargs='*',default=c('0'))
arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])
arg$diffmn <- strsplit(arg$diffmn,',')[[1]]
arg$diffsd <- strsplit(arg$diffsd,',')[[1]]
arg$diffw <- strsplit(arg$diffw,',')[[1]]
arg$modmn <- strsplit(arg$modmn,',')[[1]]
arg$modsd <- strsplit(arg$modsd,',')[[1]]
arg$modw <- strsplit(arg$modw,',')[[1]]
arg$bmn <- strsplit(arg$bmn,',')[[1]]
arg$bsd <- strsplit(arg$bsd,',')[[1]]
arg$bw <- strsplit(arg$bw,',')[[1]]
arg$amn <- strsplit(arg$amn,',')[[1]]
arg$asd <- strsplit(arg$asd,',')[[1]]
arg$aw <- strsplit(arg$aw,',')[[1]]
arg$thmn <- strsplit(arg$thmn,',')[[1]]
arg$thsd <- strsplit(arg$thsd,',')[[1]]
arg$thw <- strsplit(arg$thw,',')[[1]]
arg$diffmninc <- strsplit(arg$diffmninc,',')[[1]]
arg$diffsdinc <- strsplit(arg$diffsdinc,',')[[1]]
arg$diffwinc <- strsplit(arg$diffwinc,',')[[1]]
arg$modmninc <- strsplit(arg$modmninc,',')[[1]]
arg$modsdinc <- strsplit(arg$modsdinc,',')[[1]]
arg$modwinc <- strsplit(arg$modwinc,',')[[1]]
arg$bmninc <- strsplit(arg$bmninc,',')[[1]]
arg$bsdinc <- strsplit(arg$bsdinc,',')[[1]]
arg$bwinc <- strsplit(arg$bwinc,',')[[1]]
arg$amninc <- strsplit(arg$amninc,',')[[1]]
arg$asdinc <- strsplit(arg$asdinc,',')[[1]]
arg$awinc <- strsplit(arg$awinc,',')[[1]]
arg$thmninc <- strsplit(arg$thmninc,',')[[1]]
arg$thsdinc <- strsplit(arg$thsdinc,',')[[1]]
arg$thwinc <- strsplit(arg$thwinc,',')[[1]]

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

###############################################################################################################
##################################################FUNCTIONS####################################################
###############################################################################################################
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

#Function to increment arguments
argincr <- function(val, incr, nincr){
	vallst <- list()
	templist <- list()
	for (i in 1:length(val)){
		if (incr[i] == '0'){
			templist[[i]] <- list(rep(val[i],nincr))
		}else {
			templist[[i]] <- list(seq(from = as.numeric(val[i]), by = as.numeric(incr[i]), length.out = nincr))
		}
	}
	for (j in 1:nincr){
		i <- 1
		temp <- ''
		while (i <= length(val)){
			tempvals <- unlist(templist[[i]])
			if (i == 1){
				temp <- paste0(temp,tempvals[j])
			}else {
				temp <- paste0(temp,',',tempvals[j])
			}
			i <- i + 1
		}
		vallst <- append(vallst,temp)
	}
	return(vallst)
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
		
		#Incrementing values that need to be incremented	
		if (arg$g == 'NOISE' | arg$g == 'CTT'){
			arg$diffmn <- argincr(val = arg$diffmn, incr = arg$diffmninc, nincr = nrun)
			arg$diffsd <- argincr(val = arg$diffsd, incr = arg$diffsdinc, nincr = nrun)
			arg$diffw <- argincr(val = arg$diffw, incr = arg$diffwinc, nincr = nrun)
			if (arg$g == 'CTT'){
				arg$modmn <- argincr(val = arg$modmn, incr = arg$modmninc, nincr = nrun)
				arg$modsd <- argincr(val = arg$modsd, incr = arg$modsdinc, nincr = nrun)
				arg$modw <- argincr(val = arg$modw, incr = arg$modwinc, nincr = nrun)
			}
		}else if (arg$g == 'IRT'){
			arg$bmn <- argincr(val = arg$bmn, incr = arg$bmninc, nincr = nrun)
			arg$bsd <- argincr(val = arg$bsd, incr = arg$bsdinc, nincr = nrun)
			arg$bw <- argincr(val = arg$bw, incr = arg$bwinc, nincr = nrun)
			arg$amn <- argincr(val = arg$amn, incr = arg$amninc, nincr = nrun)
			arg$asd <- argincr(val = arg$asd, incr = arg$asdinc, nincr = nrun)
			arg$aw <- argincr(val = arg$aw, incr = arg$awinc, nincr = nrun)
			arg$thmn <- argincr(val = arg$thmn, incr = arg$thmninc, nincr = nrun)
			arg$thsd <- argincr(val = arg$thsd, incr = arg$thsdinc, nincr = nrun)
			arg$thw <- argincr(val = arg$thw, incr = arg$thwinc, nincr = nrun)
		}
	}else {
		nrun <- 1
	}
	for (nit in numitems){
		for (nst in numst){
			for (r in 1:nrun){
				#Setting incremented values
				nitems <- nit
				Item <- paste0('Item',1:nitems)
				ns <- nst
				if (arg$g == 'NOISE' | arg$g == 'CTT'){
					diffmn <- strsplit((arg$diffmn[[r]]),',')[[1]]
					diffsd <- strsplit((arg$diffsd[[r]]),',')[[1]]
					diffw <- strsplit((arg$diffw[[r]]),',')[[1]]
					if (arg$g == 'CTT'){
						modmn <- strsplit((arg$modmn[[r]]),',')[[1]]
						modsd <- strsplit((arg$modsd[[r]]),',')[[1]]
						modw <- strsplit((arg$modw[[r]]),',')[[1]]
					}
				}else if (arg$g == 'IRT'){
					bmn <- strsplit((arg$bmn[[r]]),',')[[1]]
					bsd <- strsplit((arg$bsd[[r]]),',')[[1]]
					bw <- strsplit((arg$bw[[r]]),',')[[1]]
					amn <- strsplit((arg$amn[[r]]),',')[[1]]
					asd <- strsplit((arg$asd[[r]]),',')[[1]]
					aw <- strsplit((arg$aw[[r]]),',')[[1]]
					thmn <- strsplit((arg$thmn[[r]]),',')[[1]]
					thsd <- strsplit((arg$thsd[[r]]),',')[[1]]
					thw <- strsplit((arg$thw[[r]]),',')[[1]]
				}
				
				#Saving item generators
				if (!dir.exists(paste0('simdata/flex/',arg$g,'/',arg$name,'/',nitems,'items','/',ns,'students'))){dir.create(paste0('simdata/flex/',arg$g,'/',arg$name,'/',nitems,'items','/',ns,'students'), recursive = TRUE)}
				gen <- file(paste0('simdata/flex/',arg$g,'/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Generators.txt'), 'w')
				if (arg$g == 'NOISE' | arg$g == 'CTT'){
					writeLines(paste0('Number of Items: ',nitems), con = gen)
					writeLines(paste0('Number of Students: ',ns), con = gen)
					writeLines(paste0('Parameter Distribution Type: ',arg$pardist), con = gen)
					writeLines(paste0('Difficulty Mean: ',paste0(diffmn,collapse=',')), con = gen)
					writeLines(paste0('Difficulty Standard Deviation: ',paste0(diffsd,collapse=',')), con = gen)
					writeLines(paste0('Difficulty Weighting: ',paste0(diffw,collapse=',')), con = gen)
					if (arg$g == 'CTT'){
						writeLines(paste0('Modifier Mean: ',paste0(modmn,collapse=',')), con = gen)
						writeLines(paste0('Modifier Standard Deviation: ',paste0(modsd,collapse=',')), con = gen)
						writeLines(paste0('Modifier Weighting: ',paste0(modw,collapse=',')), con = gen)
					}
				}else if (arg$g == 'IRT'){
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
				}
				close(gen)

				#True item parameters that will be used in the generated data
				if (arg$g == 'NOISE' | arg$g == 'CTT'){
					par <- data.frame(Items = Item, Difficulty = multirnorm(nitems, mean=diffmn, sd=diffsd, w=diffw))
				}else if (arg$g == 'IRT'){
					par <- data.frame(Items = Item, Difficulty = multirnorm(nitems, mean=bmn, sd=bsd, w=bw), Discrimination = multirnorm(nitems, mean=amn, sd=asd, w=aw))
				}
				print_color(paste0('==============================================================================\n'),'bold')
				print_color(paste0('==============================Item Parameters=================================\n'),'bold')
				print_color(paste0('==============================================================================\n'),'bold')
				print(par)
				write.csv(par, paste0('simdata/flex/',arg$g,'/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Items.csv'), row.names = FALSE)	

				#Setting true proficiencies
				if (arg$g == 'NOISE'){
					df <- data.frame(ID = 1:ns)
				}else if (arg$g == 'CTT'){
					df <- data.frame(ID = 1:ns, Modifier = multirnorm(ns, mean=arg$modmn, sd=arg$modsd, w=arg$modw))
				}else if (arg$g == 'IRT'){
					df <- data.frame(ID = 1:ns, Theta = multirnorm(ns, mean=arg$thmn, sd=arg$thsd, w=arg$thw))
				}	

				#Fill in student responses 
				print_color(paste0('==============================================================================\n'),'bcyan')
				print_color(paste0('========================Generating Student Responses==========================\n'),'bcyan')
				print_color(paste0('==============================================================================\n'),'bcyan')
				for (j in Item){
					temp <- c()
					
					for (i in 1:ns){
						if (arg$g == 'NOISE'){
							resp <- response(g='NOISE', itempar=par[par$Items == j,])
							temp <- c(temp, resp)
						}else if (arg$g == 'CTT'){
							resp <- response(g='CTT', itempar=par[par$Items == j,], mod=df[df$ID == i,]$Modifier)
							temp <- c(temp, resp)
						}else if (arg$g == 'IRT'){
							resp <- response(g='IRT', itempar=par[par$Items == j,], th=df[df$ID == i,]$Theta)
							temp <- c(temp, resp)
						}
					}
					df[[j]] <- temp
				}
				print(as_tibble(df))
				
				#Saving flex datasets
				write.csv(df, paste0('simdata/flex/',arg$g,'/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Data.csv'), row.names = FALSE)
			}#end of nrun loop
		}#end of ns loop
	}#end of nitems loop

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

#Curious about runtime 
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
