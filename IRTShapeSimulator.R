#!/usr/bin/env Rscript
#Above line allows code to be run using ./IRTShapeSimulator.R in terminal

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
parser <- add_argument(parser, "--run", help = 'running in run mode when TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--nrun", help = 'number of runs when in run mode',nargs='*',default=c(1,10,1))
parser <- add_argument(parser, "--name", help = 'name of output when in flex/run mode if other name desired',nargs='*',default='TEST')
parser <- add_argument(parser, "--shape", help = 'shape between difficulty and discrimination',nargs='*',default='NONE')

#parameter arguments
parser <- add_argument(parser, "--bmn", help = 'IRT difficulty mean',nargs='*',default=c(-1.5,0,1.5))
parser <- add_argument(parser, "--bsd", help = 'IRT difficulty sd',nargs='*',default=c(.5,1,.5))
parser <- add_argument(parser, "--bw", help = 'IRT difficulty weights',nargs='*',default=c(.25,.5,.25))

#flex mode arguments
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
parser <- add_argument(parser, "--pardist", help = 'parameter distribution type when in flex mode, options are norm and unif: default is norm',nargs='*',default='norm')
arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])

#Running checks on user input
print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FLEX MODE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')

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

###############################################################################################################
##################################################SIMULATION###################################################
###############################################################################################################
#Simulating data 
#Flexible simulations based on user input
#If multiple runs of the same set of items or students, but with potentially varying parameters
if (arg$run){
	nrun <- seq(from = arg$nrun[1], to = arg$nrun[2], by = arg$nrun[3])
}else {
	nrun <- c(1)
}

formshape <- c('LIN','EXP','ASYM','GAUSS','LOGIST')

#Build datasets
for (nit in numitems){
	for (nst in numst){
		
		#Generate data
		for (r in nrun){
			#Using while loop to ensure simulated data will converge in IRT fitting 
			DataCheck <- TRUE
			while (DataCheck){
				print_color(paste0('==============================================================================\n'),'bviolet')
				print_color(paste0('===============================RUN NUMBER ',r,'===================================\n'),'bviolet')
				print_color(paste0('==============================================================================\n'),'bviolet')
				
				#Setting incremented values
				nitems <- nit
				Item <- paste0('Item',1:nitems)
				ns <- nst
				thmn <- c(0)
				thsd <- c(1)
				thw <- c('eq')
				bmn <- arg$bmn
				bsd <- arg$bsd
				bw <- arg$bw
				itemdiff <- multirnorm(nitems, mean=bmn, sd=bsd, w=bw)
				
				#Changing shapes of IRT parameter distributions
				if (arg$shape == 'NONE'){
					amn <- c(1,2,3)
					asd <- c(.5,.5,.5)
					aw <- c(.6,.3,.1)
					itemdisc <- multirnorm(nitems, mean=amn, sd=asd, w=aw)
				}else if (arg$shape == 'SPLIT'){
					amn <- c(.5,3)
					asd <- c(.1,.1)
					aw <- c(.7,.3)
					itemdisc <- multirnorm(nitems, mean=amn, sd=asd, w=aw)
				}else if (grepl('LIN',arg$shape)){
					asd <- c(.1)
					aw <- c('eq')
					itemdisc <- c()
					for (diff in itemdiff){
						if (grepl('n',arg$shape)){
							formula <- 'Disc = -1 * Diff + 1.5'
							amn <- -1 * diff + 1.5
						}else {
							formula <- 'Disc = 1 * Diff + 1.5'
							amn <- 1 * diff + 1.5
						}
						disc <- multirnorm(1, mean=amn, sd=asd, w=aw)
						itemdisc <- c(itemdisc,disc)
					}
				}else if (grepl('GAUSS',arg$shape)){
					asd <- c(.1)
					aw <- c('eq')
					itemdisc <- c()
					for (diff in itemdiff){
						if (grepl('i',arg$shape)){
							formula <- 'Disc = -2.25 * exp(-.5 * (Diff - 0)**2) + 3.5'
							amn <- -2.5 * exp(-.5 * (diff - 0)**2) + 3.5
						}else {
							formula <- 'Disc = 2.25 * exp(-.5 * (Diff - 0)**2) + .75'
							amn <- 2.25 * exp(-.5 * (diff - 0)**2) + .75
						}
						disc <- multirnorm(1, mean=amn, sd=asd, w=aw)
						itemdisc <- c(itemdisc,disc)
					}
				}else if (grepl('EXP',arg$shape)){
					asd <- c(.1)
					aw <- c('eq')
					itemdisc <- c()
					for (diff in itemdiff){
						if (grepl('d',arg$shape)){
							formula <- 'Disc = 1.2 * exp(-.4 * (Diff - 0))'
							amn <- 1.2 * exp(-.4 * (diff - 0))
						}else {
							formula <- 'Disc = 1.2 * exp(.4 * (Diff - 0))'
							amn <- 1.2 * exp(.4 * (diff - 0))
						}
						disc <- multirnorm(1, mean=amn, sd=asd, w=aw)
						itemdisc <- c(itemdisc,disc)
					}
				}else if (grepl('LOGIST',arg$shape)){
					asd <- c(.1)
					aw <- c('eq')
					itemdisc <- c()
					for (diff in itemdiff){
						if (grepl('r',arg$shape)){
							formula <- 'Disc = .5  + (3.25 - .5) * (1 / (1 + exp(1.5 * (Diff - 0))))'
							amn <- .5  + (3.25 - .5) * (1 / (1 + exp(1.5 * (diff - 0))))
						}else {
							formula <- 'Disc = .5  + (3.25 - .5) * (1 / (1 + exp(-1.5 * (Diff - 0))))'
							amn <- .5  + (3.25 - .5) * (1 / (1 + exp(-1.5 * (diff - 0))))
						}
						disc <- multirnorm(1, mean=amn, sd=asd, w=aw)
						itemdisc <- c(itemdisc,disc)
					}
				}else if (grepl('ASYM',arg$shape)){
					asd <- c(.1)
					aw <- c('eq')
					itemdisc <- c()
					for (diff in itemdiff){
						if (grepl('r',arg$shape)){
							formula <- 'Disc = 3.25  - (3.25 - 1) * exp(-.7 * (-1 * Diff + 2.5))'
							amn <- 3.25  - (3.25 - 1) * exp(-.7 * (-1 * diff + 2.5))
						}else {
							formula <- 'Disc = 3.25  - (3.25 - 1) * exp(-.7 * (Diff + 2.5))'
							amn <- 3.25  - (3.25 - 1) * exp(-.7 * (diff + 2.5))
						}
						disc <- multirnorm(1, mean=amn, sd=asd, w=aw)
						itemdisc <- c(itemdisc,disc)
					}
				}

				#Saving item generators
				if (!dir.exists(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'))){dir.create(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'), recursive = TRUE)}
				gen <- file(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Generators.txt'), 'w')
				writeLines(paste0('Number of Items: ',nitems), con = gen)
				writeLines(paste0('Number of Students: ',ns), con = gen)
				writeLines(paste0('Parameter Distribution Type: ',arg$pardist), con = gen)
				writeLines(paste0('Difficulty Mean: ',paste0(bmn,collapse=',')), con = gen)
				writeLines(paste0('Difficulty Standard Deviation: ',paste0(bsd,collapse=',')), con = gen)
				writeLines(paste0('Difficulty Weighting: ',paste0(bw,collapse=',')), con = gen)
				if (!grepl(paste(formshape,collapse='|'),arg$shape)){
					writeLines(paste0('Discrimination Mean: ',paste0(amn,collapse=',')), con = gen)
				}
				if (grepl(paste(formshape,collapse='|'),arg$shape)){
					writeLines(paste0('Discrimination Formula: ',formula), con = gen)
				}
				writeLines(paste0('Discrimination Standard Deviation: ',paste0(asd,collapse=',')), con = gen)
				writeLines(paste0('Discrimination Weighting: ',paste0(aw,collapse=',')), con = gen)
				writeLines(paste0('Theta Mean: ',paste0(thmn,collapse=',')), con = gen)
				writeLines(paste0('Theta Standard Deviation: ',paste0(thsd,collapse=',')), con = gen)
				writeLines(paste0('Theta Weighting: ',paste0(thw,collapse=',')), con = gen)
				close(gen)

				#True item parameters that will be used in the generated data
				par <- data.frame(Items = Item, Difficulty = itemdiff, Discrimination = itemdisc)
				print_color(paste0('==============================================================================\n'),'bold')
				print_color(paste0('==============================Item Parameters=================================\n'),'bold')
				print_color(paste0('==============================================================================\n'),'bold')
				print(par)
				write.csv(par, paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Items.csv'), row.names = FALSE)	

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

				checkvec <- c()
				#Check data quality
				for (j in Item){
					freq <- table(df[[j]])
					#print(freq)#will suppress after testing
					#print(freq[1] > 50 & freq[2] > 50)#will suppress after testing
					checkvar <- freq[1] > 50 & freq[2] > 50
					checkvec <- c(checkvec,checkvar)
				}
				print(checkvec)#will suppress after testing
				print(all(checkvec))#will suppress after testing
				check <- all(checkvec)
				if (is.na(check)){
					check <- FALSE
				}	
				if (check){
					print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!DATA PASSED QUALITY CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
					DataCheck <- FALSE
				}else {
					print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!DATA FAILED QUALITY CHECK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
				}
				
			}#end of while loop to check data quality 
	
			#Saving flex datasets
			write.csv(df, paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Data.csv'), row.names = FALSE)
		}#end of nrun loop
	}#end of ns loop
}#end of nitems loop

#Curious about runtime 
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
