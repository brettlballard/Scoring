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
parser <- add_argument(parser, "--legacy", help = 'which legacy instrument to emulate',nargs='*',default='FCI')

#flex mode arguments
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])

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


#Build datasets
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
			ns <- nst
			thmn <- c(0)
			thsd <- c(1)
			thw <- c('eq')
			
			#Changing shapes of IRT parameter distributions
			if (arg$legacy == 'FCI'){
				temp <- read.csv('analysisout/summary/FCI/post/2PLpar-4187.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'FMCE'){
				temp <- read.csv('analysisout/summary/FMCE/post/2PLpar-4689.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'FMCETh'){
				temp <- read.csv('analysisout/summary/FMCETh/post/2PLpar-4689.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'CSEMsam1'){
				temp <- read.csv('analysisout/summary/CSEMsam1/post/2PLpar-2280.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'CSEMsam2'){
				temp <- read.csv('analysisout/summary/CSEMsam2/post/2PLpar-4371.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'K1-20'){
				temp <- read.csv('analysisout/summary/K1-20/post/2PLpar-1514.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}else if (arg$legacy == 'K1-7'){
				temp <- read.csv('analysisout/summary/K1-7/post/2PLpar-1514.csv')
				Item <- temp$Items
				itemdiff <- temp$Est.Difficulty.2PL
				itemdisc <- temp$Est.Discrimination.2PL
			}

			#Set number of items parameter to match
			nitems <- length(Item)

			#Saving item generators
			if (!dir.exists(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'))){dir.create(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'), recursive = TRUE)}
			gen <- file(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,r),'-Generators.txt'), 'w')
			writeLines(paste0('Number of Items: ',nitems), con = gen)
			writeLines(paste0('Number of Students: ',ns), con = gen)
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

#Curious about runtime 
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
