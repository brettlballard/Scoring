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
parser <- add_argument(parser, "--flex", help = 'running in flex mode when TRUE',nargs='*',default=TRUE)
parser <- add_argument(parser, "--run", help = 'running in run mode when TRUE',nargs='*',default=FALSE)
parser <- add_argument(parser, "--nrun", help = 'number of runs when in run mode',nargs='*',default=10)
parser <- add_argument(parser, "--name", help = 'name of output when in flex/run mode if other name desired',nargs='*',default='TEST')
parser <- add_argument(parser, "--shape", help = 'shape between difficulty and discrimination',nargs='*',default='NONE')

#flex mode arguments
parser <- add_argument(parser, "--nitems", help = 'number of items when in flex mode: format input as begin,end,increment',nargs='*',default=c(10,10,0))
parser <- add_argument(parser, "--ns", help = 'number of students when in flex mode: format input as begin,end,increment',nargs='*',default=c(1000,1000,0))
parser <- add_argument(parser, "--thmn", help = 'IRT theta mean when in flex mode',nargs='*',default=c(0,0,0))
parser <- add_argument(parser, "--thsd", help = 'IRT theta sd when in flex mode',nargs='*',default=c(1,1,0))
parser <- add_argument(parser, "--thw", help = 'IRT theta weights when in flex mode',nargs='*',default=c('eq'))
parser <- add_argument(parser, "--pardist", help = 'parameter distribution type when in flex mode, options are norm and unif: default is norm',nargs='*',default='norm')

arg <- parse_args(parser)

#Turning multiple input arguments into vectors
numitems <- seq(from = arg$nitems[1], to = arg$nitems[2], by = arg$nitems[3])
numst <- seq(from = arg$ns[1], to = arg$ns[2], by = arg$ns[3])

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

###############################################################################################################
##################################################SIMULATION###################################################
###############################################################################################################
#Simulating data 
#Flexible simulations based on user input
#If multiple runs of the same set of items or students, but with potentially varying parameters
if (arg$run){
	nrun <- arg$nrun
}else {
	nrun <- 1
}

#Keep track of different files
filecount <- 1

#Build datasets
for (nit in numitems){
	for (nst in numst){
		#Generate data
		for (r in 1:nrun){
			#Setting incremented values
			nitems <- nit
			Item <- paste0('Item',1:nitems)
			ns <- nst
			
			#Changing shapes of IRT parameter distributions
			if (arg$shape == 'NONE'){
				bmn = c(-1.5,0,1.5)
				bsd = c(.5,1,.5)
				bw = c(.25,.5,.25)
				itemdiff <- multirnorm(nitems, mean=bmn, sd=bsd, w=bw)
				amn = c(1,2,3)
				asd = c(.5,.5,.5)
				aw = c(.6,.3,.1)
				itemdisc <- multirnorm(nitems, mean=amn, sd=asd, w=aw)
			}else if ('LIN' %in% arg$shape){
				bmn = c(-1.5,0,1.5)
				bsd = c(.5,1,.5)
				bw = c(.25,.5,.25)
				itemdiff <- multirnorm(nitems, mean=bmn, sd=bsd, w=bw)
				if ('n' %in% arg$shape){
					formula <- 'Disc = -1 * Diff + 1.5'
					itemdisc <- -1 * itemdiff + 1.5
				}else {
					formula <- 'Disc = 1 * Diff + 1.5'
					itemdisc <- 1 * itemdiff + 1.5
				}
			}else if ('EXP' %in% arg$shape){
				bmn = c(-1.5,0,1.5)
				bsd = c(.5,1,.5)
				bw = c(.25,.5,.25)
				itemdiff <- multirnorm(nitems, mean=bmn, sd=bsd, w=bw)
				if ('d' %in% arg$shape){
					cut <- -2
				}else {
					cut <- 2
				}
				amn = c(1,2.5)
				asd = c(.15,.5)
				aw = c('eq')
				itemdisc <- c()
				for (diff in itemdiff){
					if ('d' %in% arg$shape){
						if (diff < cut){
							disc <- multirnorm(1, mean=amn[2], sd=asd[2], w=aw)
						}else {
							disc <- multirnorm(1, mean=amn[1], sd=asd[1], w=aw)
						}
					}else {
						if (diff > cut){
							disc <- multirnorm(1, mean=amn[2], sd=asd[2], w=aw)
						}else {
							disc <- multirnorm(1, mean=amn[1], sd=asd[1], w=aw)
						}
					}
					itemdisc <- c(itemdisc,disc)
				}
			}else if ('LN' %in% arg$shape){
				bmn = c(-1.5,0,1.5)
				bsd = c(.5,1,.5)
				bw = c(.25,.5,.25)
				itemdiff <- multirnorm(nitems, mean=bmn, sd=bsd, w=bw)
				if ('r' %in% arg$shape){
					cut <- 2
				}else {
					cut <- -2
				}
				amn = c(3,1.5)
				asd = c(.15,.5)
				aw = c('eq')
				itemdisc <- c()
				for (diff in itemdiff){
					if ('r' %in% arg$shape){
						if (diff > cut){
							disc <- multirnorm(1, mean=amn[2], sd=asd[2], w=aw)
						}else {
							disc <- multirnorm(1, mean=amn[1], sd=asd[1], w=aw)
						}
					}else {
						if (diff < cut){
							disc <- multirnorm(1, mean=amn[2], sd=asd[2], w=aw)
						}else {
							disc <- multirnorm(1, mean=amn[1], sd=asd[1], w=aw)
						}
					}
					itemdisc <- c(itemdisc,disc)
				}
			}

			#Saving item generators
			if (!dir.exists(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'))){dir.create(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students'), recursive = TRUE)}
			gen <- file(paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,filecount),'-Generators.txt'), 'w')
			writeLines(paste0('Number of Items: ',nitems), con = gen)
			writeLines(paste0('Number of Students: ',ns), con = gen)
			writeLines(paste0('Parameter Distribution Type: ',arg$pardist), con = gen)
			writeLines(paste0('Difficulty Mean: ',paste0(bmn,collapse=',')), con = gen)
			writeLines(paste0('Difficulty Standard Deviation: ',paste0(bsd,collapse=',')), con = gen)
			writeLines(paste0('Difficulty Weighting: ',paste0(bw,collapse=',')), con = gen)
			if (arg$shape == 'NULL'){
				writeLines(paste0('Discrimination Mean: ',paste0(amn,collapse=',')), con = gen)
				writeLines(paste0('Discrimination Standard Deviation: ',paste0(asd,collapse=',')), con = gen)
				writeLines(paste0('Discrimination Weighting: ',paste0(aw,collapse=',')), con = gen)
			}else if ('LIN' %in% arg$shape){
				writeLines(paste0('Discrimination Formula: ',formula), con = gen)
			}else ('EXP' %in% arg$shape | 'LN' %in% arg%shape){
				writeLines(paste0('Cut value used: ',cut), con = gen)
			}
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
			write.csv(par, paste0('simdata/flex/IRT/',arg$name,'/',nitems,'items','/',ns,'students','/',paste0(arg$name,filecount),'-Items.csv'), row.names = FALSE)	

			#Setting true proficiencies
			df <- data.frame(ID = 1:ns, Theta = multirnorm(ns, mean=arg$thmn, sd=arg$thsd, w=arg$thw))

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
	}#end of ns loop
}#end of nitems loop

#Curious about runtime 
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
