#!/usr/bin/env Rscript
#Above line allows code to be run using ./MIRTConstrainPrePost.R in terminal

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

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the scoring analysis')
parser <- add_argument(parser, "--data", help = 'data being used; options are in the code',nargs='*',default='FCI')
#
#FCI pre & post
#FMCE pre & post
#FMCE Thornton pre & post
#Kin1D-PD-Ver1 pre & post
#
parser <- add_argument(parser, "--rmitems", help = 'items being removed',nargs='*',default=c('NULL'))
parser <- add_argument(parser, "--useitems", help = 'items being used',nargs='*',default=c('NULL'))
arg <- parse_args(parser)

#Checking arguments and setting parameters based on them
if (arg$data == 'FCI'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FCI ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FCI'
}else if (arg$data == 'FMCE'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCE ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCE'
}else if (arg$data == 'FMCETh'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING FMCETh ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'FMCETh'
}else if (arg$data == 'Kin1D-PD-Ver1'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING KIN-1D-VER1 ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'Kin1D-PD-Ver1'
}else {
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!INVALID DATA ARGUMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
        break
}

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

#Pull Item Parameters from previous unidimensional IRT model and format them to use in new model
FixParameters <- function(parmodel, fixed.pars=c('a1','d'), nitems){
	coeff <- coef(parmodel)
	start <- 'START =' 	
	fixed <- 'FIXED ='
	startvec <- c()
	fixedvec <- c()
	for (fixpar in fixed.pars){
		fixedvec <- c(fixedvec,paste0('(','1-',nit,',',fixpar,')'))	
		for (i in 1:nitems){
			pars <- coeff[i][[1]] 
			startvec <- c(startvec,paste0('(',i,',',fixpar,',',pars['par',fixpar],')'))		
		}
	}
	model <- paste0(start,paste(startvec, collapse=','),'\n',fixed,paste(fixedvec, collapse=','))
	return(model)
}

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

predf <- read.csv(paste0('../realdata/',test,'-pre.csv'))
postdf <- read.csv(paste0('../realdata/',test,'-post.csv'))

#Defining Items based on argparser
Item <- colnames(postdf)
Item <- Item[grepl('Item',Item)]
arg$rmitems <- strsplit(arg$rmitems,',')[[1]]
arg$useitems <- strsplit(arg$useitems,',')[[1]]
if (!('NULL' %in% arg$rmitems)){
	rmitems <- paste0('Item',arg$rmitems)
	Item <- Item[!(Item %in% rmitems)]
}
if (!('NULL' %in% arg$useitems)){
	useitems <- paste0('Item',arg$useitems)
	Item <- Item[Item %in% useitems]
}
print(Item)
nit <- length(Item)
predata <- as_tibble(predf)
postdata <- as_tibble(postdf)

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('====================Pretest Data Set With Selected Items====================\n','bold')
print_color('============================================================================\n','bold')
predata <- predata %>%
	select(all_of(Item))
predata$SimSum.Score <- apply(predata[,Item],1,sum)
print(predata)
nprepart <- nrow(predata)

print_color('============================================================================\n','bold')
print_color('====================Posttest Data Set With Selected Items===================\n','bold')
print_color('============================================================================\n','bold')
postdata <- postdata %>%
	select(all_of(Item))
postdata$SimSum.Score <- apply(postdata[,Item],1,sum)
print(postdata)
npostpart <- nrow(postdata)

##############################################################################################################
###############################################IRT ANALYSIS###################################################
##############################################################################################################

#Retrieve 2pl parameters for each item and theta estimates from data
print_color('============================================================================\n','bgreen')
print_color('=================Posttest 2PL Parameter Values For Each Item================\n','bgreen')
print_color('============================================================================\n','bgreen')

#2PL Model
unimodel <- mirt.model('Theta = 1-29')
postmodel <- mirt(data=postdata[,Item], model=unimodel, itemtype='2PL')
coeff1 <- coef(postmodel)
coeff2 <- coef(postmodel, IRTpars=TRUE, simplify=TRUE)

#Model fit
modfit <- M2(postmodel)
print(modfit)

print(itemfit(postmodel, fit_stats = c('S_X2')))
print(coeff1)
print(coeff2)

#Save estimated thetas
scores <- fscores(postmodel, method = 'EAP', full.scores = TRUE, full.scores.SE = TRUE) 
est.theta <- scores[,1]
postdata$Est.Theta <- est.theta
print(head(postdata$Est.Theta,10))

#Making dataframe to save IRT parameters
avec <- c()
dvec <- c()
for (i in Item){
	avec <- c(avec,(coeff1[i][[1]])[1])
	dvec <- c(dvec,(coeff1[i][[1]])[2])
}
est.post.pardf <- data.frame(Items = Item, Est.Slopes = avec, Est.Intercepts = dvec)
print(est.post.pardf)

#Retrieve 2pl parameters for each item and theta estimates from data
print_color('============================================================================\n','bgreen')
print_color('=================Pretest 2PL Parameter Values For Each Item================\n','bgreen')
print_color('============================================================================\n','bgreen')

#2PL Model
premodel <- mirt(data=predata[,Item], model=unimodel, itemtype='2PL')
coeff1 <- coef(premodel)
coeff2 <- coef(premodel, IRTpars=TRUE, simplify=TRUE)

#Model fit
modfit <- M2(premodel)
print(modfit)

print(itemfit(premodel, fit_stats = c('S_X2')))
print(coeff1)
print(coeff2)

#Save estimated thetas
scores <- fscores(premodel, method = 'EAP', full.scores = TRUE, full.scores.SE = TRUE) 
est.theta <- scores[,1]
predata$Est.Theta <- est.theta
print(head(predata$Est.Theta,10))

#Making dataframe to save IRT parameters
avec <- c()
dvec <- c()
for (i in Item){
	avec <- c(avec,(coeff1[i][[1]])[1])
	dvec <- c(dvec,(coeff1[i][[1]])[2])
}
est.pre.pardf <- data.frame(Items = Item, Est.Slopes = avec, Est.Intercepts = dvec)
print(est.pre.pardf)


#Retrieve theta estimates from pretest data with posttest parameters
print_color('============================================================================\n','bgreen')
print_color('===============Pretest Thetas With Posttest Parameters Attempt==============\n','bgreen')
print_color('============================================================================\n','bgreen')

#2PL Model
#mod <- 'Theta = 1-29
#	START = (1,a1,1.6216726),(1,d,4.3252960),(2,a1,1.0062510),(2,d,.7168091),(3,a1,1.4693274),(3,d,3.0475913),(4,a1,1.0425213),(4,d,0.6149890),(5,a1,1.2214504),(5,d,0.3554415)
#	FIXED = (1-5,a1),(1-5,d)'
#mixmod <- mirt.model(mod)

#Use personal function above
print_color('=================Testing Personal Model Formatting Function=================\n','bgreen')
parmod <- FixParameters(parmodel = postmodel, nitems = nit)
print(parmod)
mod <- paste0('Theta = 1-29\n',parmod)
print(mod)
print_color('=================Testing Personal Model Formatting Function=================\n','bgreen')
mixmod <- mirt.model(mod)

#Run model
mixmodel <- mirt(data=predata[,Item], model=mixmod, itemtype='2PL')
coeff1 <- coef(mixmodel)
print(coeff1)
coeff2 <- coef(mixmodel, IRTpars=TRUE, simplify=TRUE)

#Model fit
modfit <- M2(mixmodel)
print(modfit)

print(itemfit(mixmodel, fit_stats = c('S_X2')))
print(coeff1)
print(coeff2)

#Save estimated thetas
scores <- fscores(mixmodel, method = 'EAP', full.scores = TRUE, full.scores.SE = TRUE) 
est.theta <- scores[,1]
predata$Est.Mix.Theta <- est.theta
print(head(predata$Est.Mix.Theta,10))

#Making dataframe to save IRT parameters
avec <- c()
dvec <- c()
for (i in Item){
	avec <- c(avec,(coeff1[i][[1]])[1])
	dvec <- c(dvec,(coeff1[i][[1]])[2])
}
est.mix.pardf <- data.frame(Items = Item, Est.Slopes = avec, Est.Intercepts = dvec)
print(est.mix.pardf)







#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
