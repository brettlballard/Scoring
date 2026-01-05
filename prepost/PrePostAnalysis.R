#!/usr/bin/env Rscript
#Above line allows code to be run using ./PrePostAnalysis.R in terminal

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
library(cowplot)#combining plots
library(geomtextpath)#geom_text_segment
library(ggrepel)#geom_text_repel

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the scoring analysis')
parser <- add_argument(parser, "--data", help = 'data being used; options are in the code',nargs='*',default='FCI')
#
#FCI pre & post
#FMCE pre & post
#FMCE Thornton pre & post
#K1-20 pre & post
#K1-7 pre & post
#CSEMsam1 pre & post
#CSEMsam2 pre & post
#
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
}else if (arg$data == 'K1-20'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING K1-20 ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'K1-20'
}else if (arg$data == 'K1-7'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!RUNNING K1-7 ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'K1-7'
}else if (arg$data == 'CSEMsam1'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING CSEM SAMPLE 1 ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CSEMsam1'
}else if (arg$data == 'CSEMsam2'){
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING CSEM SAMPLE 2 ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')
	test <- 'CSEMsam2'
}else {
        print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!!!!INVALID DATA ARGUMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bred')
        break
}

#Make directory to store stuff for each test
if (!dir.exists(paste0('prepostout/',test,'/'))){dir.create(paste0('prepostout/',test,'/'), recursive = TRUE)}

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################



##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

predf <- read.csv(paste0('../analysisout/summary/',test,'/pre/AnalysisOutput1.csv'))
postdf <- read.csv(paste0('../analysisout/summary/',test,'/post/AnalysisOutput1.csv'))

prenstud <- unique(predf$Number.Students.Original)
postnstud <- unique(postdf$Number.Students.Original)

prescoresdf <- read.csv(paste0('../analysisout/summary/',test,'/pre/Scores-',prenstud,'.csv'))
postscoresdf <- read.csv(paste0('../analysisout/summary/',test,'/post/Scores-',postnstud,'.csv'))

#Printing out the full tibble so one can see column names and data types
print_color('============================================================================\n','bold')
print_color('==============================Pretest Data Set==============================\n','bold')
print_color('============================================================================\n','bold')
prescores <- prescoresdf %>%
	as_tibble() %>%
	print()
	       
print_color('============================================================================\n','bold')
print_color('==============================Posttest Data Set=============================\n','bold')
print_color('============================================================================\n','bold')
postscores <- postscoresdf %>%
	as_tibble() %>%
	print()

#Combining the scores for comparative analysis
print_color('============================================================================\n','bold')
print_color('==============================Scores Data Set===============================\n','bold')
print_color('============================================================================\n','bold')
scores <- data.frame('Post.Est.Th' = postscores$Est.Theta, 'Pre.Est.Th' = prescores$Est.Theta, 'Post.SimSumSc' = postscores$SimSum.Score, 'Pre.SimSumSc' = prescores$SimSum.Score, 'Post.WSc' = postscores$Scaled.Weighted.Score, 'Pre.WSc' = prescores$Scaled.Weighted.Score)
scores <- scores %>%
	as_tibble() %>%
	print()

#Comparing linear predictiveness
print_color('============================================================================\n','bcyan')
print_color('=======================R-Squared For Different Models=======================\n','bcyan')
print_color('============================================================================\n','bcyan')

print_color('=======================Predicting Posttest Est Theta========================\n','bgreen')
mod <- lm(Post.Est.Th ~ Pre.Est.Th, data = scores)
print_color(paste0('R^2 for Post.EstTh ~ Pre.EstTh: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.Est.Th ~ Pre.SimSumSc, data = scores)
print_color(paste0('R^2 for Post.EstTh ~ Pre.SimSumSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.Est.Th ~ Pre.WSc, data = scores)
print_color(paste0('R^2 for Post.EstTh ~ Pre.WSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
print_color('=======================Predicting Posttest SimSumSc=========================\n','bgreen')
mod <- lm(Post.SimSumSc ~ Pre.Est.Th, data = scores)
print_color(paste0('R^2 for Post.SimSumSc ~ Pre.Est.Th: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.SimSumSc ~ Pre.SimSumSc, data = scores)
print_color(paste0('R^2 for Post.SimSumSc ~ Pre.SimSumSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.SimSumSc ~ Pre.WSc, data = scores)
print_color(paste0('R^2 for Post.SimSumSc ~ Pre.WSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
print_color('=========================Predicting Posttest WSc============================\n','bgreen')
mod <- lm(Post.WSc ~ Pre.Est.Th, data = scores)
print_color(paste0('R^2 for Post.WSc ~ Pre.Est.Th: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.WSc ~ Pre.SimSumSc, data = scores)
print_color(paste0('R^2 for Post.WSc ~ Pre.SimSumSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')
mod <- lm(Post.WSc ~ Pre.WSc, data = scores)
print_color(paste0('R^2 for Post.WSc ~ Pre.WSc: ',round(summary(mod)$r.squared,4),'\n'),'bviolet')









#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
