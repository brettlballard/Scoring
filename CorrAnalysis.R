#!/usr/bin/env Rscript
#Above line allows code to be run using ./CorrAnalysis.R in terminal

#Libraries and what they are used for commented next to them
library(dplyr)#as_tibble and many other dataframe manipulation shortcuts
library(data.table)#setnames function
library(reshape2)#melt function
library(insight)#print_color function
library(argparser)#anything parser related
library(mirt)#IRT stuff
library(ggplot2)#plot related

#Adding argument parsers so that I can vary the scoring analysis from the command line
parser <- arg_parser('Options for varying the run of the scoring analysis')
parser <- add_argument(parser, "--runname", help = 'name for set of runs being investigated',nargs='*',default='run')
parser <- add_argument(parser, "--runlength", help = 'how many files in run length',nargs='*',default=1)
arg <- parse_args(parser)

print_color(paste0('!!!!!!!!!!!!!!!!!!!!!!!RUNNING ',arg$runname,' ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!\n'),'bgreen')

##############################################################################################################
#################################################FUNCTIONS####################################################
##############################################################################################################

##############################################################################################################
###################################################DATA#######################################################
##############################################################################################################

df <- read.csv(paste0('rawdata/',test,'-',tt,'.csv'))

data <- as_tibble(df)

