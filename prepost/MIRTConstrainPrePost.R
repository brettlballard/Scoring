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

#Make directory to store stuff for each test
if (!dir.exists(paste0(test,'/'))){dir.create(paste0(test,'/'), recursive = TRUE)}

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
predata$SimSumSc <- apply(predata[,Item],1,sum)
print(predata)
nprepart <- nrow(predata)

print_color('============================================================================\n','bold')
print_color('====================Posttest Data Set With Selected Items===================\n','bold')
print_color('============================================================================\n','bold')
postdata <- postdata %>%
	select(all_of(Item))
postdata$SimSumSc <- apply(postdata[,Item],1,sum)
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
unimodel <- mirt.model(paste0('Theta = 1-',nit))
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
#Use personal function above
parmod <- FixParameters(parmodel = postmodel, nitems = nit)
mod <- paste0('Theta = 1-',nit,'\n',parmod)
mixmod <- mirt.model(mod)

#Run model
mixmodel <- mirt(data=predata[,Item], model=mixmod, itemtype='2PL')
coeff1 <- coef(mixmodel)
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
predata$Adj.Est.Theta <- est.theta
print(head(predata$Adj.Est.Theta,10))

#Making dataframe to save IRT parameters
avec <- c()
dvec <- c()
for (i in Item){
	avec <- c(avec,(coeff1[i][[1]])[1])
	dvec <- c(dvec,(coeff1[i][[1]])[2])
}
est.mix.pardf <- data.frame(Items = Item, Est.Slopes = avec, Est.Intercepts = dvec)
print(est.mix.pardf)

#Comparison of various thetas
print_color('============================================================================\n','bgreen')
print_color('============================Comparison of Thetas============================\n','bgreen')
print_color('============================================================================\n','bgreen')
scores <- data.frame(Posttest.SimSumSc = postdata$SimSumSc, Pretest.SimSumSc = predata$SimSumSc, Posttest.EstTheta = postdata$Est.Theta, Pretest.EstTheta = predata$Est.Theta, Pretest.AdjEstTheta = predata$Adj.Est.Theta)
print(head(scores,10))

#Scale everything to be in 0-100 interval
scores$Posttest.SimSumSc <- 100 * (scores$Posttest.SimSumSc - min(scores$Posttest.SimSumSc))/(max(scores$Posttest.SimSumSc) - min(scores$Posttest.SimSumSc))
scores$Pretest.SimSumSc <- 100 * (scores$Pretest.SimSumSc - min(scores$Pretest.SimSumSc))/(max(scores$Pretest.SimSumSc) - min(scores$Pretest.SimSumSc))
scores$Posttest.EstTheta <- 100 * (scores$Posttest.EstTheta - min(scores$Posttest.EstTheta))/(max(scores$Posttest.EstTheta) - min(scores$Posttest.EstTheta))
scores$Pretest.EstTheta <- 100 * (scores$Pretest.EstTheta - min(scores$Pretest.EstTheta))/(max(scores$Pretest.EstTheta) - min(scores$Pretest.EstTheta))
scores$Pretest.AdjEstTheta <- 100 * (scores$Pretest.AdjEstTheta - min(scores$Pretest.AdjEstTheta))/(max(scores$Pretest.AdjEstTheta) - min(scores$Pretest.AdjEstTheta))
scores$Pretest.ThetaDiff <- scores$Pretest.EstTheta - scores$Pretest.AdjEstTheta

#Bin things and check frequency counts for each bin
scores$Posttest.SimSumSc.Binned <- as.numeric(as.character(cut(scores$Posttest.SimSumSc, breaks=seq(from = 0, to = 100, by = 10), labels=seq(from = 5, to = 95, by = 10), include.lowest = TRUE)))
scores$Pretest.SimSumSc.Binned <- as.numeric(as.character(cut(scores$Pretest.SimSumSc, breaks=seq(from = 0, to = 100, by = 10), labels=seq(from = 5, to = 95, by = 10), include.lowest = TRUE)))
scores$Posttest.EstTheta.Binned <- as.numeric(as.character(cut(scores$Posttest.EstTheta, breaks=seq(from = 0, to = 100, by = 10), labels=seq(from = 5, to = 95, by = 10), include.lowest = TRUE)))
scores$Pretest.EstTheta.Binned <- as.numeric(as.character(cut(scores$Pretest.EstTheta, breaks=seq(from = 0, to = 100, by = 10), labels=seq(from = 5, to = 95, by = 10), include.lowest = TRUE)))
scores$Pretest.AdjEstTheta.Binned <- as.numeric(as.character(cut(scores$Pretest.AdjEstTheta, breaks=seq(from = 0, to = 100, by = 10), labels=seq(from = 5, to = 95, by = 10), include.lowest = TRUE)))

print(table(scores$Posttest.SimSumSc.Binned))
print(table(scores$Pretest.SimSumSc.Binned))
print(table(scores$Posttest.EstTheta.Binned))
print(table(scores$Pretest.EstTheta.Binned))
print(table(scores$Pretest.AdjEstTheta.Binned))

#Pre vs Post SimSumSc Binned
prepostsss <- ggplot(data=scores, mapping=aes(x=Pretest.SimSumSc.Binned,y=Posttest.SimSumSc.Binned))+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Simple Sum Score', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-SimSumSc-Binned-',nit,'.pdf'), path=paste0(test,'/'), prepostsss)

#Pre vs Post Thetas(Adj & Normal) Binned 
prepostadjth <- ggplot(data=scores, mapping=aes(x=Pretest.AdjEstTheta.Binned,y=Posttest.EstTheta.Binned))+scale_x_continuous(name='Pretest Adjusted Estimated Theta', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-AdjEstTh-Binned-',nit,'.pdf'), path=paste0(test,'/'), prepostadjth)

prepostth <- ggplot(data=scores, mapping=aes(x=Pretest.EstTheta.Binned,y=Posttest.EstTheta.Binned))+scale_x_continuous(name='Pretest Estimated Theta', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-EstTh-Binned-',nit,'.pdf'), path=paste0(test,'/'), prepostth)


#Pre vs Post SimSumSc 
prepostsss <- ggplot(data=scores, mapping=aes(x=Pretest.SimSumSc,y=Posttest.SimSumSc))+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Simple Sum Score', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-SimSumSc-',nit,'.pdf'), path=paste0(test,'/'), prepostsss)

#Pre vs Post Thetas(Adj & Normal) 
prepostadjth <- ggplot(data=scores, mapping=aes(x=Pretest.AdjEstTheta,y=Posttest.EstTheta))+scale_x_continuous(name='Pretest Adjusted Estimated Theta', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-AdjEstTh-',nit,'.pdf'), path=paste0(test,'/'), prepostadjth)

prepostth <- ggplot(data=scores, mapping=aes(x=Pretest.EstTheta,y=Posttest.EstTheta))+scale_x_continuous(name='Pretest Estimated Theta', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PrePost-EstTh-',nit,'.pdf'), path=paste0(test,'/'), prepostth)

#Plotting Binned Pretest.SimSumSc vs posttests measures combined
plotdf <- melt(scores, id = 'Pretest.SimSumSc.Binned', measure.vars = c('Pretest.EstTheta.Binned','Pretest.AdjEstTheta.Binned'))
precom <- ggplot(data=plotdf, mapping=aes(x=Pretest.SimSumSc.Binned,y=value,group=variable,color=variable))+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Scaled Pretest Measures', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PreSimSumSc-Binned-PreThetas-Binned-',nit,'.pdf'), path=paste0(test,'/'), precom)

#Plotting Binned Posttest.SimSumSc vs pretests measures combined
plotdf <- melt(scores, id = 'Posttest.SimSumSc.Binned', measure.vars = c('Pretest.SimSumSc.Binned','Pretest.EstTheta.Binned','Pretest.AdjEstTheta.Binned'))
postcom <- ggplot(data=plotdf, mapping=aes(x=value,y=Posttest.SimSumSc.Binned,group=variable,color=variable))+scale_x_continuous(name='Scaled Pretest Measure', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Simple Sum Score', n.breaks=10, limits=c(0,100))+geom_smooth()+annotate('segment', x=0, y=0, xend=100, yend=100, colour='black', linetype='dashed')
ggsave(file=paste0(test,'-PostSimSumSc-Binned-PreAll-Binned-',nit,'.pdf'), path=paste0(test,'/'), postcom)





#Random plots to try and find any other patterns
pretest <- ggplot(data=scores, mapping=aes(x=Pretest.SimSumSc,y=Pretest.ThetaDiff))+geom_point(size=1,aes(color=Posttest.SimSumSc))+scale_colour_gradient(low='red',high='green')+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Pretest Est Theta Difference: Normal - Adjusted', n.breaks=10)+geom_smooth()
ggsave(file=paste0(test,'-Pretest-EstThDiffvSimSumSc-',nit,'.pdf'), path=paste0(test,'/'), pretest)

#Will combine the two below
preth <- ggplot(data=scores, mapping=aes(x=Pretest.SimSumSc,y=Pretest.EstTheta))+geom_point(size=1,aes(color=Posttest.SimSumSc))+scale_colour_gradient(low='red',high='green')+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Pretest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()

preadjth <- ggplot(data=scores, mapping=aes(x=Pretest.SimSumSc,y=Pretest.AdjEstTheta))+geom_point(size=1,aes(color=Posttest.SimSumSc))+scale_colour_gradient(low='red',high='green')+scale_x_continuous(name='Pretest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Pretest Adjusted Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()

pre <- plot_grid(preth+theme(legend.position='none'), preadjth+theme(legend.position='none'), labels = c('A','B'))
legend <- get_legend(preth+guides(color = guide_legend(nrow=1))+theme(legend.position = 'bottom'))
pre <- plot_grid(pre, legend, ncol = 1, rel_heights=c(1, .1))
ggsave(file=paste0(test,'-Pretest-EstThvSimSumSc-',nit,'.pdf'), path=paste0(test,'/'), pre)

#Will combine the two below
post.preth <- ggplot(data=scores, mapping=aes(x=Posttest.SimSumSc,y=Posttest.EstTheta))+geom_point(size=1,aes(color=Pretest.EstTheta))+scale_colour_gradient(low='red',high='green')+scale_x_continuous(name='Posttest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()

post.preadjth <- ggplot(data=scores, mapping=aes(x=Posttest.SimSumSc,y=Posttest.EstTheta))+geom_point(size=1,aes(color=Pretest.AdjEstTheta))+scale_colour_gradient(low='red',high='green')+scale_x_continuous(name='Posttest Simple Sum Score', n.breaks=10, limits=c(0,100))+scale_y_continuous(name='Posttest Estimated Theta', n.breaks=10, limits=c(0,100))+geom_smooth()

post <- plot_grid(post.preth, post.preadjth, labels = c('A','B'), ncol=1)
ggsave(file=paste0(test,'-Posttest-EstThvSimSumSc-',nit,'.pdf'), path=paste0(test,'/'), post)








#Curious about runtime
end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
