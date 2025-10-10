#!/usr/bin/env Rscript
#Above line allows code to be run using ./FxnFitStart.R in terminal

#Curious about runtime
start <- Sys.time()

#Libraries and what they are used for commented next to them
library(insight)#print_color function
library(ggplot2)#plot related

set.seed(328)
x <- rnorm(1000,0,1)
df <- data.frame(X = x)

#Testing for potential start values to use for nls fxn in FitParDist.R code

#Exponential Growth
#formula: y = a*exp(b*(x-c))
df$EG <- 1.2 * exp(.5 * (df$X - 0))

#Exponential Decay
#formula: y = a*exp(-b*(x-c))
df$ED <- 1.2 * exp(-.5 * (df$X - 0)) 

#Left-Asym
#formula: y = a-(a-b)*exp(-c*(x-d))
df$LAsym <- 3 - (3 - 1)*exp(-.7 * (df$X + 2))

#Right-Asym
#formula: y = a-(a-b)*exp(-c*(-1*x-d))
df$RAsym <- 3 - (3 - 1)*exp(-.7 * (-1 * df$X + 2))

#Gaussian
#formula: y = a*exp((-b*(x-c)**2))+d
df$G <- 3 * exp((-.5 * (df$X - 0)**2)) + .75 

#Inverted Gaussian
#formula: y = -a*exp((-b*(x-c)**2))+d
df$IG <- -3 * exp((-.5 * (df$X - 0)**2)) + 3.75 

#4PL Logistic
#formula: y = a+(b-a)*(1/(1+exp(-c(x-d))))
df$Log4 <- .5 + (3.25 - .5)*(1 / (1 + exp(-1.5 * (df$X - 0))))

#Reflected 4PL Logistic
#formula: y = a+(b-a)*(1/(1+exp(c(x-d))))
df$RLog4 <- .5 + (3.25 - .5)*(1 / (1 + exp(1.5 * (df$X - 0))))

pdf('FittingFxns.pdf')

print(ggplot(data=df, mapping=aes(x=X,y=EG))+geom_point()+geom_line()+labs(title=paste0('Exponential Growth Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=ED))+geom_point()+geom_line()+labs(title=paste0('Exponential Decay Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=LAsym))+geom_point()+geom_line()+labs(title=paste0('Left Asymptote Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=RAsym))+geom_point()+geom_line()+labs(title=paste0('Right Asymptote Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=G))+geom_point()+geom_line()+labs(title=paste0('Gaussian Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=IG))+geom_point()+geom_line()+labs(title=paste0('Inverted Gaussian Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=Log4))+geom_point()+geom_line()+labs(title=paste0('4PL Logistic Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))
print(ggplot(data=df, mapping=aes(x=X,y=RLog4))+geom_point()+geom_line()+labs(title=paste0('Reflected 4PL Logistic Fxn'))+scale_x_continuous(name='X', n.breaks=10)+scale_y_continuous(name='Y', n.breaks=10))


dev.off()

end <- Sys.time()
hrdiff <- as.numeric(difftime(end, start, units = 'hours'))
mindiff <- as.numeric(difftime(end, start, units = 'mins'))
secdiff <- as.numeric(difftime(end, start, units = 'secs'))
print_color(paste0('Runtime: ',floor(hrdiff),' hours ',floor(mindiff %% 60),' mins ',round(secdiff %% 60),' seconds\n'),'bgreen')
