library(readxl)
library(IAcsSPCR)
library(cusumcharter)
library(qcc)
library(dplyr)
library(ggplot2)

data <- data(Lowry)
#lambda is fixed 0.1
Sigma<-matrix(c(1, .5, .5, 1), nrow=2, ncol=2)
mu<- c(0,0)
MEWMA(Lowry, Sigma, mu, Sigma.known = TRUE)
##############################################################
#T2 hotelling
a <-  n/(var1*var2 -(var12)^2)
t <- a * (var2*(xbar1-mean1)^2 + var1*(xbar2-mean2)^2 - (2*var12*(xbar1-mean1)*(xbar2-mean2)))
#phase 1 Tsq
f <- qf(alpha, p, m*n-m-p+1,lower.tail=F)
ucl_f <-((p*(m-1)*(n-1))/ (m*n-m-p+1)) * f
#phase 2 T sq
f <- qf(alpha, p, m*n-m-p+1,lower.tail=F)
ucl_f <-((p*(m+1)*(n-1))/ (m*n-m-p+1)) * f
#when m is large
ucl_chisq <- qchisq(alpha,p)

#phase 1 n=1
b <- qbeta(alpha, p/2, (m-p-1)/2, lower.tail=F)
ucl_b <- ((m-1)^2/m) * b
#phase 2 n=1
f <- qf(alpha, p, m-p,lower.tail=F)
ucl_f <-((p*(m+1)*(m-1))/ (m^2 -(m*p))) * f

t_chart <- ggplot(data, aes(x=sample,y=data$t)) +geom_line() +geom_point() +geom_hline(yintercept = c(0,ucl_f), linetype = "dashed", color="green")+ labs(x="Group", y =" T^2 Statitsics", title=" Hotelling T^2 Control Chart")+ theme_minimal()

####################################################
#cusum
q <- cusum(x, std.dev = 25, se.shift=1, center=1050, decision.interval=5, head.start=H/2)
cusum_control(data, target=1050, std_dev=25, k=0.5, h=5, desired_shift=1)

qcc(x,type="xbar.one", std.dev=25, center=1050, plot=T)

# ********************** ARL Calculation ***********************
mu0=8.02
sigma=0.05
mu1=mean(y)
delta1<-(mu1-mu0)/sigma
h1<-4.77
b1<-h1+1.66

k1<-0.5
tri_p1<-delta1-k1

##ARL+
ARL1plus<-(exp(-2*tri_p1*b1)+(2*tri_p1*b1)-1)/(2*tri_p1*tri_p1)
##ARL1-
tri_n1<- -delta1-k1
ARL1neg<- (exp(-2*tri_n1*b1)+(2*tri_n1*b1)-1)/(2*tri_n1*tri_n1)

#ARL1
ARL1<-1/((1/ARL1plus)+(1/ARL1neg))
ARL1

Scaled cusum
standardized_y <- (y-ctr)/sigma
cusum(standardized_y, std.dev=1,center=0, decision.interval=5)

###################################################################
#p chart (n notconst)
P <- d/n (fraction nonconforming)
Phat <- mean(p)
trial_ucl <- phat + 3*sqrt((phat*(1-phat))/n)
Trial_ucl 
Cl <- phat
Trial_lcl <- phat-3*sqrt((phat*(1-phat))/n)
Trial_lcl (if negative, convert to 0)

qcc(data=p,type="p", sizes=1, stdev=sqrt((phat*(1-phat))/n),limits=c(0,trial_ucl),plot=TRUE)
qcc(data=Fraction.Nonconforming..pi,type="p", sizes=1, stdev=sqrt((p*(1-p))/n),limits=c(revised_lcl,revised_ucl),plot=TRUE,newdata=orangeJuice2$Sample.Fraction.Nonconforming..pi,newsizes=1, center=p)
#########################################################################
#np chart (n constant)
np <- mean(number_nonconform)
np
npq <- sd(number_nonconform)
npq

np_ucl <- np + 3*sqrt(npq)
np_ucl

np_cl <- np
np_cl

np_lcl <- np - 3*sqrt(npq)
np_lcl #(lcl=0 if negative)

################################################################################
#c chart
c <- mean(number_nonconform)
c_ucl <- c + 3*sqrt(c)
c_cl <- c
c_lcl <- c - 3*sqrt(c)
c_lcl=0

qcc(data=number_nonconform, type="c", sizes=1, stdev=sqrt(c), limits=c(np_lcl,np_ucl), plot=TRUE)

################################################################
#uchart (n tak constant)
paper1 <- paper %>% group_by(Day) %>% mutate(mean= total_imperfections/number_rolls_produced)
ubar <- mean(paper1$mean)
Ubar


paper.ucl <- paper1 %>% group_by(Day) %>% mutate(ucl = ubar + 3*sqrt(ubar/number_rolls_produced))

paper.lcl <- paper.ucl %>% group_by(Day) %>% mutate(lcl = ubar - 3*sqrt(ubar/number_rolls_produced))
qcc(data=paper1$mean, sizes=1, type="u", limits=c(paper.lcl$lcl,paper.ucl$ucl))

#avg n
avg_n <- mean(number_rolls_produced)
avg_n
paper2 <- paper %>% group_by(Day) %>% mutate(mean= total_imperfections/avg_n)
ubar2 <- mean(paper2$mean)

avg.ucl <- ubar2 + 3*sqrt(ubar2/avg_n)
avg.ucl
avg.lcl <- ubar2 - 3*sqrt(ubar2/avg_n)
avg.lcl

qcc(data=paper2$mean, sizes=1, type="u", stdev=sqrt(ubar2/avg_n), limits=c(avg.lcl,avg.ucl))

###################################
#x bar and s
#n not constant
diameterP <- qcc.groups(`Inner Diameter`, `Sample Number`) #tukar to wider data
summary(qcc(diameterP,type="xbar"))
summary(qcc(diameterP,type="S"))

data$xbar <- apply(data[,2:6], 1, mean)
data$r <- apply(data[,2:6], 1, function(x) diff(range(x)))
data$s <- apply(data[,2:6], 1, sd)
centerline_xbar <- mean(data$xbar)
centerline_r <- mean(data$r)
centerline_s <- mean(data$s)

A2 <- 0.577
D4 <- 2.114
D3 <- 0
ucl_xbar <- centerline_xbar + A2*centerline_r
lcl_xbar <- centerline_xbar - A2*centerline_r
ucl_r <- D4*centerline_r
lcl_r <- D3*centerline_r

A3 <- 1.427
B4 <- 2.089
B3 <- 0
ucl_xbar <- centerline_xbar + A3*centerline_s
lcl_xbar <- centerline_xbar - A3*centerline_s
ucl_s <- B4*centerline_s
lcl_s <- B3*centerline_s

qcc( data=data$xbar, type=”xbar”, sizes=5, digits=2, limits=c(lcl_xbar, ucl_xbar), plot=TRUE, stdev=centerline_r)
qcc(data=data$r, type=”R”, sizes=5, digits=2, limits=c(lcl_r,ucl_r),plot=TRUE, stdev=centerline_r, title=”Sample R chart”)
qcc(data=data$s, type=”S”, sizes=5, digits=2, limits=c(lcl_s,ucl_s),plot=TRUE, stdev=centerline_s, title=”Sample S chart”)

#########################################
#process capability calculation
#ESTIMATE PROCESS CAPABILITY (purpose : to understand the chance of nonconforming products that will be produced)

usl = 40
lsl = 20
d2=2.326
p.sd= g.R.mean/d2
pcr <- (usl-lsl)/(6*p.sd)

#PERCENTAGE OF SPECIFICATION BAND
p <- (1/pcr)*100


 
