#######################################################################
#####   MH-within-Gibbs function 
#######################################################################
LogTargetDensity <- function(m,y,X,beta,Sigma_inv){
    a = 1/2 * beta%*%Sigma_inv%*%as.vector(beta)
    b = y*(X%*%beta) - m*log(1+exp(X%*%beta)) 
    return(-a+sum(b))
}


bayes.logreg <- function(m,y,X,beta.0,Sigma.0.inv, niter=10000,burnin=1000,
                           print.every=1000,retune=100, verbose=TRUE){
	p = ncol(X)
	v = rep(1,p)
	beta.curr <- beta.0
    	beta.t <- matrix(0,niter,p)
	beta.t[1,] <- beta.0
 	accept <- rep(0,p)
   for(t in 1:niter){  
	for(i in 1:p){
		beta.prop <- beta.curr
  		beta.prop[i] <- rnorm(1,beta.curr[i], v[i])
		log_alpha <- LogTargetDensity(m,y,X,beta.prop,Sigma.0.inv)-LogTargetDensity(m,y,X,beta.curr,Sigma.0.inv)
		log_u = log(runif(1))
    		if (log_u < log_alpha){
        		beta.curr = beta.prop
			accept[i] = accept[i] + 1
		}
      } 
      beta.t[t,] = beta.curr
   
      if(t%%retune == 0 && t <= burnin){
		if(verbose==TRUE){
		cat("Acceptance rate is", accept/retune,"\n")
		cat("Proposal variances are", v, "\n")
		cat("-----------------------------------------\n")
		}
		v[which(accept/retune < 0.3)] = v[which(accept/retune < 0.3)]/2
		v[which(accept/retune > 0.6)] = v[which(accept/retune > 0.6)]*sqrt(2)
		accept <- rep(0,p)
      }
   	
	if(t%%print.every == 0 && t > burnin){
		cat("The number of iteration is:",t,"\n")
		cat("Acceptance rate is:", accept/(niter-burnin),"\n")
		cat("Proposal variances are:", v, "\n")
		cat("Beta value:",beta.curr,"\n")
		cat("-----------------------------------------\n")
	}

   }   # t-loop
   cat("The total acceptance rate after burnin is", accept/(niter-burnin),"\n")
   return(beta.t)
}

#######################################################################################
#### read data and assign value
#######################################################################################
data = read.table("C:/Users/siyuanzhou/Stuff/HW1/blr_data_1197.csv",sep=",",header=T)
beta.tr = read.table("C:/Users/siyuanzhou/Stuff/HW1/blr_pars_1197.csv",sep=",",header=T)

y = data$y
m = data$n
X = cbind(data$X1, data$X2)
Sigma_inv = diag(1,2)
source("C:/Users/siyuanzhou/Stuff/HW1/function.txt")

beta.0 = rep(0,ncol(X))
Sigma.0.inv = diag(1,2)
burnin = 1000
niter = 10000
draw = bayes.logreg(m,y,X,beta.0,Sigma.0.inv)
gibbs.samples <- draw[(burnin+1):niter,]
beta_quant = apply(gibbs.samples,2,function(x) quantile(x,1:99/100))

#####  save results #####################################################################
write.table(beta_quant,file="C:/Users/siyuanzhou/Stuff/HW1/beta_quant.csv",sep=",",row.names=F,col.names=F)
###########################################################################################


#########################################################################################
######    traceplots and histogram of parameter   #######################################

j=1
x11()
par(mfrow=c(2,2))
plot(1:9000,gibbs.samples[,j],type='l',ylab=paste("beta_",j,sep=""),xlab="t")
hist(gibbs.samples[,j],xlab=paste("beta_",j,sep=""),main=paste("Histogram of beta_",j,sep=""))
abline(v=beta.tr[j,1],col=2)

#########################################################################################
######    verify that it produces posterior credible intervals 
######    that cover the true parameter values   #######################################
library(MASS)
cancer = read.table("C:/Users/siyuanzhou/Stuff/HW1/BayesLogit/breast_cancer.txt",header=T)
 
y = as.numeric(cancer$diagnosis == "M")
m = rep(1,nrow(cancer))
X = cbind(rep(1,nrow(cancer)),as.matrix(cancer[,1:10]))
beta.0 = rep(0,11)
Sigma.0.inv = diag(1/1000,11)
burnin = 2000
niter = 20000
draw = bayes.logreg(m,y,X,beta.0,Sigma.0.inv,niter = niter, burnin=burnin,way="MH")
gibbs.samples <- draw[(burnin+1):niter,]]

###  traceplot  
x11()
par(mfrow=c(3,4))
for(j in 1:11){
plot(1:nrow(gibbs.samples),gibbs.samples[,j],type='l',ylab=names(cancer[j]),xlab="t")
}


###  Compute the lag-1 autocorrelation for each component of ¦Â
par(mfrow=c(3,4))
for(j in 1:11){
acf(gibbs.samples[,j],lag.max=1,plot=TRUE,ylab=names(cancer[j]),main="")
}


###  a posterior predictive check
M = 5000
mean.hat.y = numeric(0)
samples = sample(1:(niter-burnin),M,replace = F)
for (i in 1:length(samples)){
mean.hat.y[i] = mean(exp(X%*%gibbs.samples[samples[i],])/(1+exp(X%*%gibbs.samples[samples[i],])))
}
pdf("C:/Users/siyuanzhou/Stuff/HW1/cancer_postcheck.pdf")
hist(mean.hat.y,breaks=25)
abline(v=sum(y)/sum(m),col=2)
dev.off()
p.value = min(sum(mean.hat.y > sum(y)/sum(m))/M,sum(mean.hat.y < sum(y)/sum(m))/M)


###  central credible interval
CI = matrix(0,11,2)
for(j in 1:11){
 CI[j,] = quantile(gibbs.samples[,j],c(0.025, 0.975))
}
rownames(CI)=c("intercept",names(cancer[1:10]))
colnames(CI)=c("2.5%","97.5%")
library(xtable)
print(xtable(CI))

####################################################################################
#######   fit a logistic model
cancer$y = y
cancer.fit = glm(y~1+area+compactness+concavepts+concavity+fracdim+perimeter+radius+
		smoothness+symmetry+texture,data=cancer,family=binomial(link=logit))
std.cancer = summary(cancer.fit)$coefficients[,2]
COV	 <- summary(cancer.fit)$cov.scaled
beta.tr <- cancer.fit$coefficients

bayes.logreg <- function(m,y,X,beta.0,Sigma.0.inv, niter=10000,burnin=1000,
                           print.every=1000,retune=100, way = "MHGibbs", verbose=TRUE){
	p = ncol(X)
	beta.curr <- beta.0
    	beta.t <- matrix(0,niter,p)
	beta.t[1,] <- beta.0
 	if(way == "MHGibbs"){
      	accept <- rep(0,p)
      	v = std.cancer
   		for(t in 1:niter){  
		for(i in 1:p){
		   beta.prop <- beta.curr
  		   beta.prop[i] <- rnorm(1,beta.curr[i], v[i])
		   log_alpha <- LogTargetDensity(m,y,X,beta.prop,Sigma.0.inv)-LogTargetDensity(m,y,X,beta.curr,Sigma.0.inv)
		   log_u = log(runif(1))
    		   if (log_u < log_alpha){
        		beta.curr = beta.prop
			accept[i] = accept[i] + 1
		   }
      	   beta.t[t,] = beta.curr
      	   }

	         if(t%%retune == 0 && t <= burnin){
		   if(verbose==TRUE){
			cat("Acceptance rate is", accept/retune,"\n")
			cat("Proposal variances are", v, "\n")
			cat("-----------------------------------------\n")
		   }
		   v[which(accept/retune < 0.3)] = v[which(accept/retune < 0.3)]/2
		   v[which(accept/retune > 0.6)] = v[which(accept/retune > 0.6)]*sqrt(2)
		   accept <- rep(0,p)
      	   }
		   if(t%%print.every == 0 && t > burnin){
			cat("The number of iteration is:",t,"\n")
			cat("Acceptance rate is:", accept/(niter-burnin),"\n")
			cat("Proposal variances are:", v, "\n")
			cat("Beta value:",beta.curr,"\n")
			cat("-----------------------------------------\n")
		   }
	 	} # end of t-loop
	}
	if(way == "MH"){
		accept = 0
		v = 1
		for(t in 1:niter){  
		   beta.prop <- mvrnorm(1,mu=beta.curr, Sigma=v*COV)
  		   log_alpha <- min(0,LogTargetDensity(m,y,X,beta.prop,Sigma.0.inv)-LogTargetDensity(m,y,X,beta.curr,Sigma.0.inv))
		   log_u = log(runif(1))
    		   if (log_u < log_alpha){
        		beta.curr = beta.prop
			accept = accept + 1
		   }
      	   beta.t[t,] = beta.curr
      	
	         if(t%%retune == 0 && t <= burnin){
		   	if(verbose==TRUE){
				cat("Acceptance rate is", accept/retune,"\n")
				cat("Proposal variances are", v, "\n")
				cat("-----------------------------------------\n")
			}
		   if(accept/retune < 0.3) v = v/2
		   if(accept/retune > 0.6) v= v*sqrt(2)
		   accept <- 0
   		   }
		   if(t%%print.every == 0 && t > burnin){
			cat("The number of iteration is:",t,"\n")
			cat("Acceptance rate is:", accept/(niter-burnin),"\n")
			cat("Proposal variances are:", v, "\n")
			cat("Beta value:",beta.curr,"\n")
			cat("-----------------------------------------\n")
		   }
	
		}# t-loop
   	}   
   cat("The total acceptance rate after burnin is", accept/(niter-burnin),"\n")
   return(beta.t)
}


