library(BayesTree)
#--------------------------------------------------
if(1) { # simulate simple model with binary y
f = function(x){(x/2)^2-1}
set.seed(99)
n = 2000 #size of train data
x= .9*rnorm(n)
x = sort(x)
fx = f(x)
px = pnorm(fx)
y = rbinom(n,1,px)
xp = seq(from=-2.5,to=2.5,length.out=15)
np = length(xp)
}
#--------------------------------------------------
if(1) { # run bart
br = bart(x,y,xp)
}
#--------------------------------------------------
if(1) { #plot results
par(mfrow=c(1,1))
pdr = pnorm(br$yhat.test)
plot(range(xp),range(pdr),type='n',xlab='xp',ylab='P(Y=1)')
pxp = pnorm(f(xp))
points(xp,pxp,col='blue',type='l')
phat = apply(pdr,2,mean)
points(xp,phat,type='l',col='red')
}
