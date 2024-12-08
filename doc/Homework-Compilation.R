## -----------------------------------------------------------------------------
s<-matrix(runif(10*20),nrow=100,ncol=2)
plot(s,main="Hundred points",xlab="x",ylab="y")

## -----------------------------------------------------------------------------
s<-sample(0:100,size=10,replace=FALSE)
barplot(s,main='outcome',ylab='value',xlab='number')

## -----------------------------------------------------------------------------
nub<-matrix(0,101,1)
for(i in 1:1000){
s<-sample(c(0,1),size=100,replace=TRUE)
s1<-sum(s)
nub[s1]<-nub[s1]+1
}
barplot(t(nub),main='Random Walk',xlab="step number",ylab="frequency")


## ----fig.height=4-------------------------------------------------------------
n <- 1e5;j<-k<-0;y <- numeric(n);
sig<-1;
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}
hist(y,breaks=20,main="Rayleigh distribution at sigma=1",xlim=c(0,4),ylim=c(0,1),freq=F,xlab="x")
par(new=TRUE)
curve((x/sig^2*exp(-x^2/2/sig^2)),0,4,xlab="",ylab="",xaxt="n",ylim=c(0,1))

## ----fig.height=4-------------------------------------------------------------
n <- 1e5;j<-k<-0;y <- numeric(n);
sig<-0.5;
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}
hist(y,breaks=20,main="Rayleigh distribution at sigma=0.5",xlim=c(0,4),ylim=c(0,1.5),freq=F,xlab="x")
par(new=TRUE)
curve((x/sig^2*exp(-x^2/2/sig^2)),0,4,xlab="",ylab="",xaxt="n",ylim=c(0,1.5))

## ----fig.height=4-------------------------------------------------------------
n <- 1e5;j<-k<-0;y <- numeric(n);
sig<-2;
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}
hist(y,breaks=20,main="Rayleigh distribution at sigma=2",xlim=c(0,10),ylim=c(0,0.5),freq=F,xlab="x")
par(new=TRUE)
curve((x/sig^2*exp(-x^2/2/sig^2)),0,10,xlab="",ylab="",xaxt="n",ylim=c(0,0.5))

## ----fig.height=4-------------------------------------------------------------
n<-1e4;p1<-0.25;j<-0;x<-numeric(n)
while(j<n){
  u<-runif(1)
  j<-j+1
  if(u<p1){
    x[j]<-rnorm(1,0,1)
  }
  else{
    x[j]<-rnorm(1,3,1)
  }
}
hist(x,freq=F,main="p1=0.25",breaks=50)
  

## ----fig.height=4-------------------------------------------------------------
n<-1e4;p1<-0.5;j<-0;x<-numeric(n)
while(j<n){
  u<-runif(1)
  j<-j+1
  if(u<p1){
    x[j]<-rnorm(1,0,1)
  }
  else{
    x[j]<-rnorm(1,3,1)
  }
}
hist(x,freq=F,main="p1=0.5",breaks=50)
  

## ----fig.height=3-------------------------------------------------------------

    n <- 1e4; r <- 4; beta <- 3;j<-0;y<-numeric(n);
while(j<n){
    #lambda <- rgamma(1, r, beta)#假设lambda依然由gamma分布给出
  lambda<-2
    x1 <- floor(10*lambda); 
    #y1 <- sum(rnbinom(x1,size=r,prob=beta/(1+beta)));
    y1<-sum(rgamma(x1,r,beta))
    y[j]<-y1;
    j=j+1;
   
}
   ave=mean(y)
   avar=var(y)
   tave=2*10*4/3
   tvar=2*10*4/3/3
    hist(y,main="Compound Possion Gamma Process at t=10",xlab="X",ylab="Count")

## ----fig.height=4-------------------------------------------------------------
m<-1e4;j<-1;mcbeta<-numeric(9);fc<-numeric(9);
for (x in seq(from=0.1, to=0.9, length.out=9)){
  y<-runif(m,min=0,max=x)
  theta.hat<-mean(y^2*(1-y)^2)
  mcbeta[j]<-theta.hat*x*gamma(6)/gamma(3)/gamma(3)
  fc[j]<-(pbeta(x,3,3)-mcbeta[j])/pbeta(x,3,3)
  j<-j+1
}
plot(seq(from=0.1, to=0.9, length.out=9),fc*100,xlim=c(0.1,0.9),ylim=c(0,100),main="蒙特卡洛估算的相对误差",xlab="x",ylab="相对误差值(%)")
lines(seq(from=0.1, to=0.9, length.out=9),fc*100,xlim=c(0.1,0.9),ylim=c(0,100),col="red")

## ----fig.height=4-------------------------------------------------------------
n <- 1e3;j<-k<-0;y1 <- numeric(n);y2 <- numeric(n);y3<-numeric(n);y4<-numeric(n)
sig<-1;
u0<-runif(5*n)
while (k < n) {
  u <- u0[j+1]
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y1[k] <- x
  }
}
j<-k<-0;
while (k < n) {
  u <- u0[j+1]
  u <- 1-u
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y2[k] <- x
  }
}
var1=var((y1+y2)/2)
j<-k<-0;
n<-500;
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y3[k] <- x
  }
}
j<-k<-0;
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- -sig*log(1-runif(1),base=exp(1)) #random variate from g(.)
  if  ((x/(sig)^2*exp(-x^2/(2*sig^2)))/(2.5*exp(-x/sig)/sig)> u) {#c=2.5
    #we accept x
    k <- k + 1
    y4[k] <- x
  }
}
var2=var((y3+y4)/2)

## ----fig.height=4-------------------------------------------------------------
curve(x^2/sqrt(2*pi)*exp(-x^2/2),1,5,col=1)
curve(1/(1+cos(1))*sin(x),1,5,,col=2,add=TRUE)
curve(exp(1-x),1,5,,col=3,add=TRUE)


## -----------------------------------------------------------------------------
n<-1e5;k<-1;y1<-y2<-numeric(n)
while(k<=n){
  u<- runif(1)
  y1[k]<- acos(cos(1)-(cos(1)+1)*u);
  if(y1[k]>=1){
  x<- y1[k];
  y2[k]<-(x^2/sqrt(2*pi)*exp(-x^2/2))/(1/(1+cos(1))*sin(x))
  k=k+1
  }
  }
mean(y2)
var(y2)

## ----fig.height4--------------------------------------------------------------
n<-1e5;k<-1;y1<-y3<-numeric(n)
while(k<=n){
  u<- runif(1)
  y1[k]<- 1-log(u,base=exp(1));
  if(y1[k]>=1){
  x<- y1[k];
  y3[k]<-(x^2/sqrt(2*pi)*exp(-x^2/2))/(exp(1-x))
  k=k+1
  }
  }
mean(y3)
var(y3)

## -----------------------------------------------------------------------------
n<-1e4;u<-runif(n,0,10)
x<-(u>1)*u
y4<-mean(x^2/sqrt(2*pi)*exp(-x^2/2))

## -----------------------------------------------------------------------------
library(tictoc)
fastsort <- function(x){
    if(length(x)<=1)return(x)
    x0 <- x[1]
    loc <- 1
    low <- 1
    n <- length(x)
    high <- n
    while(low != high){
        
        if(loc == low){
            if(x[high] < x[loc]){
                tmp <- x[high]
                x[high] <- x[loc]
                x[loc] <- tmp
                low = low+1
                loc = high
            }else{
                    high = high - 1
                }
            }else{
                if(x[low] > x[loc]){
                    tmp <- x[low]
                    x[low] <- x[loc]
                    x[loc] <- tmp
                    high = high -1
                    loc = low
                }else{
                    low = low+1
                }
           }
        }
    L = c()
    R = c()
    if(low>1) L = x[1:(low-1)]
    if(low<length(x)) R = x[(low+1):n]
    return(c(fastsort(L),x[low],fastsort(R)))
}
for(n in c(1e4,2e4,4e4,6e4,8e4)){
 
  tic()
  for(i in seq(1,10)){
    m<-sample(n,replace=FALSE)
    fastsort(m)
  }
  toc()
}

## -----------------------------------------------------------------------------
timq<-c(0.28,0.5,1.07,1.56,2.14);
plot(c(1e4,2e4,4e4,6e4,8e4),c(0.28,0.5,1.07,1.56,2.14),main="calculation time",xlab="n",ylab="time")
x<-c(1e4,2e4,4e4,6e4,8e4);
y<-c(0.28,0.5,1.07,1.56,2.14);
regmodel<-lm(formula=y~x*log(x))
x<-seq(from=1e4,to=1e5)
y1<-predict(regmodel,newdata=list(x=seq(
  from=1e4,to=1e5)))
matlines(x,y1, lwd=1.5)

## -----------------------------------------------------------------------------
library(sn)
    alpha <- -2; beta <- 1 # alternative hypothesis!
    m <- 1e3; n <- 10; set.seed(123)
    beta.hat <- beta.se <- p.val1 <- numeric(m)
    x <- rexp(n)
    for(i in 1:m){
      y <- alpha + beta * x + rsn(n,0,1,0.5)
      coe <- summary(lm(y~x))$coef
      beta.hat[i] <- coe[2,1];beta.se[i] <- coe[2,2]
      p.val1[i] <- coe[2,4] # t-test p-value
    }
   # p.val2 <- 2*(1-pt(abs(beta.hat/beta.se),n-2))
    print(c(mean(p.val1<=0.025),mean(p.val1<=0.05),mean(p.val1<=0.95),mean(p.val1<=0.975)))
   

## -----------------------------------------------------------------------------
xq<-c(0.525,0.657,0.999,0.999);
q<-c(0.025,0.05,0.095,0.0975);
n<-1e3;
varq<-q*(1-q)/(n*xq);
print(varq)

## ----setup, include=FALSE-----------------------------------------------------

n<-1e4;
j<-1;
fwdr1<-fwdr2<-fdr1<-fdr2<-numeric(n);
while(j<=-n){ 
np<-runif(950);
ap<-rbeta(50,0.1,1);
np.adj1<-p.adjust(np,method="bonferroni")
ap.adj1<-p.adjust(ap,method="bonferroni")
np.adj2<-p.adjust(np,method="BH")
ap.adj2<-p.adjust(ap,method="BH")
fwdr11<-fwdr22<fdr11<-fdr22<--1;
for(i1 in seq(0,length(np.adj1))){
  fwdr11<-fwdr11*(1-np.adj1[i])
  fwdr22<-fwdr22*(1-np.adj2[i])
  fdr11<-fdr11*(1-ap.adj1[i])
  fdr22<-fdr22*(1-ap.adj2[i])
}
fwdr1[j]<-fwdr11;
fwdr2[j]<-fwdr22;
fdr1[j]<-fdr11;
fdr2[j]<-fdr22;
j<-j+1;
}
fwdr1<-mean(fwdr1)
fwdr2<-mean(fwdr2)
fdr1<-mean(fdr1)
fdr2<-mean(fdr2)
tpr1<-1-fwdr1
tpr2<-1-fwdr2

## -----------------------------------------------------------------------------
library(bootstrap)
theta.hat<-c(3,5,7,18,43,85,91,98,100,130,230,487)
B<-1000;#large estimate
n<-1e4
blam<-numeric(B)
for (b in 1:B){
  i<-sample(theta.hat,size=n,replace=TRUE);
  blam[b]<-n/sum(i)
}
mse<-sd(blam)
bia<-mean(10/sum(theta.hat)-blam)
print(c(mse,bia))

## -----------------------------------------------------------------------------
library(boot)
theta1.hat<-theta.hat
theta1.boot<-function(dat,ind){
 i<-sample(dat,size=1e4,replace=TRUE);
 result<-sum(i)/1e4
 return(result)
}
boot.obj<-boot(theta1.hat,theta1.boot,R=1000)
boot.ci(boot.obj,type=c("basic","norm","perc","bca"))


## -----------------------------------------------------------------------------
rm(list=ls())
library(bootstrap)
data1<-scor
covd<-cov(data1)
lambdaeig<-eigen(covd,only.values = TRUE)
lambda.hat<-lambdaeig$values[1]/sum(lambdaeig$values)
n<-1e4;
lambda.jack<-numeric(n);
    for(i in 1:n){
      covjack <- cov(data1[(1:88)[-i],1:5])
      jacklam<-eigen(covjack,only.values=TRUE)
      jacklam<-jacklam$values
      lambda.jack[i]<-jacklam[1]/sum(jacklam)
    }
    bias.jack <- (n-1)*(mean(lambda.jack)-lambda.hat)
    se.jack <- sqrt((n-1)*mean((lambda.jack-lambda.hat)^2))
    round(c(original=lambda.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
rm(list=ls())
library(bootstrap)
data1<-scor
covd<-cov(data1)
lambdaeig<-eigen(covd,only.values = TRUE)
lambda.hat<-lambdaeig$values[1]/sum(lambdaeig$values)
n<-1e4;
lambda.jack<-numeric(n);
    for(i in 1:n){
      covjack <- cov(data1[(1:88)[-i],1:5])
      jacklam<-eigen(covjack,only.values=TRUE)
      jacklam<-jacklam$values
      lambda.jack[i]<-jacklam[1]/sum(jacklam)
    }
    bias.jack <- (n-1)*(mean(lambda.jack)-lambda.hat)
    se.jack <- sqrt((n-1)*mean((lambda.jack-lambda.hat)^2))
    round(c(original=lambda.hat,bias.jack=bias.jack,se.jack=se.jack),3)

## -----------------------------------------------------------------------------
rm(list=ls())
a <- seq(10, 40, .1) 
library(DAAG,quietly=TRUE);
attach(ironslag)
L1 <- lm(magnetic ~ chemical)
yhat1 <- L1$coef[1] + L1$coef[2] * a
L2 <- lm(magnetic ~ chemical + I(chemical^2))
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
L3 <- lm(log(magnetic) ~ chemical)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
L4 <- lm(magnetic ~ chemical + I(chemical^2)+I(chemical^3))
yhat4<-L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2+L4$coef[4]*a^3

n <- length(magnetic)   #in DAAG ironslag
 e1 <- e2 <- e3 <- e4 <- numeric(n)
 for (k in 1:n) {
        y <- magnetic[-k]
        x <- chemical[-k]

        J1 <- lm(y ~ x)
        yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
        e1[k] <- magnetic[k] - yhat1

        J2 <- lm(y ~ x + I(x^2))
        yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
                J2$coef[3] * chemical[k]^2
        e2[k] <- magnetic[k] - yhat2

        J3 <- lm(log(y) ~ x)
        logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
        yhat3 <- exp(logyhat3)
        e3[k] <- magnetic[k] - yhat3

        J4 <- lm(y ~ x+I(x^2)+I(x^3))
        yhat4<- J4$coef[1]+J4$coef[2]*chemical[k]+J4$coef[3]*chemical[k]^2+J4$coef[4]*chemical[k]^3
        e4[k] <- magnetic[k] - yhat4
    }

    c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

## ----echo=TRUE----------------------------------------------------------------
rm(list=ls())
library(twosamples)
library(goftest)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

R <- 999;z <- c(x, y);K <- 1:26;n<-length(x)
set.seed(12345); method <- 2
reps <- numeric(R);t0 <- cvm_test(x, y)[1]
for (i in 1:R) {
  if(method==1){
    k <- sample(K, size = n, replace = FALSE)
    x1 <- z[k]; y1 <- z[-k] #complement of x1
  }else{
    xy <- sample(z);
    x1 <- xy[1:n]; y1 <- xy[-(1:n)]
  }
  reps[i] <-cvm_test(x1, y1)[1]
}
p <- mean(abs(c(t0, reps)) >= abs(t0))
round(c(cmvtest=p,cvm_test(x,y)[2]),3)

## -----------------------------------------------------------------------------
rm(list=ls())
library(twosamples)
library(goftest)
library(twosamples)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
R <- 999;z <- c(x, y);K <- 1:26;D<-numeric(R)
options(warn = -1)
D0 <- cvm_test(x, y)[1]
for (i in 1:R) {
  k <- sample(K, size = 14, replace = FALSE)
x1 <- z[k]
y1 <- z[-k]
D[i]<-cvm_test(x1, y1)[1]
}
p<-mean(c(D0,D)>=D0)
options(warn=0)
round(c(cvmtest=p,cvm_test(x,y)[2]),3)

## -----------------------------------------------------------------------------
rm(list=ls())
library(twosamples)
library(goftest)
attach(chickwts)
x <- sort(as.vector(weight[feed == "sunflower"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)

R <- 999;z <- c(x, y);K <- 1:26;n<-length(x);
set.seed(12345); method <- 2
reps <- numeric(R); t0 <- t.test(x, y)$statistic
for (i in 1:R) {
    xy <- sample(z);
    x1 <- xy[1:n]; y1 <- xy[-(1:n)]
  reps[i] <-t.test(x1, y1)$statistic
}
p1 <- mean(abs(c(t0, reps)) >= abs(t0))

reps1 <- numeric(R);t0 <- cor(x, y,method='spearman')
for (i in 1:R) {
  if(method==1){
    k <- sample(K, size = n, replace = FALSE)
    x1 <- z[k]; y1 <- z[-k] #complement of x1
  }else{
    xy <- sample(z);
    x1 <- xy[1:n]; y1 <- xy[-(1:n)]
  }
  reps1[i] <-cor(x1, y1,method='spearman')
}
p2 <- mean(abs(c(t0, reps1)) >= abs(t0))
round(c(p=p1,spearman_p=p2),5)

## -----------------------------------------------------------------------------
set.seed(123)  # 设置随机种子以保证结果可重复

# 定义标准柯西分布的概率密度函数
dcauchy_standard <- function(x) {
  return(1 / (pi * (1 + x^2)))
}

# Metropolis-Hastings算法实现
metropolis_hastings <- function(start, iterations, proposal_sd, burn_in) {
  samples <- numeric(iterations)
  samples[1] <- start
  current_x <- start
  
  for (i in 2:iterations) {
    # 使用正态分布作为建议分布生成候选样本
    proposal_x <- rnorm(1, mean = current_x, sd = proposal_sd)
    
    # 计算接受概率
    acceptance_ratio <- dcauchy_standard(proposal_x) / dcauchy_standard(current_x)
    acceptance_probability <- min(1, acceptance_ratio)
    
    # 接受或拒绝候选样本
    if (runif(1) < acceptance_probability) {
      current_x <- proposal_x
    }
    
    samples[i] <- current_x
  }
  
  # 舍弃前burn_in个样本
  return(samples[(burn_in + 1):iterations])
}

# 使用Metropolis-Hastings算法进行抽样
start_value <- 0  # 初始值
iterations <- 11000  # 总迭代次数，包括烧入期
burn_in <- 1000  # 舍弃的初始样本数
proposal_sd <- 1  # 建议分布的标准差

samples <- metropolis_hastings(start_value, iterations, proposal_sd, burn_in)

# 绘制抽样结果的直方图
hist(samples, breaks = 50, probability = TRUE, main = "Metropolis-Hastings Sampling of Cauchy Distribution",
     xlab = "x", ylab = "Density")
curve(dcauchy_standard(x), add = TRUE, col = "red", lwd = 2)

## -----------------------------------------------------------------------------
# 加载必要的库
library(ggplot2)

# 定义参数
n <- 10  # 二项分布的试验次数
a <- 2   # Beta分布的参数a
b <- 2   # Beta分布的参数b
num_samples <- 10000  # 采样次数
burn_in <- 1000       # 烧入期

# 初始化
x_samples <- numeric(num_samples)
y_samples <- numeric(num_samples)
x_samples[1] <- rbeta(1, a, b) * n  # 初始x值
y_samples[1] <- rbinom(1, n, x_samples[1] / n)  # 初始y值

# 吉布斯采样
for (i in 2:num_samples) {
  # 从条件二项分布中采样y
  y_samples[i] <- rbinom(1, n, x_samples[i-1] / n)
  
  # 从条件Beta分布中采样x
  x_samples[i] <- rbeta(1, y_samples[i] + a, n - y_samples[i] + b) * n
}

# 丢弃烧入期样本
x_samples <- x_samples[-(1:burn_in)]
y_samples <- y_samples[-(1:burn_in)]

# 绘制样本的散点图
df <- data.frame(x = x_samples, y = y_samples)
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(title = "Gibbs Sampling of Bivariate Distribution",
       x = "x", y = "y") +
  theme_minimal()

## -----------------------------------------------------------------------------
# 安装coda包（如果尚未安装）
if (!requireNamespace("coda", quietly = TRUE)) {
  install.packages("coda")
}

# 加载coda包
library(coda)

# 定义参数
n <- 10
a <- 2
b <- 2
num_samples <- 10000
burn_in <- 1000
num_chains <- 3  # 运行的链数量

# 初始化存储链的列表
chains <- vector("list", num_chains)

# 运行多个独立的吉布斯采样链
for (chain in 1:num_chains) {
  x_samples <- numeric(num_samples)
  y_samples <- numeric(num_samples)
  x_samples[1] <- rbeta(1, a, b) * n
  y_samples[1] <- rbinom(1, n, x_samples[1] / n)
  
  for (i in 2:num_samples) {
    y_samples[i] <- rbinom(1, n, x_samples[i-1] / n)
    x_samples[i] <- rbeta(1, y_samples[i] + a, n - y_samples[i] + b) * n
  }
  
  # 丢弃烧入期样本
  x_samples <- x_samples[-(1:burn_in)]
  y_samples <- y_samples[-(1:burn_in)]
  
  # 将链存为mcmc对象
  chains[[chain]] <- mcmc(cbind(x_samples, y_samples))
}

# 计算Gelman-Rubin诊断
gelman_diag <- gelman.diag(mcmc.list(chains), autoburnin = FALSE)

# 输出Gelman-Rubin诊断结果
print(gelman_diag)

# 检查收敛性
if (all(gelman_diag$psrf[, "Point est."] < 1.2)) {
  cat("Chains have converged according to Gelman-Rubin diagnostic.\n")
} else {
  cat("Chains have NOT converged yet. Consider running for more iterations.\n")
}


## -----------------------------------------------------------------------------
# 定义函数
k<-1000;
d<-100;
a<-c(1,2);
a_norm<-sqrt(sum(a^2))
resul<-((-1)^k / factorial(k) / (2^k)*(a_norm^(2 * k + 2)) / ((2 * k + 1) * (2 * k + 2))*gamma((d + 1) / 2) * gamma(k + 3 / 2) / gamma(k + d / 2 + 1))
term1<-(-1)^k / ((2 * k + 1) * (2 * k + 2))
term2 <- exp( (2*k+2)*log(a_norm,base=exp(1))+lgamma((d + 1) / 2)+lgamma(k + 3 / 2)- sum(log(1:k,base=exp(1))) -k*log(2,base=exp(1))-lgamma(k + d / 2 + 1))
resul1<-term1*term2
c(resul,resul1)

## -----------------------------------------------------------------------------
k<-1000;
d<-100;
term3<-numeric(1e3)
for(k in 1:1e3){
term1<-(-1)^k / ((2 * k + 1) * (2 * k + 2))
term2<-exp( (2*k+2)*log(a_norm,base=exp(1))+lgamma((d + 1) / 2)+lgamma(k + 3 / 2)- sum(log(1:k,base=exp(1))) -k*log(2,base=exp(1))-lgamma(k + d / 2 + 1))
term3[k]<-term1*term2
}
sumresul<-sum(term3)
sumresul

## -----------------------------------------------------------------------------
# 加载必要的库
library(stats)

# 定义 k 的值
k <- 4  # 你可以根据需要修改这个值

# 定义 c_k 函数，增加对 a 的限制
c_k <- function(a, k) {
  if (a^2 >= k + 1) {
    return(NA)  # 返回 NA 以避免后续计算出错
  }
  return(sqrt(a^2 * k / (k + 1 - a^2)))
}

# 定义左侧的积分函数
left_integral <- function(a, k) {
  c_k_value <- c_k(a, k)
  if (is.na(c_k_value)) {
    return(NA)  # 如果 c_k_value 是 NA，返回 NA
  }
  
  integrand <- function(u) {
    (1 + u^2 / (k - 1))^(-k / 2)
  }
  integral_value <- integrate(integrand, 0, c_k_value)$value
  return((2 * gamma(k / 2) / sqrt(pi * (k - 1) * gamma((k - 1) / 2))) * integral_value)
}

# 定义右侧的积分函数
right_integral <- function(a, k) {
  c_k_value <- c_k(a, k)
  if (is.na(c_k_value)) {
    return(NA)  # 如果 c_k_value 是 NA，返回 NA
  }
  
  integrand <- function(u) {
    (1 + u^2 / k)^(-(k + 1) / 2)
  }
  integral_value <- integrate(integrand, 0, c_k_value)$value
  return((2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2))) * integral_value)
}

# 定义方程
equation <- function(a, k) {
  left <- left_integral(a, k)
  right <- right_integral(a, k)
  
  if (is.na(left) || is.na(right)) {
    return(NA)  # 如果任一侧是 NA，返回 NA
  }
  
  return(left - right)
}

# 绘制方程的图像以找到适合的范围
a_values <- seq(-sqrt(k + 1)+ 0.01, sqrt(k + 1) - 0.01, length.out = 1000)  # 限制 a 的上限
equation_values <- sapply(a_values, equation, k = k)

plot(a_values, equation_values, type = "l", col = "blue", 
     xlab = "a", ylab = "方程值", main = "方程的图像")
abline(h = 0, col = "red", lty = 2)  # 添加 y=0 的参考线

# 使用 uniroot 函数来找到 a 的解
# 请根据图像选择合适的范围
#a_solution <- uniroot(equation, c(-sqrt(k + 1)+ 0.01, sqrt(k + 1) - 0.01), k = k)  # 你可以根据需要调整范围
#cat("a 的解为:", a_solution$root, "\n")
a_solution <- uniroot(equation, c(0, 0.1), k = k)  # 你可以根据需要调整范围
cat("a 的解为:", a_solution$root, "\n")

## -----------------------------------------------------------------------------
# 观察数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

# E-M 算法
em_algorithm <- function(Y, max_iter = 100, tol = 1e-6) {
  n <- length(Y)
  
  # 初始化 lambda
  lambda <- 1 / mean(Y[Y < 1])  # 基于观察数据的初始猜测
  
  for (iter in 1:max_iter) {
    # E 步：计算期望值
    # 计算给定 Y_i 的 T_i 的期望值
    expected_T <- ifelse(Y < 1, Y, 1)  # 对于 Y_i < 1, T_i = Y_i; 对于 Y_i = 1, T_i 是未知的
    expected_T[Y == 1] <- 1 + (1 / lambda)  # E[T | T > 1] = 1 + 1/lambda
    
    # M 步：更新 lambda
    lambda_new <- n / sum(expected_T)
    
    # 检查收敛性
    if (abs(lambda_new - lambda) < tol) {
      break
    }
    
    lambda <- lambda_new
  }
  
  return(lambda)
}

# 运行 E-M 算法
lambda_estimate <- em_algorithm(Y)
lambda_estimate


## -----------------------------------------------------------------------------
# 计算 lambda 的 MLE
m <- sum(Y == 1)  # 删失观察值的数量
lambda_mle <- length(Y) / (sum(Y[Y < 1]) + m * 1)  # 在 tau = 1 处删失
lambda_mle


## -----------------------------------------------------------------------------
c('E-M'=lambda_estimate,'MLE'=lambda_mle)

## -----------------------------------------------------------------------------

library(lpSolve)
library(knitr)

objective_coefficients <- c(4, 2, 9)

constraint_matrix <- matrix(c(2, 1, 1,
                              1, -1, 3), 
                            nrow = 2, byrow = TRUE)

constraint_rhs <- c(2, 3)

constraint_directions <- c("<=", "<=")

# 解决线性规划问题，默认假设 x, y, z >= 0
lp_solution <- lp("min", objective_coefficients, 
                  constraint_matrix, constraint_directions, 
                  constraint_rhs, compute.sens = TRUE)

optimal_values <- lp_solution$solution
optimal_objective <- lp_solution$objval

# 创建结果数据框
result_table <- data.frame(
  Variables = c("x", "y", "z"),
  Optimal_Values = optimal_values,
  Objective_Value = optimal_objective
)

# 打印结果表格
kable(result_table, caption = "线性规划最优解结果")



## -----------------------------------------------------------------------------
data(mtcars)

model_formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用for循环
model_results_for <- list()
for (index in seq_along(model_formulas)) {
  model <- lm(model_formulas[[index]], data = mtcars)
  model_results_for[[index]] <- model
}

# 使用lapply
model_results_apply <- lapply(model_formulas, function(formula) {
  lm(formula, data = mtcars)
})

for (index in seq_along(model_results_apply)) {
  model <- model_results_apply[[index]]
  cat("Model", index, ":")
  cat("Coefficients:", coef(model), "\n")
  cat("R-squared:", summary(model)$r.squared, "\n")
  cat("p-value:", summary(model)$fstatistic[3], "\n\n")
}


## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

fit_linear_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

model_results <- lapply(bootstraps, fit_linear_model)

coefficients_df <- data.frame(
  Model = 1:length(model_results),
  Intercept = sapply(model_results, function(model) coef(model)[1]),
  Disp_Coefficient = sapply(model_results, function(model) coef(model)[2])
)

print(coefficients_df)


## -----------------------------------------------------------------------------
# 载入数据
data(mtcars)

# 定义模型公式
model_formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 定义提取R²的函数
rsq <- function(mod) {
  summary(mod)$r.squared
}

# 使用lapply拟合模型并提取R²值
model_results_apply <- lapply(model_formulas, function(formula) {
  lm(formula, data = mtcars)
})

# 创建包含模型和R²值的数据框
results_df <- data.frame(
  Model = 1:length(model_results_apply),
  R_squared = sapply(model_results_apply, rsq)
)

# 打印结果
print(results_df)

## -----------------------------------------------------------------------------

data(mtcars)

#bootstrap
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)  
  mtcars[rows, ]
})


fit_linear_model <- function(data) {
  lm(mpg ~ disp, data = data)
}


model_results_for <- list()
for (i in seq_along(bootstraps)) {
  model_results_for[[i]] <- fit_linear_model(bootstraps[[i]])
}

# 使用lapply拟合模型
model_results_lapply <- lapply(bootstraps, fit_linear_model)


rsq <- function(mod) {
  summary(mod)$r.squared
}


r_squared_for <- sapply(model_results_for, rsq)
r_squared_lapply <- sapply(model_results_lapply, rsq)


results_df <- data.frame(
  Bootstrap_Sample = 1:10,
  R_squared_For = r_squared_for,
  R_squared_Lapply = r_squared_lapply
)

print(results_df)



## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
p_values_direct <- sapply(trials, `[[`, "p.value")
c("direct_p_mean"=mean(p_values_direct))

## -----------------------------------------------------------------------------
parallel_vapply <- function(FUN, ..., FUN.VALUE) {
  inputs <- list(...)
  results <- Map(FUN, ...)
  vapply(results, identity, FUN.VALUE)
}

## -----------------------------------------------------------------------------
fast_chisq_test <- function(x, y) {
  

  observed <- table(x, y)
  
  # 期望频率
  row_totals <- rowSums(observed)
  col_totals <- colSums(observed)
  total <- sum(observed)
  expected <- outer(row_totals, col_totals) / total
  
  # 卡方统计量
  chisq_stat <- sum((observed - expected)^2 / expected)
  
  return(chisq_stat)
}

# Example
x <- c(1, 2, 1, 2, 1, 2,1,2,1,1)
y <- c(1, 1, 2, 2, 1, 2,1,1,2,1)
chisq_statistic <- fast_chisq_test(x, y)
print(chisq_statistic)


## -----------------------------------------------------------------------------
fast_count_table <- function(vec1, vec2) {
  # 确保输入是整数向量
  stopifnot(is.integer(vec1), is.integer(vec2))
  
  # 找到唯一值并初始化频数矩阵
  count_matrix <- table(factor(vec1, levels = unique(vec1)), 
                         factor(vec2, levels = unique(vec2)))
  
  return(count_matrix)
}

# 使用快速count_table()加速卡方检验
optimized_chisq_test <- function(vec1, vec2) {
  observed_counts <- fast_count_table(vec1, vec2)
  
  # 计算期望频率
  total_counts <- sum(observed_counts)
  expected_counts <- outer(rowSums(observed_counts), colSums(observed_counts)) / total_counts
  
  # 计算卡方统计量
  chi_square_stat <- sum((observed_counts - expected_counts)^2 / expected_counts)
  
  return(chi_square_stat)
}

# 示例
vector_a <- as.integer(c(1, 2, 1, 2, 1, 2))
vector_b <- as.integer(c(1, 1, 2, 2, 1, 2))
chi_square_result <- optimized_chisq_test(vector_a, vector_b)
print(chi_square_result)


## -----------------------------------------------------------------------------
library(Rcpp)
library(ggplot2)
# 参数设置
n <- 20      # Binomial的试验次数
a <- 2       # Beta分布的参数
b <- 5       # Beta分布的参数
num_samples <- 5000  # 样本数量
burn_in <- 1000      # Burn-in阶段的样本数量

# 运行Gibbs采样
samples <- gibbs_sampler(n, a, b, num_samples, burn_in)

# 转换为数据框
df <- data.frame(x = samples$x_samples, y = samples$y_samples)

# 绘制二维分布的散点图
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, color = "red") +
  theme_minimal() +
  labs(title = "二维分布散点图",
       x = "x (Binomial Samples)",
       y = "y (Beta Samples)")


## -----------------------------------------------------------------------------
# 加载必要的库
library(ggplot2)

# 参数设置
n <- 20      # Binomial的试验次数
a <- 2       # Beta分布的参数
b <- 5       # Beta分布的参数
num_samples <- 5000  # 样本数量
burn_in <- 1000      # Burn-in阶段的样本数量

# 初始化
x_samples <- numeric(num_samples)
y_samples <- numeric(num_samples)
#x_samples[1] <- rbeta(1, a, b) * n  # 初始x值
x_samples[1] <- n/2
y_samples[1] <-0.5
#y_samples[1] <- rbinom(1, n, x_samples[1] / n)  # 初始y值

# 吉布斯采样
for (i in 2:num_samples) {
  # 从条件二项分布中采样y
  y_samples[i] <- rbinom(1, n, x_samples[i-1] / n)
  
  # 从条件Beta分布中采样x
  x_samples[i] <- rbeta(1, y_samples[i] + a, n - y_samples[i] + b) * n
}

# 丢弃烧入期样本
x_samples <- x_samples[-(1:burn_in)]
y_samples <- y_samples[-(1:burn_in)]

# 绘制样本的散点图
df <- data.frame(x = x_samples, y = y_samples)
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(title = "Gibbs Sampling of Bivariate Distribution",
       x = "x", y = "y") +
  theme_minimal()

## -----------------------------------------------------------------------------
# 从Rcpp函数获取样本
samples_rcpp <- gibbs_sampler(n, a, b, num_samples, burn_in)

# 提取Rcpp生成的样本
x_samples_rcpp <- samples_rcpp$x_samples
y_samples_rcpp <- samples_rcpp$y_samples
# 比较x_samples
qqplot(x_samples_rcpp, x_samples, main = "QQ Plot of x_samples",
       xlab = "Rcpp x_samples", ylab = "R x_samples")
abline(0, 1, col = "red")

# 比较y_samples
qqplot(y_samples_rcpp, y_samples, main = "QQ Plot of y_samples",
       xlab = "Rcpp y_samples", ylab = "R y_samples")
abline(0, 1, col = "red")



## -----------------------------------------------------------------------------
# Load necessary libraries
library(Rcpp)
library(microbenchmark)

# Source the Rcpp code
#sourceCpp("path_to_your_rcpp_file.cpp")  # Replace with the actual path to your Rcpp file

# Define the R function for Gibbs sampling
gibbs_sampler_r <- function(n, a, b, num_samples, burn_in) {
  x_samples <- numeric(num_samples)
  y_samples <- numeric(num_samples)
  x_samples[1] <- n / 2
  y_samples[1] <- 0.5
  
  for (i in 2:num_samples) {
    y_samples[i] <- rbinom(1, n, x_samples[i-1] / n)
    x_samples[i] <- rbeta(1, y_samples[i] + a, n - y_samples[i] + b) * n
  }
  
  x_samples <- x_samples[-(1:burn_in)]
  y_samples <- y_samples[-(1:burn_in)]
  
  list(x_samples = x_samples, y_samples = y_samples)
}

# Set parameters
n <- 20
a <- 2
b <- 5
num_samples <- 5000
burn_in <- 1000

# Run microbenchmark
benchmark_results <- microbenchmark(
  Rcpp = gibbs_sampler(n, a, b, num_samples, burn_in),
  R = gibbs_sampler_r(n, a, b, num_samples, burn_in),
  times = 10
)

# Print benchmark results
print(benchmark_results)


