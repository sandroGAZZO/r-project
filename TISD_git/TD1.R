##################################
# Une introduction au logiciel R #
##################################

###  1.1.1  ##########################################################

a<-c(10,5,3,6,21)
a
a[2]

a[1,3]

b<-array(data=c(15,3,12,2,1), dim=c(1,5))
b
b[2]
b[1,3]

nrow(a)
ncol(a)
dim(a)

nrow(b)
ncol(b)
dim(b)

c<-array(data=c(1), dim=c(1,5))

d<- seq(from=1, to=10, by=2)
d

1:5

e<- array(data=c(1:5), dim=c(1,5))
e

diag(c(1,5,9))
 
###  1.1.2  ##########################################################

2*a+b+1
e[3]
cos(a)
exp(a)
a*e
a%*%e
b%*%e

f<-t(b)%*%e
f
dim(f)
f[2,3]
f[,3]
f[2:5,]
f[2:3,4]

cbind(b,e)
rbind(b,e)
cbind(a,b)
rbind(a,b)

nrow(rbind(a,b))
ncol(rbind(a,b))



c(a,b)
nrow(c(a,b))
ncol(c(a,b))

c(b,b)
nrow(c(b,b))
ncol(c(b,b))

array(data=c(b,b),dim=c(1,2*ncol(b)))
###  1.2  ##########################################################

Lst<-list(name="Fred", wife="Mary", no.children=3, child.ages=c(9,7,4))
Lst

Lst[[1]]
Lst$name

Lst[[4]]
Lst[[4]][3]

Lst$child.ages[3]


##################################
# Exercice 1                     #
##################################

A<-array(data=c(1,0,0,0,1,3,0,2,1),dim=c(3,3))
A
 
b<-array(data=c(1,2,1), dim=c(3,1))

det(A)

sol<-solve(A,b)
sol

A%*%sol

decomposition<-eigen(A, symmetric=FALSE, only.values=FALSE)
decomposition

D<-diag(decomposition$values)
D
P<-decomposition$vectors
P
Pinv<-solve(P)
Pinv
Pinv%*%P
P%*%Pinv

P%*%D%*%Pinv

##################################
# Combinatoire avec R            #
##################################

#Exercice 2

choose(13,3)
factorial(13)/(factorial(3)*factorial(10))

choose(9,3)

#Exercice 3

Amis<-c("Bertrand", "Jean", "Marc", "Marie", "Ouassila", "Radu")
combn(Amis,3) 

ncol(combn(Amis,3))
choose(6,3)



rm(list=ls(all=TRUE))
##################################
# Exo 4 Lois de Bernoulli        #
##################################

#PARTIE A


x<-array(data=c(0:5), dim=c(1,6))

y<-array(data=c(0), dim=c(5,6))
for(j in 1:6)
{
	for(i in 1:5)#n dans la binomiale
	{
		if(j<=i+1)
		{
			y[i,j]=choose(i,j-1)*0.4^(j-1)*0.6^(i-j+1)
			#y[i,j]=dbinom(j-1,i,0.4)
		}
	}
}

plot(x,y[1,])

plot(x,y[1,], type="h", col="red")
for(i in 2:5)
{
points(x+i*0.03,y[i,], type="h")
}



x<-array(data=c(-1:5),dim=c(1,7))


z<-array(data=c(0), dim=c(2,7))
for(j in 2:7)
{
	z[1,j]=sum(y[1,1:(j-1)])
	z[2,j]=sum(y[5,1:(j-1)])
}
plot(x,z[1,],type="s", col="red", xlim=c(-1,5), ylim=c(0,1))
lines(x,z[2,], type="s", col="blue")


#PARTIE B

#2
hist(rbinom(n=3000, size=1000, prob=0.5)/1000)

x<-seq(from=0, to=1, by=0.001)
y<-1-pbinom(1000/2, size=1000, prob=x)
plot(x,y, type='l')

#
y<-array(data=c(0), dim=c(1,1001))
for(i in 1:1001)
{
	p=(i-1)/1000
	y[i]=1-pbinom(1000/2, size=1000, prob=p)
}
plot(x,y, type='l')
 
#3

x<-seq(from=0, to=1, by=0.001)
z=pbinom(1000*(x-0.01), size=1000, prob=x)+1-pbinom(1000*(x+0.01), size=1000, prob=x)
plot(x,z,type="l")

#
z<-array(data=c(0), dim=c(1,1001))
for(i in 1:1001)
{
	p=(i-1)/1000
	z[i]=pbinom(1000*(p-0.01), size=1000, prob=p)+1-pbinom(1000*(p+0.01), size=1000, prob=p)
}
plot(x,z, type='l')
min(z)
max(z)

z[11]
z[501]
z[751]

##############
z<-array(data=c(0), dim=c(1,1001))
for(i in 1:1001)
{
	p=(i-1)/1000
	z[i]=pbinom(1000*(p+0.01), size=1000, prob=p)-pbinom(1000*(p-0.01), size=1000, prob=p)
}
plot(x,z, type='l',ylim=c(0,1))
min(z)
max(z)

z[11]
z[501]
z[751]


#4

1/(4*0.01^2*0.05)

0.51-sqrt(0.51*0.49/1000)*qnorm(0.975,0,1)
0.51+sqrt(0.51*0.49/1000)*qnorm(0.975,0,1)

# pour que la largeur de l'intervalle soit <0.01

(qnorm(0.975,0,1))^2*0.49*0.51/(0.01^2)

##################################
# Simulation de fractales        #
##################################

#spirale
n<-1000

a1<-array(data=c(0.839,  0.383,-0.303, 0.924),dim=c(2,2))
a2<-array(data=c(-0.161,  0.138,-0.136, -0.182),dim=c(2,2))
b1<-array(data=c(0.232,  -0.08),dim=c(2,1))
b2<-array(data=c(0.921,  0.178),dim=c(2,1))

simul<-array(data=c(0), dim=c(2,n))
decision<-rbinom(n,1,0.1)

simul[1,1]=0
simul[2,1]=0

for(i in 1:(n-1))
{
	if(decision[i]==0)
	{
		simul[,i+1]=a1%*%simul[,i]+b1
	}
	else
	{
		simul[,i+1]=a2%*%simul[,i]+b2
	}
	plot(t(simul[,1:i]), type='p', pch=21, xlim=c(-0.1, 1), ylim=c(-0.1, 1.1))
	points(simul[1,i+1],simul[2,i+1], pch=19, col='red')
}
###############################################################################
#feuille
n<-2000

a1<-array(data=c(0,  0,0, 0.16),dim=c(2,2))
a2<-array(data=c(0.2,0.23, -0.26, 0.22),dim=c(2,2))
a3<-array(data=c(-0.15, 0.26, 0.28, 0.24),dim=c(2,2))
a4<-array(data=c(0.85, -0.04, 0.04, 0.85),dim=c(2,2))

b1<-array(data=c(0,  0),dim=c(2,1))
b2<-array(data=c(0,  1.6),dim=c(2,1))
b3<-array(data=c(0,  0.44),dim=c(2,1))
b4<-array(data=c(0,  1.6),dim=c(2,1))


simul<-array(data=c(0), dim=c(2,n))
decision<-runif(n,0,1)

simul[1,1]=0
simul[2,1]=0

for(i in 1:(n-1))
{
	if(decision[i]<0.1)
	{
		simul[,i+1]=a1%*%simul[,i]+b1
	}
	if(0.1<=decision[i] && decision[i] <0.25)
	{
		simul[,i+1]=a2%*%simul[,i]+b2
	}
	if(0.25<=decision[i] && decision[i]<0.4)
	{
		simul[,i+1]=a3%*%simul[,i]+b3
	}
	if(0.4<=decision[i])
	{
		simul[,i+1]=a4%*%simul[,i]+b4
	}
	plot(t(simul[,1:i]), type='p', pch='.', xlim=c(-5, 5), ylim=c(-1, 10))
	points(simul[1,i+1],simul[2,i+1], pch=19, col='red')
}

#############################################
# Poisson, Géométrique, Binômiale négative  #
#############################################

# Poisson

x<-array(data=c(0:9), dim=c(1,10))
y=dpois(x,0.5)
plot(x,y, type="h", col="red")

y=dpois(x,2)
points(x+0.08,y, type="h", col="blue")

y=dpois(x,4)
points(x+0.1,y, type="h", col="orange")


n=1000
test<-array(data=c(0), dim=c(1,n))
for(i in 1:n)
{
	test[i]=rpois(1,500)+rpois(1,300)
}
qqplot(test, rpois(n,800))

# Géométrique

x<-array(data=c(0:19), dim=c(1,20))
y=dgeom(x,0.5)
plot(x,y, type="h", col="red")

y=dgeom(x,1/3)
points(x+0.08,y, type="h", col="blue")

# Binômiales négatives

x<-array(data=c(0:20), dim=c(1,21))
y=dnbinom(x,size=1000, prob=1000/1001)
plot(x,y, type="h", col="red")

#############################################
# Loi multinômiale                          #
#############################################

n=100
N1=480
N2=450
N=1000
n1=0
n2=0
n3=0
for(i in 1:n)
{	
	u=runif(1,0,1)
	if(u<N1/N)
	{
		n1=n1+1
	}
	else if((N1/N<=u) && (u<(N1+N2)/N))
	{
		n2=n2+1
	}
	else #if(N2/N<=u)
	{
		n3=n3+1
	}
}
c(n1,n2,n3)

tiragehyper<-array(data=c(0), dim=c(2,100))
tiragehyper[1,]<-rhyper(100, m=480, n=450, k=100)
tiragehyper[2,]=100-tiragehyper[1,]

#############################################
# Lois uniformes et beta                    #
#############################################

# Beta

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,0.8, 0.6)
plot(x,y, type='l', xlim=c(0,1), ylim=c(0,3))

hist(rbeta(100000,0.8,0.9))

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,0.8, 2.6)
lines(x,y, type='l', col="red")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,1, 2.6)
lines(x,y, type='l', col="orange")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,0.8, 1.3)
lines(x,y, type='l', col="magenta")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,1, 1)
lines(x,y, type='l', col="blue")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,1.6, 0.5)
lines(x,y, type='l', col="green")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,1.6, 1.5)
lines(x,y, type='l', col="cyan")

x<-array(data=seq(from=0, to=1, by=0.001), dim=c(1,1001))
y=dbeta(x,4.6, 2.5)
lines(x,y, type='l', col="black")



# test du générateur uniforme de R

hist(runif(10000,0,1))

sim=runif(10000, 0,1)
sim2=floor(10*sim)
nsim=table(sim2)/10000
x<-0:9
plot(x,nsim, type='h', ylim=c(0,0.2))


x=runif(10000,0,1)
plot(x[1:9999],x[2:10000], pch='.')

cor(x[1:9999],x[2:10000])

#############################################
# Lois normales                             #
#############################################

x<-array(data=seq(-10,10,0.01))
y=dnorm(x,0,1)
plot(x,y, type='l')
y=dnorm(x,1,1)
lines(x,y,type='l', col="blue")
y=dnorm(x,mean=1,sd=4)
lines(x,y,type='l', col="red")

qnorm(0.025, 0,1)
qnorm(0.975, 0,1)
qnorm(0.25, 0,1)
qnorm(0.75, 0,1)
qnorm(0.005, 0,1)
qnorm(0.995, 0,1)

x<-rnorm(1000, mean=5,sd=sqrt(100))
y<-(x-mean(x))/sqrt(var(x))
qqplot(y,rnorm(1000,0,1))

mean(y)
var(y)


X<-array(data=rnorm(20000,0,1),dim=c(10000,2))
rho=-1
A<-array(data=c(1,rho,rho,1),dim=c(2,2))
plot(X%*%A)
cor(X%*%A[,1],X%*%A[,2])


abline(a=0,b=0.4,col='red')


#############################################
# Lois exponentielles et Gamma              #
#############################################

# exponentielles 

x<-seq(0,10,0.001)
y=dexp(x,0.5)
plot(x,y,type='l', col="red")
y=dexp(x,2)
lines(x,y,type='l', col="blue")

x<-seq(0,1,0.001)
y=pexp(x,0.5)
plot(x,y,type='l', col="red", ylim=c(0,1))
y=pexp(x,2)
lines(x,y,type='l', col="blue")

y=qexp(x,0.5)
lines(x,y,type='l', col="red")
y=qexp(x,2)
lines(x,y,type='l', col="blue")
lines(x,x, type='l', col="black")


val=c(seq(from = 1000, to=10000, by =500))
res=array(data=c(0), dim=c(2,19))

for(j in 1:19)
{
n=val[j]
x=rexp(n, 2)
plusgds=0
plusgdst=0
plusgdt=0
for(i in 1:n)
{
	if(x[i]>1) plusgds=plusgds+1
	if(x[i]>1.4) plusgdst=plusgdst+1
	if(x[i]>0.4) plusgdt=plusgdt+1
}
res[1,j]=plusgdst/plusgds
res[2,j]=plusgdt/n
}
plot(val,res[1,],type='l')
lines(val, res[2,], type='l')

# Gamma

x<-seq(0,1.5,0.001)
y=dgamma(x,0.5,1)
plot(x,y,type='l', col="red", ylim=c(0,5))
y=dgamma(x,1,1)
lines(x,y,type='l', col="blue")
y=dgamma(x,2,1)
lines(x,y,type='l', col="orange")
y=dgamma(x,2,2)
lines(x,y,type='l', col="magenta")

qqplot(rexp(1000,2)+rexp(1000,2)+rexp(1000,2),rgamma(1000,3,6))


#############################################
# Lois de Weibull                           #
#############################################

x<-seq(0,1.5,0.001)
y=dweibull(x,0.5,1)
plot(x,y,type='l', col="red", ylim=c(0,2))
y=dweibull(x,1,1)
lines(x,y,type='l', col="blue")
y=dweibull(x,1,2)
lines(x,y,type='l', col="orange")
y=dweibull(x,2,1)
lines(x,y,type='l', col="magenta")

x<-seq(0,1.5,0.001)
y=pweibull(x,0.5,1)
plot(x,y,type='l', col="red", ylim=c(0,1))
y=pweibull(x,1,1)
lines(x,y,type='l', col="blue")
y=pweibull(x,1,2)
lines(x,y,type='l', col="orange")
y=pweibull(x,2,1)
lines(x,y,type='l', col="magenta")

lines(x,x,type='l', col="black")
y=qweibull(x,1,2)
lines(x,y,type='l', col="orange")


##################Loi du Chi2
x<-seq(0,1.5,0.001)
y=dchisq(x,2)
plot(x,y,type='l', col="red")

