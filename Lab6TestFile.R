# task 2


#create layout matrix to hold graphs
layout_mat = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
layout(layout_mat)

#plot all density functions outlined in write-up
curve(dnorm(x, 10, 4), xlim=c(-20,20))
title(main="Mean=10, Standard Deviation=4")

curve(dnorm(x, 10, 2), xlim=c(-16,16))
title(main="Mean=10, Standard Deviation=2")

curve(dnorm(x, 5, 10), xlim=c(-30, 30))
title(main="Mean=5, Standard Deviation=10")

curve(dnorm(x, 5, 1/2), xlim=c(-7,7))
title(main="Mean=5, Standard Deviation=0.5")

#***********************


#new graphical layout
layout(layout_mat)

#normal curve
curve(dnorm(x, 0, 1), xlim=c(-3, 3.5))

#want to graph Y~Norm(0, 1), P(Y >= 2)
#this is equivalent to 1 - pnorm(2, 0, 1)

#find 1000 x-vals between 2-4 and corresponding yvalues
xvals = seq(2, 4, length = 1000)
yvals = dnorm(xvals, 0, 1)

#create polygon out of those values
polygon(c(2, xvals, 4), c(0, yvals, 0), col = "Red")

#embellish
title(main = list("Y~Norm(0, 1), P(Y > 2)", col="Dark blue"))

#probability, lower-tail
prob0 = round(1-pnorm(2), 4)
loc0 <- locator(1)
loc0
text(loc0, paste("Area = ", prob0, sep=""))

#*********************************


#normal curve 2, mean = 4, standard distribution = 2
curve(dnorm(x, 4, 2), xlim=c(-10, 10))
xvals = seq(1, 5, length = 1000)
yvals = dnorm(xvals, 4, 2)
polygon(c(1, xvals, 5), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(4, 2), P(1 < Y < 5)", col="Dark blue"))

#probability
prob1 = round((pnorm(5, 4, 2) - pnorm(2, 4, 2)), 4)
loc1 <- locator(1)
loc1
text(loc1, paste("Area = ", prob1, sep=""))

#******************************************


#normal curve 3, mean = 10, sd = 4, P(Y < 10)
curve(dnorm(x, 10, 4), xlim=c(-22, 22))
xvals = seq(-22, 10, length = 1000)
yvals = dnorm(xvals, 10, 4)
polygon(c(-22, xvals, 10), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(10, 4), P(Y < 10)", col="Dark blue"))

#probability
prob2 = round(pnorm(10, 10, 4), 4)

loc2 <- locator(1)
loc2
text(loc2, paste("Area = ", prob2, sep=""))

#***************************


#normal curve 4, mean = -2, sd = 0.5, P(-3 < Y < -2)
curve(dnorm(x, -2, 0.5), xlim=c(-6, 4))
xvals = seq(-3, -2, length = 1000)
yvals = dnorm(xvals, -2, 0.5)
polygon(c(-3, xvals, -2), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(-2, 0.5), P(-3 < Y < -2)", col="Dark blue"))

#probability
prob3 = round((pnorm(-2, -2, 0.5) - pnorm(-3, -2, 0.5)), 4)

loc3 <- locator(1)
loc3
text(loc3, paste("Area = ", prob3, sep=""))


#********** Gamma *****************

#Task 3
layout(1)

#Y∼Gamma(shape=1,scale=1)
curve(dgamma(x, 1, 1), main="Y~Gamma(a=1, 3, 5; b=1)", xlim=c(0, 10))
title(col="blue")

#Y∼Gamma(shape=3,scale=1)
curve(dgamma(x, 3, 1), xlim=c(0, 10), add=TRUE)


#Y∼Gamma(shape=5,scale=1)
curve(dgamma(x, 5, 1),  xlim=c(0, 10), add=TRUE)

#******* Gamma with Probabilities *******

#Y~Gamma(shape=3,scale=2),P(2<Y<5)

#shape * scale_squared = variance
#let xlim = alpha + sqrt(3*variance)
alpha=3; beta_sq=4; xupper=alpha + 3*sqrt(alpha*beta_sq)

curve(dgamma(x, shape=3, rate=1/2), xlim=c(0, xupper))
xvals = seq(2, 5, length = 1000)
yvals = dgamma(xvals, 3, 1/2)
polygon(c(2, xvals, 5), c(0, yvals, 0), col="pink")
title(main = list("Y~Gamma(Shape=3, Scale=2), P(2 < Y < 5)", col="violetred3"))

#probability
prob0 = round(pgamma(5, 3, rate=1/2) - pgamma(2, 3, rate=1/2), 4)
loc0 = locator(1)
loc0
text(loc0, paste("Area = ", prob0, sep= ""))


#Y∼Gamma(shape=6,scale=3),P(1≤Y≤4)

#shape * scale_squared = variance
#let xlim = sqrt(3*sd)
alpha=6; beta_sq=9; xupper=alpha + 3*sqrt(alpha*beta_sq)

curve(dgamma(x, shape=6, rate=1/3), xlim=c(0, xupper))
xvals = seq(1, 4, length = 1000)
yvals = dgamma(xvals, 6, rate=1/3)
polygon(c(1, xvals, 4), c(0, yvals, 0), col="palegreen1")
title(main = list("Y~Gamma(Shape=6, Scale=3), P(1 < Y < 4)", col="palegreen4"))

#probability
prob1 = round(pgamma(4, 6, rate=1/3) - pgamma(1, 6, rate=1/3), 4)
loc2 = locator(1)
loc2
text(loc2, paste("Area = ", prob1, sep= ""))

#Y∼Gamma(shape=2,scale=4),P(3≤Y<6)

#shape * scale_squared = variance
#let xlim = sqrt(3*sd)
alpha=2; beta_sq=16; xupper=alpha + 3*sqrt(alpha*beta_sq)

curve(dgamma(x, shape=2, rate=1/4), xlim=c(0, xupper))
xvals = seq(3, 6, length = 1000)
yvals = dgamma(xvals, 2, rate=1/4)
polygon(c(3, xvals, 6), c(0, yvals, 0), col="sienna1")
title(main = list("Y~Gamma(Shape=2, Scale=4), P(3 < Y < 6)", col="sienna2"))

#probability
prob1 = round(pgamma(6, 2, rate=1/4) - pgamma(3, 2, rate=1/4), 4)
loc3 = locator(1)
loc3
text(loc3, paste("Area = ", prob1, sep= ""))

#****** Chi-Square ******

layout(layout_mat)

#Y∼chisq(df=1)
#nu = 1, mu = 1, variance = 2mu
nu = 1
mu = nu
var = 2*nu
xupper = mu + 3*sqrt(var)
curve(dchisq(x, df=nu), xlim=c(0, xupper))
title(main=list("Y~Chisq(Degrees of Freedom = 1)", col = "red2"))


#Y∼chisq(df=2)
nu = 2
mu = nu
var = 2*nu
xupper = mu + 3*sqrt(var)
curve(dchisq(x, df=nu), xlim=c(0, xupper))
title(main=list("Y~Chisq(Degrees of Freedom = 2)", col = "magenta3"))


#Y∼chisq(df=4)
nu = 4
mu = nu
var = 2*nu
xupper = mu + 3*sqrt(var)
curve(dchisq(x, df=nu), xlim=c(0, xupper))
title(main=list("Y~Chisq(Degrees of Freedom = 4)", col = "indianred3"))


#Y∼chisq(df=20)
nu = 20
mu = nu
var = 2*nu
xupper = mu + 3*sqrt(var)
curve(dchisq(x, df=nu), xlim=c(0, xupper))
title(main=list("Y~Chisq(Degrees of Freedom = 20)", col = "olivedrab3"))

#Y∼chisq(df=2),P(2≤Y≤4)
nu = 2
mu = nu
var = 2*nu
xupper=mu + 3*sqrt(var)

x1=2; x2=4

curve(dchisq(x, nu), xlim=c(0, xupper))
xvals = seq(x1, x2, length = 1000)
yvals = dchisq(xvals, nu)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="sienna1")
title(main = list("Y~Chisq(DoF = 2), P(2 < Y < 4)", col="sienna2"))

#probability
prob1 = round(pchisq(x2, nu) - pchisq(x1, nu), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob1, sep= ""))



#Y∼chisq(df=3),P(3≤Y≤5)
nu = 3
mu = nu
var = 2*nu
xupper=mu + 3*sqrt(var)

x1=3; x2=5

curve(dchisq(x, nu), xlim=c(0, xupper))
xvals = seq(x1, x2, length = 1000)
yvals = dchisq(xvals, nu)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="pink")
title(main = list("Y~Chisq(DoF = 3), P(3 < Y < 5)", col="violetred3"))

#probability
prob1 = round(pchisq(x2, nu) - pchisq(x1, nu), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob1, sep= ""))



#Y∼chisq(df=20),P(10<Y≤21) 
nu = 20
mu = nu
var = 2*nu
xupper=mu + 3*sqrt(var)

x1=10; x2=21

curve(dchisq(x, nu), xlim=c(0, xupper))
xvals = seq(x1, x2, length = 1000)
yvals = dchisq(xvals, nu)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="palegreen1")
title(main = list("Y~Chisq(DoF = 3), P(3 < Y < 5)", col="palegreen3"))

#probability
prob1 = round(pchisq(x2, nu) - pchisq(x1, nu), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob1, sep= ""))


#************ Weibull ************


layout(layout_mat)

#weibull density distributions, two parameters: alpha=shape, beta=scale
#in r, two parameters are a= alpha, b = beta^(1/alpha)

#Y~Weib(shape=2, scale=1)
alpha = 2.0
beta = 1.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))
title(main = list("Y~Weibull(Shape = 2.0, Scale = 1.0)", col="red2"))


#Y~Weib(shape=4, scale=1)
alpha = 4.0
beta = 1.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))
title(main = list("Y~Weibull(Shape = 4.0, Scale = 1.0)", col="magenta3"))

#Y~Weib(shape=2, scale=2)
alpha = 2.0
beta = 2.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))
title(main = list("Y~Weibull(Shape = 4.0, Scale = 1.0)", col="indianred2"))

#Y~Weib(shape=1, scale=5)
alpha = 1.0
beta = 5.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))
title(main = list("Y~Weibull(Shape = 1.0, Scale = 5.0)~Exp(Scale = 5.0)", col="olivedrab3"))


#Weibull distribution with probability shading
layout(1)

#Y~Weib(shape=2, scale=5), P(2 < Y < 5)
alpha = 2.0
beta = 5.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

x1 = 2
x2 = 5

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))

xvals = seq(x1, x2, length = 1000)
yvals = dweibull(xvals, alpha, beta)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="sienna1")

title(main = list("Y~Weibull(Shape = 2.0, Scale = 5.0), P(2<Y<5)", col="sienna2"))

#probability
prob1 = round(pweibull(x2, alpha, beta) - pweibull(x1, alpha, beta), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob1, sep= ""))

#Y~Weib(shape=4, scale=3), P(3 < Y < 4)
alpha = 4.0
beta = 3.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

x1 = 3
x2 = 4

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper + 5))

xvals = seq(x1, x2, length = 1000)
yvals = dweibull(xvals, alpha, beta)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="pink")

title(main = list("Y~Weibull(Shape = 4.0, Scale = 3.0), P(3<Y<4)", col="violetred3"))

#probability
prob1 = round(pweibull(x2, alpha, beta) - pweibull(x1, alpha, beta), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob1, sep= ""))

#Y~Weib(shape=5, scale=1), P(.5 < Y < 2)
alpha = 5.0
beta = 1.0

b = beta^(1/alpha)

mu = (b)*gamma((alpha+1)/alpha)
var = (b^2)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)

x1 = 0.5
x2 = 2.0

xupper = mu + 3*sqrt(var)
curve(dweibull(x, alpha, scale=beta), xlim=c(0, xupper))

xvals = seq(x1, x2, length = 1000)
yvals = dweibull(xvals, alpha, beta)
polygon(c(x1, xvals, x2), c(0, yvals, 0), col="palegreen1")

title(main = list("Y~Weibull(Shape = 5.0, Scale = 1.0), P(0.5<Y<2)", col="palegreen3"))

#probability
prob = round(pweibull(x2, alpha, beta) - pweibull(x1, alpha, beta), 4)
loc = locator(1)
loc
text(loc, paste("Area = ", prob, sep= ""))