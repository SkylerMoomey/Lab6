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
