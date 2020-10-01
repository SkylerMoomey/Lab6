# task 2

?layout

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

#normal curve 2, mean = 4, standard distribution = 2
curve(dnorm(x, 4, 2), xlim=c(-10, 10))
xvals = seq(1, 5, length = 1000)
yvals = dnorm(xvals, 4, 2)
polygon(c(1, xvals, 5), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(4, 2), P(1 < Y < 5)", col="Dark blue"))

#normal curve 3, mean = 10, sd = 4, P(Y < 10)
curve(dnorm(x, 10, 4), xlim=c(-22, 22))
xvals = seq(-22, 10, length = 1000)
yvals = dnorm(xvals, 10, 4)
polygon(c(-22, xvals, 10), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(10, 4), P(Y < 10)", col="Dark blue"))

#normal curve 4, mean = -2, sd = 0.5, P(-3 < Y < -2)
curve(dnorm(x, -2, 0.5), xlim=c(-6, 4))
xvals = seq(-3, -2, length = 1000)
yvals = dnorm(xvals, -2, 0.5)
polygon(c(-3, xvals, -2), c(0, yvals, 0), col="Red")
title(main = list("Y~Norm(-2, 0.5), P(-3 < Y < -2)", col="Dark blue"))