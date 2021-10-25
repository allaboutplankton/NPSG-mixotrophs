#script for fitting a type II functional response to ingestion rate data

#use package bbmle for maximum likelihood
library(bbmle)

#plot the data: prey.concentration, ingestion.rate
plot(prey.concentration, ingestion.rate, pch = 21, bg = 'grey80', log = 'yx', xaxt = 'n', yaxt = 'n', xlab = expression(italic(Prochlorococcus)~(cells~mL^-1)), ylab = expression(Ingestion~(cells~grazer^-1~h^-1)), xlim = c(1e4, 5e7), ylim = c(0.04, 120), cex.lab = 0.75)
axis(1, at = c(1e4, 1e5, 1e6, 1e7), labels = c(expression(10^4), expression(10^5), expression(10^6), expression(10^7)), cex.axis = 0.75)
axis(2, at = c(1e-2, 1e-1,1,10, 100, 1000), labels = c(expression(10^-2), expression(10^-1), 1, 10, expression(10^2), expression(10^3)), las = 1, cex.axis = 0.75)

#define the function that returns the negative log-likelihood of the type II functional response model
typeII = function(log.Imax, log.Cmax, sigma) {
    #expected = ingestion rate predicted by the functional response, for a particular set of parameter values
    expected = exp(log.Imax)*(prey.concentration/(prey.concentration+exp(log.Imax)/exp(log.Cmax)))
    #calculate the negative log likelihood for this set of parameter values
    NLL = -sum(dlnorm(ingestion.rate, meanlog = log(expected), sdlog = exp(sigma), log = TRUE))
  }
  
#find the maximum likelihood parameter values using mle2
model2 = mle2(typeII, start = list(log.Imax = log.Imax.guess, log.Cmax = log.Cmax.guess, sigma = log(1)), control = list(maxit = 100000, reltol = 1e-8))

#extract the maximum likelihood parameter values
Imax = exp(coef(model2)[1])
Cmax = exp(coef(model2)[2])
sigma = exp(coef(model2)[3])

#plot the fitted curve
curve(Imax*x/(x + Imax/Cmax), add = TRUE, lwd = 1.5, col = 'black', from = 10^3, to = 10^8)


