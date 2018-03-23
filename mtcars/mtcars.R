

hist(mtcars$mpg[mtcars$am==0], col = rgb(0,0,1,0.5), xlim = c(10,35), breaks = seq(10,35,5/2), xlab = 'MPG', main = 'MPG Comparison by Transmission')
hist(mtcars$mpg[mtcars$am==1], col = rgb(1,0,0,0.5), xlim = c(10,35), breaks = seq(10,35,5/2), add = T)
abline(v = median(mtcars$mpg[mtcars$am==0]), col='blue')
abline(v = median(mtcars$mpg[mtcars$am==1]), col='red')
legend('topright', c('Automatic','Manual'), col=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)), lwd=10)

hist(mtcars$mpg[mtcars$vs==0], col = rgb(0,0,1,0.5), xlim = c(10,35), breaks = seq(10,35,5/2), xlab = 'MPG', main = 'MPG Comparison by Engine Type')
hist(mtcars$mpg[mtcars$vs==1], col = rgb(1,0,0,0.5), xlim = c(10,35), breaks = seq(10,35,5/2), add = T)
abline(v = median(mtcars$mpg[mtcars$vs==0]), col='blue')
abline(v = median(mtcars$mpg[mtcars$vs==1]), col='red')
legend('topright', c('Straight','V'), col=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)), lwd=10)

xtab = xtabs(data = mtcars[,(c('am','vs'))])
chisq.test(xtab)$p.value

plot(mpg ~ wt, data = mtcars, type = 'n', xlab = 'Weight (1000 lbs)', ylab = 'MPG', main = 'MPG/Weight Comparison by Transmission Type')
points(mpg ~ wt, data = mtcars, subset = am == 0, pch = 19, col = 'blue')
points(mpg ~ wt, data = mtcars, subset = am == 1, pch = 19, col = 'red')
legend('topright',  c('Automatic','Manual'), col = c('blue','red'), pch = 19)

fit = lm(formula = mpg ~ I(wt^-1), data = mtcars)
xdummy = seq(1.5,5.5,0.01)
lines(xdummy,fit$coefficients[1] + fit$coefficients[2]/xdummy, col = 'purple')

mtcut = mtcars[mtcars$wt < 5,]; fit0 = lm(mpg ~ wt, data = mtcut)
fit1 = lm(mpg ~ wt + hp, data = mtcut)
fit2 = lm(mpg ~ wt + hp + factor(am) , data = mtcut)
fit3 = lm(mpg ~ wt + hp + factor(am) + factor(vs), data = mtcut)
fit4 = lm(mpg ~ wt + hp  + factor(am) + factor(vs)+ gear + carb + drat, data = mtcut)
anova(fit0,fit1,fit2,fit3,fit4)


plot(mpg ~ hp, data = mtcars, type = 'n', xlab = 'Horsepower', ylab = 'MPG', main = 'MPG/Horsepower Comparison by Engine Type')
points(mpg ~ hp, data = mtcars, subset = am == 0, pch = 19, col = 'blue')
points(mpg ~ hp, data = mtcars, subset = am == 1, pch = 19, col = 'red')
legend('topright',  c('Automatic','Manual'), col = c('blue','red'), pch = 19)

plot(mtcut$wt, resid(fit0), xlab = 'Weight (1000 lbs)', ylab = 'Residuals on MPG', main = 'Residual plot, linear fit to weight, mpg with weight < 5')
plot(hp ~ cyl, data = mtcars, xlab = 'Number of Cylinders', ylab = 'Horsepower', main = '')
plot(mtcars$hp / mtcars$wt, mtcars$qsec, xlab = 'Horsepower/Weight', ylab = '1/4 Mile Time (sec)', main = '')