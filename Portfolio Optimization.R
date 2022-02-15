data <-read.csv("E:\\Projects\\waleed\\Stock.csv")
data$Date=as.Date(data$Date)

data$INTC<-as.xts(x = data$INTC, order.by = data$Date)
data$VZ<-as.xts(x = data$VZ, order.by = data$Date)
data$AMZN<- as.xts(x = data$AMZN, order.by = data$Date)
class(data$INTC)

data.1<- na.omit(merge(data$INTC,data$VZ,data$AMZN)) #combining stock returns
Return <- na.omit(diff(log(data.1)) * 100)

mean.r <- apply(Return, 2 , mean) # mean of stock returns

cov.r <- cov(Return) #Covariance of stock returns
sd.r <- sqrt(diag(cov.r)) #standard deviation of stock returns

Amat<-cbind(rep(1,3),mean.r) #equality contraint vector
mu.P <- seq(min(mean.r * 0.75), max(mean.r * 1.25), length.out = 500)##lenght of possible 300 target portfolio return
sigma.p <- mu.P #stdev of portfolio return
weights<- matrix(0, nrow = 500, ncol = ncol(Return))# storage for portfolio weights
colnames(weights) <- colnames(Return)

for (i in 1:length(mu.P)) {
  bvec = c(1, mu.P[i])
  result = solve.QP(Dmat = 2 * cov.r,
                    dvec = rep(0, 3), Amat = Amat,
                    bvec = bvec, meq = 2)
  sigma.p[i] = sqrt(result$value)
  weights[i, ] = result$solution
}
par(mfrow = c(1,1))
plot(sigma.p, mu.P, type = "l", xlim = c(0, max(sd.r)* 1.1), ylim = c(0, max(mean.r)*1.1), lty = 3, lwd =3)#plot
mu.free = 0.02
points(0, mu.free, cex = 1, pch = "+")

sharpe = (mu.P-mu.free)/sigma.p ##compute sharpe ratios
ind = (sharpe == max(sharpe)) ## find max sharpe
options(digits = 3)
lines(c(0,2), mu.free + c(0,2)* (mu.P[ind] - mu.free)/sigma.p[ind], lwd = 4, lty = 1,
      col = "blue")
##show line of optimal portfolios
points(sigma.p[ind], mu.P[ind], cex = 4, pch = "*")# show tandancy port folio 

ind2 = (sigma.p == min(sigma.p)) #find minimum variance portfolio.
points(sigma.p[ind2], mu.P[ind2], cex = 2, pch = "+") # show min var portfolio

ind3 =(mu.P>mu.P[ind2]) #efficient frontier
lines(sigma.p[ind3], mu.P[ind3], type = "l",
      xlim = c(0,max(sd.r)*1.1), ylim = c(min(mean.r)*1.05, max(mean.r)*1.1),
      lwd = 3, col = "red") # plot the efficient frontier
text(sd.r[1], mean.r[1], "INTC", cex = 1.15)
text(sd.r[2], mean.r[2], "VZ", cex = 1.15)
text(sd.r[3], mean.r[3], "AMZN", cex = 1.15)


##under short sale constrainst
Amat<-cbind(rep(1,3),mean.r,diag(1, nrow = 3)) #equality contraint vector
mu.P <- seq(min(mean.r + 0.0001), max(mean.r - 0.0001), length.out = 300)##lenght of possible 300 target portfolio return
sigma.p <- mu.P #stdev of portfolio return
weights<- matrix(0, nrow = 300, ncol = 3)# storage for portfolio weights


for (i in 1:length(mu.P)) {
  bvec = c(1, mu.P[i], rep(0,3))
  result = solve.QP(Dmat = 2 * cov.r,
                    dvec = rep(0, 3), Amat = Amat,
                    bvec = bvec, meq = 2)
  sigma.p[i] = sqrt(result$value)
  weights[i, ] = result$solution
}
par(mfrow = c(1,1))
plot(sigma.p, mu.P, type = "l", xlim = c(0, max(sd.r)* 1.1), ylim = c(0, max(mean.r)*1.1), lty = 3, lwd =3)#plot
mu.free = 0.02
points(0, mu.free, cex = 1, pch = "+")

sharpe = (mu.P-mu.free)/sigma.p ##compute sharpe ratios
max(sharpe)












#sharpe ratio under no sale constraint
Amat<-cbind(rep(1,3),mean.r) # set the equality constraint matrix
bvec = c(1, 0.08) #constraint vector
result = solve.QP(Dmat = 2 * cov.r,
                           dvec = rep(0, 3), Amat = Amat,
                           bvec = bvec, meq = 2)
sigma.p = sqrt(result$value)
weights = result$solution
mu.free = 0.02
sharpe = (0.08 - mu.free)/sigma.p ##compute sharpe ratios at target returns
print(sharpe)


#sharpe ratio under sale constraint
Amat<-cbind(rep(1,3),mean.r, diag(1, nrow = 3)) # set the equality and inequality constraint matrix
bvec = c(1, 0.08, rep(0,3)) #constraint vector with no short position
result = solve.QP(Dmat = 2 * cov.r,
                  dvec = rep(0, 3), Amat = Amat,
                  bvec = bvec, meq = 2)
sigma.p = sqrt(result$value)
weights = result$solution
mu.free = 0.02
sharpe = (0.08 - mu.free)/sigma.p ##compute sharpe ratios at target returns
print(sharpe)



#	Run three regressions 
choose.files()
E_data<- read.csv("E:\\Projects\\waleed\\Event Study.csv")
E_data$Date=as.Date(E_data$Date) #indicate R about date
#Convert data in to xts object
E_data$Return<-as.xts(x = E_data$Return, order.by = E_data$Date)
E_data$total.change<-as.xts(x = E_data$total.change, order.by = E_data$Date)
E_data$surprise<-as.xts(x = E_data$surprise, order.by = E_data$Date)
E_data$expected<-as.xts(x = E_data$expected, order.by = E_data$Date)
E_data$Scheduled<-as.xts(x = E_data$Scheduled, order.by = E_data$Date)

#In the third regression, Y is Return, X is Surprise.
lm_rt = lm(E_data$Return~E_data$total.change, data = E_data)
summary(lm_rt)

#In the second regression, Y is Return, X is Expected;
lm_rt2 = lm(E_data$Return~E_data$expected, data = E_data)
summary(lm_rt2)

#In the third regression, Y is Return, X is Surprise.
lm_rt3 = lm(E_data$Return~E_data$surprise, data = E_data)
summary(lm_rt3)

#	Create a variable which equals to Scheduled*Surprise
E_data$Scheduled_Surprise <- E_data$Scheduled * E_data$surprise

#run a regression Y is Return, the first X variable is Surprise, and the second X variable is Scheduled*Surprise.
lm_rt4 <- lm(E_data$Return ~ E_data$surprise + E_data$Scheduled_Surprise, data = E_data)
summary(lm_rt4)




