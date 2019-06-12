# "Presidential Address: Discount Rates" by John H. Cochrane, JF 2011

# Value Premium Puzzle?

graphics.off()

myData0 <- as.matrix(read.table(file="BMdeciles1950-2009.csv", header=TRUE, sep=","))

Date <- myData0[ ,1]
Rf <- myData0[ ,2]
Mkt <- myData0[ ,3]
deciles <- myData0[ ,4:13]

MktER <- Mkt - Rf #MktER means Market Excess Return
myEER <- rep(0,10)#myEER Expected Excess Return
myBetas <- rep(0,10)

for(i in 1:10){
  ER <- deciles[ ,i] - Rf
  myEER[i] <- mean(ER)
  myResult <- lm(ER ~ MktER)
  myCoef <- coef(summary(myResult))
  myBetas[i] <- myCoef[2,1]
  }

plot(c(1:10), myEER, type="o", pch=25, xlab="Growth ------------------------------------------ Value", ylab="Expected Excess Returns", main="Evidence of Value Premium")
points(c(1:10), myBetas*mean(MktER), type="o")
