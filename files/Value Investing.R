# Value Investing in R

# Data: Quarterly 292 firms
#       QuarterlyReturns292.csv: 1996Q1 - 2015Q3 (79 Periods)
#       ME292.csv              : 1995Q4 - 2015Q2 (79 Periods)
#       BE292.csv              : 1995Q3 - 2015Q1 (79 Periods)

# i=1, order 292 firms in terms of BE(1995Q3) / ME(1995Q4)

# Select the top 30 'Value' firms and construct the equal weighted portfolio.
# Compute the cumulative returns of the top 30 'value' frims and plot your result.


graphics.off()

myME0 <- as.matrix(read.table(file="ME292.csv", header=TRUE, sep=","))
myBE0 <- as.matrix(read.table(file="BE292.csv", header=TRUE, sep=","))
myR0 <- as.matrix(read.table(file="QuarterlyReturns292.csv", header=TRUE, sep=","))

myME1 <- myME0[ ,-1]
myBE1 <- myBE0[ ,-1]
myR1 <- myR0[ ,-1]

myBEME <- myBE1/myME1

myNames <- colnames(myBEME)
p.R <- rep(0,79) # p.R means portforlio return
ValueStocks <- matrix(1, 79, 30)

# Period 1
BM <- myBEME[1, ]
MyOrder <- order(BM, decreasing = TRUE)
Top30 <- MyOrder[1:30]
ValueStocks[1, ] <- myNames[Top30]
p.R[1] <- mean(myR1[1, Top30])

# Repeat it for 79 periods

for(t in 1:79){
  BM <- myBEME[t, ]
  MyOrder <- order(BM, decreasing = TRUE)
  Top30 <- MyOrder[1:30]
  ValueStocks[t, ] <- myNames[Top30]
  p.R[t] <- mean(myR1[t, Top30])

}

G.p.R <- p.R + 1 # G.p.R means growth portfolio return
cumuR <- rep(0, 79)

for (t in 1:79){
  cumuR[t] <- prod(G.p.R[1:t])
}

plot(c(1:79), cumuR, type="l", col="red") 

# or

plot(seq(from=1996, by=0.25, length=79), cumuR, type="l", col="red")

# Additional comments from Nick

# temp <- as.data.frame(myBE0)

# df <- data.frame()
# for i in myNames {
#   dfi <- as.data.frame(temp$year, i)
#    df <- rbind(df, dfi)
# }
