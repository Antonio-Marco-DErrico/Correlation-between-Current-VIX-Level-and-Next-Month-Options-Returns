library(readxl)
library(stats)
library(ggplot2)
library(corrplot)
Monthly_prices <- read_excel("C:/Users/ac18a/Desktop/Monthly_prices_S&P500_Options_Benchmarks.xlsx", 
        sheet = "Only monthly VIX and returns",
        col_types = c("date", "numeric", "numeric", 
                      "numeric", "numeric", "numeric", 
                      "numeric", "numeric", "numeric", 
                      "numeric", "numeric"),
        range = "A1:K354")

dataopt <- data.frame(Monthly_prices)


aritm_means <- NULL

medians <- NULL

variances <- NULL

mins <- NULL

maxs <- NULL

ranges <- NULL
 
standard_devs <- NULL

quart_1 <- NULL

quart_3 <- NULL

interq_ranges <- NULL

coef_variations <- NULL

for (column in 2:11){
 
  
date <- dataopt[, 1]

Month_return <- dataopt[, column]
   
   temp_aritm_means <- mean(Month_return)
   temp_medians <- median(Month_return)
   temp_variances <- var(Month_return)
   temp_mins <- min(Month_return)
   temp_maxs <- max(Month_return)
   temp_ranges <- temp_maxs - temp_mins
   temp_standard_devs <- sd(Month_return)
   temp_quart_1 <- quantile(Month_return,0.25)
   temp_quart_3 <- quantile(Month_return,0.75)
   temp_interq_ranges <- temp_quart_3 - temp_quart_1
   temp_coef_variations <- temp_standard_devs/temp_aritm_means
   
   
  aritm_means <- c(aritm_means,temp_aritm_means)
  medians <- c(medians,temp_medians) 
  variances <- c(variances,temp_variances)
  mins <- c(mins,temp_mins)
  maxs <- c(maxs,temp_maxs)
  ranges <- c(ranges,temp_ranges)
  standard_devs<- c(standard_devs,temp_standard_devs)
  quart_1 <- c(quart_1,temp_quart_1)
  quart_3 <- c(quart_3,temp_quart_3)
  interq_ranges <- c(interq_ranges,temp_interq_ranges)
  coef_variations <- c(coef_variations,temp_coef_variations)

}


par(mfrow=c(3,3))

hist(dataopt[,2], col="blue", main="BXM Histogram", xlab="Monthly Returns")
hist(dataopt[,3], col="blue", main="PUT Histogram", xlab="Monthly Returns")
hist(dataopt[,4], col="blue", main="CLL Histogram", xlab="Monthly Returns")
hist(dataopt[,5], col="blue", main="BXY Histogram", xlab="Monthly Returns")
hist(dataopt[,6], col="blue", main="BXMD Histogram", xlab="Monthly Returns")
hist(dataopt[,7], col="blue", main="BFLY Histogram", xlab="Monthly Returns")
hist(dataopt[,8], col="blue", main="CLLZ Histogram", xlab="Monthly Returns")
hist(dataopt[,9], col="blue", main="CNDR Histogram", xlab="Monthly Returns")
hist(dataopt[,10], col="blue", main="PPUT Histogram", xlab="Monthly Returns")

par(mfrow=c(3,3))

plot(dataopt[,2], col="red", x = date, main="BXM Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,3], col="red", x = date, main="PUT Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,4], col="red", x = date, main="CLL Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,5], col="red", x = date, main="BXY Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,6], col="red", x = date, main="BXMD Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,7], col="red", x = date, main="BFLY Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,8], col="red", x = date, main="CLLZ Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,9], col="red", x = date, main="CNDR Plot", ylab="Monthly Returns", xlab = "")
plot(dataopt[,10], col="red", x = date, main="PPUT Plot", ylab="Monthly Returns", xlab = "")

par(mfrow=c(3,3))

plot(ecdf(dataopt[,2]), col="green", main="BXM Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,3]), col="green", main="PUT Cum Distr",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,4]), col="green", main="CLL Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,5]), col="green", main="BXY Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,6]), col="green", main="BXMD Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,7]), col="green", main="BFLY Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,8]), col="green", main="CLLZ Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,9]), col="green", main="CNDR Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")
plot(ecdf(dataopt[,10]), col="green", main="PPUT Cum Dist",cex=0,xlab= "Month_returns", ylab="Frequency")

par(mfrow=c(1,1))

boxplot(dataopt[2:10], col="yellow", main=" Indexes' Monthly Returns Boxplots", names = c("BXM","PUT","CLL","BXY","BXMD","BFLY","CLLZ","CNDR","PPUT"))

par(mfrow=c(2,2))

  
boxplot(dataopt[,11], col="yellow", main="VIX Boxplot",ylab="VIX_Level")
hist(dataopt[,11], col="blue", main="VIX histogram", xlab="VIX_Level")
plot(dataopt[,11], col="red", x = date, main= "VIX plot", ylab="VIX_Level")
plot(ecdf(dataopt[,11]), col="green", main="VIX Cum Dist",cex=0,xlab= "VIX_Level", ylab="Frequency")
  
par(mfrow=c(1,1))

df_final <- data.frame(aritm_means,medians,variances,mins,maxs,ranges,standard_devs,quart_1,quart_3,interq_ranges,coef_variations)

rownames(df_final) <- names(dataopt)[-1]

View(df_final)



indexes_returns_2 <- data.frame(read_excel("C:/Users/ac18a/Desktop/Monthly_prices_S&P500_Options_Benchmarks.xlsx", 
                                        sheet = "Only monthly VIX and returns", 
                                        range = "A356:J358", col_names = c("returns_data",names(dataopt)[2:10])))

rownames(indexes_returns_2) <- data.frame(indexes_returns_2)[1:3,1]

indexes_returns <- indexes_returns_2[,-1]

rm(indexes_returns_2)

View(indexes_returns)


dataopt_simple_names <- dataopt

names(dataopt_simple_names) = c("Date","BXM","PUT","CLL","BXY","BXMD","BFLY","CLLZ","CNDR","PPUT","VIX")

correlations <- cor(dataopt_simple_names[,2:11])

covariances <- cov(dataopt_simple_names[,2:11])

View(correlations)

View(covariances)

par(mfrow=c(1,1))

coul <- c("red3","yellow","grey91","steelblue1","blue4")

coul <- colorRampPalette(coul)(500)

corrplot(correlations, method="number", type = "upper", col= coul, tl.col ="black", diag = FALSE)

B_0 <- NULL

B_1 <- NULL

R2 <- NULL

for (i in 2:10){

    date <- dataopt[, 1]
    
    Month_return <- dataopt[, column]
    
    temp_B_1 <- cov(dataopt[,11],dataopt[,i])/var(dataopt[,11])

    temp_B_0 <- mean(dataopt[,i])-temp_B_1*mean(dataopt[,11])
    
    temp_R2 <- summary(lm(dataopt[,11]~dataopt[,i]))$r.squared
    
    
    B_0 <- c(B_0,temp_B_0)
    
    B_1 <- c(B_1,temp_B_1)
    
    R2 <- c(R2, temp_R2)
  
  
}  
  Regression_lines_cov_and_cor <- data.frame(B_0,B_1,R2,covariances[1:9,10],correlations[1:9,10])
  
  rownames(Regression_lines_cov_and_cor) <- c("BXM","PUT","CLL","BXY","BXMD","BFLY","CLLZ","CNDR","PPUT")
  
  colnames(Regression_lines_cov_and_cor) <- c("B0(VIX)","B1(VIX)","R^2(VIX)","Cov(VIX)","Cor(VIX)")
  
  View(Regression_lines_cov_and_cor)
  
  
  

par(mfrow=c(3,3))


plot(dataopt[,11],dataopt[,2], col="darkgoldenrod1", ylab = "BMX Return", xlab = "VIX Level")
abline(B_0[1],B_1[1])

plot(dataopt[,11],dataopt[,3],col="darkgoldenrod1", ylab = "PUT Return", xlab = "VIX Level")
abline(B_0[2],B_1[2])

plot(dataopt[,11],dataopt[,4],col="darkgoldenrod1", ylab = "CLL Return", xlab = "VIX Level")
abline(B_0[3],B_1[3])

plot(dataopt[,11],dataopt[,5],col="darkgoldenrod1", ylab = "BXY Return", xlab = "VIX Level")
abline(B_0[4],B_1[4])

plot(dataopt[,11],dataopt[,6],col="darkgoldenrod1", ylab = "BXMD Return", xlab = "VIX Level")
abline(B_0[5],B_1[5])

plot(dataopt[,11],dataopt[,7],col="darkgoldenrod1", ylab = "BFLY Return", xlab = "VIX Level")
abline(B_0[6],B_1[6])

plot(dataopt[,11],dataopt[,8],col="darkgoldenrod1", ylab = "CLLZ Return", xlab = "VIX Level")
abline(B_0[7],B_1[7])

plot(dataopt[,11],dataopt[,9],col="darkgoldenrod1", ylab = "CNDR Return", xlab = "VIX Level")
abline(B_0[8],B_1[8])

plot(dataopt[,11],dataopt[,10],col="darkgoldenrod1", ylab = "PPUT Return", xlab = "VIX Level")
abline(B_0[9],B_1[9])

#ggplot(dataopt, aes(dataopt[,11],dataopt[,3])) + geom_point() + geom_smooth(span = 0.8)
