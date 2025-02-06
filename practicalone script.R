###Practical one ####
airquality
#---number of missing data rows and number of total columns in the air quality database---#
number_of_missing_cases <- sum(!complete.cases(airquality))
number_of_columns<-ncol(airquality)
print(paste("missing values data frame will have ",number_of_missing_cases," rows and ",number_of_columns," columns"))

#---data frame with size = to number rows with missing values---#
# names of headings also set to match #
airquality_matrix<-as.matrix(airquality)
missingValues_df <-data.frame(matrix(NA,nrow=number_of_missing_cases, ncol=number_of_columns))
colnames(missingValues_df)<-colnames(airquality_matrix)
missingValues_df

index<-1
for(i in 1:nrow(airquality)){
  if(!complete.cases(airquality[i,])){
    missingValues_df[index,]<-airquality[i,]
    index<<-index+1
    
  }
}
index<-1
missingValues_df

#--------------------------------------------------------------------------------

#---mean, min, standard deviation, max for ozone and temperature---#

Ozone_mean <- mean(airquality[,1],
                   ,
                   na.rm=TRUE)

Ozone_sd<-sd(airquality[,1],
             na.rm=TRUE)

Ozone_min <- min(airquality[,1],
                 na.rm=TRUE)

Ozone_max <- max(airquality[,1],
                 na.rm=TRUE)

Temp_mean <- mean(airquality[,4],
                  na.rm=TRUE)

Temp_sd <- sd(airquality[,4],
              na.rm=TRUE)

Temp_min <- min(airquality[,4],
                na.rm=TRUE)

Temp_max <- max(airquality[,4],
                na.rm=TRUE)

ozone_output<- paste("Ozone mean: ",Ozone_mean,
                     "\nOzone standard deviation: ",Ozone_sd,
                     "\nOzone max: ",Ozone_max,
                     "\nOzone min: ", Ozone_min)

temperature_output<-paste("Temperature mean: ",Temp_mean,
                          "\nTemperature standard deviation: ",Temp_sd,
                          "\nTemperature max: ",Temp_max,
                          "\nTemperature min: ",Temp_min)

cat(paste0(ozone_output,"\n\n",temperature_output))

#---Question 3 ---#
intercept<-rep(1,nrow(cars))
X<-as.matrix(cbind(intercept,(cars[,1])))
Y<-as.matrix(cars[,2])

f<-function(X,Y){
  beta_coefficients <- solve(t(X)%*%X) %*% (t(X)%*%Y)
  colnames(beta_coefficients)<-"Coefficients"
  return(list(beta_coefficients))
}
f(X,Y)
#---Question 4 ---#
a<-lm(cars[,2]~cars[,1])
summary(a)




