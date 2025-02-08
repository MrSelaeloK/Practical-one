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
X<-as.matrix(cars[,1])
Y<-as.matrix(cars[,2])

f<-function(X,Y){
#beta coefficients  
X<-as.matrix(cbind(intercept,(cars[,1])))
beta_coefficients <- solve(t(X)%*%X) %*% (t(X)%*%Y)

#std errors
error<-Y-(X%*%beta_coefficients)
n_p<-nrow(cars)-ncol((cars))
variance_estimate<-(t(error)%*%error)/(n_p)
variance_estimate
Covariance_beta<- as.numeric(variance_estimate)*(solve(t(X)%*%X))
std_err<-sqrt(diag(Covariance_beta))

#T stats
T_stats<-matrix(NA,ncol=1,nrow=2)

#matrices
coefficients<-as.matrix(beta_coefficients)
standarderror<-as.matrix(std_err)
T_stats<-coefficients/standarderror

#P-values
p_values<-matrix(NA,nrow=2,ncol=1)
T_stats[1,1]
p_1<-pt(as.numeric(abs(T_stats[1,1])),
        48,
        lower.tail=FALSE)
p_2<-pt(as.numeric(abs(T_stats[2,1])),
        48,
        lower.tail=FALSE)

p_values[1,1]<-p_1*2
p_values[2,1]<-p_2*2
p_values

#changing of row names and column names
rownames(coefficients)<-c("intercept","Slope")
colnames(coefficients)<-"Coefficients"
colnames(standarderror)<-"std_err"
colnames(T_stats)<-"T-stat"
colnames(p_values)<-"p values"

return(cbind(coefficients,
             standarderror,
             T_stats,p_values))
}
f(X,Y) 
  
#---Question 4 ---#
a<-lm(cars[,2]~cars[,1])
summary(a)




