###Practical one ####
airquality
#---number of missing data rows and number of total columns in the air quality database---#
number_of_missing_cases <- sum(!complete.cases(airquality))
number_of_missing_cases
number_of_columns<-ncol(airquality)
number_of_columns

#---data frame with size = to number rows with missing values---#
# names of headings also set to match #
airquality_matrix<-as.matrix(airquality)
missingValues_df <-data.frame(matrix(NA,nrow=number_of_missing_cases, ncol=number_of_columns))
colnames(missingValues_df)<-colnames(airquality_matrix)
missingValues_df


for(i in 1:nrow(airquality)){
  if(!complete.cases(airquality)){
    
  }
}


