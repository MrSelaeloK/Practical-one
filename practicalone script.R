###Practical one ####
airquality
#---number of missing data rows and number of total columns in the air quality database---#
number_of_missing_cases <- sum(!complete.cases(airquality))
number_of_missing_cases
number_of_columns<-ncol(airquality)
number_of_columns

#---data frame with size = to number rows with missing values---#
mvalues_df <-data.frame()