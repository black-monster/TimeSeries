library(data.table)
setwd("C:/Users/NDH00158/Desktop/TimeSeries")

## 1) Read File

data <- fread("Monthly-Demand-raw.csv")





### 2) Find number of data points in Time Series
no_of_points = nrow(data)



### 3) Calculate 12 MA moving window


######  Initialize 12 MA column with all zero
data[, c("12-MA")] = 0

###### Create 12 MA moving window
window_12 <- c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6)

for (t in c(6:(no_of_points-6))){
  ### This loop iterates over all month having non zero trend
  sum = 0
  for (k in window_12){
  ### This loop sums demand from all months within 12 window, window centerd at the month as 6th point
    sum = sum + as.numeric(data[t+k, c("Monthly Demand in Millions")])
  }
  sum = sum/12
  data[t, c("12-MA")] = sum
}

### 4) Calculate 2 MA on top of 12 MA


###### Initialize Trend (2*12 MA) column
data[, c("Trend")] = 0

###### create 2 MA moving window
window_2 <- c(-1, 0)

for (t in c(7:(no_of_points-6))){
  ### This loop iterates over all the months having non zero trend
  sum = 0 
  ### This loop finds 2MA (previous month and current month) over 12 MA series
  for (k in window_2){
    sum = sum + as.numeric(data[t+k, c("12-MA")])
  }
  sum = sum/2
  data[t, c("Trend")] = sum
  
}
### 5a) Detrend the data additively
data[ , c("Additive deTrended")] = data[, c("Monthly Demand in Millions")] - data[ , c("Trend")]


### 6a) Find Additive Seasonality

###### Initialize seasonality column
data[ , c("Additive Seasonality")] = 0




for( i in seq(7,18)){
  #### This loop iterates over first 12 dates having non zero trend.
  seasonality_value = 0
  no_of_months = 0
  ### This loop sums up all the detrended values having month same as month i
  # print (data[i, ])
  for( t in seq(0, 200, by=12)){
    
    if( i + t > (no_of_points-6)){
      break
    }
    no_of_months = no_of_months + 1
    seasonality_value = seasonality_value +  as.numeric(data[ i+t , c("Additive deTrended")])
    # print (as.numeric( data[ i+t , c("Additive deTrended")]) )
  }
  
  for( t in seq(0, 200, by=12)){
    if( i + t > (no_of_points)){
      break
    }
    data[i + t , c("Additive Seasonality")] = seasonality_value/no_of_months
    
  }
  print (seasonality_value, no_of_months)
  
}

### 7a) Find Additive Residual
data[, c("Additive Residual")] = data[, c("Monthly Demand in Millions")] - data[ , c("Trend")] - data[, c("Additive Seasonality")]

### 8a) Plot the results


### 5b) Detrend the data multiplicatively
data[ , c("Multiplicative deTrended")] = data[, c("Monthly Demand in Millions")] / data[ , c("Trend")]


### 6b) Find Multiplicative Seasonality

###### Initialize seasonality column
data[ , c("Multiplicative Seasonality")] = 0




for( i in seq(7,18)){
  #### This loop iterates over first 12 dates having non zero trend.
  seasonality_value = 0
  no_of_months = 0
  ### This loop sums up all the detrended values having month same as month i
  # print (data[i, ])
  for( t in seq(0, 200, by=12)){
    
    if( i + t > (no_of_points-6)){
      break
    }
    no_of_months = no_of_months + 1
    seasonality_value = seasonality_value +  as.numeric(data[ i+t , c("Multiplicative deTrended")])
    # print (as.numeric( data[ i+t , c("Multiplicative deTrended")]) )
  }
  
  for( t in seq(0, 200, by=12)){
    if( i + t > (no_of_points)){
      break
    }
    data[i + t , c("Multiplicative Seasonality")] = seasonality_value/no_of_months
    
  }
  print (seasonality_value, no_of_months)
  
}

### 7b) Find Muliplicative Residual
data[, c("Multiplicative Residual")] = data[, c("Monthly Demand in Millions")] / (data[ , c("Trend")] * data[, c("Multiplicative Seasonality")])

write.csv(data, "Monthly-Demand-R-output.csv")



































