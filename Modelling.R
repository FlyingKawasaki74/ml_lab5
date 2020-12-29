library(tidyverse)
library(readr)
library(randomForest)

data <- read_csv('./0_Data/model_input_data.csv')

# Define allowed hyperparameter values
arr_ntree = c(100,200,300,400,500,600,700,800,900,1000)
arr_mtry_divideby = c(4,3,2)
arr_data_set = c("full", "relevant_only")
cv_k = c(seq(1,10,1))

temparr_ntree = NULL
temparr_mtry_divideby = NULL
temparr_dataset = NULL
temparr_rmse = NULL
temparr_bias = NULL
temparr_var = NULL
index = 1

evalRMSE = function(y, yhat) {
  sqrt(mean((y - yhat)^2, na.rm = TRUE))
}

get_bias = function(y, y_hat) {
  (mean(y_hat) - y)^2
}

# Each possible value for ntree
for (ntree in arr_ntree){
  # Each possible value for mtry, being p/...
  for (mtry in arr_mtry_divideby){
    # Decide whether to use the full data set, or only variables with strong correlation
    for (set_type in arr_data_set){
      if (set_type == "relevant_only"){
        current_data = data %>% select("Rent", "livingspace", "ost",
                                       "dum_builtinkitchen", "berlin", "bayern",
                                       "hamburg", "baden")
      }
      else{
        current_data = data
      }
      
      # Mark subsets for CV with corresponding k
      sampleInd = sample(x = c(seq(1,10,1)), size = nrow(current_data),
                         prob = c(rep(0.1,10)), replace = TRUE)
      
      temparr_cv_rmse = NULL
      temparr_cv_bias = NULL
      temparr_cv_variance = NULL
      # Do CV
      for (k in cv_k){
        # Random row sampling 
        cv_train = data[sampleInd!=k,]
        cv_test = data[sampleInd==k,]
        
        rf = randomForest(Rent ~ .,cv_train, ntree=ntree, mtry=(ncol(cv_train)/mtry))
        predictions = predict(rf, cv_test)
        error = evalRMSE(cv_test[["Rent"]], predictions)
        temparr_cv_rmse[k] = error
        temparr_cv_bias[k] = get_bias(cv_test[["Rent"]], predictions)
        temparr_cv_variance[k] = var(predictions)
      }
      
      avg_error = mean(temparr_cv_rmse)
      avg_bias = mean(temparr_cv_bias)
      avg_variance = mean(temparr_cv_variance)
      
      # Save results of current hyperparameter evaluation
      temparr_ntree[index] = ntree
      temparr_mtry_divideby[index] = mtry
      temparr_dataset[index] = set_type
      temparr_rmse[index] = avg_error
      temparr_bias[index] = avg_bias
      temparr_var[index] = avg_variance
      index=index+1
      
      # For tracking progress
      print(paste0("Current combination (index ",index,"): ",ntree,"|",mtry,"|",set_type,"|",avg_error))
      
      # Save partial progress
      cv_results = tibble(temparr_ntree, temparr_mtry_divideby, temparr_dataset, temparr_rmse, temparr_bias, temparr_var)
      write_csv(cv_results, "grid_search_temp.csv")
    }
  }
}

cv_results = tibble(temparr_ntree, temparr_mtry_divideby, temparr_dataset, temparr_rmse, temparr_bias, temparr_var)
write_csv(cv_results, "grid_search.csv")


# Final model
#---------------------------------------


# Plot predictions and true target for final model
#rf = randomForest(Rent ~ .,data, ntree=100, mtry=(ncol(data)/3))

predictions = predict(rf, data)
error = evalRMSE(data[["Rent"]], predictions)

data[["predictions"]] <- predictions

ggplot(data=data, aes(x=livingspace))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

# Parameter importance
imp <- importance(rf)

partialPlot(rf, as.data.frame(data), livingspace)