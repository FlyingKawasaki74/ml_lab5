library(tidyverse)
library(readr)
library(randomForest)
library(Metrics)

data <- read_csv('./0_Data/model_input_data.csv')
previous_gridsearch_results = read_csv('./grid_search_temp.csv')

# Define allowed hyperparameter values
arr_ntree = c(50,100,200,300,400,500)
arr_mtry_divideby = c(2)
arr_data_set = c("relevant_only")
cv_k = c(seq(1,5,1))

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

# Each possible value for ntree
for (ntree in arr_ntree){
  # Each possible value for mtry, being p/...
  for (mtry in arr_mtry_divideby){
    # Decide whether to use the full data set, or only variables with strong correlation
    for (set_type in arr_data_set){
      # Check if we already did a CV for that hyperparam combination
      if(nrow(previous_gridsearch_results %>% filter(temparr_ntree==ntree & temparr_mtry_divideby==mtry & temparr_dataset==set_type))==0){
        if (set_type == "relevant_only"){
          current_data = data %>% select("Rent", "livingspace", "ost",
                                         "dum_builtinkitchen", "berlin", "bayern",
                                         "hamburg", "baden")
        }
        else{
          current_data = data
        }
        
        # Mark subsets for CV with corresponding k
        set.seed(123)
        sampleInd = sample(x = c(seq(1,10,1)), size = nrow(current_data),
                           prob = c(rep(0.1,10)), replace = TRUE)
        
        temparr_cv_rmse = NULL
        temparr_cv_bias = NULL
        temparr_cv_variance = NULL
        # Do CV
        for (k in cv_k){
          # Random row sampling 
          cv_train = current_data[sampleInd!=k,]
          cv_test = current_data[sampleInd==k,]
          
          rf = randomForest(Rent ~ .,cv_train, ntree=ntree, mtry=(ncol(cv_train)/mtry))
          predictions = predict(rf, cv_test)
          error = evalRMSE(cv_test[["Rent"]], predictions)
          temparr_cv_rmse[k] = error
          temparr_cv_bias[k] = bias(cv_test[["Rent"]], predictions)
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
        
        # For tracking progress
        print(paste0(Sys.time(), " | Finished combination (index ",index,"): ",ntree,"|",mtry,"|",set_type,"|",avg_error))
        
        index=index+1
        
        # Save partial progress
        cv_results = tibble(temparr_ntree, temparr_mtry_divideby, temparr_dataset, temparr_rmse, temparr_bias, temparr_var)
        write_csv(union(previous_gridsearch_results,cv_results), "grid_search_temp.csv")
      }
      else{
        print(paste0("Skipping combination ntree=",ntree,";mtry=p/",mtry,";set_type=",set_type))
      }
    }
  }
}

cv_results = tibble(temparr_ntree, temparr_mtry_divideby, temparr_dataset, temparr_rmse, temparr_bias, temparr_var)
write_csv(union(previous_gridsearch_results,cv_results), "grid_search.csv")


# Final model
#---------------------------------------


# Plot predictions and true target for final model
# Ideal combo for full data
rf_fulldata = randomForest(Rent ~ .,data, ntree=400, mtry=(ncol(data)/2))
save(rf_fulldata, file = "./model_rf_fulldata.rda")
load(file = "./model_rf_fulldata.rda")

predictions = predict(rf_fulldata, data)
error = evalRMSE(data[["Rent"]], predictions)
bias = bias(data[["Rent"]], predictions)
var = var(predictions)

data[["predictions"]] <- predictions

ggplot(data=data, aes(x=livingspace))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

# Parameter importance
imp <- importance(rf_fulldata)

partialPlot(rf_fulldata, as.data.frame(data), livingspace)
#---------------------------------------



# Benchmark model
#---------------------------------------

current_data = data %>% select("Rent", "livingspace", "ost",
                               "dum_builtinkitchen", "berlin", "bayern",
                               "hamburg", "baden")
simple_model <- lm(Rent ~ ., current_data)

predictions_benchmark = predict(simple_model, current_data)
error_benchmark = evalRMSE(data[["Rent"]], predictions_benchmark)
bias_benchmark = bias(data[["Rent"]], predictions_benchmark)
var_benchmark = var(predictions_benchmark)


#---------------------------------------