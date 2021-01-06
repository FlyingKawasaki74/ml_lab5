library(tidyverse)
library(readr)
library(randomForest)
library(Metrics)
library(forcats)

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
# Utility function for subplot support, Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#-----------------------------------------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title="") {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#-----------------------------------------------------------------------------------

# Grid Search
#-----------------------------------------------------------------------------------
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

predictions = predict(rf_fulldata, data)
error = evalRMSE(data[["Rent"]], predictions)
bias = bias(data[["Rent"]], predictions)
var = var(predictions)

data[["predictions"]] <- predictions

ggplot(data=data, aes(x=livingspace))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

ggplot(data=data, aes(x=lat))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

p1 <- ggplot(data=data, aes(x=ost, group=ost))+
  geom_boxplot(aes(y=Rent))
p2 <- ggplot(data=data, aes(x=ost, group=ost))+
  geom_boxplot(aes(y=predictions))
multiplot(p1, p2, cols=2)

ggplot(data=data, aes(x=einwohner_total))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

ggplot(data=data, aes(x=lon))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

p1 <- ggplot(data=data, aes(x=NoOfRooms, group=NoOfRooms))+
  geom_boxplot(aes(y=Rent))
p2 <- ggplot(data=data, aes(x=NoOfRooms, group=NoOfRooms))+
  geom_boxplot(aes(y=predictions))
multiplot(p1, p2, cols=1)

ggplot(data=data, aes(x=einwohner))+
  geom_point(aes(y=Rent), color="blue")+
  geom_point(aes(y=predictions), color="red")

p1 <- ggplot(data=data, aes(x=dum_builtinkitchen, group=dum_builtinkitchen))+
  geom_boxplot(aes(y=Rent))+
  geom_hline(yintercept=600)
p2 <- ggplot(data=data, aes(x=dum_builtinkitchen, group=dum_builtinkitchen))+
  geom_boxplot(aes(y=predictions))+
  geom_hline(yintercept=600)
multiplot(p1, p2, cols=2)


# Parameter importance
imp <- importance(rf_fulldata)

partialPlot(rf_fulldata, as.data.frame(data), livingspace)
partialPlot(rf_fulldata, as.data.frame(data), lat)
partialPlot(rf_fulldata, as.data.frame(data), ost)
partialPlot(rf_fulldata, as.data.frame(data), einwohner_total)
partialPlot(rf_fulldata, as.data.frame(data), lon)
partialPlot(rf_fulldata, as.data.frame(data), NoOfRooms)
partialPlot(rf_fulldata, as.data.frame(data), einwohner)
partialPlot(rf_fulldata, as.data.frame(data), dum_builtinkitchen)

png(filename = "./1_Plots/Partial_Dependence_livingspace.png", height=350, width=350)
partialPlot(rf_fulldata, as.data.frame(data), livingspace)
dev.off()

png(filename = "./1_Plots/Partial_Dependence_lat.png", height=350, width=350)
partialPlot(rf_fulldata, as.data.frame(data), lat)
dev.off()

png(filename = "./1_Plots/Partial_Dependence_einwohner_total.png", height=350, width=350)
partialPlot(rf_fulldata, as.data.frame(data), einwohner_total)
dev.off()

png(filename = "./1_Plots/Partial_Dependence_einwohner.png", height=350, width=350)
partialPlot(rf_fulldata, as.data.frame(data), einwohner)
dev.off()

importance = read.csv("./0_Data/RF_importance.csv",sep=";")
importance = importance[1:10,-2]

png(filename = "./1_Plots/Histogramm.png", height=550, width=550)
importance %>%
  mutate(Regressor = fct_reorder(Regressor, Rounded)) %>%
  ggplot(aes(x=Regressor, y=Rounded)) +
  geom_bar(stat="identity", width=.6) +
  coord_flip() +
  xlab("") +
  ylab("") + 
  theme_bw()
dev.off()


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
