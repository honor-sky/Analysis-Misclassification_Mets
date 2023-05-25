# -- Load library --
install.packages("Ckmeans.1d.dp")
library(xgboost) # xgboost
library(tidyverse) # for select and pipe operation
library(caret) #for confusion matrix
library(igraph)
#-- Set Working Directory --
setwd("D:/단국대학교/연구실/글로벌 핵심인재 양성/라이프로그 대사증후군 진단/5월 논문 준비/CBFI source code")
source("CB_FeatureImp.r")
source("CB_FeatureInteract.r")
source("CB_FItable.r")
source("CB_plot.contribute.r")
source("CB_plot.FIgraph.r")
source("CB_plot.imp.r")
source("CB_plot.interact.r")
source("CB_plot.PA.r")

# -- Define Function for predict --
get_prediction <- function(fit, ts, label, feature){
  # col 1 = mets
  
  ts_slt <- ts %>% 
    select(all_of(label), all_of(feature)) %>%
    mutate(across(1, ~ factor(.x)))
  
  ts_x <- as.matrix(ts_slt[,-1])
  ts_y <- as.matrix(ts_slt[,1])
  dts <- xgb.DMatrix(data=ts_x, label=ts_y)
  
  predict(fit, dts)

}


#-- Load a model --
xg_mdl <- readRDS('xg_model_blc_f10.rds')
summary(xg_mdl)
xg_mdl$params
xg_mdl$importance
xg_mdl$feature


#-- Load dataset --
ts <- read.csv('test_blc.csv')
ts<- ts[,c("BPWC", "WC","BFP","BP","whr","bp_bi","bmi","waist_bi", "age", "pulse","mets")]
pred <- get_prediction(xg_mdl$fit,ts,'mets',xg_mdl$feature)

#-- Draw graph --
result <- CB_FeatureImp(xg_mdl$fit, ts, "mets", itr=50, task="classification")
CB_plot.imp(result, class="1") 

result <- CB_FeatureInteract(xg_mdl$fit, train=ts, target.name="mets", F1="whr", F2=NULL, itr=50, task="classification")
CB_plot.interact(result, class="_all_")

FIobj <- CB_FItable(xg_mdl$fit, ts, "mets", itr=50, task="classification")
CB_plot.FIgraph( gtype="S", FIobj=FIobj, task="classification", class="_all_",
                 show.edge.weight=TRUE, seed=104)
CB_plot.FIgraph( gtype="S", FIobj=FIobj, task="classification", class="0", #클래스별
                 show.edge.weight=TRUE, seed=104)
CB_plot.FIgraph( gtype="S", FIobj=FIobj, task="classification", class="1", #클래스별
                 show.edge.weight=TRUE, seed=104)


CB_plot.PA(xg_mdl$fit, ts, "mets", F1="BPWC", F2="BP", itr=10, task="classification")


result = CB_FeatureInteract(xg_mdl$fit, ts, "mets",  F1="BP", F2="WC", itr=50, task="classification")
CB_plot.contribute(result, class="_all_")

importance_matrix <- xgb.importance(feature_names=c("BPWC", "WC","BFP","BP","whr","bp_bi","bmi","waist_bi", "age", "pulse"), model = xg_mdl$fit)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")
(gg <- xgb.ggplot.importance(importance_matrix, measure = "Frequency", rel_to_first = TRUE,n_clusters = c(1:2)))
gg + ggplot2::ylab("Frequency")

#-- Get Metrics --
confusionMatrix(table(pred, ts$mets), positive = '1') # Positive case == mets

