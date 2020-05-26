#load packages
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readxl)) 
  install.packages("readxl", repos = "http://cran.us.r-project.org")
#Rborist and ranger are random forest algorithm wrappers
if(!require(Rborist)) 
  install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(ranger)) 
  install.packages("ranger", repos = "http://cran.us.r-project.org")
#matrix stats to compute row medians and means
if(!require(matrixStats)) 
  install.packages("matrixStats", repos = "http://cran.us.r-project.org")
#rpart.plot shows the decision tree of an rpart result
if(!require(rpart.plot)) 
  install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
#kableExtra allows more customization of tables
if(!require(kableExtra)) 
  install.packages("kableExtra")
if(!require(RColorBrewer)) 
  install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")



#load data from github repo
#divide urls into chunks to get pdf to wrap long urls
urlRemote<-"https://raw.github.com/sumitrodatta/most-improved-player/master/"
statsfile<-"NBA%20Season%20Stats%201984%20to%202020.xlsx"
mipsharefile<-"MIP%20Vote%20Share.xlsx"
#process is to:
#1. create temporary file
#2. download the file into that temp file
#3. read in data from temp file
#4. delete temp file
tmp<-tempfile()
download.file(paste0(urlRemote,statsfile),tmp)
combined<-read_xlsx(tmp,sheet="Combined")
tmp2<-tempfile()
download.file(paste0(urlRemote,mipsharefile),tmp2)
mip_share<-read_xlsx(tmp2,sheet="Sheet1")
file.remove(tmp)
file.remove(tmp2)
rm(tmp,tmp2)


#show missing data
sum(is.na(combined))
sum(is.na(mip_share))

#change missing data to zeros
combined[is.na(combined)]<-0
mip_share[is.na(mip_share)]<-0
#create master data with all combined stats and vote shares
combined<-left_join(combined,mip_share)
rm(mip_share) #to clean up environment (all relevant info in combined)


#table of mip winners, the season they won and the vote share they received
mip_winners<-combined %>% group_by(Season) %>% 
  #get player names and season IDs from indexes
  summarize(player=Player[which.max(`MIP Share Current Season`)],
            SeasID=SeasID[which.max(`MIP Share Current Season`)],
            vote_share=max(`MIP Share Current Season`))
#print out all rows
print(mip_winners,n=40)


# graph points per game vs season for mip winners
# take out missing seasons
mip_winners <- mip_winners %>% filter (!(Season %in% c(1985,1987,2020)))
#remove repeated columns
mip_stats <- left_join(mip_winners,combined,by="SeasID") %>% 
  select(-c(`MIP Share Current Season`,`MIP Share Next Season`,Season.y))
rm(mip_winners) #to clean up environment (all relevant info in mip_stats)
mip_stats %>%
  #minus in fill so more intense correlates to higher points
  ggplot(aes(x=Season.x,y=`PTS/G`,fill=-`PTS/G`)) + 
  geom_bar(stat="identity")+
  scale_fill_distiller(palette="YlOrRd")+
  #player label within bar, points per game label above bar
  geom_text(aes(label=Player),size=3,angle="90",hjust=1.1)+
  geom_text(aes(label=`PTS/G`),size=2,vjust=-1)+
  theme(legend.position = "none")


# graph win shares per 48 vs season for mip winners
mip_stats %>%
  #minus in fill so more intense correlates to higher ws/48
  ggplot(aes(x=Season.x,y=`WS/48`,fill=-`WS/48`)) +
  geom_bar(stat="identity",position="dodge")+
  scale_fill_distiller(palette="YlOrRd")+
  #player label within bar, win shares per 48 label above bar
  geom_text(aes(label=Player),size=3,angle="90",hjust=1.1)+
  geom_text(aes(label=round(`WS/48`,digits=2)),size=2,vjust=-1)+
  theme(legend.position = "none")


# graph mip winner vote share vs season
befo_2003_avg<-mip_stats %>% filter (Season.x<2003) %>% 
  summarize(mean(vote_share))
after_2003_avg<-mip_stats %>% filter (Season.x>=2003) %>% 
  summarize(mean(vote_share))
mip_stats %>% 
  ggplot(aes(Season.x,vote_share))+geom_bar(stat="identity")+
  geom_vline(xintercept = 2003,color="red")+
  #add averages for before and after voting system changed in 2003
  annotate("text",x=1990,y=0.75,
           label=paste("Mean before 2003: \n",
                       round(befo_2003_avg, digits = 3)))+
  annotate("text",x=2010,y=0.875,
           label=paste("Mean after 2003: \n",
                       round(after_2003_avg, digits = 3)))
#to clean up environment (no other use for vars)
rm(after_2003_avg,befo_2003_avg, mip_stats)


#create data frame of players with only one season in data
#one season players have never won MIP (there's noting to compare to!)
one_seasoners<-combined %>% group_by (Player,BirthYear) %>% 
  mutate(n=n()) %>% filter(n==1) %>% select(SeasID,Player,BirthYear)
full_jumps_data<-combined %>% 
  #remove the one-season players
  filter(!(SeasID %in% one_seasoners$SeasID)) %>%
  group_by(Player,BirthYear) %>% 
  #arrange seasons from first to last within each player
  arrange(Season,.by_group=TRUE) %>%
  #subtract prior season from current season
  mutate_at(.vars=c(colnames(combined[8:88])),
            .funs=funs(`diff`=.-lag(.,default=first(.)))) %>%
  #do not keep the raw stats, only differences
  select(SeasID:BirthYear, G_diff:VORP_diff,
         `MIP Share Next Season`:`MIP Share Current Season`) %>% 
  #remove first season from every group (no difference)
  slice(-1) %>% arrange(SeasID) %>% ungroup() %>% 
  #do not keep identifying information
  select(SeasID,Age,Season,G_diff:`MIP Share Current Season`)


#remove identifying info from a copy of master data
full_data<-combined
full_data<-full_data %>% select(-c(Player,Tm,Lg,BirthYear))


#check whether any variables have variance near zero
#only variances near zero are outcomes
nzv<-nearZeroVar(full_jumps_data,saveMetrics = TRUE)
rownames(nzv)[which(nzv$nzv==TRUE)]
#remove nzv because no future use
rm(nzv)


#transfer out evaluation set (2020 season) and place in new tibble
eval_2020<-full_data %>% filter (Season==2020)
full_data<-full_data %>% filter (Season != 2020)
eval_jumps_2020<-full_jumps_data %>% filter (Season ==2020)
full_jumps_data<-full_jumps_data %>% filter (Season !=2020)


#transfer out test sets and place in new tibbles
eval_1986<-full_data %>% filter (Season==1986)
eval_2019<-full_data %>% filter (Season==2019)
full_data<-full_data %>% filter(Season !=1986 & Season !=2019)
eval_jumps_1987<-full_jumps_data %>% filter (Season ==1987)
full_jumps_data<-full_jumps_data %>% filter (Season !=1987)


#linear model for 2020 winner
#use 10-fold cross validation, which splits data set into 10 and trains on each subset
linear<-train(`MIP Share Current Season`~.-Season-`MIP Share Next Season`-
                SeasID-G_diff-GS_diff, data=full_jumps_data,method="lm",
              trControl=trainControl(method="cv",number=10,p=0.9))

#function to get top 5 most important variables in a train model
get_5_top_important<- function(model){
  ImpMeasure<-data.frame(varImp(model)$importance)
  ImpMeasure$Vars<-row.names(ImpMeasure)
  # minus to order descending
  ImpMeasure[order(-ImpMeasure$Overall),][1:5,]
}

#linear_top_vars
get_5_top_important(linear)


#function to get names of top predicted vote shares
get_top_vote_shares<-function(num, eval_data){
  (combined %>% filter (SeasID==eval_data[[num,1]]))[[2]]
}

#linear model performance on 1987 test set
#get season ID of winner Dale Ellis
ellis_seas_id<-(combined %>% filter (Player=="Dale Ellis" & Season==1987) 
                %>% select(SeasID))[[1]]
#get index of prediction
ellis_ind<-which(eval_jumps_1987$SeasID==ellis_seas_id)
#get rmse of model
lin_rmse<-linear$results[["RMSE"]]
linear_predict<-predict(linear,eval_jumps_1987)
#get ellis's predicted vote shares using above index
ellis_prob<-linear_predict[[ellis_ind]]
#top 3 names
top_3<-order(-linear_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_jumps_1987)
#top 3 vote shares
top_3_prob<-linear_predict[order(-linear_predict)[1:3]]
models<-tibble(Method="Linear",RMSE=lin_rmse,"Ellis Vote Share"=ellis_prob,
               "Top 3 Candidates"=top_3,"Top 3 Vote Shares"=top_3_prob)
models %>% knitr::kable()


#knn model for 2020 winner
#check best amount of neighbors to compare to using tuneGrid
knn<-train(`MIP Share Next Season`~.-Season-`MIP Share Current Season`-
             G_diff-GS_diff-SeasID, data=full_jumps_data,method="knn", 
           tuneGrid=data.frame(k=seq(5,50)),
           trControl=trainControl(method="cv",number=10,p=0.9))
knn$finalModel


#knn model performance on 1987 test set
knn_rmse<-min(knn$results[["RMSE"]])
knn_predict<-predict(knn, eval_jumps_1987)
ellis_prob<-knn_predict[[ellis_ind]]
top_3<-order(-knn_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_jumps_1987)
top_3_prob<-knn_predict[order(-knn_predict)[1:3]]
#add rows to existing table to compare
models<-bind_rows(models,
                  tibble(Method="KNN",RMSE=knn_rmse,
                         "Ellis Vote Share"=ellis_prob,"Top 3 Candidates"=top_3,
                         "Top 3 Vote Shares"=top_3_prob))
models %>% knitr::kable()


#decision tree for 2020 winner
#seed needs to be set, since it a random partition
#check which complexity parameter gives lowest RMSE
#complexity param is minimum amount RMSE needs to improve by in order to add a new decision
set.seed(2,sample.kind = "Rounding")
rpart<-train(x=full_jumps_data[,!(names(full_jumps_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season",
                                      "Season","SeasID"))],
             y=full_jumps_data$`MIP Share Current Season`,
             method="rpart", tuneGrid = data.frame(cp=seq(0,0.02,0.001)),
             trControl=trainControl(method="cv",number=10,p=0.9))
rpart$bestTune


#show decision tree
rpart.plot(rpart$finalModel)


#decision tree performance on 1987 test set
rpart_rmse<-min(rpart$results[["RMSE"]])
rpart_predict<-predict(rpart, eval_jumps_1987)
ellis_prob<-rpart_predict[[ellis_ind]]
top_3<-order(-rpart_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_jumps_1987)
top_3_prob<-rpart_predict[order(-rpart_predict)[1:3]]
models<-bind_rows(models,
                  tibble(Method="Decision Tree",RMSE=rpart_rmse,
                         "Ellis Vote Share"=ellis_prob,"Top 3 Candidates"=top_3,
                         "Top 3 Vote Shares"=top_3_prob))
models %>% knitr::kable()


#ranger model for 2020 winner
#ranger is a random forest wrapper
#have to specify type of variable importance
#permutation shuffles a predictor column and sees how much the RMSE increases
#higher increase->worse prediction->more important to make accurate predictions
set.seed(1,sample.kind = "Rounding")
ranger<-train(x=full_jumps_data[,!(names(full_jumps_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season","G_diff",
                                      "GS_diff","Season","SeasID"))],
             y=full_jumps_data$`MIP Share Current Season`,
             method="ranger", importance="permutation",
             trControl=trainControl(method="cv",number=10,p=0.9))


#ranger top 5 most important variables
get_5_top_important(ranger)


#ranger performance on 1987 test set
ranger_rmse<-min(ranger$results[["RMSE"]])
ranger_predict<-predict(ranger, eval_jumps_1987)
ellis_prob<-ranger_predict[[ellis_ind]]
top_3<-order(-ranger_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_jumps_1987)
top_3_prob<-ranger_predict[order(-ranger_predict)[1:3]]
models<-bind_rows(models,
                  tibble(Method="Ranger",
                         RMSE=ranger_rmse,"Ellis Vote Share"=ellis_prob,
                         "Top 3 Candidates"=top_3,
                         "Top 3 Vote Shares"=top_3_prob))
models %>% knitr::kable()


#rborist model for 2020 winner
#rborist is another random forest wrapper
set.seed(25,sample.kind="Rounding")
rborist<-train(x=full_jumps_data[,!(names(full_jumps_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season","G_diff",
                                      "GS_diff","Season","SeasID"))],
             y=full_jumps_data$`MIP Share Current Season`,
             method="Rborist", importance="permutation",
             trControl=trainControl(method="cv",number=10,p=0.9))


#rborist top 5 most important variables
get_5_top_important(rborist)


#rborist performance on 1987 test set
rborist_rmse<-min(rborist$results[["RMSE"]])
rborist_predict<-predict(rborist, eval_jumps_1987)
ellis_prob<-rborist_predict[[ellis_ind]]
top_3<-order(-rborist_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_jumps_1987)
top_3_prob<-rborist_predict[order(-rborist_predict)[1:3]]
models<-bind_rows(models,
                  tibble(Method="Rborist",
                         RMSE=rborist_rmse,"Ellis Vote Share"=ellis_prob,
                         "Top 3 Candidates"=top_3,
                         "Top 3 Vote Shares"=top_3_prob))
models %>% knitr::kable()


#remove variables that are not used further in the code to free up space
rm(ellis_ind,ellis_prob,ellis_seas_id,lin_rmse,knn_rmse,rpart_rmse,ranger_rmse,rborist_rmse)
rm(linear_predict,knn_predict,rpart_predict,ranger_predict,rborist_predict)
rm(models)


#linear model 2021 candidates
linear_2021<-train(`MIP Share Next Season`~.-Season-`MIP Share Current Season`
                   -SeasID,
              data=full_data,method="lm",
              trControl=trainControl(method="cv",number=10,p=0.9))


#top 5 linear model most important variables
get_5_top_important(linear_2021)


#linear model performance on 1986 and 2019 test sets
lin2021_rmse<-linear_2021$results[["RMSE"]]
linear1986_predict<-predict(linear_2021,eval_1986)
linear2019_predict<-predict(linear_2021,eval_2019)
top_3<-order(-linear1986_predict)[1:3]
top_3_2019<-order(-linear2019_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_1986)
top_3_2019<-sapply(top_3_2019, get_top_vote_shares,eval_data=eval_2019)
top_3_prob<-linear1986_predict[order(-linear1986_predict)[1:3]]
top_3_prob_2019<-linear2019_predict[order(-linear2019_predict)[1:3]]
models_2021<-tibble(Method="Linear",RMSE=lin2021_rmse,
               "Top 3 Candidates for 1987"=top_3,
               "Top 3 Vote Shares for 1987"=top_3_prob,
               "Top 3 Candidates for 2020"=top_3_2019,
               "Top 3 Vote Shares for 2020"=top_3_prob_2019)
models_2021 %>% knitr::kable() %>%
  #scale down a table that is too wide
  kable_styling(latex_options="scale_down")


#knn model 2021 candidates
knn_2021<-train(`MIP Share Next Season`~.-Season-
                  `MIP Share Current Season`-SeasID,
           data=full_data,method="knn", tuneGrid=data.frame(k=seq(5,50)),
           trControl=trainControl(method="cv",number=10,p=0.9))
knn_2021$finalModel


#knn performance on 1986 and 2019 test sets
knn2021_rmse<-min(knn_2021$results[["RMSE"]])
knn1986_predict<-predict(knn_2021,eval_1986)
knn2019_predict<-predict(knn_2021,eval_2019)
top_3<-order(-knn1986_predict)[1:3]
top_3_2019<-order(-knn2019_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_1986)
top_3_2019<-sapply(top_3_2019, get_top_vote_shares,eval_data=eval_2019)
top_3_prob<-knn1986_predict[order(-knn1986_predict)[1:3]]
top_3_prob_2019<-knn2019_predict[order(-knn2019_predict)[1:3]]
models_2021<-bind_rows(models_2021,tibble(Method="KNN",RMSE=knn2021_rmse,
               "Top 3 Candidates for 1987"=top_3,
               "Top 3 Vote Shares for 1987"=top_3_prob,
               "Top 3 Candidates for 2020"=top_3_2019,
               "Top 3 Vote Shares for 2020"=top_3_prob_2019))
models_2021 %>% knitr::kable() %>%
  kable_styling(latex_options="scale_down")


#decision tree model 2021 candidates
set.seed(50,sample.kind = "Rounding")
rpart_2021<-train(x=full_data[,!(names(full_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season","Season",
                                      "SeasID"))],
             y=full_data$`MIP Share Next Season`,method="rpart", 
             tuneGrid = data.frame(cp=seq(0,0.01,0.001)),
             trControl=trainControl(method="cv",number=10,p=0.9))
rpart_2021$bestTune


#plot decision tree for 2021 candidates
rpart.plot(rpart_2021$finalModel)

#decision tree performance on 1986 and 2019 test sets
rpart2021_rmse<-min(rpart_2021$results[["RMSE"]])
rpart1986_predict<-predict(rpart_2021,eval_1986)
rpart2019_predict<-predict(rpart_2021,eval_2019)
top_3<-order(-rpart1986_predict)[1:3]
top_3_2019<-order(-rpart2019_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_1986)
top_3_2019<-sapply(top_3_2019, get_top_vote_shares,eval_data=eval_2019)
top_3_prob<-rpart1986_predict[order(-rpart1986_predict)[1:3]]
top_3_prob_2019<-rpart2019_predict[order(-rpart2019_predict)[1:3]]
models_2021<-bind_rows(models_2021,
                       tibble(Method="Decision Tree",RMSE=rpart2021_rmse,
               "Top 3 Candidates for 1987"=top_3,
               "Top 3 Vote Shares for 1987"=top_3_prob,
               "Top 3 Candidates for 2020"=top_3_2019,
               "Top 3 Vote Shares for 2020"=top_3_prob_2019))
models_2021 %>% knitr::kable() %>%
  kable_styling(latex_options="scale_down")


#ranger model 2021 candidates
set.seed(75,sample.kind = "Rounding")
ranger_2021<-train(x=full_data[,!(names(full_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season","Season",
                                      "SeasID"))],
             y=full_data$`MIP Share Next Season`,
             method="ranger", importance="permutation",
             trControl=trainControl(method="cv",number=10,p=0.9))


#ranger top 5 most important variables
get_5_top_important(ranger_2021)


#ranger performance on 1986 and 2019 test sets
ranger2021_rmse<-min(ranger_2021$results[["RMSE"]])
ranger1986_predict<-predict(ranger_2021,eval_1986)
ranger2019_predict<-predict(ranger_2021,eval_2019)
top_3<-order(-ranger1986_predict)[1:3]
top_3_2019<-order(-ranger2019_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_1986)
top_3_2019<-sapply(top_3_2019, get_top_vote_shares,eval_data=eval_2019)
top_3_prob<-ranger1986_predict[order(-ranger1986_predict)[1:3]]
top_3_prob_2019<-ranger2019_predict[order(-ranger2019_predict)[1:3]]
models_2021<-bind_rows(models_2021,
                       tibble(Method="Ranger",RMSE=ranger2021_rmse,
               "Top 3 Candidates for 1987"=top_3,
               "Top 3 Vote Shares for 1987"=top_3_prob,
               "Top 3 Candidates for 2020"=top_3_2019,
               "Top 3 Vote Shares for 2020"=top_3_prob_2019))
models_2021 %>% knitr::kable() %>%
  kable_styling(latex_options="scale_down")


#rborist model 2021 candidates
set.seed(100,sample.kind = "Rounding")
rborist_2021<-train(x=full_data[,!(names(full_data) %in% 
                                    c("MIP Share Next Season",
                                      "MIP Share Current Season","Season",
                                      "SeasID"))],
             y=full_data$`MIP Share Next Season`,
             method="Rborist", importance="permutation",
             trControl=trainControl(method="cv",number=10,p=0.9))


#rborist top 5 most important variables
get_5_top_important(rborist_2021)


#rborist performance on 1986 and 2019 test sets
rborist2021_rmse<-min(rborist_2021$results[["RMSE"]])
rborist1986_predict<-predict(rborist_2021,eval_1986)
rborist2019_predict<-predict(rborist_2021,eval_2019)
top_3<-order(-rborist1986_predict)[1:3]
top_3_2019<-order(-rborist2019_predict)[1:3]
top_3<-sapply(top_3, get_top_vote_shares,eval_data=eval_1986)
top_3_2019<-sapply(top_3_2019, get_top_vote_shares,eval_data=eval_2019)
top_3_prob<-rborist1986_predict[order(-rborist1986_predict)[1:3]]
top_3_prob_2019<-rborist2019_predict[order(-rborist2019_predict)[1:3]]
models_2021<-bind_rows(models_2021,
                       tibble(Method="Rborist",RMSE=rborist2021_rmse,
               "Top 3 Candidates for 1987"=top_3,
               "Top 3 Vote Shares for 1987"=top_3_prob,
               "Top 3 Candidates for 2020"=top_3_2019,
               "Top 3 Vote Shares for 2020"=top_3_prob_2019))
models_2021 %>% knitr::kable() %>%
  kable_styling(latex_options="scale_down")


#remove more variables not used in the future
rm(lin2021_rmse,knn2021_rmse,rpart2021_rmse,ranger2021_rmse,rborist2021_rmse)
rm(top_3,top_3_2019,top_3_prob,top_3_prob_2019,models_2021)
rm(linear1986_predict,knn1986_predict,rpart1986_predict,ranger1986_predict,rborist1986_predict,
   linear2019_predict,knn2019_predict,rpart2019_predict,ranger2019_predict,rborist2019_predict)

#predict 2020 winner
linear2020_predict<-predict(linear, eval_jumps_2020)
rpart2020_predict<-predict(rpart, eval_jumps_2020)
ranger2020_predict<-predict(ranger, eval_jumps_2020)
rborist2020_predict<-predict(rborist, eval_jumps_2020)
#store each model's predictions as column in tibble
predictions_2020<-tibble("Linear"=linear2020_predict,
                         "Decision Tree"=rpart2020_predict,
                         "Ranger"=ranger2020_predict,
                         "Rborist"=rborist2020_predict)
rm(linear2020_predict,rpart2020_predict,ranger2020_predict,rborist2020_predict)

#add a median+mean column
#median favors consistency, mean favors one or two large probabilities
predictions_2020<-predictions_2020 %>% 
  mutate("Median+Mean"=rowMedians(as.matrix(.))+rowMeans(.))

#tried to automate this process by creating a function to sapply over each column, but no luck
top_5<-order(-predictions_2020$Linear)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_jumps_2020)
top_5_prob<-predictions_2020$Linear[order(-predictions_2020$Linear)[1:5]]
winner2020<-tibble("Model"="Linear","Top 5 Candidates"=top_5,
                   "Top 5 Predicted Vote Shares"=top_5_prob)

top_5<-order(-predictions_2020$`Decision Tree`)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_jumps_2020)
top_5_prob<-predictions_2020$`Decision Tree`[order(
  -predictions_2020$`Decision Tree`)[1:5]]
winner2020<-bind_rows(winner2020,tibble
                      ("Model"="Decision Tree","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2020$Ranger)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_jumps_2020)
top_5_prob<-predictions_2020$Ranger[order(
  -predictions_2020$Ranger)[1:5]]
winner2020<-bind_rows(winner2020,tibble
                      ("Model"="Ranger","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2020$Rborist)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_jumps_2020)
top_5_prob<-predictions_2020$Rborist[order(
  -predictions_2020$Rborist)[1:5]]
winner2020<-bind_rows(winner2020,tibble
                      ("Model"="Rborist","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2020$`Median+Mean`)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_jumps_2020)
top_5_prob<-predictions_2020$`Median+Mean`[order(
  -predictions_2020$`Median+Mean`)[1:5]]
winner2020<-bind_rows(winner2020,tibble
                      ("Model"="Median+Mean","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))
#print model results
knitr::kable(winner2020[1:20,],align="c") %>% 
  kable_styling(position="center") %>%
  #bold header row
  row_spec(0,bold=T) %>%
  #put a double line after the end of every model
  row_spec(seq(5,20,5),hline_after=T) %>%
  #set widths to match with composite
  column_spec(1,width="7em") %>%
  column_spec(2,width="11em") %>%
  column_spec(3,width="15em")
#print composite result
knitr::kable(winner2020[21:25,],align="c") %>%
  kable_styling(position="center") %>%
  row_spec(0,bold=T) %>%
  column_spec(1,width="7em") %>%
  column_spec(2,width="11em") %>%
  column_spec(3,width="15em")


#predict 2021 candidates
linear2021_predict<-predict(linear_2021, eval_2020)
knn2021_predict<-predict(knn_2021,eval_2020)
rpart2021_predict<-predict(rpart_2021, eval_2020)
ranger2021_predict<-predict(ranger_2021, eval_2020)
rborist2021_predict<-predict(rborist_2021, eval_2020)
predictions_2021<-tibble("Linear"=linear2021_predict,
                         "KNN"=knn2021_predict,
                         "Decision Tree"=rpart2021_predict,
                         "Ranger"=ranger2021_predict,
                         "Rborist"=rborist2021_predict)
rm(linear2021_predict,knn2021_predict,rpart2021_predict,ranger2021_predict,
   rborist2021_predict)
predictions_2021<-predictions_2021 %>% 
  mutate("Median+Mean"=rowMedians(as.matrix(.))+rowMeans(.))

top_5<-order(-predictions_2021$Linear)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$Linear[order(-predictions_2021$Linear)[1:5]]
winner2021<-tibble("Model"="Linear","Top 5 Candidates"=top_5,
                   "Top 5 Predicted Vote Shares"=top_5_prob)

top_5<-order(-predictions_2021$KNN)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$KNN[order(-predictions_2021$KNN)[1:5]]
winner2021<-bind_rows(winner2021,tibble("Model"="KNN","Top 5 Candidates"=top_5,
                   "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2021$`Decision Tree`)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$`Decision Tree`[order(
  -predictions_2021$`Decision Tree`)[1:5]]
winner2021<-bind_rows(winner2021,tibble
                      ("Model"="Decision Tree","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2021$Ranger)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$Ranger[order(
  -predictions_2021$Ranger)[1:5]]
winner2021<-bind_rows(winner2021,tibble
                      ("Model"="Ranger","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2021$Rborist)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$Rborist[order(
  -predictions_2021$Rborist)[1:5]]
winner2021<-bind_rows(winner2021,tibble
                      ("Model"="Rborist","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

top_5<-order(-predictions_2021$`Median+Mean`)[1:5]
top_5<-sapply(top_5, get_top_vote_shares,eval_data=eval_2020)
top_5_prob<-predictions_2021$`Median+Mean`[order(
  -predictions_2021$`Median+Mean`)[1:5]]
winner2021<-bind_rows(winner2021,tibble
                      ("Model"="Median+Mean","Top 5 Candidates"=top_5,
                        "Top 5 Predicted Vote Shares"=top_5_prob))

knitr::kable(winner2021[1:25,],align = "c") %>%
  kable_styling(position="center") %>%
  row_spec(0,bold=T) %>%
  row_spec(seq(5,25,5),hline_after=T) %>%
  column_spec(1,width="7em") %>%
  column_spec(2,width="11em") %>%
  column_spec(3,width="15em")
knitr::kable(winner2021[26:30,],align = "c") %>%
  kable_styling(position="center") %>%
  row_spec(0,bold=T) %>%
  column_spec(1,width="7em") %>%
  column_spec(2,width="11em") %>%
  column_spec(3,width="15em")

