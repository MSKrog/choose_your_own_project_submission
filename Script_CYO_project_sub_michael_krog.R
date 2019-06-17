
## Downloading and cleaning the data

#Load the necessary packages:
library(tidyverse)
library(caret)

#Load the CSV data file:
video_games_full <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


#Cleaning the data set:
#Remove NA's:
video_games_full <- video_games_full %>% filter(!is.na(video_games_full$Critic_Score))
video_games_full <- video_games_full %>% filter(!is.na(video_games_full$User_Score))
video_games_full <- video_games_full %>% filter(!is.na(video_games_full$Critic_Count))
video_games_full <- video_games_full %>% filter(!is.na(video_games_full$User_Count))



#Changing user score columns to numeric from character:
video_games_full$User_Score <- as.numeric(video_games_full$User_Score)

#Confirming class of column
class(video_games_full$User_Score)






## Data Exploration:


#Count of the number of rows & columns
nrow(video_games_full)
ncol(video_games_full)


#Number of user scores below or equal to six, and above or equal to seven
length(which(video_games_full$User_Score <= 6))
length(which(video_games_full$User_Score >= 7))


#Number of different games:
diff_games <- unique(video_games_full$Name)
length(diff_games)


#Number of different platforms:
n_distinct(video_games_full$Platform)


#Number of different publishers:
n_distinct(video_games_full$Publisher)


#Avg score per platform:
#Create vector 'plat_ratings'
plat_ratings <- video_games_full %>%
  #Pipe from video_games_full and group by platform.
  group_by(Platform) %>%
  #Summarise avg_rating as the mean user score.
  summarise(avg_rating = mean(User_Score)) %>%
  #Arrange in descending order based on avg_rating.
  arrange(desc(avg_rating))

#Plot platform_ratings in descending order as a bar groph, with Platforms as x, and avg rating as y.
ggplot(plat_ratings, aes(x= reorder(Platform, -avg_rating), y= as.factor(avg_rating), fill = Platform)) +
  #bar graph.
  geom_bar(stat = "identity") + 
  #Classic theme
  theme_classic() +
  #A hue fill
  scale_fill_hue(c = 40) +
  #Adjust axis text to 90 degrees.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

  
#Looking at number of games per platform:
#Create vector title_count_per_platform piped from full data set.
title_count_per_platform <- video_games_full %>%
  #Group by platform.
  group_by(Platform) %>%
  #summarise the count.
  summarise(number = n()) %>%
  #Arrange in descending order.
  arrange(desc(number))

#Plot title_count_per_platform in descending order.
ggplot(title_count_per_platform, aes(x= reorder(Platform, -number), y= as.factor(number), fill = Platform)) +
  #As a bar plot.
  geom_bar(stat = "identity") + 
  #Classic theme
  theme_classic() +
  #Hue fill
  scale_fill_hue(c = 40) +
  #Adjust text angle to 90 degrees.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#Create vector avg_score_count_per_platfrom piped from full data set.
avg_score_count_per_plat <- video_games_full %>%
  #Group by platform.
  group_by(Platform) %>%
  #Summarise by average score count of users, and of critics.
  summarise(avg_score_count_user = mean(User_Count), avg_score_count_critic = mean(Critic_Count)) %>%
  #Arrange in descending order based on avg_score_count_user.
  arrange(desc(avg_score_count_user))


#Plot average score count per platform.
ggplot(avg_score_count_per_plat) +
  #Segment by platform (user and critic avg scores will be plotted with a black line)
  geom_segment( aes(x=Platform, xend=Platform, y=avg_score_count_user, yend=avg_score_count_critic), color="black") +
  #avg_user_score_counts plotted in green.
  geom_point( aes(x=Platform, y=avg_score_count_user), color=rgb(0.2,0.7,0.1,0.5), size=3, show.legend = TRUE) +
  #avg critic score counts plotted in red.
  geom_point( aes(x=Platform, y=avg_score_count_critic), color=rgb(0.7,0.2,0.1,0.5), size=3, show.legend = TRUE) +
  #Flip x & y.
  coord_flip()+
  #Light theme.
  theme_light() +
  #Legend position at bottom.
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
  ) +
  # X label.
  xlab("Platform") +
  # Y label.
  ylab("Avg score count per plat - User VS Critic") +
  #Scale colour.
  scale_color_manual(name = "Year", labels = c("critic", "user"))



#Avg count of scores users:
#Plot average score count per platform in descending order.
ggplot(avg_score_count_per_plat, aes(x= reorder(Platform, -avg_score_count_user), y= (avg_score_count_user), fill = Platform)) +
  #Create a bar plot
  geom_bar(stat = "identity") + 
  #Use 'classic' theme.
  theme_classic() +
  #Use a hue fill.
  scale_fill_hue(c = 40) +
  #Rotate axis text 90 degrees.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




#Avg score count critic:
#Plot average score count per platform in descending order.
ggplot(avg_score_count_per_plat, aes(x= reorder(Platform, -avg_score_count_critic), y= (avg_score_count_critic), fill = Platform)) +
  #Create a bar plot.
  geom_bar(stat = "identity") + 
  #Classic theme.
  theme_classic() +
  #Hue scale colour fill.
  scale_fill_hue(c = 40) +
  #Rotate label text 90 degrees.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



#Plat critic score against the log2 of global sales.
ggplot(video_games_full, aes(x=Critic_Score, y= log2(Global_Sales))) +
  #Make points blue with black border, circle size 2.
  geom_point(
    color="black",
    fill="blue",
    shape=21,
    alpha=0.5,
    size=2,
    stroke = 1
  )






#Total sales value North America:
sum(video_games_full$NA_Sales)


#Total sales value Europe:
sum(video_games_full$EU_Sales)


#Total sales value Japan:
sum(video_games_full$JP_Sales)


#Total sales value other regions:
sum(video_games_full$Other_Sales)





#Count of video games with global sales bigger than or equal to 6.08
plyr::count(video_games_full$Global_Sales >= 6.08)



#Count of video games with global sales smaller than or equal to 6.08 & bigger than or equal to 1.279
plyr::count(video_games_full$Global_Sales <= 6.08 & video_games_full$Global_Sales >= 1.279)



#Create column 'performance', if global sales bigger than or equal to 6.08 = "top_hundred".
video_games_full$performance <- ifelse(video_games_full$Global_Sales >= 6.08,"top_hundred",
                                       #If smaller than 6.08 or bigger than or equal to 1.279 = "top_thousand".
                                       ifelse(video_games_full$Global_Sales < 6.08 & video_games_full$Global_Sales >= 1.279,"top_thousand",
                                              #If smaller than 1.279 = "thousand_plus" otherwise "other".
                                              ifelse(video_games_full$Global_Sales < 1.279, "thousand_plus", "other"
                                              )))

#Remove unnecessary columns:
video_games_full = subset(video_games_full, select = -c(Name, Platform, Year_of_Release, Genre, Publisher, Critic_Score, Critic_Count, User_Score, User_Count, Developer, Rating, Global_Sales) )







#Creating test and training sets:
set.seed(1)
index_video_games <- createDataPartition(video_games_full$Other_Sales, times = 1, p = 0.2, list = FALSE)

train_set_vg <- video_games_full[index_video_games, ]
test_set_vg <- video_games_full[-index_video_games, ]  





# Creating the KNN prediction model.



#Looking at a table of the performance factors:
table(train_set_vg$performance)

#Making sure the performance column is a factor:
train_set_vg[["performance"]] = factor(train_set_vg[["performance"]])

#Creating the model using trainControl from Caret package.
#Using repeated cross validation method for re-sampling.
#Creating 10 re-sampling iterations (folds)
#Using 3 repeats.
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#Setting the seed:
set.seed(3)
#Using KNN method setting the target variable 'performance' using all classifiers.
knn_fit <- train(performance ~., data = train_set_vg, method = "knn",
#trControl passed with results of the trainControl method.
 trControl=train_control,
#Pre-processing the data - centering & scaling.
#Create mean value approximately '0' and standard deviation '1'
 preProcess = c("center", "scale"),
#Tune the algorithm
 tuneLength = 10)

#See results in knn_fit:
knn_fit



# Running our KNN Algorithm on the test set

#We are now ready to run the algorith on the test set.


#Creating test_pred vector using 'predict'
test_pred <- predict(knn_fit, newdata = test_set_vg)

#Make sure that 'perfomance' is a factor before running the confusion matrix.
test_set_vg[["performance"]] = factor(test_set_vg[["performance"]])

#Run the confusion matrix of predictions vs actuals:
confusionMatrix(test_pred, test_set_vg$performance)


