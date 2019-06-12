#####------------------------Machine Learning approach using CRF(Conditional Random Fields) ------------------------#####

#install the packages
install.packages("crfsuite")
library(crfsuite)
library(data.table)
library(caret)
library(lattice)
if (!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")

#Reading the tokenised files of CNN and Fox
CNN_details = read.csv("CNN_details.csv",stringsAsFactors = FALSE)
fox_details = read.csv("fox_details.csv",stringsAsFactors = FALSE)

CNN_details = CNN_details[,-1]
fox_details = CNN_details[,-1]

#Extracting only the columns like docid,tokens,upos and entity
CRF_CNN = CNN_details[,c(1,6,8,15)]

#Viewing the structure
str(CRF_CNN)

CRF_CNN$Entity = as.character(CRF_CNN$Entity)

#Changing the value of not relavant entity to CNN
CRF_CNN$Entity[!(CRF_CNN$Entity %in% c("Organization", "Religion", "Personality","Hobbies","Profession"))] = 'Irrelavant'

#Model building using CNN data

# Calculating the previous and the next token values and UPOS values for building the model
CRF_model1_CNN = as.data.table(CRF_CNN)
CRF_model1_CNN <- CRF_model1_CNN[, pos_previous   := shift(upos, n = 1, type = "lag"), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, pos_next       := shift(upos, n = 1, type = "lead"), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, token_previous := shift(token, n = 1, type = "lag"), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, token_next     := shift(token, n = 1, type = "lead"), by = list(doc_id)]

CRF_model1_CNN <- CRF_model1_CNN[, pos_previous   := txt_sprintf("pos[w-1]=%s", pos_previous), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, pos_next       := txt_sprintf("pos[w+1]=%s", pos_next), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, token_previous := txt_sprintf("token[w-1]=%s", token_previous), by = list(doc_id)]
CRF_model1_CNN <- CRF_model1_CNN[, token_next     := txt_sprintf("token[w-1]=%s", token_next), by = list(doc_id)]

CRF_model1_CNN = as.data.frame(CRF_model1_CNN)

#Dividing the data into training and validationtest set in CNN

CRF_model_train = sample(1:nrow(CRF_model1_CNN), 0.8 * nrow(CRF_model1_CNN))
CRF_model_test <- setdiff(1:nrow(CRF_model1_CNN), CRF_model_train)

CRF_model_train = CRF_model1_CNN[CRF_model_train,]
CRF_model_test = CRF_model1_CNN[CRF_model_test,]

#Saving the train and tess file
write.csv(CRF_model_train,file = "CRF_model_train.csv")
write.csv(CRF_model_test,file = "CRF_model_test.csv")

#Training the model using Test

model <- crf(y = CRF_model_train$Entity, 
             x = CRF_model_train[, c("upos", "pos_previous", "pos_next", 
                                     "token", "token_previous", "token_next")], 
             group = CRF_model_train$doc_id, 
             method = "lbfgs",
             options = list(max_iterations = 35))

stats = summary(model)

#Evaluating the loss function
plot(stats$iterations$loss, pch = 20, type = "b", 
     main = "CRF Loss evolution", xlab = "Iteration", ylab = "Loss")

#Evaluating the model validation data in CNN

scores <- predict(model, 
                  newdata = CRF_model_test[, c("upos", "pos_previous", "pos_next", 
                                               "token", "token_previous", "token_next")], 
                  group = CRF_model_test$doc_id)
CRF_model_test$label <- scores$label


#creating the confusion matrix and other metrics for evaluation

CRF_model_test1 = CRF_model_test

#Factorising the entity values which are character
CRF_model_test1$Entity = as.factor(CRF_model_test1$Entity)
CRF_model_test1$label = as.factor(CRF_model_test1$label)

#removing the factor 'religion' as it is not present in the predicted label
CRF_model_test1 = CRF_model_test1[which(CRF_model_test1$Entity != 'Religion'),]

#Checking the unique factors
table(CRF_model_test1$Entity)
table(CRF_model_test1$label)

#Saving the file
write.csv(CRF_model_test1,file = "CRF_model_test1.csv")

#Checking the confusion matrix
result = confusionMatrix(CRF_model_test1$label, CRF_model_test1$Entity, mode = "prec_recall")

#Viewing the metrics
view(result)

#Evaluting the model on the Fox news data which is unlabelled.

#Create the POS previous and next values along with the token previous and next values for Fox data

CRF_Fox = fox_details[,c(2,7,9)]


CRF_Fox = as.data.table(CRF_Fox)
CRF_Fox <- CRF_Fox[, pos_previous   := shift(upos, n = 1, type = "lag"), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, pos_next       := shift(upos, n = 1, type = "lead"), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, token_previous := shift(token, n = 1, type = "lag"), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, token_next     := shift(token, n = 1, type = "lead"), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, pos_previous   := txt_sprintf("pos[w-1]=%s", pos_previous), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, pos_next       := txt_sprintf("pos[w+1]=%s", pos_next), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, token_previous := txt_sprintf("token[w-1]=%s", token_previous), by = list(doc_id)]
CRF_Fox <- CRF_Fox[, token_next     := txt_sprintf("token[w-1]=%s", token_next), by = list(doc_id)]

CRF_Fox = as.data.frame(CRF_Fox)


#Predicting the Entities for Fox news.
scores_fox = predict(model, 
                     newdata = CRF_Fox[, c("upos", "pos_previous", "pos_next", 
                                           "token", "token_previous", "token_next")], 
                     group = CRF_Fox$doc_id)

#Combining the predicted values for Fox with the original table
Fox_CRF_Predicted_Entity = cbind(CRF_Fox,scores_fox)

Fox_CRF_Predicted_Entity = Fox_CRF_Predicted_Entity[,c(1,2,3,8)]

#Checking the number of predicted values for each entity
table(Fox_CRF_Predicted_Entity$label)

#Saving the file
write.csv(Fox_CRF_Predicted_Entity,file = "Fox_CRF_Predicted_Entity.csv")

#Checking if the values have been tagged properly by looking at top values in every entity mapping

#Profession
stats <- subset(Fox_CRF_Predicted_Entity, label %in% c("Profession")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Predicted Profession in Fox news", xlab = "Freq")

#Personality
stats1 <- subset(Fox_CRF_Predicted_Entity, label %in% c("Personality")) 
stats1 <- txt_freq(stats1$token)
stats1$key <- factor(stats1$key, levels = rev(stats1$key))
barchart(key ~ freq, data = head(stats1, 20), col = "cadetblue", 
         main = "Predicted Personality in Fox news", xlab = "Freq")

#Organisation
stats2 <- subset(Fox_CRF_Predicted_Entity, label %in% c("Organization")) 
stats2 <- txt_freq(stats2$token)
stats2$key <- factor(stats2$key, levels = rev(stats2$key))
barchart(key ~ freq, data = head(stats2, 20), col = "cadetblue", 
         main = "Predicted Organisation in Fox news", xlab = "Freq")

#Hobbies
stats3 <- subset(Fox_CRF_Predicted_Entity, label %in% c("Hobbies")) 
stats3 <- txt_freq(stats3$token)
stats3$key <- factor(stats3$key, levels = rev(stats3$key))
barchart(key ~ freq, data = head(stats3, 20), col = "cadetblue", 
         main = "Predicted Hobbies in Fox news", xlab = "Freq")

#################################################################################################
