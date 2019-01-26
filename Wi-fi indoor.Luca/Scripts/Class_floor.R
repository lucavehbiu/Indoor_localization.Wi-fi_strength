#Classification for floors
data <- logged.and.scaled %>% 
  dplyr:: select(-c(WAP012, WAP069, WAP096, WAP097, WAP102, WAP110, 
                    WAP261, WAP262, WAP278, WAP282, WAP313, WAP314, 
                    WAP316, WAP329, WAP334, WAP340, WAP344, WAP351)) %>% 
  filter(BUILDINGID == 2)
data$FLOOR <- factor(data$FLOOR) #convert FLOOR to a factor

valid_floors <- logged.and.scaled_valid %>%   
  dplyr:: select(-c(WAP351, WAP012, WAP069, WAP096, WAP097, WAP102, 
                    WAP110, WAP261, WAP262, WAP278, WAP282, WAP313, 
                    WAP314, WAP316 , WAP329, WAP334, WAP340, WAP344)) %>% 
  filter(BUILDINGID == 2)#select the right variables for confusion matrix

valid_floors$FLOOR <- as.factor(valid_floors$FLOOR)

#run classifiers
set.seed(1616)
trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)
kgrid <- expand.grid(k = c(1:10))

knn_fit_floors  <- train(FLOOR~ ., data = data %>% 
                           select(starts_with("WAP"), FLOOR),
                         method = 'knn', trControl = trctrl,
                         tuneGrid = kgrid)

gbm_fit_floors <- train(FLOOR~ ., data = data %>% 
                          dplyr::select(starts_with("WAP"), FLOOR), 
                        method = 'gbm', 
                        trControl = trctrl)

svm_fit_floors <- train(FLOOR~ ., data = data %>% 
                          dplyr:: select(starts_with("WAP"), FLOOR), 
                        method = 'svmLinear3', 
                        trControl = trctrl)


# display results
results <- resamples(list('k-nn' = knn_fit_floors, 
                          SVM = svm_fit_floors,
                          GBM = gbm_fit_floors))
bwplot(results) #boxplot the results based on kappa and accuracy




#SVM confusion matrix
class_pred3 <- predict(svm_fit_floors, newdata = valid_floors)
confusionMatrix(class_pred3, valid_floors$FLOOR)


confusion_matrix3 <- as.data.frame(table(class_pred3, valid_floors$FLOOR))


g3 <- ggplot(data = confusion_matrix3,
             mapping = aes(x = class_pred3,
                           y = confusion_matrix3$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs( title = "Confusion Matrix - svmLinear Building Classification (no scale, B.ID ==1 out)", x = "Predictions", y = "Actual Values") +
  theme(legend.position = "none")




#random forest
class_pred <- predict(knn_fit_floors, newdata = valid_floors)
confusionMatrix(class_pred, valid_floors$FLOOR)

confusion_matrix <- as.data.frame(table(class_pred, valid_floors$FLOOR))

g <- ggplot(data = confusion_matrix,
            mapping = aes(x = class_pred,
                          y = confusion_matrix$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs( title = "Confusion Matrix - k-NN 1, FLoor classification",
        x = "Predictions", y = "Actual Values") +
  theme(legend.position = "none")


#gbm
class_pred2 <- predict(gbm_fit_floors, newdata = valid_floors)
confusionMatrix(class_pred2, valid_floors$FLOOR)


confusion_matrix2 <- as.data.frame(table(class_pred2, valid_floors$FLOOR))


g2 <- ggplot(data = confusion_matrix2,
             mapping = aes(x = class_pred2,
                           y = confusion_matrix2$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs( title = "Confusion Matrix - GBM", x = "Predictions", y = "Actual Values") +
  theme(legend.position = "none")




