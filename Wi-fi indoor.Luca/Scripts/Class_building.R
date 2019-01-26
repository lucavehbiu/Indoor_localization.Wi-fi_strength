#Classify BuildingID
valid_building <- logged.and.scaled_valid %>% 
  select(-c( FLOOR, LONGITUDE, LATITUDE)) #select the right variables for confusion matrix

valid_building$BUILDINGID <- as.factor(valid_building$BUILDINGID) #convert to a factor

data <- logged.and.scaled
data$BUILDINGID <- as.factor(data$BUILDINGID) #repeat the process for training set as well


#run classifiers
set.seed(1616)
trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 2)
kgrid <- expand.grid(k = c(1:2))


knn_fit_floors  <- train(BUILDINGID~ ., data = data %>% 
                           select(starts_with("WAP"), BUILDINGID),
                         method = 'knn', trControl = trctrl)

gbm_fit_floors <- train(BUILDINGID~ ., data = data %>% 
                          select(starts_with("WAP"), BUILDINGID), method = 'gbm', trControl = trctrl)


svm_fit_floors <- train(BUILDINGID~ ., data = data %>% 
                          select(starts_with("WAP"), BUILDINGID), method = 'svmLinear', trControl = trctrl)


#boxplot the result based on kappa and accuracy
results <- resamples(list("k-NN"= knn_fit_floors, 
                          SVM = svm_fit_floors,
                          GBM = gbm_fit_floors))
bwplot(results) 


#knn confusion matrix
class_pred <- predict(knn_fit_floors, newdata = valid_building)

confusion_matrix <- as.data.frame(table(class_pred, valid_building$BUILDINGID))

g <- ggplot(data = confusion_matrix,
            mapping = aes(x = class_pred,
                          y = confusion_matrix$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs(x = "Predictions", y = "Actual Values") +
  theme(legend.position = "none")


#gbm confusion matrix
class_pred2 <- predict(gbm_fit_floors, newdata = valid_building)

confusion_matrix2 <- as.data.frame(table(class_pred2, valid_building$BUILDINGID))


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

#svm confusion matrix
class_pred3 <- predict(svm_fit_floors, newdata = valid_building)

confusionMatrix(class_pred3, valid_building$BUILDINGID)

confusion_matrix3 <- as.data.frame(table(class_pred3, valid_building$BUILDINGID))


g3 <- ggplot(data = confusion_matrix3,
             mapping = aes(x = class_pred3,
                           y = confusion_matrix3$Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%2.0f", Freq)), vjust = 2) +
  scale_fill_gradient(low = "lightblue",
                      high = "orange",
                      trans = "log") +
  labs( x = "Predictions", y = "Actual Values") +
  theme(legend.position = "none") 

grid.arrange(g, g2, g3)
summary(gbm_fit_floors)






