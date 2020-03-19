# naive

# Importing the dataset


dataset = read.csv('gender.csv')


# Encoding the target feature as factor
dataset$person= factor(dataset$person,levels = c("male", "female"),labels = c(1,2))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
training_set = subset(dataset, split == TRUE)
set.seed(123)
split = sample.split(dataset$person, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Fitting naive to the Training set
library(naivebayes)

classifier=naive_bayes(person~Height+Weight+Footsize,usekernel = T,data=training_set)
# Create your classifier here

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-4])

# Making the Confusion Matrix
cm = table(test_set[,4], y_pred)

#build data frame with unseen data to predict 
mydata <- data.frame(Height=numeric(0), Weight=numeric(0), Footsize=numeric(0))
mydata <- fix(mydata)
mydata$newprediction=predict(classifier, newdata =mydata)


summary(classifier)
print(classifier)
table(training_set$person) %>% prop.table()
table(test_set$person) %>% prop.table()

h2o.confusionMatrix(classifier)

training_set %>%
  filter(person == 2) %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

train %>% 
  select(Height,Weight,Footsize) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

confusionMatrix(y_pred,test_set[,4])
