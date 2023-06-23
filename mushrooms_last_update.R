library(readr)

library(funModeling)
mushrooms <- read_csv("C:/Users/Lenovo/Desktop/Fourth-level/2ndSemester/Big-Data-Analytics/project/mushrooms.csv")
#mushrooms<-data.frame(mushrooms)
mushrooms

###########preprocessing of data###############
##1- checking null
is.null(mushrooms) 
#mushrooms~cap-shape

cross_plot(data = mushrooms, 
           input = c("population", "habitat","spore.print.color","cap.color", "odor"), target = "class")

## 2- apply label encoding
dataset <- mushrooms
class(mushrooms$`cap-shape`)

cols <- sapply(mushrooms, is.logical)
mushrooms[,cols] <- lapply(mushrooms[,cols], as.numeric)
mushrooms

mushrooms$`cap-shape` <- as.numeric(factor(mushrooms$`cap-shape`))

#view updated data frame

mushrooms$`cap-shape`

mushrooms$`cap-surface` <- as.numeric(factor(mushrooms$`cap-surface`))

#view updated data frame

mushrooms$`cap-surface`

mushrooms$class <- as.numeric(factor(mushrooms$class))
#++++++++++++++++++++++++
mushrooms$class <- as.numeric(mushrooms$class)
mushrooms$class
#+++++++++++++++++++
#view updated data frame
mushrooms$class

mushrooms$`cap-color` <- as.numeric(factor(mushrooms$`cap-color`))

#view updated data frame
mushrooms$`cap-color`


mushrooms$odor <- as.numeric(factor(mushrooms$odor))

#view updated data frame
mushrooms$odor

mushrooms$`gill-attachment` <- as.numeric(factor(mushrooms$`gill-attachment`))

#view updated data frame
mushrooms$`gill-attachment`

mushrooms$`gill-spacing` <- as.numeric(factor(mushrooms$`gill-spacing`))

#view updated data frame
mushrooms$`gill-spacing`

mushrooms$`gill-size` <- as.numeric(factor(mushrooms$`gill-size`))

#view updated data frame
mushrooms$`gill-size`

mushrooms$`gill-color` <- as.numeric(factor(mushrooms$`gill-color`))

#view updated data frame
mushrooms$`gill-color`


mushrooms$`stalk-shape` <- as.numeric(factor(mushrooms$`stalk-shape`))

#view updated data frame
mushrooms$`stalk-shape`

#habitat
mushrooms$habitat <- as.numeric(factor(mushrooms$habitat))
mushrooms$habitat

#population
mushrooms$population <- as.numeric(factor(mushrooms$population))
mushrooms$population

mushrooms$`spore-print-color` <- as.numeric(factor(mushrooms$`spore-print-color`))
mushrooms$`spore-print-color`

mushrooms$`ring-type` <- as.numeric(factor(mushrooms$`ring-type`))
mushrooms$`ring-type`

mushrooms$`ring-number` <- as.numeric(factor(mushrooms$`ring-number`))
mushrooms$`ring-number`

mushrooms$`veil-color` <- as.numeric(factor(mushrooms$`veil-color`))
mushrooms$`veil-color`

mushrooms$`veil-type` <- as.numeric(factor(mushrooms$`veil-type`))
mushrooms$`veil-type`

mushrooms$`stalk-color-below-ring` <- as.numeric(factor(mushrooms$`stalk-color-below-ring`))
mushrooms$`stalk-color-below-ring`

mushrooms$`stalk-surface-above-ring` <- as.numeric(factor(mushrooms$`stalk-surface-above-ring`))
mushrooms$`stalk-surface-above-ring`

mushrooms$`stalk-surface-below-ring` <- as.numeric(factor(mushrooms$`stalk-surface-below-ring`))
mushrooms$`stalk-surface-below-ring`

mushrooms$`stalk-root` <- as.numeric(factor(mushrooms$`stalk-root`))
mushrooms$`stalk-root`

mushrooms$`stalk-color-above-ring` <- as.numeric(factor(mushrooms$`stalk-color-above-ring`))
mushrooms$`stalk-color-above-ring`


mushrooms

#after checking all unique value to remove the duplicte we found that "viel-type" contains only one value

for(col in 1:ncol(mushrooms)){
  transpose <- t(t(mushrooms[,col]))
  if (length(unique(transpose)) < 2){
    mushrooms <- mushrooms[,-col]
    
  }
}
mushrooms


res <- cor(mushrooms)
res
df <- data.frame(res)

# library("writexl")
# write_xlsx(df, "D://big")

#converting some values 

mushrooms$class[mushrooms$class == 1] <- 0
mushrooms$class[mushrooms$class == 2] <- 1
mushrooms

library("Hmisc")
df<-data.frame(mushrooms)
#Matrix of Correlations and P-values

res2 <- rcorr(as.matrix(mushrooms))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  #to get the upper part of the corr matrix to remove the duplicated values.
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

library(Hmisc)
res2<-rcorr(as.matrix(df[,1:18]))
final <-flattenCorrMatrix(res2$r, res2$P)
final
# res$r
# x<-upper.tri()
# x
#library("writexl")
#write_xlsx(final, "C:/Users/Lenovo/Desktop/Fourth-level/2ndSemester/Big-Data-Analytics/project/P-value.xlsx")
#write_xlsx(final, "C:/Users/Lenovo/Desktop/Fourth-level/2ndSemester/Big-Data-Analytics/project/P-value2.xlsx")
#+++++++++++++++++++++++++++

#removing some unwanted columns due  to low correlations between them and the rest of data

mushrooms$`stalk-surface-above-ring`<-NULL
mushrooms$`stalk-surface-below-ring`<-NULL
mushrooms$`veil-color`<-NULL
mushrooms$`stalk-root`<-NULL


#checking if there is still existing or not
mushrooms$`veil-type`
mushrooms$`stalk-surface-above-ring`
mushrooms$`stalk-surface-below-ring`
mushrooms$`veil-color`
mushrooms$`stalk-root`

#++++++++++++++++++++++++++++

hist(mushrooms$`stalk-color-above-ring` ,xlab = "stalk-color-above-ring values", main = "frequency of stalk-color-above-ring attribute")
hist(mushrooms$`stalk-color-below-ring`,xlab = "stalk-color-below-ring values", main = "frequency of stalk-color-below-ring attribute")
#++++++++++++++++++++++++++++
boxplot(mushrooms$`stalk-color-above-ring`,mushrooms$`stalk-color-below-ring`,col=c('red','pink'))
title("stalk-color-above-ring vs. stalk-color-below-ring")
#++++++++++++++++++++++++++++++++++++

boxplot(mushrooms$population,mushrooms$habitat,col=c('blue','green'))
title("population vs. habitat")

#++++++++++++++++++
#after trying this variables in hypothesis testing 
#we find that it is alternative hypothesis
#t = 90.47, df = 16246, p-value < 2.2e-16

x <-mushrooms$population
y <-mushrooms$habitat

hyp<-t.test(x, y,var.equal = TRUE)
hyp

result <- qt(p = 0.05/2,df =16246,lower.tail = FALSE)
result
# t-value is greater than the result so that doesn't satisfy our condition which is
#qt < t-value -> null hypothesis is not rejected.

x2 <-mushrooms$`stalk-color-above-ring`
y2 <-mushrooms$`stalk-color-below-ring`
hyp2<-t.test(x2, y2,var.equal = TRUE)
hyp2
result2 <- qt(p = 0.05/2,df =16246,lower.tail = FALSE)
result2
#  0.72498 < 1.96011 true 

#++++++++++++++++++++++++++++++++
mushrooms$`stalk-color-above-ring`<- NULL

mushrooms$`stalk-color-above-ring`
#+++++++++++++++++++++++++
# Load the caret package
library(caTools)
set.seed(123)

sample <- sample.split(mushrooms$class,SplitRatio = 0.8)

# creating training dataset
train_dataset  <- subset(mushrooms,split = TRUE)

# creating testing dataset
test_dataset  <- subset(mushrooms,split = FALSE)

model <- glm(class ~ . , data = train_dataset, family = "binomial")
predictions <- predict(model, newdata = test_dataset, type = "response")

# Calculate accuracy of predictions
actual_results <- test_dataset$class
predicted_results <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_results == actual_results)
accuracy #0.9475628
#==========================



