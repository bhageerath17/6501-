
## Bhageerath Bogi-ISYE 6501-OAN-Homework-1

#install.packages("kernlab")


## Question 2.2.1

library(kernlab)
rm(list = ls())
set.seed(1987)

ccdata_1 <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

head(ccdata_1)
tail(ccdata_1)


ccmodel_1 <- ksvm(as.matrix(ccdata_1[,1:10]),as.factor(ccdata_1[,11]),
                type = "C-svc",
                kernel = "vanilladot",
                C = 1000,
                scaled = TRUE)


attributes(ccmodel_1)

a <- colSums(ccmodel_1@xmatrix[[1]]*ccmodel_1@coef[[1]])
a0 <-  -ccmodel_1@b

a
a0

ccmodel_1


pred1 <- predict(ccmodel_1,ccdata_1[,1:10])

sum(pred1 == ccdata_1[,11])/nrow(ccdata_1)


## Question 2.2.2
## Used a non linear kernel - rbfdot

library(kernlab)


ccdata_2 <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

head(ccdata_2)
tail(ccdata_2)


ccmodel_2 <- ksvm(as.matrix(ccdata_2[,1:10]),as.factor(ccdata_2[,11]),
                type = "C-svc",
                kernel = "rbfdot",
                C = 1000000,
                scaled = TRUE)


ccmodel_2


pred2 <- predict(ccmodel_2,ccdata_2[,1:10])

sum(pred2 == ccdata_2[,11])/nrow(ccdata_2)

## Question 2.2.3
## Used a KNN classifier

#install.packages("kknn")

library(kknn)

i =167
                                 
ccdata <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)

ccmodel_knn = kknn( V11~.,
                    ccdata[-i,],
                    ccdata[i,],
                    k = 10,
                    distance = 2,
                    kernel = "optimal",
                    scale = TRUE)
                  
              
fitted.values(ccmodel_knn)



