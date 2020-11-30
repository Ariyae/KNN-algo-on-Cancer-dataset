wbcd<-read.csv(file.choose(), stringsAsFactors = F)
str(wbcd)
wbcd<-wbcd[-1]
wbcd<-wbcd[-32]
View(wbcd)
ncol(wbcd)
nrow(wbcd)
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,labels = c('Malign','Benign'))
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
summary(wbcd[c("radius_mean","texture_mean")])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#normalize(c(1,2,3,4,5))
View(wbcd)
str(wbcd)
ncol(wbcd)
wbcd_n=as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)
wbcd_train<-wbcd_n[1:450, ]
View(wbcd_train)
wbcd_test<-wbcd_n[451:569, ]
View(wbcd_test)
wbcd_train_label<-wbcd[1:450,1]
View(wbcd_train_label)
wbcd_test_label<-wbcd[451:569,1]
View(wbcd_test_label)
#wbcd[451,1]

###From here KNN classification starts
install.packages("class")
library(class)
wbcd_test_pred<-knn(train= wbcd_train,test = wbcd_test,cl=wbcd_train_label,k=21)
####checking the accuracy 
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_label,y=wbcd_test_pred,prop.chisq = F)

###Re-scaling using z score standardization
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcdz_train<-wbcd_z[1:450, ]
wbcdz_test<-wbcd_z[451:569, ]
wbcdz_train_label<-wbcd[1:450,1]
str(wbcd_z)
wbcdz_test_label<-wbcd[451:569,1]
wbcdz_test_pred<-knn(train=wbcdz_train,test=wbcdz_test,cl=wbcdz_train_label,k=21)
View(wbcdz_test_pred)
CrossTable(wbcdz_test_label,wbcdz_test_pred,prop.r=FALSE,
           prop.t=FALSE, prop.chisq=FALSE,chisq=F, missing.include=FALSE)
