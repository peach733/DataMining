install.packages("scatterplot3d")
library(scatterplot3d)

x = c(3.0, 6.0, 3.0, 6.0, 7.5, 7.5, 15.0)
u = c(10.0, 10.0, 20.0, 20.0, 5.0, 10.0, 12.0)
y = c(4.56, 5.9, 6.7, 8.02, 7.7, 8.1, 6.1)

scatterplot3d(x, u, y, xlim = 2:16, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h')

m = lm(y~ x + u)
coef(m)

s = scatterplot3d(x, u, y, xlim = 2:16, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h')
s$plane3d(m)

nx = c(7.5, 5.0)
nu = c(15.0, 12.0)

new_data = data.frame(x = nx, u = nu)
ny = predict(m, new_data)
ny

s = scatterplot3d(nx, nu, ny, xlim = 0:10, ylim = 7:23, zlim = 0:10, pch = 20, type = 'h', color = 'red', angle = 60)
s$plane3d(m)


ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
ucla$admit = factor(ucla$admit)
str(ucla)

row_num = nrow(ucla)
index = 1:row_num
sixty_sample = sample(index, row_num*0.6)
test_data = setdiff(index, sixty_sample)

ucla_sixty = ucla[sixty_sample, ] #60% 학습 데이터를 저장합니다.
ucla_test_data = ucla[test_data, ] #나머지 40% 테스트 데이터를 저장합니다.

nrow(ucla_sixty)
nrow(ucla_test_data)

#결정트리 모델 
install.packages("rpart")
library(rpart)
rpart_model = rpart(admit~., data = ucla_sixty)

#랜덤포레스트(트리 개수 50개) 모델
install.packages("randomForest")
library(randomForest)
fifty_forest_model = randomForest(admit~., data = ucla_sixty, ntree = 50)

#랜덤포레스트(트리 개수 1000개) 모델
thousand_forest_model = randomForest(admit~., data = ucla_sixty, ntree = 1000)

#K-NN 모델
install.packages("class")
library(class)
knn_model = knn(ucla_sixty, ucla_test_data, ucla_sixty$admit, k = 5)

#SVM(radial basis) 모델
install.packages("e1071")
library(e1071)
svm_model = svm(admit~., data = ucla_sixty)

#SVM(polynomial) 모델
svm_poly_model = svm(admit~., data = ucla_sixty, kernel = 'polynomial')

#결정트리 테스트 데이터 예측
newd = data.frame(gre = ucla_test_data$gre, gpa = ucla_test_data$gpa, rank = ucla_test_data$rank)
predict(rpart_model, newdata = newd)
rpart_pred = predict(rpart_model, newd, type = 'class')
confusionMatrix(rpart_pred, ucla_test_data$admit)

#랜덤포레스트(트리 개수 50개) 테스트 데이터 예측
fifty_forest_pred = predict(fifty_forest_model, newdata = ucla_test_data)
table(fifty_forest_pred, ucla_test_data$admit)

#랜덤포레스트(트리 개수 1000개) 테스트 데이터 예측
thousand_forest_pred = predict(thousand_forest_model, newdata = ucla_test_data)
table(thousand_forest_pred, ucla_test_data$admit)

#K-NN 테스트 데이터 예측
knn_pred = knn(ucla_sixty, ucla_test_data, ucla_sixty$admit, k = 5)
table(knn_pred, ucla_test_data$admit)

#SVM(radial basis) 테스트 데이터 예측
svm_pred = predict(svm_model, newdata = ucla_test_data)
table(svm_pred, ucla_test_data$admit)

#SVM(polynomial) 테스트 데이터예측
svm_poly_pred = predict(svm_poly_model, newdata = ucla_test_data)
table(svm_poly_pred, ucla_test_data$admit)

