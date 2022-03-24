library(readr)
library(neuralnet)
data=read_csv("C:/Users/Srirama/Downloads/Deep Learning Internal Test -1/Set 5 - Q4.csv")
scaledata = scale(data)
norm = function(x){
  return((x-min(x))/max(x)-min(x))
}
minmax_df1 = as.data.frame(lapply(data,norm))
train1 = minmax_df1[1:150,]
test1 = minmax_df1[151:200,]
names(data)
nn = neuralnet(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness,data = train1,rep=5,hidden = c(2,1),threshold = 0.01)
nn$result.matrix
plot(nn)

backprop = neuralnet(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness,data = train1,algorithm="backprop",hidden = c(2,1),threshold = 0.01,learningrate = 0.01)
plot(backprop)
