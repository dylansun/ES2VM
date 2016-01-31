source("trainESN.R")
source("predictESN.R")
HS300 = read.table("../data/000300.txt")
data = HS300$V6; 
trainLen = 2000; 
initLen = 100; 
inSize = 5;
outSize =1;
resSize = 100;
alpha = 0.8;
leakingRate = 0.7

model = trainESN(data)

Y_predict = predictESN(model=model, data = data)