write.csv(iris, file.choose())

install.packages('neuralnet')
library("neuralnet")

trainingdata <- read.csv(file.choose())
net.model <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,trainingdata, hidden=10, threshold=0.01)

print(net.model)
plot(net.model)

