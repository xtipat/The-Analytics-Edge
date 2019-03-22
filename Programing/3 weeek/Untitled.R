quality = read.csv("quality.csv")
str(quality)

library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio  = 0.75)
split
