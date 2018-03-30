import caret as cr
import pandas as pd
import seaborn as sns
import matplotlib as plt
from sklearn import tree


trainClean = pd.read_csv("trainClean.csv", encoding = "utf-8-sig")
testClean = pd.read_csv("testClean.csv", encoding= "utf-8-sig")

giniModel = tree.DecisionTreeClassifier(criterion="gini")
entropyModel = tree.DecisionTreeClassifier(criterion="entropy")
chiModel = tree.DecisionTreeClassifier(criterion="chi")

giniModel.fit(trainClean, trainClean["Survived"])
giniPredicted = giniModel.predict(testClean)
print(giniPredicted)
