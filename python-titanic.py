import caret as cr
import pandas as pd
import seaborn as sns
import matplotlib as plt


trainClean = pd.read_csv("trainClean.csv", encoding = "utf-8-sig")
testClean = pd.read_csv("testClean.csv", encoding= "utf-8-sig")

print sns.heatmap(trainClean.isnull(), cmap="viridis", cbar=False, yticklabels=False)
