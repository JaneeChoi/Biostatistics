# Load rpart and rpart.plot
library(rpart)
library(rpart.plot)
# Create a decision tree model
tree_aacr = rawdata_aacr[,-(1:3)]
tree_aacr$pdac = (rawdata_aacr$type == "PC")
tree_aacr = tree_aacr[,-1]

new_index = (tree_aacr$CA19_9 < 3.6)
new_aacr = tree_aacr[new_index,]

aacr_fil = new_aacr[ , -which(names(new_aacr) %in% c("GLPGEVLGAQPGPR"))]

tree <- rpart(pdac~., data=tree_aacr, cp=.02)
tree_fil <-rpart(pdac~., data=aacr_fil, cp=.02)
new_tree <- rpart(pdac~., data=new_aacr, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
rpart.plot(tree_fil, box.palette="RdBu", shadow.col="gray", nn=TRUE)

rpart.plot(new_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)


GLPGEVLGAQPGPR