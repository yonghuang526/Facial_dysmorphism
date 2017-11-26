############################################################################
### do a random forest on the facial measures
############################################################################

# load library for random Forest
library(randomForest)

# load training data
X = read.csv(...)
rownames(X) = ...
# load affected status
aff = read.csv(...)
# load age and sex
tmp = read.csv(...)
age = ...
sex = ...

### get Euclidean distances between facial landmarks
X.euc = ...

### note that I'm using the strata argument to effectively not sample
### affecteds at all (400 unaffected vs. 1 affected for each tree)
rf = randomForest(y=as.factor(sex),x=X.euc,importance=T,do.trace=50,strata=as.factor(aff),sampsize=c(400,1),ntree=5000)

# get correlation on votes and sex
face_s = lm(rf$votes[,2]~sex)$resid[names(age)]

# plot boxplot based on affected status and sex

