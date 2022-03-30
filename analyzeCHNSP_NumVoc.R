library(lme4)
library(lmerTest)

setwd('~/Documents/opensmile/HumanData_analysis/completeDataset/AllAges_CHNSP_FAN_MAN')
ourdata = read.csv('baby_list.csv')

plot(ourdata$AGE, ourdata$CHNSPentropy,
     xlab = "Infant age (days)",
     ylab = "Entropy of infant vocalization types")

######
# Read dataset
######
CHNSPentropy_UMAP = ourdata$CHNSPentropyUMAP
CHNSPentropy_tSNE = ourdata$CHNSPentropytSNE
CHNSPcentroid_self_UMAP = ourdata$CENTROIDdist_CHSNPself_UMAP
CHNSP_FAN_centroid_UMAP = ourdata$CENTROID_CHSNP_FAN_UMAP
CHNSPcentroid_self_PCA = ourdata$CENTROIDdist_CHSNPself_PCA
CHNSP_FAN_centroid_PCA = ourdata$CENTROID_CHSNP_FAN_PCA
CHNSPcentroid_self_tSNE = ourdata$CENTROIDdist_CHSNPself_tSNE
CHNSP_FAN_centroid_tSNE = ourdata$CENTROID_CHSNP_FAN_tSNE
CHNSP_selfPRE_covariance = ourdata$cov_BBself_pre
CHNSP_self_covariance = ourdata$cov_BBself
AGE =ourdata$AGE
AgeGroup = ourdata$AGEGROUP 
ChildID = ourdata$CHILDID
CHILDvocNumber = ourdata$NUMCHNSPVOC

#############################################
# ENTROPY
#############################################
# UMAP
plot(ourdata$AGE, ourdata$CHNSPentropyUMAP,
     xlab = "Infant age (days)",
     ylab = "CHNSP entropy UMAP")

lmPoly = lmer(CHNSPentropy_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNSP_entropy[ix])

# tSNE
plot(ourdata$AGE, ourdata$CHNSPentropytSNE,
     xlab = "Infant age (days)",
     ylab = "CHNSP entropy UMAP")

lmPoly = lmer(CHNSPentropytSNE ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNSP_entropy[ix])

##############################################
# MEAN DISTANCE FROM THE CHNSP CENTROID (self)
##############################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROIDdist_CHSNPself_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHSNP")

lmPoly = lmer(CHNSPcentroid_self_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])

# PCA
plot(ourdata$AGE, ourdata$CENTROIDdist_CHSNPself_PCA,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHSNP")

lmPoly = lmer(CHNSPcentroid_self_PCA ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])

# tSNE
plot(ourdata$AGE, ourdata$CENTROIDdist_CHSNPself_tSNE,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHSNP")

lmPoly = lmer(CHNSPcentroid_self_tSNE ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])

######################################
# DISTANCE CENTROIDS CHNSP versus FAN
######################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHSNP_FAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lmer(CHNSP_FAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])

# PCA
plot(ourdata$AGE, ourdata$CENTROID_CHSNP_FAN_PCA,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lmer(CHNSP_FAN_centroid_PCA ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])

# tSNE
plot(ourdata$AGE, ourdata$CENTROID_CHSNP_FAN_tSNE,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lmer(CHNSP_FAN_centroid_tSNE ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])

######################################
# COVARIANCE CENTROIDS CHNSP SELF 
######################################
# UMAP
# consecutive vocalizations
plot(ourdata$AGE, ourdata$cov_BBself_pre,
     xlab = "Infant age (days)",
     ylab = "Cov CHNSP self pre")

lmPoly = lmer(CHNSP_selfPRE_covariance ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_cov = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_cov[ix]

lines(ourdata$AGE[ix],predCHNSP_cov[ix])

# Self vocalizations
plot(ourdata$AGE, ourdata$cov_BBself,
     xlab = "Infant age (days)",
     ylab = "Cov CHSNP self")

lmPoly = lmer(CHNSP_self_covariance ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_cov = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_cov[ix]

lines(ourdata$AGE[ix],predCHNSP_cov[ix])

