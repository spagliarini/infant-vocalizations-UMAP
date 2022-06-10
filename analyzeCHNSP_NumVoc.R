library(lme4)
library(lmerTest)

setwd('~/Documents/opensmile/HumanData_analysis/completeDataset/AllAges_CHNNSP_CHNSP_FAN_MAN_200')
ourdata = read.csv('baby_list.csv')

plot(ourdata$AGE, ourdata$CHNSPentropy,
     xlab = "Infant age (days)",
     ylab = "Entropy of infant vocalization types")

######
# Read dataset
######
######
# Read dataset
######
CHNSPentropy_UMAP = ourdata$CHNSPentropyUMAP
CHNSPentropy_tSNE = ourdata$CHNSPentropytSNE
CHNSPcentroid_self_UMAP = ourdata$CENTROIDdist_CHNSPself_UMAP
CHNNSPcentroid_self_UMAP = ourdata$CENTROIDdist_CHNNSPself_UMAP
CHNSP_FAN_centroid_UMAP = ourdata$CENTROID_CHNSP_FAN_UMAP
CHNSP_MAN_centroid_UMAP = ourdata$CENTROID_CHNSP_MAN_UMAP
CHNNSP_FAN_centroid_UMAP = ourdata$CENTROID_CHNNSP_FAN_UMAP
CHNNSP_CHNSP_centroid_UMAP = ourdata$CENTROID_CHNNSP_CHNSP_UMAP
CHNNSP_MAN_centroid_UMAP = ourdata$CENTROID_CHNNSP_MAN_UMAP
CHNSP_FAN_centroid_PCA = ourdata$CENTROID_CHNSP_FAN_PCA
CHNSP_FAN_centroid_tSNE = ourdata$CENTROID_CHNSP_FAN_tSNE
AGE =ourdata$AGE
AgeGroup = ourdata$AGEGROUP 
ChildID = ourdata$CHILDID
CHILDvocNumber = ourdata$NUMCHNSPVOC

# z_norm values (z_score)
mean_CHNSPentropy_UMAP = mean(CHNSPentropy_UMAP)
mean_CHNSPentropy_tSNE = mean(CHNSPentropy_tSNE)
mean_CHNSPcentroid_self_UMAP = mean(CHNSPcentroid_self_UMAP)
mean_CHNNSPcentroid_self_UMAP = mean(CHNNSPcentroid_self_UMAP)
mean_CHNSP_FAN_centroid_UMAP = mean(CHNSP_FAN_centroid_UMAP)
mean_CHNSP_MAN_centroid_UMAP = mean(CHNSP_MAN_centroid_UMAP)
mean_CHNNSP_FAN_centroid_UMAP = mean(CHNNSP_FAN_centroid_UMAP)
mean_CHNNSP_MAN_centroid_UMAP = mean(CHNNSP_MAN_centroid_UMAP)

sd_CHNSPentropy_UMAP = sd(CHNSPentropy_UMAP)
sd_CHNSPentropy_tSNE = sd(CHNSPentropy_tSNE)
sd_CHNSPcentroid_self_UMAP = sd(CHNSPcentroid_self_UMAP)
sd_CHNNSPcentroid_self_UMAP = sd(CHNNSPcentroid_self_UMAP)
sd_CHNSP_FAN_centroid_UMAP = sd(CHNSP_FAN_centroid_UMAP)
sd_CHNSP_MAN_centroid_UMAP = sd(CHNSP_MAN_centroid_UMAP)
sd_CHNNSP_FAN_centroid_UMAP = sd(CHNNSP_FAN_centroid_UMAP)
sd_CHNNSP_MAN_centroid_UMAP = sd(CHNNSP_MAN_centroid_UMAP)

z_CHNSPentropy_UMAP = (CHNSPentropy_UMAP - mean_CHNSPentropy_UMAP)/sd_CHNSPentropy_UMAP
z_CHNSPentropy_tSNE = (CHNSPentropy_tSNE - mean_CHNSPentropy_tSNE)/sd_CHNSPentropy_tSNE
z_CHNSPcentroid_self_UMAP = (CHNSPcentroid_self_UMAP - mean_CHNSPcentroid_self_UMAP)/sd_CHNSPcentroid_self_UMAP
z_CHNNSPcentroid_self_UMAP = (CHNNSPcentroid_self_UMAP - mean_CHNNSPcentroid_self_UMAP)/sd_CHNSPcentroid_self_UMAP
z_CHNSP_FAN_centroid_UMAP = (CHNSP_FAN_centroid_UMAP - mean_CHNSP_FAN_centroid_UMAP)/sd_CHNSP_FAN_centroid_UMAP
z_CHNSP_MAN_centroid_UMAP = (CHNSP_MAN_centroid_UMAP - mean_CHNSP_MAN_centroid_UMAP)/sd_CHNSP_MAN_centroid_UMAP
z_CHNNSP_FAN_centroid_UMAP = (CHNNSP_FAN_centroid_UMAP - mean_CHNNSP_FAN_centroid_UMAP)/sd_CHNNSP_FAN_centroid_UMAP
z_CHNNSP_MAN_centroid_UMAP = (CHNNSP_MAN_centroid_UMAP - mean_CHNNSP_MAN_centroid_UMAP)/sd_CHNNSP_MAN_centroid_UMAP 

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

# z
lmPoly = lmer(z_CHNSPentropy_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

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

# z
lmPoly = lmer(z_CHNSPentropytSNE ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

##############################################
# MEAN DISTANCE FROM THE CENTROID (self)
##############################################
# UMAP
#CHNSP
plot(ourdata$AGE, ourdata$CENTROIDdist_CHNSPself_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHNSP")

lmPoly = lmer(CHNSPcentroid_self_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])

# z
lmPoly = lmer(z_CHNSPcentroid_self_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

# CHNNSP
plot(ourdata$AGE, ourdata$CENTROIDdist_CHNNSPself_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHNNSP")

lmPoly = lmer(CHNNSPcentroid_self_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])

# z
lmPoly = lmer(z_CHNNSPcentroid_self_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

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
# DISTANCE CENTROIDS CHNSP versus adult
######################################
# UMAP
# FAN
# CHNSP
plot(ourdata$AGE, ourdata$CENTROID_CHNSP_FAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lmer(CHNSP_FAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])

# z
lmPoly = lmer(z_CHNSP_FAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

# CHNNSP
plot(ourdata$AGE, ourdata$CENTROID_CHNNSP_FAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lmer(CHNNSP_FAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNNSP_FAN[ix])

# z
lmPoly = lmer(z_CHNNSP_FAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

# MAN
# CHNSP
plot(ourdata$AGE, ourdata$CENTROID_CHNSP_MAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-MAN)")

lmPoly = lmer(CHNSP_MAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_MAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_MAN[ix]

lines(ourdata$AGE[ix],predCHNSP_MAN[ix])

# z
lmPoly = lmer(z_CHNSP_MAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

# CHNNSP
plot(ourdata$AGE, ourdata$CENTROID_CHNNSP_MAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-MAN)")

lmPoly = lmer(CHNNSP_MAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_MAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_MAN[ix]

lines(ourdata$AGE[ix],predCHNNSP_MAN[ix])

# z
lmPoly = lmer(z_CHNNSP_MAN_centroid_UMAP ~ poly(AGE,2) + (1|ChildID) + CHILDvocNumber, data = ourdata)
summary(lmPoly)
confint(lmPoly)

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

