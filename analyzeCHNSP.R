library(lme4)
library(lmerTest)

setwd('~/Documents/opensmile/HumanData_analysis/completeDataset/AllAges_CHNNSP_CHNSP_FAN_MAN_200')
ourdata = read.csv('baby_list.csv')

######
# Read dataset
######
CHNSPentropy_UMAP = ourdata$CHNSPentropyUMAP
CHNSPentropy_tSNE = ourdata$CHNSPentropytSNE
CHNNSPentropy_UMAP = ourdata$CHNNSPentropyUMAP
CHNNSPentropy_tSNE = ourdata$CHNNSPentropytSNE
CHNSPcentroid_self_UMAP = ourdata$CENTROIDdist_CHNSPself_UMAP
CHNNSPcentroid_self_UMAP = ourdata$CENTROIDdist_CHNNSPself_UMAP
CHNSP_FAN_centroid_UMAP = ourdata$CENTROID_CHNSP_FAN_UMAP
CHNSP_MAN_centroid_UMAP = ourdata$CENTROID_CHNSP_MAN_UMAP
CHNNSP_FAN_centroid_UMAP = ourdata$CENTROID_CHNNSP_FAN_UMAP
CHNNSP_MAN_centroid_UMAP = ourdata$CENTROID_CHNNSP_MAN_UMAP
CHNSPcentroid_self_tSNE = ourdata$CENTROIDdist_CHNSPself_tSNE
CHNNSPcentroid_self_tSNE = ourdata$CENTROIDdist_CHNNSPself_tSNE
CHNSP_FAN_centroid_tSNE = ourdata$CENTROID_CHNSP_FAN_tSNE
CHNNSP_CHNSP_centroid_UMAP = ourdata$CENTROID_CHNNSP_CHNSP_UMAP
AGE =ourdata$AGE
AgeGroup = ourdata$AGEGROUP 
ChildID = ourdata$CHILDID

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
#### CHNSP
# UMAP
plot(ourdata$AGE, ourdata$CHNSPentropyUMAP,
     xlab = "Infant age (days)",
     ylab = "CHNSP entropy UMAP")

lmPoly = lm(CHNSPentropy_UMAP ~ poly(AGE,2) , data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNSP_entropy[ix])
save(pred, file="UMAP_CHNSPentropy.Rdata")

# tSNE
plot(ourdata$AGE, ourdata$CHNSPentropytSNE,
     xlab = "Infant age (days)",
     ylab = "CHNSP entropy UMAP")

lmPoly = lm(CHNSPentropytSNE ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNSP_entropy[ix])
save(pred, file="tSNE_CHNSPentropy.Rdata")

#### CHNNSP
# UMAP
plot(ourdata$AGE, ourdata$CHNNSPentropyUMAP,
     xlab = "Infant age (days)",
     ylab = "CHNNSP entropy UMAP")

lmPoly = lm(CHNNSPentropy_UMAP ~ poly(AGE,2) , data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNNSP_entropy[ix])
save(pred, file="UMAP_CHNNSPentropy.Rdata")

# tSNE
plot(ourdata$AGE, ourdata$CHNNSPentropytSNE,
     xlab = "Infant age (days)",
     ylab = "CHNNSP entropy UMAP")

lmPoly = lm(CHNNSPentropytSNE ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_entropy = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_entropy[ix]

lines(ourdata$AGE[ix],predCHNNSP_entropy[ix])
save(pred, file="tSNE_CHNNSPentropy.Rdata")

##############################################
# MEAN DISTANCE FROM THE CHNSP CENTROID (self)
##############################################
#### CHNSP
# UMAP
plot(ourdata$AGE, ourdata$CENTROIDdist_CHNSPself_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHNSP")

lmPoly = lm(CHNSPcentroid_self_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])
save(pred, file="UMAP_CHNSPcentroidSELF.Rdata")

# tSNE
plot(ourdata$AGE, ourdata$CENTROIDdist_CHNSPself_tSNE,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHSNP")

lmPoly = lm(CHNSPcentroid_self_tSNE ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])
save(pred, file="tSNE_CHNNSPcentroidSELF.Rdata")

#### CHNNSP
# UMAP
plot(ourdata$AGE, ourdata$CENTROIDdist_CHNNSPself_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance from the centroid CHNNSP")

lmPoly = lm(CHNNSPcentroid_self_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCentroidself = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCentroidself[ix]

lines(ourdata$AGE[ix],predCentroidself[ix])
save(pred, file="UMAP_CHNNSPcentroidSELF.Rdata")

######################################
# DISTANCE CENTROIDS CHNSP versus FAN
######################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHNSP_FAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lm(CHNSP_FAN_centroid_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])
save(pred, file="UMAP_CHNSP_FAN_centroid.Rdata")

# tSNE
plot(ourdata$AGE, ourdata$CENTROID_CHNSP_FAN_tSNE,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-FAN)")

lmPoly = lm(CHNSP_FAN_centroid_tSNE ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNSP_FAN[ix])
save(pred, file="tSNE_CHNSP_FAN_centroid.Rdata")

######################################
# DISTANCE CENTROIDS CHNSP versus MAN
######################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHNSP_MAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNSP-MAN)")

lmPoly = lm(CHNSP_MAN_centroid_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNSP_MAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNSP_MAN[ix]

lines(ourdata$AGE[ix],predCHNSP_MAN[ix])
save(pred, file="UMAP_CHNSP_MAN_centroid.Rdata")

######################################
# DISTANCE CENTROIDS CHNNSP versus FAN
######################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHNNSP_FAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNNSP-FAN)")

lmPoly = lm(CHNNSP_FAN_centroid_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_FAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_FAN[ix]

lines(ourdata$AGE[ix],predCHNNSP_FAN[ix])
save(pred, file="UMAP_CHNNSP_FAN_centroid.Rdata")

######################################
# DISTANCE CENTROIDS CHNNSP versus MAN
######################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHNNSP_MAN_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNNSP-MAN)")

lmPoly = lm(CHNNSP_MAN_centroid_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_MAN = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_MAN[ix]

lines(ourdata$AGE[ix],predCHNNSP_MAN[ix])
save(pred, file="UMAP_CHNNSP_MAN_centroid.Rdata")


########################################
# DISTANCE CENTROIDS CHNNSP versus CHNSP
########################################
# UMAP
plot(ourdata$AGE, ourdata$CENTROID_CHNNSP_CHNSP_UMAP,
     xlab = "Infant age (days)",
     ylab = "Distance between centroids (CHNNSP-CHNSP)")

lmPoly = lm(CHNNSP_CHNSP_centroid_UMAP ~ poly(AGE,2), data = ourdata)
summary(lmPoly)
confint(lmPoly)

predCHNNSP_CHNSP = predict(lmPoly)
ix = sort(ourdata$AGE,index.return=T)$ix
pred = predCHNNSP_CHNSP[ix]

lines(ourdata$AGE[ix],predCHNNSP_MAN[ix])
save(pred, file="UMAP_CHNNSP_CHNSP_centroid.Rdata")
