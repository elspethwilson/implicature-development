
# creating dataframe for model comparison 


Results_full_ed2year <- read.csv("Results_full_ed2year.csv")
Study1resultsBecky_EWed <- read.csv("Study1resultsBecky-EWed.csv")

# remove participant without age
Results2 <- Results_full_ed2year[-c(85),]
Results2$Agegroup <- factor(Results2$Agegroup)

Results2$TOM <- as.numeric(as.character(Results2$TOM))
Results2$Multiling <- as.factor(Results2$Multiling)

# Results2 and Results_full_ed2year are the same, except for removal of one participant without age in Results2

# Create single dataframe of results
names(Study1resultsBecky_EWed)[1] <- "ID" 
Results <- merge(Results2, Study1resultsBecky_EWed, by="ID")


# Change factors to factors
Results$Score <- as.numeric(as.character(Results$Correct))
Results$Critical <- as.factor(as.character(Results$Critical))
Results$Type <- as.factor(Results$Type)
Results$Multiling <- as.factor(Results$Multiling)
Results$SES2 <- as.numeric(as.character(Results$SES))
Results$Gender <- as.factor(Results$Gender)
Results$Setting <- as.factor(Results$Setting)
Results$School_Nursery <- as.factor(Results$School_Nursery)

# contrasts to sum

contrasts(Results$Critical) <- contr.sum(2)
contrasts(Results$Type) <- contr.sum(4)
contrasts(Results$Agegroup) <- contr.sum(3)
contrasts(Results$Multiling) <- contr.sum(2)
contrasts(Results$Gender) <- contr.sum(2)
contrasts(Results$School_Nursery) <- contr.sum(2)

Results_crit <- subset(Results, Results$Critical==1)

# ages 
ages2 <- read.csv("ages_months.csv")
colnames(ages2)[1] <- "ID"
Age_months <- ages2[, c(1,4)]

ages2 <- merge(ages2, Results2, by = "ID")


# create dataframe with all critical trial results (bi and mono)

# remove NAs # take out BPVS - standard score is missing for youngest kids 

Results_crit_NArm <- subset(Results_crit, select = -c(21:23))

Results_crit_NArm[c("BPVS")] <- list(NULL)


Results_crit_NArm$TOM <- as.numeric(as.character(Results_crit_NArm$TOM))
Results_crit_NArm$TROG2 <- as.numeric(as.character(Results_crit_NArm$TROG))
Results_crit_NArm$BPVS_Raw2 <- as.numeric(as.character(Results_crit_NArm$BPVS_Raw))

row.has.na <- apply(Results_crit_NArm, 1, function(x){any(is.na(x))})
Results_crit_NArm <- Results_crit_NArm[!row.has.na,]
sum(row.has.na)

# remove extra participant with TOM NA 

Results_crit_NArm <- subset(Results_crit_NArm, !Results_crit_NArm$ID=="BH011635")

# centre ands scale variables

Results_crit_NArm$SESctd <- scale(Results_crit_NArm$SES2, center = T, scale = T)
Results_crit_NArm$TROGctd <- scale(Results_crit_NArm$TROG2, center = T, scale = T)
Results_crit_NArm$TOMctd <- scale(Results_crit_NArm$TOM, center = T, scale = T)
Results_crit_NArm$BPVS_Rawctd <- scale(Results_crit_NArm$BPVS_Raw2, center = T, scale = T)

Results_crit_NArmd$Age_monthsctd <- scale(Results_crit_NArmd$Age_months, center = T, scale = T)


Results_crit_NArmd_ag <- aggregate(Score ~ ID + Agegroup + SESctd + TOMctd + TOM + TROGctd + BPVS_Raw2 + TROG2 + SES +  Gender + School_Nursery + Multiling, FUN  = mean, data = Results_crit_NArm)
Results_crit_NArmd_ag$SES <- as.numeric(as.character(Results_crit_NArmd_ag$SES))

# add in age in months 


Results_crit_NArmd <- merge(Results_crit_NArm, Age_months, by ="ID")
Results_crit_NArmd$Age_ctd <- scale(Results_crit_NArmd$Age_months, center = T, scale = T)


# add in SES 

Results_crit_NArmd <- merge(Results_crit_NArmd, SES_scores, by = "ID")
# centre and scale each separately 
Results_crit_NArmd$Ecucation_av_ctd <- scale(Results_crit_NArmd$Ecucation_av, center = T, scale = T)
Results_crit_NArmd$Family_total_ctd <- scale(Results_crit_NArmd$Family_total, center = T, scale = T)
# average these two together 
Results_crit_NArmd$SES_total_ctd <- rowMeans(Results_crit_NArmd[, c("Family_total_ctd", "Ecucation_av_ctd")])



# 84 participants left, of which 58 mono
# would be 85, but SES score file below removes extra - and best this way, as BPVS not for such little kids 

# create file with no WL trials 
Results_crit_NArm_imp <- subset(Results_crit_NArm, !Results_crit_NArm$Type=="W")



# import SES scores 

SES_scores <- read.csv("Multiling_info.csv")
names(SES_scores)[1] <- "ID"
Results_crit_NArm_imp <- merge(Results_crit_NArm_imp, SES_scores, by = "ID")
# centre and scale each separately 
Results_crit_NArm_imp$Ecucation_av_ctd <- scale(Results_crit_NArm_imp$Ecucation_av, center = T, scale = T)
Results_crit_NArm_imp$Family_total_ctd <- scale(Results_crit_NArm_imp$Family_total, center = T, scale = T)
# average these two together 
Results_crit_NArm_imp$SES_total_ctd <- rowMeans(Results_crit_NArm_imp[, c("Family_total_ctd", "Ecucation_av_ctd")])

# with age by months 
Results_crit_NArm_imp_agm <- merge(Results_crit_NArm_imp, Age_months, by = "ID")

Results_crit_NArm_imp_agm$Age_ctd <- scale(Results_crit_NArm_imp_agm$Age_months, center = T, scale = T)

# make composite of BPVS_Rawctd and TROGctd

Results_crit_NArm_imp_agm$StructuralLangctd <- rowMeans(Results_crit_NArm_imp_agm[, c("BPVS_Rawctd", "TROGctd")])

# bilinguals only # removed WL 

Biling_crit_Narmd <- subset(Results_crit_NArm_imp_agm, Results_crit_NArm_imp_agm$Multiling==1)

# make new centred/scaled variables for just this dataset

Biling_crit_Narmd$TROGctd <- scale(Biling_crit_Narmd$TROG2, center = T, scale = T)
Biling_crit_Narmd$BPVS_Rawctd <- scale(Biling_crit_Narmd$BPVS_Raw2, center = T, scale = T)
# average these two together 
Biling_crit_Narmd$StructuralLangctd <- rowMeans(Biling_crit_Narmd[, c("BPVS_Rawctd", "TROGctd")])

Biling_crit_Narmd$TOMctd <- scale(Biling_crit_Narmd$TOM, center = T, scale = T)

Biling_crit_Narmd$Age_ctd <- scale(Biling_crit_Narmd$Age_months, center = T, scale = T)

Biling_crit_Narmd$Ecucation_av_ctd <- scale(Biling_crit_Narmd$Ecucation_av, center = T, scale = T)
Biling_crit_Narmd$Family_total_ctd <- scale(Biling_crit_Narmd$Family_total, center = T, scale = T)
# average these two together 
Biling_crit_Narmd$SES_total_ctd <- rowMeans(Biling_crit_Narmd[, c("Family_total_ctd", "Ecucation_av_ctd")])



# do this with monolingual only # removed WL 

Monoling_crit_Narmd_imp <- subset(Results_crit_NArm_imp_agm, Results_crit_NArm_imp_agm$Multiling==0)


Monoling_crit_Narmd_imp$TROGctd <- scale(Monoling_crit_Narmd_imp$TROG2, center = T, scale = T)
Monoling_crit_Narmd_imp$BPVS_Rawctd <- scale(Monoling_crit_Narmd_imp$BPVS_Raw2, center = T, scale = T)
# average these two together 
Monoling_crit_Narmd_imp$StructuralLangctd <- rowMeans(Monoling_crit_Narmd_imp[, c("BPVS_Rawctd", "TROGctd")])

Monoling_crit_Narmd_imp$TOMctd <- scale(Monoling_crit_Narmd_imp$TOM, center = T, scale = T)

Monoling_crit_Narmd_imp$Age_ctd <- scale(Monoling_crit_Narmd_imp$Age_months, center = T, scale = T)

Monoling_crit_Narmd_imp$Ecucation_av_ctd <- scale(Monoling_crit_Narmd_imp$Ecucation_av, center = T, scale = T)
Monoling_crit_Narmd_imp$Family_total_ctd <- scale(Monoling_crit_Narmd_imp$Family_total, center = T, scale = T)
# average these two together 
Monoling_crit_Narmd_imp$SES_total_ctd <- rowMeans(Monoling_crit_Narmd_imp[, c("Family_total_ctd", "Ecucation_av_ctd")])


# Monoling and Biling normed WL subsets 

Monoling_WL_NArm <- subset(subset(Results_crit_NArm, Results_crit_NArm$Type=="W" & Results_crit_NArm$Multiling=="0"))

Monoling_WL_NArm$TROGctd <- scale(Monoling_WL_NArm$TROG2, center = T, scale = T)
Monoling_WL_NArm$BPVS_Rawctd <- scale(Monoling_WL_NArm$BPVS_Raw2, center = T, scale = T)
# average these two together 
Monoling_WL_NArm$StructuralLangctd <- rowMeans(Monoling_WL_NArm[, c("BPVS_Rawctd", "TROGctd")])

Monoling_WL_NArm$TOMctd <- scale(Monoling_WL_NArm$TOM, center = T, scale = T)

Monoling_WL_NArm<- merge(Monoling_WL_NArm, Age_months, by = "ID")

Monoling_WL_NArm$Age_ctd <- scale(Monoling_WL_NArm$Age_months, center = T, scale = T)


Monoling_WL_NArm<- merge(Monoling_WL_NArm, SES_scores, by = "ID")

Monoling_WL_NArm$Ecucation_av_ctd <- scale(Monoling_WL_NArm$Ecucation_av, center = T, scale = T)
Monoling_WL_NArm$Family_total_ctd <- scale(Monoling_WL_NArm$Family_total, center = T, scale = T)
# average these two together 
Monoling_WL_NArm$SES_total_ctd <- rowMeans(Monoling_WL_NArm[, c("Family_total_ctd", "Ecucation_av_ctd")])


# bilingual

Biling_WL_NArm <- subset(subset(Results_crit_NArm, Results_crit_NArm$Type=="W" & Results_crit_NArm$Multiling=="1"))

Biling_WL_NArm$TROGctd <- scale(Biling_WL_NArm$TROG2, center = T, scale = T)
Biling_WL_NArm$BPVS_Rawctd <- scale(Biling_WL_NArm$BPVS_Raw2, center = T, scale = T)
# average these two together 
Biling_WL_NArm$StructuralLangctd <- rowMeans(Biling_WL_NArm[, c("BPVS_Rawctd", "TROGctd")])

Biling_WL_NArm$TOMctd <- scale(Biling_WL_NArm$TOM, center = T, scale = T)

Biling_WL_NArm<- merge(Biling_WL_NArm, Age_months, by = "ID")

Biling_WL_NArm$Age_ctd <- scale(Biling_WL_NArm$Age_months, center = T, scale = T)


Biling_WL_NArm<- merge(Biling_WL_NArm, SES_scores, by = "ID")

Biling_WL_NArm$Ecucation_av_ctd <- scale(Biling_WL_NArm$Ecucation_av, center = T, scale = T)
Biling_WL_NArm$Family_total_ctd <- scale(Biling_WL_NArm$Family_total, center = T, scale = T)
# average these two together 
Biling_WL_NArm$SES_total_ctd <- rowMeans(Biling_WL_NArm[, c("Family_total_ctd", "Ecucation_av_ctd")])



# by participant 
Results_crit_NArm_imp_ag <- aggregate(Score~ID + Multiling + SESctd + TROGctd + TROG2 +  BPVS_Rawctd + TOM + TOMctd + Agegroup + Gender, data = Results_crit_NArm_imp, FUN = mean)

# by participant and type 

Results_crit_Type <- aggregate(Score ~ Type + ID + Multiling, data = Results_crit_NArmd, FUN = mean)


### old code for monolinguals ###
# Monlinguals only 

# remove NAs

Monoling_crit_NArm <- subset(Monoling_crit, select = -c(21:23))
row.has.na <- apply(Monoling_crit_NArm, 1, function(x){any(is.na(x))})

sum(row.has.na)

Monoling_crit_NArmd <- Monoling_crit_NArm[!row.has.na,]

Monoling_crit_NArmd$TOM <- as.numeric(as.character(Monoling_crit_NArmd$TOM))
Monoling_crit_NArmd$BPVS_Raw <- as.numeric(as.character(Monoling_crit_NArmd$BPVS_Raw))

# remove extra participant with TOM NA 

Monoling_crit_NArmd <- subset(Monoling_crit_NArmd, !Monoling_crit_NArmd$ID=="BH011635")

Monoling_crit_NArmd$SESctd <- scale(Monoling_crit_NArmd$SES2, center = T, scale = T)
Monoling_crit_NArmd$TROGctd <- scale(Monoling_crit_NArmd$TROG2, center = T, scale = T)
Monoling_crit_NArmd$TOMctd <- scale(Monoling_crit_NArmd$TOM, center = T, scale = T)
Monoling_crit_NArmd$BPVSctd <- scale(Monoling_crit_NArmd$BPVS_Raw2, center = T, scale = T)
Monoling_crit_NArmd$Age_monthsctd <- scale(Monoling_crit_NArmd$Age_months, center = T, scale = T)

Monoling_crit_NArmd_ag <- aggregate(Score ~ ID + Agegroup + SESctd + TOMctd + TROGctd + Gender + School_Nursery + BPVS_Raw2 + TOM, FUN  = mean, data = Monoling_crit_NArmd)

Monoling_crit_NArmd <- merge(Monoling_crit_NArmd, Age_months, by = "ID")

# leaves 58 participants, out of 71 monolinguals
# centre and scale TROG and SES and TOM scores
# take out WL trials 

Monoling_crit_NArmd_imp <- subset(Monoling_crit_NArmd, !Monoling_crit_NArmd$Type=="W") 

# create dummy coded version 

Results_crit_NArmd_dum <- Results_crit_NArmd

contrasts(Results_crit_NArmd_dum$Multiling) <- contr.treatment(2)

# creat by participant aggregation 

Results_crit_NArmd_dum_id <- aggregate(Score ~ ID + BPVS_Raw2 + SES_total_ctd + TROG2 + Multiling, Results_crit_NArmd_dum, mean)
