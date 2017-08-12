
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


#Create dataframe with only monolingual results
Monoling <- subset(Results, Results$Multiling=="0")

contrasts(Monoling$Critical) <- contr.sum(2)
contrasts(Monoling$Type) <- contr.sum(4)
contrasts(Monoling$Agegroup) <- contr.sum(3)



Monoling$Item.no <- as.character(Monoling$Item.no)
Monoling$Item.no <- as.factor(Monoling$Item.no)

Monoling$TROG2 <- as.numeric(as.character(Monoling$TROG))
Monoling$BPVS_Raw2 <- as.numeric(as.character(Monoling$BPVS_Raw))



# Create dataframe with only bilingual results
Biling <- subset(Results, Results$Multiling=="1")

contrasts(Biling$Critical) <- contr.sum(2)
contrasts(Biling$Type) <- contr.sum(4)
contrasts(Biling$Agegroup) <- contr.sum(3)

Biling$TROG2 <- as.numeric(as.character(Biling$TROG))
Biling$BPVS_Raw2 <- as.numeric(as.character(Biling$BPVS_Raw))


data_summary <- aggregate(Score ~ Critical + Type, data=Results, mean)
data_summary_age <- aggregate(Score ~ Critical + Type + Agegroup, data=Results, mean)
data_summary_mono <- aggregate(Score ~ Critical + Type, data=Monoling, mean)
data_summary_age_mono <- aggregate(Score ~ Critical + Type + Agegroup, data=Monoling, mean)
data_summary_age_bi <- aggregate(Score ~ Critical + Type + Agegroup, data=Biling, mean)


write.csv(data_summary_age_mono, file="study1.summary.age.mono")
write.csv(data_summary_age_bi, file="study1.summary.age.bi")


# Count number in each age group
count(Results2, "Agegroup")
count(subset(Results2, Results2$Multiling==0), "Agegroup")
count(subset(Results2, Results2$Multiling==1), "Agegroup")

# count gender
summary(subset(Results2, Results2$Multiling==1 & Results2$Agegroup=="2;8-3;11")$Gender)
summary(subset(Results2, Results2$Multiling==1 & Results2$Agegroup=="4;0-4-11")$Gender)
summary(subset(Results2, Results2$Multiling==1 & Results2$Agegroup== "5;0-5;11")$Gender)


summary(subset(Results2, Results2$Multiling==0 & Results2$Agegroup=="2;8-3;11")$Gender)
summary(subset(Results2, Results2$Multiling==0 & Results2$Agegroup=="4;0-4-11")$Gender)
summary(subset(Results2, Results2$Multiling==0 & Results2$Agegroup== "5;0-5;11")$Gender)
