library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# for a more theory-dependent analysis, use backward difference conding
# A) to compare each agegroup to previous - 4-years to 5-years, and 3-years to 4-years 
# (taking 5-year-olds as control group)
# B) compare different types of implicature as per predictions - W > R > A > S 
# C) compare critical to control 

# create monoling dataset with backwarddifference coding 

Monoling_backdiff <- Monoling

print(levels(Monoling_backdiff$Agegroup))
Monoling_backdiff$Agegroup = factor(Monoling_backdiff$Agegroup, levels(Monoling_backdiff$Agegroup)[c(3,2,1)])
print(levels(Monoling_backdiff$Type))
Monoling_backdiff$Type = factor(Monoling_backdiff$Type, levels(Monoling_backdiff$Type)[c(4,2,1,3)])
print(levels(Monoling_backdiff$Critical))
contrasts(Monoling_backdiff$Agegroup) = contr.sdif(3)
contrasts(Monoling_backdiff$Type) = contr.sdif(4)
contrasts(Monoling_backdiff$Critical) = contr.sdif(2)


# run fullest by item model 

bdmodel_mono_item3 <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Agegroup | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_backdiff)
bdmodel_mono_item3_all <-  allFit(bdmodel_mono_item3)
summary(bdmodel_mono_item3_all$bobyqa)
back_diffs <- capture.output(summary(bdmodel_mono_item3_all$bobyqa))
write(back_diffs, "back_diffs.txt")


# by ID 


bdmodel_mono_id3 <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Type | ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_backdiff)
bdmodel_mono_id3_all <-  allFit(bdmodel_mono_id3)
summary(bdmodel_mono_id3_all$nlminbw)
back_diffs_id <- capture.output(summary(bdmodel_mono_id3_all$nlminbw))
write(back_diffs_id, "back_diffs_id.txt")

# Add in block as random effect 

Monoling_backdiff_trial <- Monoling_trial_s
print(levels(Monoling_backdiff_trial$Agegroup))
Monoling_backdiff_trial$Agegroup = factor(Monoling_backdiff_trial$Agegroup, levels(Monoling_backdiff_trial$Agegroup)[c(3,2,1)])
print(levels(Monoling_backdiff_trial$Type))
Monoling_backdiff_trial$Type = factor(Monoling_backdiff_trial$Type, levels(Monoling_backdiff_trial$Type)[c(4,2,1,3)])
print(levels(Monoling_backdiff_trial$Critical))
contrasts(Monoling_backdiff_trial$Agegroup) = contr.sdif(3)
contrasts(Monoling_backdiff_trial$Type) = contr.sdif(4)
contrasts(Monoling_backdiff_trial$Critical) = contr.sdif(2)
print(levels(Monoling_backdiff_trial$Agegroup_s))
Monoling_backdiff_trial$Agegroup_s = factor(Monoling_backdiff_trial$Agegroup_s, levels(Monoling_backdiff_trial$Agegroup_s)[c(6,5,4,3,2,1)])
contrasts(Monoling_backdiff_trial$Agegroup_s) = contr.sdif(6)

#backward difference with block as random effect 
bdmodel_mono_item3_trial <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_backdiff_trial)
bdmodel_mono_item3_trial_all <-  allFit(bdmodel_mono_item3_trial)
summary(bdmodel_mono_item3_trial_all$bobyqa)
back_diffs_trial <- capture.output(summary(bdmodel_mono_item3_trial_all$bobyqa))
write(back_diffs_trial, "back_diffs_trial.txt")

# small agegroups

bdmodel_mono_item3_trial_s <- glmer(Score ~ Critical + Type + Agegroup_s + (1 + Critical + Agegroup_s + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_backdiff_trial)
bdmodel_mono_item3_trial_s_all <-  allFit(bdmodel_mono_item3_trial_s)
back_diffs_trial_s <- capture.output(summary(bdmodel_mono_item3_trial_s_all$bobyqa))
write(back_diffs_trial_s, "back_diffs_trial_s.txt")

# try interaction 

# bdmodel_mono_int <- glmer(Score ~ Critical * Type * Agegroup + (1 + Critical + Agegroup | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_backdiff)
# bdmodel_mono_int_all <-  allFit(bdmodel_mono_int)
# summary(bdmodel_mono_int_all$bobyqa)
# fails to converge 