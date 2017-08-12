library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# another suggestion of Barr et al
# run different models with different random effects for each fixed effect

# subject and item by condition 

model_Condition <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical | Item.no) + (1 + Critical | ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling)
model_Condition_all <-  allFit(model_Condition)
summary(model_Condition_all$bobyqa)


#model_mono_item3_all_maineffects <- capture.output(summary(model_mono_item3_all$bobyqa))
#write(model_mono_item3_all_maineffects, "maineffects1.txt")

#  item by agegroup 

model_Agegroup <- glmer(Score ~ Critical + Type + Agegroup + (1 + Agegroup | Item.no) + (1 | ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling)
model_Agegroup_all <-  allFit(model_Agegroup)
summary(model_Agegroup_all$bobyqa)


# subject by type and item (intercept) 
model_Type <- glmer(Score ~ Critical + Type + Agegroup + (1 + Type | ID) + (1 |Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling)
model_Type_all <-  allFit(model_Type)
summary(model_Type_all$bobyqa)

alternative_analysis <- capture.output(c(summary(model_Condition_all$bobyqa), summary(model_Agegroup_all$bobyqa), summary(model_Type_all$bobyqa)))
write(alternative_analysis, "alternative_analysis.txt")
