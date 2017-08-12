library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))


# by subject random slope
# subject by type and condition random slopes did not converge 

model_mono_id4 <- glmer(Score ~ Critical + Type + Agegroup + (1 + Type | ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling)
model_mono_id4_all <-  allFit(model_mono_id4)
summary(model_mono_id4_all)
model_mono_id4_all_maineffects <- capture.output(summary(model_mono_id4_all$bobyqa))
write(model_mono_id4_all_maineffects, "model_mono_id4.txt")