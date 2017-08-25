library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))


# final version with all factors together 

# age in months instead of agegroup 
# multilingual 


# No1: language composite - language, then SES

# in data_prep make new value - average of TROGctd and BPVS_Rawctd - StructuralLangctd

# run analyses again with composite language score

# multilingual

lang_0_ag <- glmer(Score ~  1  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_Age_ag <- glmer(Score ~  Age_ctd  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_Gender_ag <-  glmer(Score ~  Age_ctd + Gender + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_TROG_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_SES_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd  + SES_total_ctd + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_TOM_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + SES_total_ctd + TOMctd  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang_full_ag <- glmer(Score ~  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)

lang_0_ag_all <- allFit(lang_0_ag)
lang_Age_ag_all <- allFit(lang_Age_ag)
lang_TROG_ag_all <- allFit(lang_TROG_ag)
lang_TOM_ag_all <- allFit(lang_TOM_ag)
lang_SES_ag_all <- allFit(lang_SES_ag)
lang_Gender_ag_all <- allFit(lang_Gender_ag)
lang_full_ag_all <- allFit(lang_full_ag)
summary(lang_full_ag_all$bobyqa)


lang_factors_model_comp_age_months <- capture.output(anova(lang_0_ag_all$bobyqa, lang_Age_ag_all$bobyqa, lang_Gender_ag_all$bobyqa,
                                                           lang_TROG_ag_all$bobyqa, lang_SES_ag_all$bobyqa, lang_TOM_ag_all$bobyqa,
                                                           lang_full_ag_all$bobyqa))
write(lang_factors_model_comp_age_months, "lang_factors_model_comp_age_months.txt")


#No2: language composite, SES, then language 

# multilingual 

lang2_0_ag <- glmer(Score ~  1  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_Age_ag <- glmer(Score ~  Age_ctd  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_Gender_ag <-  glmer(Score ~  Age_ctd + Gender + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_SES_ag <- glmer(Score ~  Age_ctd + Gender + SES_total_ctd + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_lang_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd  + SES_total_ctd + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_TOM_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + SES_total_ctd + TOMctd  + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)
lang2_full_ag <- glmer(Score ~  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling + (1 +  Age_ctd+ Gender + StructuralLangctd + SES_total_ctd+ TOMctd  + Multiling  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Results_crit_NArm_imp_agm)

lang2_0_ag_all <- allFit(lang2_0_ag)
lang2_Age_ag_all <- allFit(lang2_Age_ag)
lang2_lang_ag_all <- allFit(lang2_lang_ag)
lang2_TOM_ag_all <- allFit(lang2_TOM_ag)
lang2_SES_ag_all <- allFit(lang2_SES_ag)
lang2_Gender_ag_all <- allFit(lang2_Gender_ag)
lang2_full_ag_all <- allFit(lang2_full_ag)
summary(lang2_full_ag_all$bobyqa)


lang2_factors_model_comp_age_months <- capture.output(anova(lang2_0_ag_all$bobyqa, lang2_Age_ag_all$bobyqa, lang2_Gender_ag_all$bobyqa,
                                                            lang2_SES_ag_all$bobyqa, lang2_lang_ag_all$bobyqa, lang2_TOM_ag_all$bobyqa,
                                                            lang2_full_ag_all$bobyqa))
write(lang2_factors_model_comp_age_months, "lang2_factors_model_comp_age_months.txt")


# No3: Monolinguals only, with new datasets, where Age, ToM, SES, Lang are centred and scaled for that dataset only

lang_monoling_0_ag <- glmer(Score ~  1  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang_monoling_Age_ag <- glmer(Score ~  Age_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang_monoling_Gender_ag <- glmer(Score ~  Age_ctd  + Gender + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang_monoling_lang_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang_monoling_SES_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + SES_total_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang_monoling_TOM_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)

lang_monoling_0_ag_all <- allFit(lang_monoling_0_ag)
lang_monoling_Age_ag_all <- allFit(lang_monoling_Age_ag)
lang_monoling_Gender_ag_all <- allFit(lang_monoling_Gender_ag)
lang_monoling_lang_ag_all <- allFit(lang_monoling_lang_ag)
lang_monoling_TOM_ag_all <- allFit(lang_monoling_TOM_ag)
lang_monoling_SES_ag_all <- allFit(lang_monoling_SES_ag)


lang_monoling_full_ag_all <- allFit(lang_monoling_full_ag)
summary(lang_monoling_full_ag_all)

lang_factors_model_comp_age_months_mono <- capture.output(anova(lang_monoling_0_ag_all$bobyqa, lang_monoling_Age_ag_all$bobyqa, lang_monoling_Gender_ag_all$bobyqa, lang_monoling_lang_ag_all$bobyqa, lang_monoling_SES_ag_all$bobyqa, lang_monoling_TOM_ag_all$bobyqa))
write(lang_factors_model_comp_age_months_mono, "lang_factors_model_comp_age_months_mono.txt")

#No 4 Monolinguals only, ditto, with SES and lang switched 


lang2_monoling_0_ag <- glmer(Score ~  1  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang2_monoling_Age_ag <- glmer(Score ~  Age_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang2_monoling_Gender_ag <- glmer(Score ~  Age_ctd  + Gender + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang2_monoling_SES_ag <- glmer(Score ~  Age_ctd + Gender + SES_total_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang2_monoling_lang_ag <- glmer(Score ~  Age_ctd + Gender + SES_total_ctd + StructuralLangctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)
lang2_monoling_TOM_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  + (1 +  Age_ctd + StructuralLangctd + TOMctd + SES_total_ctd + Gender | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_crit_Narmd_imp)

lang2_monoling_0_ag_all <- allFit(lang2_monoling_0_ag)
lang2_monoling_Age_ag_all <- allFit(lang2_monoling_Age_ag)
lang2_monoling_Gender_ag_all <- allFit(lang2_monoling_Gender_ag)
lang2_monoling_SES_ag_all <- allFit(lang2_monoling_SES_ag)
lang2_monoling_lang_ag_all <- allFit(lang2_monoling_lang_ag)
lang2_monoling_TOM_ag_all <- allFit(lang2_monoling_TOM_ag)

lang2_factors_model_comp_age_months_mono <- capture.output(anova(lang2_monoling_0_ag_all$bobyqa, lang2_monoling_Age_ag_all$bobyqa, lang2_monoling_Gender_ag_all$bobyqa, lang2_monoling_SES_ag_all$bobyqa, lang2_monoling_lang_ag_all$bobyqa, lang2_monoling_TOM_ag_all$bobyqa))
write(lang2_factors_model_comp_age_months_mono, "lang2_factors_model_comp_age_months_mono.txt")


# No5: Bilinguals only, with new datasets, where Age, ToM, SES, Lang are centred and scaled for that dataset only

lang_biling_0_ag <- glmer(Score ~  1  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang_biling_Age_ag <- glmer(Score ~  Age_ctd  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang_biling_Gender_ag <- glmer(Score ~  Age_ctd  + Gender + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang_biling_lang_ag <- glmer(Score ~  Age_ctd  + Gender + StructuralLangctd  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang_biling_SES_ag <- glmer(Score ~  Age_ctd  + Gender + StructuralLangctd + SES_total_ctd  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang_biling_TOM_ag <- glmer(Score ~  Age_ctd + Gender + StructuralLangctd + SES_total_ctd  + TOMctd + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)


lang_biling_0_ag_all <- allFit(lang_biling_0_ag)
lang_biling_Gender_ag_all <- allFit(lang_biling_Gender_ag)
lang_biling_Age_ag_all <- allFit(lang_biling_Age_ag)
lang_biling_lang_ag_all <- allFit(lang_biling_lang_ag)
lang_biling_SES_ag_all <- allFit(lang_biling_SES_ag)
lang_biling_TOM_ag_all <- allFit(lang_biling_TOM_ag)

lang_factors_model_comp_age_months_bi <- capture.output(anova(lang_biling_0_ag_all$bobyqa, lang_biling_Age_ag_all$bobyqa, 
      lang_biling_Gender_ag_all$bobyqa, lang_biling_lang_ag_all$bobyqa, 
      lang_biling_SES_ag_all$bobyqa, lang_biling_TOM_ag_all$bobyqa))

write(lang_factors_model_comp_age_months_bi, "lang_factors_model_comp_age_months_bi.txt")


# No6 Bilingual with switched order of Lang and SES 

lang2_biling_SES_ag <- glmer(Score ~  Age_ctd  + Gender + SES_total_ctd  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)
lang2_biling_lang_ag <- glmer(Score ~  Age_ctd  + Gender + StructuralLangctd + SES_total_ctd  + (1 +  Age_ctd + Gender + StructuralLangctd + TOMctd + SES_total_ctd  | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_crit_Narmd)

lang2_biling_SES_ag_all <- allFit(lang2_biling_SES_ag)
lang2_biling_lang_ag_all <- allFit(lang2_biling_lang_ag)

lang2_factors_model_comp_age_months_bi <- capture.output(anova(lang_biling_0_ag_all$nlminbw, lang_biling_Age_ag_all$nlminbw, 
      lang_biling_Gender_ag_all$nlminbw, lang2_biling_SES_ag_all$nlminbw, 
      lang2_biling_lang_ag_all$nlminbw, lang_biling_TOM_ag_all$nlminbw))

write(lang2_factors_model_comp_age_months_bi, "lang2_factors_model_comp_age_months_bi.txt")

