# BPVS and WL 

# rerun with definite BPVS raw score centred
WL2_0m <-  glmer(Score ~ 1 + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL2_Agem <-  glmer(Score ~ Age_ctd  +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL2_Genderm <-  glmer(Score ~ Age_ctd + Gender +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL2_SESm <-  glmer(Score ~ Age_ctd + Gender + SES_total_ctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL2_BPVSm <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVS_Rawctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL2_Multim <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVS_Rawctd + Multiling + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))


WL2_0m_all <- allFit(WL2_0m)
WL2_Agem_all <- allFit(WL2_Agem)
WL2_Genderm_all <- allFit(WL2_Genderm)
WL2_SESm_all <- allFit(WL2_SESm)
WL2_BPVSm_all <- allFit(WL2_BPVSm)
WL2_Multim_all <- allFit(WL2_Multim)
summary(WL2_Multim_all$bobyqa)

WL_lang <- capture.output(anova(WL2_0m_all$`optimx.L-BFGS-B`, WL2_Agem_all$`optimx.L-BFGS-B`, WL2_Genderm_all$`optimx.L-BFGS-B`, WL2_SESm_all$`optimx.L-BFGS-B`, WL2_BPVSm_all$`optimx.L-BFGS-B`, WL2_Multim_all$`optimx.L-BFGS-B`))
write(WL_lang, "WL_lang_comp.txt")

# model with WL critical score and age_months as predictor, compared to 0 model
# model wtih WL crit and BPVS, compared to 0 model 
# model with WL, age, and BPVS Raw (BPVSctd is based on BPVS_Raw2)

# for monolinguals # rerun with new centred scaled variables
WL2_0m_mono <-  glmer(Score ~ 1 + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_WL_NArm)
WL2_Agem_mono <-  glmer(Score ~ Age_ctd  +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_WL_NArm)
WL2_Genderm_mono <-  glmer(Score ~ Age_ctd + Gender +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_WL_NArm)
WL2_SESm_mono <-  glmer(Score ~ Age_ctd + Gender + SES_total_ctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_WL_NArm)
WL2_BPVSm_mono <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVS_Rawctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_WL_NArm)


WL2_0m_all_mono <- allFit(WL2_0m_mono)
WL2_Agem_all_mono <- allFit(WL2_Agem_mono)
WL2_Genderm_all_mono <- allFit(WL2_Genderm_mono)
WL2_SESm_all_mono <- allFit(WL2_SESm_mono)
WL2_BPVSm_all_mono <- allFit(WL2_BPVSm_mono)

WL_lang_mono <- capture.output(anova(WL2_0m_all_mono$bobyqa, WL2_Agem_all_mono$bobyqa, WL2_Genderm_all_mono$bobyqa,
      WL2_SESm_all_mono$bobyqa, WL2_BPVSm_all_mono$bobyqa))

write(WL_lang_mono, "WL_lang_mono.txt")

# for bilinguals

WL2_0m_bi <-  glmer(Score ~ 1 + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_WL_NArm)
WL2_Agem_bi <-  glmer(Score ~ Age_ctd  +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_WL_NArm)
WL2_Genderm_bi <-  glmer(Score ~ Age_ctd + Gender +(1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_WL_NArm)
WL2_SESm_bi <-  glmer(Score ~ Age_ctd + Gender + SES_total_ctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_WL_NArm)
WL2_BPVSm_bi <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVS_Rawctd + (1 + Age_ctd + Gender + BPVS_Rawctd + SES_total_ctd | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Biling_WL_NArm)


WL2_0m_all_bi <- allFit(WL2_0m_bi)
WL2_Agem_all_bi <- allFit(WL2_Agem_bi)
WL2_Genderm_all_bi <- allFit(WL2_Genderm_bi)
WL2_SESm_all_bi <- allFit(WL2_SESm_bi)
WL2_BPVSm_all_bi <- allFit(WL2_BPVSm_bi)

WL_lang_bi <- capture.output(anova(WL2_0m_all_bi$nlminbw, WL2_Agem_all_bi$nlminbw, WL2_Genderm_all_bi$nlminbw,
                                     WL2_SESm_all_bi$nlminbw, WL2_BPVSm_all_bi$nlminbw))
write(WL_lang_bi, "WL_lang_bi.txt")

summary(WL2_Genderm_all_bi)

# converges only wihtout BPVS as ran effect
#WL_0m <- glmer(Score ~ 1 + (1 + Age_monthsctd  + BPVSctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
#WL_0m_all <- allFit(WL_0m)
WL_Agem <- glmer(Score ~ Age_ctd + (1 + Age_ctd + Gender +  BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_Agem_all <- allFit(WL_Agem)

#WL_BPVSm <- glmer(Score ~ BPVSctd + (1 + Age_monthsctd + BPVSctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_0m <-  glmer(Score ~ 1 + (1 + Age_ctd + Gender + BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_Genderm <-  glmer(Score ~ Age_ctd + Gender +(1 + Age_ctd + Gender + BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_SESm <-  glmer(Score ~ Age_ctd + Gender + SES_total_ctd + (1 + Age_ctd + Gender + BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_BPVSm <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVSctd + (1 + Age_ctd + Gender + BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_Multim <-  glmer(Score ~ Age_ctd + Gender +SES_total_ctd + BPVSctd + Multiling + (1 + Age_ctd + Gender + BPVSctd + SES_total_ctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))

WL_0m_all <- allFit(WL_0m)
WL_Genderm_all <- allFit(WL_Genderm)
WL_SESm_all <- allFit(WL_SESm)
WL_BPVSm_all <- allFit(WL_BPVSm)
WL_Multim_all <- allFit(WL_Multim)
summary(WL_Multim_all)

WL_comp <- capture.output(anova(WL_0m_all$bobyqa, WL_Agem_all$bobyqa, WL_Genderm_all$bobyqa, WL_SESm_all$bobyqa, WL_BPVSm_all$bobyqa, WL_Multim_all$bobyqa))
write(WL_comp, "WL_comp.txt")

WL_AGE_BPVSm <- glmer(Score ~ Age_monthsctd + BPVSctd + (1 + Age_monthsctd + BPVSctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))


WL_AGE_BPVS_multi <- glmer(Score ~ Age_monthsctd + BPVSctd + Multiling + (1 + Age_monthsctd + BPVSctd + Multiling| Item.no), 
                           family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                           data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
summary(WL_AGE_BPVS_multi)

WL_AGE_multi <- glmer(Score ~ Age_monthsctd + Multiling + (1 + Age_monthsctd + BPVSctd + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
WL_AGE_multi_all <- allFit(WL_AGE_multi)

#WL_multi <- glmer(Score ~ Multiling + (1  + Multiling| Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = subset(Results_crit_NArmd, Results_crit_NArmd$Type=="W"))
#summary(WL_multi)


anova(WL_0, WL_Age)
anova(WL_0, WL_BPVS)
anova(WL_0, WL_Age, WL_AGE_BPVS)
anova(WL_Agem_all$bobyqa, WL_AGE_BPVSm, WL_AGE_BPVS_multi)
anova(WL_Agem, WL_AGE_multi_all$bobyqa)


WL_BPVS_multling <- capture.output(anova(WL_Agem_all$bobyqa, WL_AGE_BPVSm, WL_AGE_BPVS_multi))
write(WL_BPVS_multling, "WL_BPVS_MULTI.txt")


# compare mono and bilinguals

lm(Score ~ Multiling, data = subset(Results_crit_Type, Results_crit_Type$Type=="W"))
t.test(subset(Results_crit_Type, Results_crit_Type$Type=="W" & Results_crit_Type$Multiling=="0")$Score, subset(Results_crit_Type, Results_crit_Type$Type=="W" & Results_crit_Type$Multiling=="1")$Score)

