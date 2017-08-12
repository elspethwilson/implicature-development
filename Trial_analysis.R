library(lme4)
require(optimx)
source(system.file("utils", "allFit.R", package = "lme4"))

# analysis with block as random effect

trial_model_item <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_trial)
trial_model_itemall <-  allFit(trial_model_item)
summary(trial_model_itemall)
# 
# 
summary(trial_model_itemall$bobyqa)
trial_model_itemall_maineffects <- capture.output(summary(trial_model_itemall$bobyqa))
write(trial_model_itemall_maineffects, "maineffects1_trial.txt")
# 
#trial_model_id <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Type + Trial_block | ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data =  Monoling_trial)
#trial_model_idall <-  allFit(trial_model_id)
#summary(trial_model_idall)
# # none converge
# 
# trial_model_id2 <- glmer(Score ~ Critical + Type + Agegroup + (1 + Type + Trial_block| ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data =  Monoling_trial)
# trial_model_id2all <-  allFit(trial_model_id2)
# summary(trial_model_id2all)
# none converge


# look at effect of block itself with model comparison 
# add critical, type, agegroup, block, keeping random effects of itemno by critical, agegroup, block constant


trial_0 <- glmer(Score ~ 1 + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), data =  Monoling_trial)
trial_critical <- glmer(Score ~ Critical + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), data =  Monoling_trial)
trial_type <- glmer(Score ~ Critical + Type + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), data =  Monoling_trial)
trial_agegroup <- glmer(Score ~ Critical + Type + Agegroup + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), data =  Monoling_trial)
trial_block <- glmer(Score ~ Critical + Type + Agegroup + Trial_block + (1 + Critical + Agegroup + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), data =  Monoling_trial)

trial_0_all <- allFit(trial_0)
trial_critical_all <- allFit(trial_critical)
trial_type_all <- allFit(trial_type)
trial_agegroup_all <- allFit(trial_agegroup)
trial_block_all <- allFit(trial_block)

trial_compare <- capture.output(anova(trial_0_all$bobyqa, trial_critical_all$bobyqa, trial_type_all$bobyqa, trial_agegroup_all$bobyqa, trial_block_all$bobyqa))
write(trial_compare, "trial_compare.txt")

# repeat with smaller agegroups 

Monoling_trial_s <- merge(Monoling_trial, ages_small_Mono, by = "ID")

plot_blocks_s <- ggplot(Monoling_trial_s, aes(Trial_block, Score, colour = Type))
plot_blocks_s + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .5))  + 
  stat_summary(fun.y = mean, geom = "line", aes(group=Type), position = position_dodge(width = .5)) +
  facet_wrap(Critical ~ Agegroup_s, nrow=2, labeller = labeller(Critical = as_labeller(c(Condition_names)))) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .5)) + 
  ylab("Mean Correct Responses") + xlab("Block") +
  ggtitle("Mean Correct Responses \nby Block, Inference type, condition, agegroup (monolingual)") +
  lims(y = c(0,1)) + ggsave("Responses_block_age_s.png", width = 6, height = 5)

# contrast coding for agegroups 
contrasts(Monoling_trial_s$Agegroup_s) <- contr.sum(6)

trial_model_item_s <- glmer(Score ~ Critical + Type + Agegroup_s + (1 + Critical + Agegroup_s + Trial_block | Item.no), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), data = Monoling_trial_s)
trial_model_itemall_s <-  allFit(trial_model_item_s)
summary(trial_model_itemall_s$bobyqa)
trial_model_itemall_s_maineffects <- capture.output(summary(trial_model_itemall_s$bobyqa))
write(trial_model_itemall_s_maineffects, "maineffects1_trial_s.txt")
