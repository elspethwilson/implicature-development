# import new results with trial 

results_trial <- read.csv("Study1resultsBecky-EWed_trial.csv")

Results_trial <- merge(Results2, results_trial, by="ID")

# Change factors to factors
Results_trial$Score <- as.numeric(as.character(Results_trial$Correct))
Results_trial$Critical <- as.factor(as.character(Results_trial$Critical))
Results_trial$Type <- as.factor(Results_trial$Type)
Results_trial$Multiling <- as.factor(Results_trial$Multiling)
Results_trial$SES2 <- as.numeric(as.character(Results_trial$SES))
Results_trial$Gender <- as.factor(Results_trial$Gender)
Results_trial$Setting <- as.factor(Results_trial$Setting)
Results_trial$School_Nursery <- as.factor(Results_trial$School_Nursery)
Results_trial$Trial_block <- as.factor(Results_trial$Trial_block)

# contrasts to sum

contrasts(Results_trial$Critical) <- contr.sum(2)
contrasts(Results_trial$Type) <- contr.sum(4)
contrasts(Results_trial$Agegroup) <- contr.sum(3)
contrasts(Results_trial$Multiling) <- contr.sum(2)
contrasts(Results_trial$Gender) <- contr.sum(2)
contrasts(Results_trial$School_Nursery) <- contr.sum(2)
contrasts(Results_trial$Trial_block) <- contr.sum(5)

# Monoling subset
Monoling_trial <- subset(Results_trial, Results_trial$Multiling=="0")

contrasts(Monoling_trial$Critical) <- contr.sum(2)
contrasts(Monoling_trial$Type) <- contr.sum(4)
contrasts(Monoling_trial$Agegroup) <- contr.sum(3)
contrasts(Monoling_trial$Trial_block) <- contr.sum(5)


Monoling_trial$Item.no <- as.character(Monoling_trial$Item.no)
Monoling_trial$Item.no <- as.factor(Monoling_trial$Item.no)

Monoling_trial$TROG2 <- as.numeric(as.character(Monoling_trial$TROG))
Monoling_trial$BPVS_Raw2 <- as.numeric(as.character(Monoling_trial$BPVS_Raw))

# plot scores by block 
Condition_names <- c('0' = "Control", '1' ="Critical")


blocks <- aggregate(Score ~ Critical + Type + Trial_block + Agegroup, FUN = mean, Monoling_trial)

plot_blocks <- ggplot(Monoling_trial, aes(Trial_block, Score, colour = Type))
plot_blocks + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3))  + 
  stat_summary(fun.y = mean, geom = "line", aes(group=Type), position = position_dodge(width = .3)) +
  facet_wrap(Critical ~ Agegroup, nrow=2, labeller = labeller(Critical = as_labeller(c(Condition_names)))) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .3)) + 
  ylab("Mean Correct Responses") + xlab("Block") +
  ggtitle("Mean Correct Responses \nby Block, Inference type, condition, agegroup (monolingual)") +
  lims(y = c(0,1)) + ggsave("Responses_block_age.png", width = 6, height = 5)
