library(ggplot2)
library(plyr)
library(reshape)


# Plot results of Implicature task
age_plot <- ggplot(Results, aes(Type, Score, fill = Critical))
age_plot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  facet_wrap(~ Agegroup, nrow=2) + 
  scale_fill_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical")) +
  ylab("Mean Correct Responses") +
  ggtitle("Mean Correct Responses \nby Inference, and age") +
  lims(y = c(0,1)) 

# plot monolings only 
age_plot_mono <- ggplot(Monoling, aes(Type, Score, fill = Critical))
age_plot_mono + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  facet_wrap(~ Agegroup, nrow=1) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge( width = 1), width = .2) +
  scale_fill_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical Inference")) +
  ylab("Mean Correct Responses") + xlab("Inference Type") +
  ggtitle("Mean Correct Responses \nby Inference type, and agegroup (monolingual)") +
  lims(y = c(0,1)) + ggsave ("three_to_fives.png", width = 6, height = 4)

age_plot_mono2 <- ggplot(Monoling, aes(Type, Score, colour = Critical))
age_plot_mono2 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3))  + 
  stat_summary(fun.y = mean, geom = "line", aes(group=Critical), position = position_dodge(width = .3)) +
  facet_wrap(~ Agegroup, nrow=1) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .3)) +
  scale_colour_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical Inference")) +
  ylab("Mean Correct Responses") + xlab("Inference Type") +  scale_x_discrete(limits=c("W","R","A","S")) +
  ggtitle("Mean Correct Responses \nby Inference type, and agegroup (monolingual)") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_line.png", width = 6, height = 4)

age_plot_mono3 <- ggplot(Monoling, aes(Agegroup, Score, colour = Critical))
age_plot_mono3 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3))  + 
  stat_summary(fun.y = mean, geom = "line", aes(group=Critical), position = position_dodge(width = .3)) +
  facet_wrap(~ Type, nrow=1) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .3)) +
  scale_colour_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical Inference")) +
  ylab("Mean Correct Responses") + xlab("Agegroup") +
  ggtitle("Mean Correct Responses \nby agegroup and inference (monolingual)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  lims(y = c(0,1)) + ggsave ("three_to_fives_line_type.png", width = 6, height = 4)



# plot bilings only 
age_plot_bi <- ggplot(Biling, aes(Type, Score, fill = Critical))
age_plot_bi + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  facet_wrap(~ Agegroup, nrow=2) + 
  scale_fill_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical")) +
  ylab("Mean Correct Responses") +
  ggtitle("Mean Correct Responses \nby Inference, and age (bilingual)") +
  lims(y = c(0,1)) 

age_plot_bi2 <- ggplot(Biling, aes(Type, Score, colour = Critical))
age_plot_bi2 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3))  + stat_summary(fun.y = mean, geom = "line", aes(group=Critical), position = position_dodge(width = .3)) +
  facet_wrap(~ Agegroup, nrow=1) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2, position = position_dodge(width = .3)) +
  scale_colour_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical Inference")) +
  ylab("Mean Correct Responses") + xlab("Inference Type") +
 scale_x_discrete(limits=c("W","R","A","S")) +
  ggtitle("Mean Correct Responses \nby Inference type, and agegroup (bilingual)") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_line_bi.png", width = 6, height = 4)



# plot by multiilng
multiling_names <- c('0'= "mono", '1' = "bi")
critical_names <- c('0'= "control", '1' = "critical")

age_plot + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  facet_wrap(Multiling ~ Agegroup, nrow=2, labeller = labeller(Multiling = as_labeller(multiling_names))) +
  scale_fill_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical")) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge( width = 1), width = .2) +
  ylab("Mean Correct Responses") + scale_x_discrete(limits=c("W","R","A","S")) + 
  ggtitle("Mean Correct Responses \nby Inference, age and languages") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_mono_bi.png", width = 6, height = 4)


age_plot2 <- ggplot(Results, aes(Type, Score, colour = Critical))
age_plot2 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = Critical), position = position_dodge(width = .3)) +
  facet_wrap(Multiling ~ Agegroup, nrow=2, labeller = labeller(Multiling = as_labeller(multiling_names))) +
  scale_colour_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical")) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .3), width = .2) +
  ylab("Mean Correct Responses") + scale_x_discrete(limits=c("W","R","A","S")) +
  ggtitle("Mean Correct Responses \nby inference, age and multilingualism") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_mono_bi_line.png", width = 6, height = 5)

age_plot3 <- ggplot(Results, aes(Agegroup, Score, colour = Critical))
age_plot3 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = Critical), position = position_dodge(width = .3)) +
  facet_wrap(Multiling ~ Type, nrow=2, labeller = labeller(Multiling = as_labeller(multiling_names))) +
  scale_colour_discrete(name="Condition", breaks=c("0", "1"), labels=c("Control", "Critical")) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .3), width = .2) +
  ylab("Mean Correct Responses") + xlab("Agegroup") +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Mean Correct Responses \nby inference, age and multilingualism") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_mono_bi_line_types.png", width = 6, height = 5)

age_plot4 <- ggplot(Results, aes(Type, Score, colour = Multiling))
age_plot4 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = Multiling), position = position_dodge(width = .3)) +
  facet_wrap(Critical ~ Agegroup, nrow=2, labeller = labeller(Critical = as_labeller(critical_names))) +
  scale_colour_discrete(name="Language", breaks=c("0", "1"), labels=c("Monolingual", "Multilingual")) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .3), width = .2) +
  ylab("Mean Correct Responses") + scale_x_discrete(limits=c("W","R","A","S")) +
  ggtitle("Mean Correct Responses \nby inference, age and multilingualism") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_mono_bi_line.png", width = 6, height = 5)

# plot with only subset data

age_plot5 <- ggplot(Results_crit_NArmd, aes(Type, Score, colour = Multiling))
age_plot5 + stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .3)) +
  stat_summary(fun.y = mean, geom = "line", aes(group = Multiling), position = position_dodge(width = .3)) +
  facet_wrap( ~ Agegroup, nrow=1, labeller = labeller(Critical = as_labeller(critical_names))) +
  scale_colour_discrete(name="Language", breaks=c("0", "1"), labels=c("Monolingual", "Multilingual")) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .3), width = .2) +
  ylab("Mean Correct Responses") + scale_x_discrete(limits=c("W","R","A","S")) +
  ggtitle("Mean Correct Responses \nby inference, age and multilingualism") +
  lims(y = c(0,1)) + ggsave ("three_to_fives_mono_bi_line_subset.png", width = 6, height = 5)

# item effects

items <- aggregate(Score ~ Type + Critical + Item.no, data = Monoling, FUN = mean)
write.csv(items, "item_scores.csv")

item_plot <- ggplot(subset(items, items$Critical==1), aes(Item.no, Score, fill = Type))
item_plot + stat_summary(fun.y = identity, geom = "bar", position = "dodge") +
  ylab("Mean Correct Responses") +
  ggtitle("Mean Correct Responses \nby Item (critical, monolingual)") +
  lims(y = c(0,1)) + 
  scale_x_discrete(limits=c(1, 5, 9, 12, 14, 16, 19, 23, 2, 4, 7, 10, 15, 18, 20, 22, 3, 6, 8, 11, 13, 17, 21, 24, 25:32)) + 
  ggsave("item_effects.png")

items

# plot A against R for 3-year-old monolinguals

monoling_3 <- subset(Monoling, Monoling$Agegroup=="2;8-3;11")
monoling_3$Agegroup <- factor(monoling_3$Agegroup)

# reorder so WL is baseline 
monoling_3$Type <-factor(as.character(monoling_3$Type), levels = c("W", "A", "R", "S"))


# aggregate by item type 

monoling_3_type <- aggregate(Score~ Type + Critical + ID, FUN = mean, data = monoling_3)
monoling_3_types <- cast(monoling_3_type, ID ~ Type + Critical, value = "Score")


plot_3_A_R <- ggplot(monoling_3_types, aes(R_1, A_1))
plot_3_A_R + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("Adhoc by Relevance inference scores in 3-year-olds") + ylab("Proportion Ad hoc correct") +
  xlab("Proportion Relevance correct")


A_R_3s_cor <- capture.output(cor.test(monoling_3_types$A_1, monoling_3_types$R_1, method = "kendall"))
write(A_R_3s_cor, "A_R_3s_cor.txt")


plot_3_A_W <- ggplot(monoling_3_types, aes(W_1, A_1))
plot_3_A_W + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("WL by Ad Hoc inference scores in 3-year-olds") + ylab("Proportion Ad hoc correct") +
  xlab("Proportion WLcorrect") + ggsave("3s_W_A.png")

plot_3_A_R <- ggplot(monoling_3_types, aes(W_1, R_1))
plot_3_A_R + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("WL by Relevance inference scores in 3-year-olds") + ylab("Proportion Relevance correct") +
  xlab("Proportion WLcorrect") + ggsave("3s_R_A.png")



# plot S against R for all monolinguals

monoling_type <- aggregate(Score~ Type + Critical + ID, FUN = mean, data = Monoling_s)
monoling_types <- cast(monoling_type, ID ~ Type + Critical, value = "Score")

plot_S_R <- ggplot(monoling_types, aes(R_1, S_1))
plot_S_R + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("Relevance by Scalar inference scores") + ylab("Proportion Scalar correct") +
  xlab("Proportion Relevance correct")


# Add in small agegroups 

monoling_types_s <- merge(monoling_types, ages_small_Mono, by = "ID")

# look at correlation in youngest group 

plot_2_A_R <- ggplot(subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2"), aes(R_1, A_1))
plot_2_A_R + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("Adhoc by Relevance inference scores in 2;8-3;2") + ylab("Proportion Ad hoc correct") +
  xlab("Proportion Relevance correct")

plot_2_A_W <- ggplot(subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2"), aes(A_1, W_1))
plot_2_A_W + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("WL by Ad Hoc inference scores in 2;8-3;2") + ylab("Proportion WL correct") +
  xlab("Proportion Ad Hoc correct")

plot_2_3_A_W <- ggplot(subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2" |monoling_types_s$Agegroup_s=="3;3-3;8" ), aes(A_1, W_1))
plot_2_3_A_W + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("WL by Ad Hoc inference scores in 2;8-3;2") + ylab("Proportion WL correct") +
  xlab("Proportion Ad Hoc correct")



plot_2_3_A <- ggplot(subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2" |monoling_types_s$Agegroup_s=="3;3-3;8" ), aes(A_1, A_0))
plot_2_3_A + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("Ad Hoc inference and control scores in 2;8-3;8") + ylab("Proportion Ad Hoc control correct") +
  xlab("Proportion Ad Hoc correct")

# look at histograms, especially for three-year-olds

plot_3_hist <- ggplot(monoling_3_type, aes(Score, fill=Critical))
plot_3_hist + geom_histogram(binwidth = .25, position = "dodge") + facet_grid(~Type) +
  ggtitle("Histogram of three-year-olds' scores by inference type (monolingual)") + scale_fill_discrete(labels=c("Control", "Critical")) +
  scale_x_continuous(breaks= c(.25, .5, .75, 1)) + ylim(NA, 25) + ggsave("Histogram_threes.png")


# histograms for four-year-olds 

monoling_4 <- subset(Monoling, Monoling$Agegroup=="4;0-4-11")
monoling_4$Agegroup <- factor(monoling_4$Agegroup)

monoling_4$Type <-factor(as.character(monoling_4$Type), levels = c("W", "A", "R", "S"))


monoling_4_type <- aggregate(Score~ Type + Critical + ID, FUN = mean, data = monoling_4)
monoling_4_types <- cast(monoling_4_type, ID ~ Type + Critical, value = "Score")



plot_4_hist <- ggplot(monoling_4_type, aes(Score, fill=Critical))
plot_4_hist + geom_histogram(binwidth = .25, position = "dodge") + facet_grid(~Type) +
  ggtitle("Histogram of four-year-olds' scores by inference type (monolingual)") + scale_fill_discrete(labels=c("Control", "Critical")) +
  scale_x_continuous(breaks= c(.25, .5, .75, 1)) + ggsave("Histogram_fours.png")


# plot ToM and implicatures across all 3-year-olds in second study


TOM_imp_3s <- ggplot(subset(Results_crit_NArmd_ag, Results_crit_NArmd_ag$Agegroup=="2;8-3;11"), aes(Score, TOMctd))
TOM_imp_3s + geom_point(position = position_jitter(width = .02, height = .02)) +geom_smooth(method = glm) +
  ggtitle("Implicature score by ToM in children aged 2;8-3;11") + xlab("Implicature score") +
  ggsave("Implicature_ToM_exp2.png", width = 4, height=4)
