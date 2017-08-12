# use glm to compare scores for each condition to chance 
# for 3-year-olds

# fit a glmer model with only an intercept and no fixed predictors 
#(e.g., glmer( Response ~ 1 + (1|Subject) + (1|Item) ) ). 
#The intercept is the overall average correct response rate.
#Zero corresponds to 50% accuracy, so you can see if the intercept is significantly different from that. 

chance_R<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(monoling_3, monoling_3$Critical==1 & monoling_3$Type=="R"))

summary(chance_R)

chance_A<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(monoling_3, monoling_3$Critical==1 & monoling_3$Type=="A"))
summary(chance_A)

chance_S<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(monoling_3, monoling_3$Critical==1 & monoling_3$Type=="S"))
summary(chance_S)

chance_W <- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                             data = subset(monoling_3, monoling_3$Critical==1 & monoling_3$Type=="W"))
summary(chance_W)

three_chance <- capture.output(c(summary(chance_A), summary(chance_R), summary(chance_S), summary(chance_W)))
write(three_chance, "3s_crit_against_chance.txt")

# compare SIs to chance 



chance_S_4 <- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                    data = subset(monoling_4, monoling_4$Critical==1 & monoling_4$Type=="S"))
summary(chance_S_4)

# look at very youngest agegroup 

Monoling_2 <- subset(Monoling_s, Monoling_s$Agegroup_s=="2;8-3;2")
Monoling_2$Agegroup_s <- factor(Monoling_2$Agegroup_s)

chance_R_2<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(Monoling_2, Monoling_2$Critical==1 & Monoling_2$Type=="R"))

summary(chance_R_2)

chance_A_2<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(Monoling_2, Monoling_2$Critical==1 & Monoling_2$Type=="A"))
summary(chance_A_2)

chance_S_2<- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=100000)), 
                 data = subset(Monoling_2, Monoling_2$Critical==1 & Monoling_2$Type=="S"))
summary(chance_S_2)

chance_W_2 <- glmer(Score ~ 1 + ( 1| Item.no) + (1|ID), family = "binomial", optimizer = "bobyqa", control=glmerControl(optCtrl=list(maxfun=200000)), 
                  data = subset(Monoling_2, Monoling_2$Critical==1 & Monoling_2$Type=="W"))
chance_W_2_all <- allFit(chance_W_2)

summary(chance_W_2_all$bobyqa)

two_chance <- capture.output(c(summary(chance_A_2), summary(chance_R_2), summary(chance_S_2), summary(chance_W_2_all$bobyqa)))
write(two_chance, "2s_crit_against_chance.txt")

