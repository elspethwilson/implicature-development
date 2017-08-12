
# merge monoling_types with other cor measures

monoling_types_NArmd <- merge(monoling_types, Monoling_crit_ages, by = "ID")
PSbyR <- pcor.test(monoling_types_NArmd$S_1, monoling_types_NArmd$R_1, monoling_types_NArmd$TROGctd, method = "kendall")
PSbyR

PSbyR_age <- pcor.test(monoling_types_NArmd$S_1, monoling_types_NArmd$R_1, monoling_types_NArmd$Age_months, method = "kendall")
PSbyR_age

PSbyR_lang <- pcor.test(monoling_types_NArmd$S_1, monoling_types_NArmd$R_1, monoling_types_NArmd$S_0, method = "kendall")
PSbyR_lang

PAbyR_lang <- pcor.test(monoling_types_NArmd$A_1, monoling_types_NArmd$R_1, monoling_types_NArmd$A_0, method = "kendall")
PAbyR_lang

PAbyR<- pcor.test(monoling_types_NArmd$A_1, monoling_types_NArmd$R_1, monoling_types_NArmd$TROGctd, method = "kendall")
PAbyR

PAbyR_age <- pcor.test(monoling_types_NArmd$A_1, monoling_types_NArmd$R_1, monoling_types_NArmd$Age_months, method = "kendall")
PAbyR_age

# repeat but with whole sample 

monoling_types <- merge(monoling_types, ages2_Mono, by = "ID")

pcor.test(monoling_types$S_1, monoling_types$R_1, monoling_types$Age_months, method = "kendall")

pcor.test(monoling_types$S_1, monoling_types$R_1, monoling_types$S_0, method = "kendall")

pcor.test(monoling_types$A_1, monoling_types$R_1, monoling_types$A_0, method = "kendall")


pcor.test(monoling_types$A_1, monoling_types$R_1, monoling_types$Age_months, method = "kendall")

# try to control for both age and language at the same time

#use ggm- no kendall option?
# A_R_lang_Age <- pcor(c("A_1", "R_1", "A_0", "Age_months"), var(monoling_types))
# A_R_lang_Age^2
# pcor.test(A_R_lang_Age, 2, 71)
# 
# S_R_lang_Age <- pcor(c("S_1", "R_1", "S_0", "Age_months"), var(monoling_types), method("kendall"))
# S_R_lang_Age^2
# pcor.test(S_R_lang_Age, 2, 71)

# use ppcor 
pcor.test(monoling_types$A_1, monoling_types$R_1, monoling_types[, c("A_0","Age_months")], method = "kendall")
pcor.test(monoling_types$S_1, monoling_types$R_1, monoling_types[, c("S_0","Age_months")], method = "kendall")

# youngest groups 

PW_A_2_3 <- pcor.test(subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2" |monoling_types_s$Agegroup_s=="3;3-3;8")$W_1, subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2" |monoling_types_s$Agegroup_s=="3;3-3;8" )$A_1, subset(monoling_types_s,monoling_types_s$Agegroup_s=="2;8-3;2" |monoling_types_s$Agegroup_s=="3;3-3;8")$A_0, method = "kendall")
PW_A_3s <- pcor.test(monoling_3_types$W_1, monoling_3_types$A_1, monoling_3_types$A_0, method = "kendall")
PW_R_3s <-  pcor.test(monoling_3_types$W_1, monoling_3_types$R_1, monoling_3_types$R_0, method = "kendall")
