library(data.table)
library(ggplot2)
library(readxl)
library(openxlsx)
library(explore)
library(dplyr)
library("FactoMineR")
library("factoextra")
library(gridExtra)
library(scales) 

# read data from merged file
ICU_data = data.table(read_xlsx(path = "Team1.xlsx"))

#---------- processing / cleaning the data -----------
# calculate the actual number of ICU stay by adding admissions to different hospitals
ab = unique(ICU_data[,c("subject_id","hadm_id","stay_id")])
ab = ab[, no_of_icu_admissions:=.N, by =c("subject_id")]
ab = unique(ab[,c("subject_id","no_of_icu_admissions")])
ICU_data = merge.data.table(ICU_data,ab,by = c("subject_id"))

# check for multiple entries of race 
ICU_data = ICU_data[,count_race:=uniqueN(race), by = c("subject_id")]
ethinicity = fread(input = "ethinicity.txt", sep = "\t", header = T, stringsAsFactors = F)
race = unique(ICU_data[,c("subject_id","race")])
race = merge.data.table(race,ethinicity,by=c("race"))
race = race[,count_ethinicity_withNA:=uniqueN(ethinicity), by=c("subject_id")]
race = race[,count_ethinicity:=uniqueN(ethinicity[!is.na(ethinicity)]), by=c("subject_id")]
#race = unique(race[count_ethinicity==1 & !is.na(ethinicity), c("subject_id","ethinicity")])

race$Noted_ethinicity = ifelse(is.na(race$ethinicity) & race$count_ethinicity_withNA==1, "UNKNOWN",
                               ifelse(race$count_ethinicity_withNA==1, race$ethinicity, "Multiple Entries"))
ethinicty = unique(race[,c("subject_id","Noted_ethinicity")])
ethinicty = merge.data.table(ethinicty,unique(ICU_data[,c("subject_id","Depression")]),by = c("subject_id"))

frequency_ethinicty  = data.table(data.frame(ftable(ethinicty$Noted_ethinicity~ethinicty$Depression)))
frequency_ethinicty = frequency_ethinicty[,perc:=Freq/sum(Freq)*100, by =c("ethinicty.Depression")]
# Create the pie chart with percentages
p1 = ggplot(frequency_ethinicty, aes(x = ethinicty.Noted_ethinicity, y = perc, fill = ethinicty.Depression)) + 
  geom_bar(stat = "identity", position = "dodge", width = 1) + 
  ylab("Patients (%)")+ xlab("") +
  theme_classic() + theme(axis.text.x = element_text(angle = 40, colour = "black", size = 12, hjust = 1),
                          axis.text.y = element_text(colour = "black", size = 12),
                          legend.position = "bottom")
pdf("ethinicty.pdf",height = 6, width=12)
print(p1)
dev.off()


ICU_data = merge.data.table(ICU_data,ethinicty[,c("subject_id","Noted_ethinicity")], by=c("subject_id"), all.x = T)


# add ctaegory definition taken from chatGPT
diagnosis_classification = data.table(read_xlsx(path = "classified_diagnosis_from_file.xlsx"))
ICU_data = merge.data.table(ICU_data,diagnosis_classification, by =c("ICU_diagnosis"),all.x = T)
ICU_data = ICU_data[,no_of_diagnosis_classification:=uniqueN(Diease_classification),by=c("subject_id")]


######  check for missing data in 
cols_to_check = c("gender","Noted_ethinicity","los_icu","age","no_of_icu_admissions")
ICU_data$is_missing = apply(is.na(ICU_data[, cols_to_check, with=FALSE]), 1, any)
# number of records with at least one missing data
records_with_missing_value = ICU_data[ICU_data$is_missing == TRUE]

missing_variable_counts = rbindlist(lapply(cols_to_check,function(x,data2check){
  cols2assess = c("subject_id",x)
  ab = unique(data2check[,cols2assess, with=FALSE])
  ab = as.vector(unlist(ab[,2]))
  na_count = length(ab[is.na(ab)])
  return(data.frame("variable"=x,"total"=length(ab),"Na_count"=na_count,"Na_perc" = na_count/length(ab)*100))
},ICU_data))


# we remove 
#---------------- MFA ----------------
# 
# ICU_data_variable = unique(ICU_data[,c("subject_id","gender","age","los_icu","race","Mortality","no_of_icu_admissions")])
# MFA(base, group, type = rep("s",length(group)), ind.sup = NULL, 
#      name.group = NULL, num.group.sup = NULL, graph = TRUE)

#-------------- exploring Gender data --------------
#plot distribution of depression with gender percentage
patient_gender = unique(ICU_data[,c("subject_id","Depression","gender")])
# check if multiple entries for the same subject with different gender
patient_gender = patient_gender[,count_check:=length(subject_id), by=c("subject_id")]

patient_gender_frequecy = as.matrix(ftable(patient_gender$Depression~patient_gender$gender))
gender_frequecy_pvalue = fisher.test(patient_gender_frequecy)$p.value

patient_gender_frequecy = as.data.frame(patient_gender_frequecy)
patient_gender_frequecy$gender = row.names(patient_gender_frequecy)
patient_gender_frequecy = melt.data.table(data.table(patient_gender_frequecy), id.vars = c("gender"),variable.name = "depressed",value.name = "count",variable.factor = F)
patient_gender_frequecy = patient_gender_frequecy[,perc:=count/sum(count)*100, by=c("depressed")]
# patient_gender_frequecy$label = paste0(patient_gender_frequecy$count,"\n(",round(patient_gender_frequecy$perc,0),"%)")
patient_gender_frequecy = patient_gender_frequecy[order(patient_gender_frequecy$depressed,patient_gender_frequecy$gender),]

p1 = ggplot(patient_gender_frequecy, aes(depressed, perc, fill=gender)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("F"="#F98072","M"="#36454F")) + 
  ylab("Patients (%)") +
  #annotation_custom(tableGrob(patient_gender_frequecy), xmin=1, xmax=2, ymin=120, ymax=130)+
  annotate("text", x=1.5,y=103, label = signif(gender_frequecy_pvalue, digits = 2)) +
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) 

pdf("gender_distribution.pdf", width = 4, height = 5)
print(p1)
dev.off()

#------------- Evaluate for mortality in ICU ---------

#plot distribution of depression with gender percentage
patient_mortality = unique(ICU_data[,c("subject_id","Depression","Mortality")])
# check if multiple entries for the same subject with different gender
patient_mortality = patient_mortality[,count_check:=length(subject_id), by=c("subject_id")]

patient_mortality_frequecy = as.matrix(ftable(patient_mortality$Depression~patient_mortality$Mortality))
mortality_frequecy_pvalue = fisher.test(patient_mortality_frequecy)$p.value

patient_mortality_frequecy = as.data.frame(patient_mortality_frequecy)
patient_mortality_frequecy$Mortality = row.names(patient_mortality_frequecy)
patient_mortality_frequecy = melt.data.table(data.table(patient_mortality_frequecy), id.vars = c("Mortality"),variable.name = "depressed",value.name = "count",variable.factor = F)
patient_mortality_frequecy = patient_mortality_frequecy[,perc:=count/sum(count)*100, by=c("depressed")]
patient_mortality_frequecy$label = paste0(patient_mortality_frequecy$count,"\n(",round(patient_mortality_frequecy$perc,0),"%)")

p1 = ggplot(patient_mortality_frequecy, aes(depressed, perc, fill=Mortality)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("Yes"="#008080","No"="#FF7F51")) + 
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) +
  annotate("text", x=1.5,y=103, label = signif(mortality_frequecy_pvalue, digits = 2))

pdf("mortality_distribution.pdf")
print(p1)
dev.off()


#----------------- No. of admissions ------------------
no_of_admission = unique(ICU_data[,c("subject_id","no_of_icu_admissions","Depression")])
no_of_admission$category = ifelse(no_of_admission$no_of_icu_admissions>1,"multiple","single")

wilcox.test(no_of_admission$no_of_icu_admissions ~ no_of_admission$Depression)

freq_admission = as.matrix(ftable(no_of_admission$category ~ no_of_admission$Depression))
readmission_frequecy_pvalue = fisher.test(freq_admission)$p.value
freq_admission = as.data.frame(freq_admission)
freq_admission$depressed = row.names(freq_admission)
freq_admission = melt.data.table(data.table(freq_admission), id.vars = c("depressed"),variable.name = "Admissions",value.name = "count",variable.factor = F)
freq_admission = freq_admission[,perc:=count/sum(count)*100, by=c("depressed")]

p1 = ggplot(freq_admission, aes(depressed, perc, fill=Admissions)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("multiple"="#008080","single"="#36454F")) + 
  ylab("Patients (%)") +
  annotate("text", x=1.5,y=103, label = signif(readmission_frequecy_pvalue, digits = 2)) +
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) 

p2 = ggplot(no_of_admission, aes(x=Depression, y=no_of_icu_admissions)) + geom_boxplot() +
   theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) 
pdf("no_of_admissions.pdf",height = 5, width = 9)
grid.arrange(p1,p2, nrow=1)
dev.off()

#-------------- No. of admission stratification -----
readmissions = unique(ICU_data[no_of_icu_admissions>1, c("subject_id","no_of_icu_admissions","gender","Depression")])
readmissions_patient_gender_frequecy = as.matrix(ftable(readmissions$Depression~readmissions$gender))
readmissions_gender_frequecy_pvalue = fisher.test(readmissions_patient_gender_frequecy)$p.value

readmissions_patient_gender_frequecy = as.data.frame(readmissions_patient_gender_frequecy)
readmissions_patient_gender_frequecy$gender = row.names(readmissions_patient_gender_frequecy)
readmissions_patient_gender_frequecy = melt.data.table(data.table(readmissions_patient_gender_frequecy), id.vars = c("gender"),variable.name = "depressed",value.name = "count",variable.factor = F)
readmissions_patient_gender_frequecy = readmissions_patient_gender_frequecy[,perc:=count/sum(count)*100, by=c("depressed")]

p1 = ggplot(readmissions_patient_gender_frequecy, aes(depressed, perc, fill=gender)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("F"="#F98072","M"="#36454F")) + 
  ylab("Patients (%)") +
  #annotation_custom(tableGrob(patient_gender_frequecy), xmin=1, xmax=2, ymin=120, ymax=130)+
  annotate("text", x=1.5,y=103, label = signif(gender_frequecy_pvalue, digits = 2)) +
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) 

pdf("readmission_gender_distribution.pdf", width = 4, height = 5)
print(p1)
dev.off()




#----------- no of stay --------
#------------- Exploring length of stay

length_of_stay = unique(ICU_data[,c("subject_id","icustay_seq","Depression")])
wilcox.test(length_of_stay$icustay_seq ~ length_of_stay$Depression)

ggplot(length_of_stay, aes(x=icustay_seq,fill=Depression)) + geom_density() + 
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black"))

#

ICU_data_readmisssion = unique(ICU_data[Diease_classification=="Cardiology" & !is.na(Diease_classification),c("subject_id","Depression", "no_of_icu_admissions","Diease_classification")])
ICU_data_readmisssion$Depression_numeric = ifelse(ICU_data_readmisssion$Depression=="Yes",1,0)
lm(Depression_numeric ~ no_of_icu_admissions, data = ICU_data_readmisssion)

model <- glm(Depression_numeric ~ no_of_icu_admissions,
             data = ICU_data_readmisssion,
             family = binomial)
summary(model)



ICU_data_readmisssion = unique(ICU_data[Diease_classification=="Cardiology" & !is.na(Diease_classification),c("subject_id","Depression", "no_of_icu_admissions","Diease_classification")])
ICU_data_readmisssion$Depression_numeric = ifelse(ICU_data_readmisssion$Depression=="Yes",1,0)
model <- glm(Depression_numeric ~ no_of_icu_admissions,
             data = ICU_data_readmisssion,
             family = binomial)
summary(model)
exp(coef(model))
exp(confint(model))
OR <- exp(coef(model))
CI <- exp(confint(model))
results_cardio <- cbind(OR, CI)
results_cardio


#### 
ICU_data_gender = unique(ICU_data[,c("subject_id","Depression", "no_of_icu_admissions","gender")])
ICU_data_gender$Depression_numeric = ifelse(ICU_data_gender$Depression=="Yes",1,0)
ICU_data_gender$gender_numeric = ifelse(ICU_data_gender$gender=="F",1,0)
model <- glm(Depression_numeric ~ gender_numeric * no_of_icu_admissions,
             data = ICU_data_gender,
             family = binomial)
summary(model)
exp(coef(model))
exp(confint(model))
OR <- exp(coef(model))
CI <- exp(confint(model))
results_gender <- cbind(OR, CI)
results_gender


