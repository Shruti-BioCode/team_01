library(data.table)
library(ggplot2)
library(readxl)
library(openxlsx)
library(explore)
library(dplyr)
library("FactoMineR")
library("factoextra")

# read data from merged file
ICU_data = data.table(read_xlsx(path = "Team1.xlsx"))

#---------- processing / cleaning the data -----------
# calculate the actual number of ICU stay by adding admissions to different hospitals
ICU_data = ICU_data[,no_of_icu_admissions:=sum(max_icustay_seq), by=c("subject_id")]

# check for multiple entries of race 
ICU_data = ICU_data[,count_race:=uniqueN(race), by = c("subject_id")]
ethinicity = fread(input = "ethinicity.txt", sep = "\t", header = T, stringsAsFactors = F)
race = unique(ICU_data[,c("subject_id","race")])
race = merge.data.table(race,ethinicity,by=c("race"))
race = race[,count_ethinicity:=uniqueN(ethinicity[!is.na(ethinicity)]), by=c("subject_id")]
race = unique(race[count_ethinicity==1 & !is.na(ethinicity), c("subject_id","ethinicity")])
ICU_data = merge.data.table(ICU_data,race, by=c("subject_id"), all.x = T)


# add ctaegory definition taken from chatGPT
diagnosis_classification = data.table(read_xlsx(path = "classified_diagnosis_from_file.xlsx"))
ICU_data = merge.data.table(ICU_data,diagnosis_classification, by =c("ICU_diagnosis"),all.x = T)
ICU_data = ICU_data[,no_of_diagnosis_classification:=uniqueN(Diease_classification),by=c("subject_id")]




######  check for missing data in 
cols_to_check = c("gender","ethinicity","los_icu","age","no_of_icu_admissions")
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

ICU_data_variable = unique(ICU_data[,c("subject_id","gender","age","los_icu","race","Mortality","no_of_icu_admissions")])
MFA(base, group, type = rep("s",length(group)), ind.sup = NULL, 
     name.group = NULL, num.group.sup = NULL, graph = TRUE)

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
patient_gender_frequecy$label = paste0(patient_gender_frequecy$count,"\n(",round(patient_gender_frequecy$perc,0),"%)")

patient_gender_frequecy <- patient_gender_frequecy %>%
  group_by(depressed) %>%
  mutate(label_y = cumsum(perc))

p1 = ggplot(patient_gender_frequecy, aes(depressed, perc, fill=gender)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("F"="#F98072","M"="#36454F")) + 
  geom_text(aes(y=label_y, label = label), vjust = 2.5, colour = "white") +
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) +
  annotate("text", x=1.5,y=103, label = signif(gender_frequecy_pvalue, digits = 2))

pdf("gender_distribution.pdf")
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

patient_mortality_frequecy <- patient_mortality_frequecy %>%
  group_by(depressed) %>%
  mutate(label_y = cumsum(perc))

p1 = ggplot(patient_mortality_frequecy, aes(depressed, perc, fill=Mortality)) +
  geom_bar(stat = "identity")+ scale_fill_manual(values = c("Yes"="#008080","No"="#FF7F51")) + 
  geom_text(aes(y=label_y, label = label), vjust = 2.5, colour = "white") +
  theme_classic() + theme(axis.text = element_text(size=12, colour = "black")) +
  annotate("text", x=1.5,y=103, label = signif(mortality_frequecy_pvalue, digits = 2))

pdf("mortality_distribution.pdf")
print(p1)
dev.off()


#------------- Exploring 


