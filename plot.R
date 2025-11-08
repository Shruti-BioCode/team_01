ICU_DiseaseClassification_Depression_Clean = data.table(read_xlsx(path = "ICU_DiseaseClassification_Depression_Clean.xlsx"))
p1 = ggplot(ICU_DiseaseClassification_Depression_Clean[p.value=="<0.0001"],aes(y=Percentage, x=Diease_classification ,fill=Depression,)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Yes"="#78C1B6","No"="#216465")) + 
  theme_classic() + theme(axis.text.x = element_text(size=12, colour = "black",angle = 40,hjust = 1),
                          axis.text.y = element_text(size=12, colour = "black")) 
pdf("common_IC_diagnosis.pdf",height = 5, width = 5)
print(p1)
dev.off()

