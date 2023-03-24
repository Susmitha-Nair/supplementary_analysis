##### for project : Analysis for Das-WT MPS profile
##### aim : correlation between different property
##### Code written by: Susmitha Shankar
##### Lab : Khmelinskii Lab, IMB

amino_Acid<-c("A",
              "C",
              "D",
              "E",
              "F",
              "G",
              "H",
              "I",
              "K",
              "L",
              "M",
              "N",
              "P",
              "Q",
              "R",
              "S",
              "T",
              "V",
              "W",
              "Y")
amino_Acid<-as.data.frame(amino_Acid)
amino_Acid$Hydrophobicity<-hydrophobicity(amino_Acid$amino_Acid)
amino_Acid$charge<-charge(amino_Acid$amino_Acid)
amino_Acid$molecularWeight<-mw(amino_Acid$amino_Acid)
amino_Acid$aliphaticIndex<-aIndex(amino_Acid$amino_Acid)

for (i in 1:20) {
#i<-1 
 crucian<-crucianiProperties(amino_Acid$amino_Acid[i])
  amino_Acid$polarity[i]<-crucian[[1]][1]
  amino_Acid$hBond[i]<-crucian[[1]][3]
}

for (i in 1:20) {
  fsgai<-fasgaiVectors(amino_Acid$amino_Acid[i])
  amino_Acid$localFlexibility[i]<-fsgai[[1]][5]
  amino_Acid$bulkyProperty[i]<-fsgai[[1]][3]
  amino_Acid$electronicproperty[i]<-fsgai[[1]][6]
}

#row.names(amino_Acid)<-amino_Acid$amino_Acid
#amino_Acid$amino_Acid<-NULL

amino_Acid_melt<-melt(amino_Acid, id = "amino_Acid")
subset(amino_Acid_melt,(amino_Acid_melt$variable %in% c("Hydrophobicity","charge","hBond","localFlexibility","bulkyProperty","electronicproperty"))) %>%
#subset(amino_Acid_melt,(amino_Acid_melt$variable %in% c("bulkyProperty","hBond"))) %>%
  ggplot(aes(x=amino_Acid, y=value, group=variable)) +
  geom_line(aes(color=variable),alpha = 0.9)+
  geom_point(aes(color=variable),alpha = 0.2)+
  ggtitle("Physiochemical Property")+ ylab("Value")+ xlab("Amino Aicd")+
  #geom_ribbon(aes(ymin = Lower_Independent, ymax = Higher_Independent), alpha = 0.1) +
  theme_bw()

subset(amino_Acid_melt,(amino_Acid_melt$variable %in% c("aliphaticIndex","molecularWeight"))) %>%
  #subset(amino_Acid_melt,(amino_Acid_melt$variable %in% c("bulkyProperty","hBond"))) %>%
  ggplot(aes(x=amino_Acid, y=value, group=variable)) +
  geom_line(aes(color=variable),alpha = 0.9)+
  geom_point(aes(color=variable),alpha = 0.2)+
  ggtitle("Physiochemical Property")+ ylab("Value")+ xlab("Amino Aicd")+
  #geom_ribbon(aes(ymin = Lower_Independent, ymax = Higher_Independent), alpha = 0.1) +
  theme_bw()

