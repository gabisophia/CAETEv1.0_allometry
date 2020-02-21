library(TPD)
table<-read.csv("/home/bia/Desktop/paper_mestrado/caete_results_server/resultado_versao_CAETEv1.0/results_first_version_caete/nClimate_constant/new_integration2/csv_final/finaltable_allpls_meanruns_pcs.csv")
names(table)

multi_traits<-table[,23:25]

#plotting the TPDs for single traits
sp_caete<-table$pls
tleaf<-table$tleaf_cwm
TPD_tleaf<-TPDs(species=sp_caete,tleaf)
FD_tleaf<-REND(TPDs = TPD_tleaf)
plotTPD(TPD_tleaf,color="LimeGreen",leg=TRUE,leg.text = paste0("PLS: ",names(FD_tleaf$species$FRichness),"\n","R=", round(FD_tleaf$species$FRichness,3),"\n","E=", round(FD_tleaf$species$FEvenness,3),"\n","D=",round(FD_tleaf$species$FDivergence,3)))

sp_caete<-table$pls
twood<-table$twood_cwm
TPD_twood<-TPDs(species=sp_caete,twood)
FD_twood<-REND(TPDs = TPD_twood)
plotTPD(TPD_twood,color="SaddleBrown",leg=TRUE,leg.text = paste0("PLS: ",names(FD_twood$species$FRichness),"\n","R=", round(FD_twood$species$FRichness,3),"\n","E=", round(FD_twood$species$FEvenness,3),"\n","D=",round(FD_twood$species$FDivergence,3)))

sp_caete<-table$pls
troot<-table$troot_cwm
TPD_troot<-TPDs(species = sp_caete,troot)
FD_troot<-REND(TPDs = TPD_troot)
plotTPD(TPD_troot,color='BlueViolet',leg=TRUE,leg.text = paste0("PLS: ",names(FD_troot$species$FRichness),"\n","R=", round(FD_troot$species$FRichness,3),"\n","E=", round(FD_troot$species$FEvenness,3),"\n","D=",round(FD_troot$species$FDivergence,3)))

sp_caete<-table$pls
aleaf<-table$aleaf_cwm
TPD_aleaf<-TPDs(species = sp_caete,aleaf)
FD_aleaf<-REND(TPDs = TPD_aleaf)
plotTPD(TPD_aleaf,color='DarkGreen',leg=TRUE,leg.text = paste0("PLS: ",names(FD_aleaf$species$FRichness),"\n","R=", round(FD_aleaf$species$FRichness,3),"\n","E=", round(FD_aleaf$species$FEvenness,3),"\n","D=",round(FD_aleaf$species$FDivergence,3)))

sp_caete<-table$pls
awood<-table$awood_cwm
TPD_awood<-TPDs(species = sp_caete,awood)
FD_awood<-REND(TPDs = TPD_awood)
plotTPD(TPD_awood,color='Chocolate',leg=TRUE,leg.text = paste0("PLS: ",names(FD_awood$species$FRichness),"\n","R=", round(FD_awood$species$FRichness,3),"\n","E=", round(FD_awood$species$FEvenness,3),"\n","D=",round(FD_awood$species$FDivergence,3)),leg.pos = "topright")

sp_caete<-table$pls
aroot<-table$aroot_cwm
TPD_aroot<-TPDs(species = sp_caete,aroot)
FD_aroot<-REND(TPDs = TPD_aroot)
plotTPD(TPD_aroot,color='DarkSlateBlue',leg=TRUE,leg.text = paste0("PLS: ",names(FD_aroot$species$FRichness),"\n","R=", round(FD_aroot$species$FRichness,3),"\n","E=", round(FD_aroot$species$FEvenness,3),"\n","D=",round(FD_aroot$species$FDivergence,3)))

#TPD for muti_traits
TPDs_multitrait<-TPDs(species=sp_caete, multi_traits)
FD_multi<-REND(TPDs=TPDs_multitrait)

#calculating the dissimilarities for single trait
dissim_tleaf<-dissim(TPD_tleaf)
dissim_tleaf

dissim_twood<-dissim(TPD_twood)
dissim_twood

dissim_troot<-dissim(TPD_troot)
dissim_troot

dissim_aleaf<-dissim(TPD_aleaf)
dissim_aleaf

dissim_awood<-dissim(TPD_awood)
dissim_awood

dissim_aroot<-dissim(TPD_aroot)
dissim_aroot

##calculating the dissimilarities for multi trait
dissim_multi<-dissim(TPDs_multitrait)
dissim_multi