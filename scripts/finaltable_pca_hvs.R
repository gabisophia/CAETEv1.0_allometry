library(dplyr)
df=read.csv("/home/bia/Desktop/paper_mestrado/caete_results_server/resultado_versao_CAETEv1.0/results_first_version_caete/nClimate_constant/new_integration2/csv_final/out_allpls_allruns_clean.csv",header=TRUE)
df_grouped=group_by(df,nx,ny,lat,lon,pls,add=FALSE)
colu=vars(gpp,ra,npp,rc,et,wue,cue,cmass,cleaf,cfroot,cawood,tleaf_cwm,twood_cwm,troot_cwm,aleaf_cwm,awood_cwm,aroot_cwm)
df_grouped_mean = summarise_at(df_grouped,.funs = mean,.vars=colu)
df_grouped_mean_traits<-df_grouped_mean[,17:22]
pca_all_meanruns<-prcomp(df_grouped_mean_traits, scale=TRUE)
summary(pca_all_meanruns)
biplot(pca_all_meanruns)
new_variables<-pca_all_meanruns$x
df_new_variables<-as.data.frame(new_variables)
colnames(df_new_variables)
pc1<-df_new_variables$PC1
write.csv(df_new_variables, file="/home/bia/Desktop/paper_mestrado/caete_results_server/resultado_versao_CAETEv1.0/results_first_version_caete/nClimate_constant/new_integration2/csv_final/pca_allpls_meanruns_pcs.csv")
final_table<-bind_cols(df_grouped_mean,df_new_variables)
write.csv(final_table, file = "/home/bia/Desktop/paper_mestrado/caete_results_server/resultado_versao_CAETEv1.0/results_first_version_caete/nClimate_constant/new_integration2/csv_final/finaltable_allpls_meanruns_pcs.csv")

table_mruns<-read.csv("/home/bia/Desktop/paper_mestrado/caete_results_server/resultado_versao_CAETEv1.0/results_first_version_caete/nClimate_constant/new_integration2/csv_final/finaltable_allpls_meanruns_pcs.csv")
#hvs all regions
ftable_50<-table_mruns[table_mruns$pls==50,23:25]
hv_50<-hypervolume(ftable_50, method="svm",name='50pls')
hv_50
plot.Hypervolume(hv_50,show.3d=TRUE, show.random=FALSE, show.density=FALSE, show.legend=TRUE, show.contour=FALSE, colors="blue1", cex.legend=2.)

ftable_100<-table_mruns[table_mruns$pls==100,23:25]
hv_100<-hypervolume(ftable_100, method="svm",name='100pls')
hv_100

ftable_200<-table_mruns[table_mruns$pls==200,23:25]
hv_200<-hypervolume(ftable_200, method="svm",name='200pls')
hv_200

ftable_500<-table_mruns[table_mruns$pls==500,23:25]
hv_500<-hypervolume(ftable_500, method = "svm",name='500pls')
hv_500

ftable_1000<-table_mruns[table_mruns$pls==1000,23:25]
hv_1000<-hypervolume(ftable_1000, method="svm",name='1000pls')
hv_1000

ftable_3000<-table_mruns[table_mruns$pls==3000,23:25]
hv_3000<-hypervolume(ftable_3000, method="svm",name='3000pls')
hv_3000
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
hv_dis50_100<-hypervolume_distance(hv_50,hv_100,type="centroid",check.memory = FALSE)
hv_dis50_100
hv_set50_100<-hypervolume_set(hv1 = hv_50,hv2=hv_100,verbose=TRUE,check.memory = FALSE)
hv_set50_100
get_volume(hv_set50_100)
hv_set50_100_ov<-hypervolume_overlap_statistics(hv_set50_100)
hv_set50_100_ov
                                                                                                                                                                                                                                                                    plot(hv_set50_100,show.random = TRUE, show.data=TRUE, show.density = FALSE, show.contour = FALSE, cex.legend=1.5)

hv_dis100_200<-hypervolume_distance(hv_100,hv_200, type="centroid", check.memory=FALSE)
hv_dis100_200
hv_set100_200<-hypervolume_set(hv1 = hv_100,hv2=hv_200,verbose=TRUE,check.memory = FALSE)
hv_set100_200
get_volume(hv_set100_200)
hv_set100_200_ov<-hypervolume_overlap_statistics(hv_set100_200)
hv_set100_200_ov
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
hv_dis200_500<-hypervolume_distance(hv_200,hv_500, type="centroid", check.memory=FALSE)
hv_dis200_500
hv_set200_500<-hypervolume_set(hv1 = hv_200,hv2=hv_500,verbose=TRUE,check.memory = FALSE)
hv_set200_500
get_volume(hv_set200_500)
hv_set200_500_ov<-hypervolume_overlap_statistics(hv_set200_500)
hv_set200_500_ov

hv_dis500_1000<-hypervolume_distance(hv_500,hv_1000, type="centroid", check.memory=FALSE)
hv_dis500_1000
hv_set500_1000<-hypervolume_set(hv1 = hv_500,hv2=hv_1000,verbose=TRUE,check.memory = FALSE)
hv_set500_1000
get_volume(hv_set500_1000)
hv_set500_1000_ov<-hypervolume_overlap_statistics(hv_set500_1000)
hv_set500_1000_ov

hv_dis1000_3000<-hypervolume_distance(hv_1000,hv_3000, type="centroid", check.memory=FALSE)
hv_dis1000_3000
hv_set1000_3000<-hypervolume_set(hv1 = hv_1000,hv2=hv_3000,verbose=TRUE,check.memory = FALSE)
hv_set1000_3000
get_volume(hv_set1000_3000)
hv_set1000_3000_ov<-hypervolume_overlap_statistics(hv_set1000_3000)
hv_set1000_3000_ov