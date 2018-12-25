##################Please use seed as 1234 for replicating the result#####################################
install.packages('psych')
install.packages('fpc')
install.packages('dbscan')
install.packages('cba')
install.packages('klaR')
install.packages("FinCal")
install.packages('sampling')
install.packages("reshape2")
install.packages("corrplot")
install.packages("operators")
install.packages("RWeka")
install.packages("Matrix")
install.packages("arules")
install.packages('cluster')
install.packages("mclust")
install.packages("ff")
install.packages("igraph")
install.packages("GGally")
install.packages('mltools')
install.packages('caret')
install.packages('cooccur')
install.packages('varhandle')
install.packages("factoextra")
install.packages("FactoMineR")
install.packages('randomForest')
install.packages("mice")
install.packages("lattice")
install.packages("missForest")
install.packages('fclust')
install.packages("NbClust")
install.packages("clValid")
install.packages('pvclust')
install.packages("plotrix")
library('psych')
library('fpc')
library('dbscan')
library('cba')
library('klaR')
library("FinCal")
library('sampling')
library("reshape2")
library("corrplot")
library(operators)
library(RWeka)
library(Matrix)
library("arules")
library('cluster')
library("mclust")
library(ff)
library(igraph)
library(GGally)
library('mltools')
library('caret')
library('cooccur')
library('varhandle')
library(factoextra)
library(FactoMineR)
library('randomForest')
library(mice)
library(lattice)
library(missForest)
library('fclust')
library(NbClust)
library(clValid)
library('pvclust')
library("plotrix")

patient <- read.csv("Patient_data.csv")

patient <- subset(patient, select = c(age_Group,sex,race,mental_Illness,intellectual_disability,
                                      autism_Spectrum,developmental_Disability,alcohol_Related_Disorder,
                                      drug_Substance_Disorder,Mobility_Impairment_Disorder,
                                      hearing_Visual_Impairment,hyperlipidemia,high_Blood_Pressure,
                                      diabetes,obesity,heart_Attack,stroke,other_Cardiac,pulmonary_Asthma,
                                      alzheimer,kidney_Disease,liver_Disease,endocrine_Condition,
                                      neurological_Condition,traumatic_Brain_Injury,joint_Disease,
                                      cancer,other_Chronic_Med_Condition,Smokes,serious.Mental.Illness))

----------------------------------------------Data Imputation with mice--------------------------------------------
## Data imputation on entire dataset
  
patient_imputed <- mice(patient, m=2, maxit = 5, method = 'pmm', seed = 1234)  

completeData_impu <- complete(patient_imputed,2)

summary(completeData_impu)
#plot(patient_imputed)

  
--------------------------------------For calcluating imputation error--------------------------------------------------------------
## Very good imputation  
#patient <- data.frame(sapply(patient,as.factor))
    
col_means_p <- colMeans(completeData_impu_x) ## All the rows in the data set after imputation(completeData_impu_x).   
col_means_i<- colMeans(patient_x)# NA are not inccluded in the mean for data set (patient_x) and some irrelevent features also removed
                                                                     
  
#boxplot(Final_result_imputed)


col_means_p <- as.data.frame(col_means_p)
col_means_i <- as.data.frame(col_means_i)
---------------------------------------------Sampling error--------------------------------------------
#Final_result_imputed_x <- apply(final_sampling_error[2:3],2, func_sam_error)
#sampling.error(sm=n$col_means_s,mu=m)

Good result    
mean(sampling.error(sm=final_sampling_error$col_means_s,mu=final_sampling_error$col_means_i))

-----------------------------------------------One Hot encoding-----------------------------
  
patient_enco <- dummyVars("~ age_Group+sex+race", data = completeData_impu)
#~ day + time + day:time
trsf <- data.frame(predict(patient_enco, newdata = completeData_impu))

completeData_impu_bind <- cbind(trsf,completeData_impu)

------------------------------------Selection of particluar column after one hot encoding--------------------------------------------

completeData_impu <- subset(completeData_impu_bind, select = c(age_Group.ADULT,age_Group.CHILD,sex.FEMALE,sex.MALE,race.BLACK.ONLY,
                                                                 race.MULTI.RACIAL,race.OTHER,race.WHITE.ONLY,mental_Illness,intellectual_disability,
                                                                 autism_Spectrum,developmental_Disability,alcohol_Related_Disorder,
                                                                 drug_Substance_Disorder,Mobility_Impairment_Disorder,
                                                                 hearing_Visual_Impairment,hyperlipidemia,high_Blood_Pressure,
                                                                 diabetes,obesity,heart_Attack,stroke,other_Cardiac,pulmonary_Asthma,
                                                                 alzheimer,kidney_Disease,liver_Disease,endocrine_Condition,
                                                                 neurological_Condition,traumatic_Brain_Injury,joint_Disease,
                                                                 cancer,other_Chronic_Med_Condition,Smokes,serious.Mental.Illness))

-------------------------------------Factoring of the required element into 0 and 1------------------------------------------- 
  
completeData_impu$mental_Illness <- factor(completeData_impu$mental_Illness, levels=c('YES','NO'),
                                             labels=c('1','0'))
completeData_impu$intellectual_disability <- factor(completeData_impu$intellectual_disability, levels=c('YES','NO'),
                                                    labels=c('1','0'))
completeData_impu$autism_Spectrum <- factor(completeData_impu$autism_Spectrum, levels=c('YES','NO'),
                                            labels=c('1','0'))
completeData_impu$developmental_Disability <- factor(completeData_impu$developmental_Disability, levels=c('YES','NO'),
                                                     labels=c('1','0'))
completeData_impu$alcohol_Related_Disorder <- factor(completeData_impu$alcohol_Related_Disorder, levels=c('YES','NO'),
                                                     labels=c('1','0'))
completeData_impu$drug_Substance_Disorder <- factor(completeData_impu$drug_Substance_Disorder, levels=c('YES','NO'),
                                                    labels=c('1','0'))
completeData_impu$Mobility_Impairment_Disorder <- factor(completeData_impu$Mobility_Impairment_Disorder, levels=c('YES','NO'),
                                                         labels=c('1','0'))
completeData_impu$hearing_Visual_Impairment <- factor(completeData_impu$hearing_Visual_Impairment, levels=c('YES','NO'),
                                                      labels=c('1','0'))
completeData_impu$hyperlipidemia <- factor(completeData_impu$hyperlipidemia, levels=c('YES','NO'),
                                           labels=c('1','0'))
completeData_impu$high_Blood_Pressure <- factor(completeData_impu$high_Blood_Pressure, levels=c('YES','NO'),
                                                labels=c('1','0'))
completeData_impu$other_Cardiac <- factor(completeData_impu$other_Cardiac, levels=c('YES','NO'),
                                          labels=c('1','0'))
completeData_impu$obesity <- factor(completeData_impu$obesity, levels=c('YES','NO'),
                                    labels=c('1','0'))
completeData_impu$heart_Attack <- factor(completeData_impu$heart_Attack, levels=c('YES','NO'),
                                         labels=c('1','0'))
completeData_impu$stroke <- factor(completeData_impu$stroke, levels=c('YES','NO'),
                                   labels=c('1','0'))
completeData_impu$pulmonary_Asthma <- factor(completeData_impu$pulmonary_Asthma, levels=c('YES','NO'),
                                             labels=c('1','0'))
completeData_impu$alzheimer <- factor(completeData_impu$alzheimer, levels=c('YES','NO'),
                                      labels=c('1','0'))
completeData_impu$kidney_Disease <- factor(completeData_impu$kidney_Disease, levels=c('YES','NO'),
                                           labels=c('1','0'))
completeData_impu$liver_Disease <- factor(completeData_impu$liver_Disease, levels=c('YES','NO'),
                                          labels=c('1','0'))
completeData_impu$endocrine_Condition <- factor(completeData_impu$endocrine_Condition, levels=c('YES','NO'),
                                                labels=c('1','0'))
completeData_impu$neurological_Condition <- factor(completeData_impu$neurological_Condition, levels=c('YES','NO'),
                                                   labels=c('1','0'))
completeData_impu$traumatic_Brain_Injury <- factor(completeData_impu$traumatic_Brain_Injury, levels=c('YES','NO'),
                                                   labels=c('1','0'))
completeData_impu$cancer <- factor(completeData_impu$cancer, levels=c('YES','NO'),
                                   labels=c('1','0'))
completeData_impu$other_Chronic_Med_Condition <- factor(completeData_impu$other_Chronic_Med_Condition, levels=c('YES','NO'),
                                                        labels=c('1','0'))
completeData_impu$Smokes <- factor(completeData_impu$Smokes, levels=c('YES','NO'),
                                   labels=c('1','0'))
completeData_impu$serious.Mental.Illness <- factor(completeData_impu$serious.Mental.Illness, levels=c('YES','NO'),
                                                   labels=c('1','0'))
completeData_impu$joint_Disease <- factor(completeData_impu$joint_Disease, levels=c('YES','NO'),
                                          labels=c('1','0'))
completeData_impu$diabetes <- factor(completeData_impu$diabetes, levels=c('YES','NO'),
                                     labels=c('1','0'))

------------------------------Now Unfactoring for conversion into numerical--------------------------------------------------------  
#completeData_impu$age_Group <- unfactor(completeData_impu$age_Group)
#completeData_impu$sex <- unfactor(completeData_impu$sex)
#completeData_impu$race <- unfactor(completeData_impu$race)
completeData_impu$mental_Illness <- unfactor(completeData_impu$mental_Illness)
completeData_impu$intellectual_disability <- unfactor(completeData_impu$intellectual_disability)
completeData_impu$autism_Spectrum <- unfactor(completeData_impu$autism_Spectrum)
completeData_impu$developmental_Disability <- unfactor(completeData_impu$developmental_Disability)
completeData_impu$alcohol_Related_Disorder <- unfactor(completeData_impu$alcohol_Related_Disorder)
completeData_impu$drug_Substance_Disorder <- unfactor(completeData_impu$drug_Substance_Disorder) 
completeData_impu$Mobility_Impairment_Disorder <- unfactor(completeData_impu$Mobility_Impairment_Disorder)
completeData_impu$hearing_Visual_Impairment <- unfactor(completeData_impu$hearing_Visual_Impairment)
completeData_impu$hyperlipidemia <- unfactor(completeData_impu$hyperlipidemia)
completeData_impu$high_Blood_Pressure <- unfactor(completeData_impu$high_Blood_Pressure)
completeData_impu$diabetes <- unfactor(completeData_impu$diabetes)
completeData_impu$obesity <- unfactor(completeData_impu$obesity)
completeData_impu$heart_Attack <- unfactor(completeData_impu$heart_Attack)
completeData_impu$stroke <- unfactor(completeData_impu$stroke)
completeData_impu$other_Cardiac <- unfactor(completeData_impu$other_Cardiac)
completeData_impu$pulmonary_Asthma <- unfactor(completeData_impu$pulmonary_Asthma)
completeData_impu$alzheimer <- unfactor(completeData_impu$alzheimer)
completeData_impu$kidney_Disease <- unfactor(completeData_impu$kidney_Disease)
completeData_impu$liver_Disease <- unfactor(completeData_impu$liver_Disease)
completeData_impu$endocrine_Condition <- unfactor(completeData_impu$endocrine_Condition)
completeData_impu$neurological_Condition <- unfactor(completeData_impu$neurological_Condition)
completeData_impu$traumatic_Brain_Injury <- unfactor(completeData_impu$traumatic_Brain_Injury)
completeData_impu$joint_Disease <- unfactor(completeData_impu$joint_Disease)
completeData_impu$cancer <- unfactor(completeData_impu$cancer)
completeData_impu$other_Chronic_Med_Condition <- unfactor(completeData_impu$other_Chronic_Med_Condition)
completeData_impu$Smokes <- unfactor(completeData_impu$Smokes)
completeData_impu$serious.Mental.Illness  <- unfactor(completeData_impu$serious.Mental.Illness )


---------------------------------------Applying Sampling and checking its feasiblity-----------------
#Good Results again
  
set.seed(1234)
sampled_completeData_impu <- completeData_impu[sample(nrow(completeData_impu),17000 ), ]

func_propor_sampl<-function(n)
{
  prop.table(table(n))   
}


Final_result_imputed_sampled <- apply(sampled_completeData_impu[1:30],2, func_propor_sampl)
Final_result_completeData_impu <- apply(completeData_impu[1:30],2,func_propor_sampl)

## checking sampling error
Sampling_error<- sampling.error(sm=mean(sampled_completeData_impu$stroke),mu=mean(completeData_impu$stroke) ) 

--------------------------------------------------------------------------------------------------------------
------------------------------------------ EDA part correlation ----------------------------------------------

##For making heat map  
  
## This is of good use
sampled_completeData_impu_heat <- subset(sampled_completeData_impu, select = c(sex.FEMALE,race.WHITE.ONLY,intellectual_disability,
                                                                                autism_Spectrum,developmental_Disability,alcohol_Related_Disorder,
                                                                                drug_Substance_Disorder,
                                                                                hearing_Visual_Impairment,hyperlipidemia,high_Blood_Pressure,
                                                                                diabetes,obesity,race.MULTI.RACIAL,sex.MALE,heart_Attack,stroke,other_Cardiac,pulmonary_Asthma,
                                                                                alzheimer,kidney_Disease,liver_Disease,endocrine_Condition,
                                                                                neurological_Condition,race.BLACK.ONLY,traumatic_Brain_Injury,joint_Disease,
                                                                                cancer,Smokes))


  
data_matrix <- as.matrix(sampled_completeData_impu_heat)
co_occurrence <- t(data_matrix) %*% data_matrix


nba_heatmap <- heatmap(co_occurrence, Rowv=NA, Colv=NA, col = heat.colors(256), margins=c(10,15))
------------------------------------------------------------------------------------------------------------
---------------------------------------------------More EDA---------------------------------
## This graph is for EDA of relative feature frequency[ EDA PART]
  
sampled_completeData_impu_plot <- sampled_completeData_impu[,-c(9,35)]
Trans.fullcluster <- as(as.matrix(sampled_completeData_impu_plot), "transactions")   


library("RColorBrewer")
arules::itemFrequencyPlot(Trans.fullcluster,
                          topN=33,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)")   

------------------------------------------------------------------------------------------------  
  
  
-------------------IMPORTANT--------------!PCA!----------Checking the Dissimlairy plot along with pearson------------- 
  
set.seed(1234)
Temp_sampling <- completeData_impu[sample(nrow(completeData_impu),7000 ), ]
temp_sampling_vat <- completeData_impu[sample(nrow(completeData_impu),5000 ), ]   
Temp_sampling_J <- completeData_impu[sample(nrow(completeData_impu),10000),] 
#Sampled_complete <- completeData_impu[sample(nrow(completeData_impu),7000 ), ]  
#sampled_scale <- scale(Sampled_complete)
#Vat.garph <- VAT(sampled_completeData_impu)  

#df_scaled <- scale(Sampled_complete)
# Method :1 
dis_impu<- get_dist(Temp_sampling,method="pearson") 
Result_validation_clustering <- dissplot(dis_impu) ## This result is used in the report for clustering tendency with small sample size due to memory issues
## This is done multiple times on different samples of different sizes to get promising results.                                       


--------------
### Checking with Kmeans ------------
set.seed(1234)
km.res <- kmeans(Temp_sampling, 3)
graph_kmeans_temp <- dissplot(dis_impu, labels = km.res$cluster)

# Methd :2 : VAT
VAT_imp <- VAT(dis_impu)

-------------------Imp----------------
## This will be used intensively for the hierarchiral and rock clustering  
  
d_jaccard <- dissimilarity(as.matrix(sampled_completeData_impu))  

########################### Trial and error###############################
## Very good result
##0.28 is the value. The data is clusterable

res <- get_clust_tendency(sampled_completeData_impu, n = nrow(sampled_completeData_impu)-1, graph = FALSE)
res$hopkins_stat ##hopkins statastics

res$plot + 
  scale_fill_gradient(low = "steelblue", high = "white")

#########################bartlett test for spherecity##################################################
library("psych")
bartlett.test(sampled_completeData_impu)
KMO(sampled_completeData_impu)

-------------------------------------------1: Part A:Usefull 3 methods for optimal clustering with K means:-------------------------
### Very good results same for all the three distances Jaccard, binary, and pearson
# Elbow method  

Kmeans_albow_Imp <- fviz_nbclust(sampled_completeData_impu, kmeans, method = "wss",diss = get_dist(sampled_completeData_impu,method="pearson")) +labs(subtitle = "Elbow method")

#### Testing with binary 

Kmeams_albow_Imp_B <- fviz_nbclust(sampled_completeData_impu, kmeans, method = "wss",diss = get_dist(sampled_completeData_impu,method="binary")) +labs(subtitle = "Elbow method")

### testing with Jaccard 
###d_jaccard_vat_elbow <- dissimilarity(as.matrix(sampled_completeData_impu))

Kmeans_albow_Imp_J <- fviz_nbclust(sampled_completeData_impu, kmeans, method = "wss",diss = d_jaccard_vat_elbow) +labs(subtitle = "Elbow method")

--------------------------------------------------------------------------------------------------------
-----------------------------------------------------For the Hclust now---------------------------------

# Elbow method

hclust_albow_Imp <- fviz_nbclust(sampled_completeData_impu, hcut, method = "wss",diss = get_dist(sampled_completeData_impu,method="pearson")) +labs(subtitle = "Elbow method")



-------------------------1: Part A:Usefull 30 methods for optimal clustering (jaccard and pearson as distance):--------------------------------------------
  
nb_e_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                  max.nc = 10, method = "complete", index ="duda")


nb_p_imp <- NbClust(sampled_completeData_impu,distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                max.nc = 10, method = "complete", index ="pseudot2")

#Number_clusters     Value_Index 
#5.0000         30.7957 

nb_b_imp <- NbClust(sampled_completeData_impu, distance = d,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                max.nc = 10, method = "complete", index ="beale")

#Number_clusters     Value_Index 
#5.0000          0.2433 

#####Usefull
nb_h_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                max.nc = 10, method = "complete", index ="hartigan")
#Number_clusters     Value_Index 
#3.000        1816.335 

nb_ch_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                 max.nc = 10, method = "complete", index ="ch")

#Number_clusters     Value_Index 
#3.000        2613.612 
#####################################Frey good for two clusters

nb_rat_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                 max.nc = 10, method = "complete", index ="ratkowsky")

#Number_clusters     Value_Index 
#4.0000          0.1197 

nb_cin_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                      max.nc = 10, method = "complete", index ="cindex")

#Number_clusters     Value_Index 
#9.0000          0.3114 

nb_gplus_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                      max.nc = 10, method = "complete", index ="gplus")



nb_db_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                        max.nc = 10, method = "complete", index ="db")


nb_db_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = d_jaccard, min.nc = 2,
                     max.nc = 10, method = "complete", index ="db")

#Number_clusters     Value_Index 
#2.0000             2.0754

nb_ball_imp <- NbClust(sampled_completeData_impu, distance = NULL,diss = get_dist(sampled_completeData_impu,method="pearson"), min.nc = 2,
                      max.nc = 10, method = "complete", index ="ball")


#Number_clusters     Value_Index 
#3.00                10694.25 


-------------------------------------------------1:Part B:-Internal validation:Connectivity,separation,compactness  : --------------------------------------
  #It shows Connectivity,Dunn and silhouette  
  # Compute clValid : It says 2 clusters
  
clmethods_imp <- c("hierarchical","kmeans","")
intern_imp <- clValid(sampled_completeData_impu, nClust = 2:9,
                  clMethods = clmethods,metric = "correlation", validation = "internal",maxitems=nrow(Sampled_complete))

# Summary
summary(intern_imp)

# Plot
plot(intern_imp)

# Display only optimal Scores
Clustering Methods:
  hierarchical kmeans 

Cluster sizes:
  2 3 4 5 6 7 8 9 

Validation Measures:
                               2         3         4         5         6         7         8         9

hierarchical Connectivity     2.9290    8.7869  141.8242  166.5433  204.1933  296.1587  296.1587  296.1587
Dunn                          0.2174    0.1432    0.0703    0.0703    0.0703    0.0703    0.0703    0.0703
Silhouette                    0.3707    0.2773    0.1700    0.1173    0.1041    0.0861    0.0624    0.0501
kmeans       Connectivity     38.1028   40.8623   66.6516  265.6270  595.0421  632.8861 1541.1139 1594.8480
Dunn                          0.1005    0.1005    0.1029    0.0570    0.0570    0.0586    0.0506    0.0508
Silhouette                    0.2767    0.1767    0.2985    0.2747    0.2890    0.3105    0.2994    0.2911

Optimal Scores:
  
             Score  Method       Clusters
Connectivity 2.9290 hierarchical 2       
Dunn         0.2174 hierarchical 2       
Silhouette   0.3707 hierarchical 2       

---IMP---
##For PAM this has been run with different size of samples[smaller size] due to the heavy computational needs
##of PAM. It has been run standalone with two samples to get consolidate results. 
## Running for PAM
set.seed(1234)
pam_sample <- sampled_completeData_impu[sample(nrow(sampled_completeData_impu),12000 ), ]

clmethods_imp_pam <- c("pam")
intern_imp_pam <- clValid(pam_sample, nClust = 2:9,
                      clMethods = clmethods_imp_pam,metric = "correlation", validation = "internal",maxitems=nrow(pam_sample))

intern_imp_pam_12000 <- clValid(pam_sample, nClust = 2:9,
                          clMethods = clmethods_imp_pam,metric = "correlation", validation = "internal",maxitems=nrow(pam_sample))


## Resuts for PAM : 8000


## Results for PAM :12000
Validation Measures:
                    2        3        4        5        6        7        8        9

pam Connectivity   31.0694 450.1298 451.4599 397.0333 470.1567 455.6183 481.6813 765.9270
Dunn               0.0987   0.0490   0.0530   0.0571   0.0585   0.0598   0.0609   0.0578
Silhouette         0.2914   0.2274   0.2278   0.2449   0.2880   0.3106   0.3306   0.3306

Optimal Scores:
  
Score        Method         Clusters
Connectivity 31.0694 pam    2       
Dunn          0.0987 pam    2       
Silhouette    0.3306 pam    8       

                    2        3        4        5        6        7        8        9

pam Connectivity   26.6948 367.1397 368.5667 321.4833 461.6556 460.5528 427.7310 617.9571
Dunn               0.1029  0.0511   0.0538   0.0585   0.0598   0.0598   0.0602   0.0568
Silhouette         0.2911  0.2278   0.2265   0.2454   0.2873   0.2959   0.3157   0.2841


optimal Results

Connectivity 26.6948 pam    2       
Dunn          0.1029 pam    2       
Silhouette    0.3157 pam    8 


--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
===========================================================================================================
------------------------------------------Part C: Stability ---------------------------------------
## Stability measures
Good results : pointing towards 2  
  
clmethods_imp <- c("hierarchical","kmeans")
stab_imp <- clValid(sampled_completeData_impu, nClust = 2:5, clMethods = clmethods,maxitems=nrow(sampled_completeData_impu),
                validation = "stability")

summary(stab_imp)
plot(stab_imp)

Optimal Scores:
  
Score      Method       Clusters
APN 0.0006 hierarchical 2  # should be less     
AD  2.1342 kmeans       5  # less  
ADM 0.0023 hierarchical 2  # less      
FOM 0.2715 kmeans       5  # less

### checking for Pam with different sample size and standalone to as it is heavily computationaly intensive.
clmethods_imp_pam <- c("pam")
stab_imp_pam <- clValid(pam_sample, nClust = 2:5, clMethods = clmethods_imp_pam,maxitems=nrow(pam_sample),
                    validation = "stability")

summary(stab_imp_pam)

Score  Method Clusters
APN 0.0250 pam    2       
AD  2.0746 pam    5       
ADM 0.0487 pam    2       
FOM 0.2660 pam    5 

#Cluster cumlative of PAM and hierarchy

Optimal Scores:
  
    Score  Method       Clusters
APN 0.0006 hierarchical 2  
AD  2.0746 pam          5  
ADM 0.0023 hierarchical 2  
FOM 0.2660 pam          5  

############################################################################################
==================================Part D ::Doing clustering now for hierarchichal cluster================================================
  
clustering_imp <-  hclust(get_dist(sampled_completeData_impu,method="pearson"), method="complete")## with pearson
clustering_imp <- hclust(d_jaccard, method = "complete")## with jaccard
  

clusterCut_imp <- cutree(clustering_imp, 2)## cutting cluster with 2

clusterCut_imp_DF <- data.frame(clusterCut_imp)## conversion into dataframe

=============================================MAking the clustered DataFrame===========================
Clustered_dataFrame <- merge(sampled_completeData_impu, clusterCut_imp_DF, by=0, all=TRUE)##[merging by common row number]

dim(Clustered_dataFrame)

Clustered_dataFrame <- Clustered_dataFrame[,-c(1)]## romoving unwanted column
str(Clustered_dataFrame)

#####Making 3 clusters now

cluster_1 <- Clustered_dataFrame[Clustered_dataFrame$clusterCut_imp %in% c(1), ]##making cluster 1
cluster_1 <- cluster_1[,-c(36,35,9)]
#cluster_1 <- data.frame(sapply(cluster_1,as.factor))
cluster_2 <- Clustered_dataFrame[Clustered_dataFrame$clusterCut_imp %in% c(2), ]##making cluster 2
cluster_2 <- cluster_2[,-c(36,35,9)]
#cluster_2 <- data.frame(sapply(cluster_2,as.factor))
#cluster_3 <- Clustered_dataFrame[Clustered_dataFrame$clusterCut_imp %in% c(3), ]
#cluster_3 <- cluster_3[,-c(36,35,9)]
  
=======================================================================================================================
======================================================================================================================= 
=======================================================Cluster:1 and 2 are ready for apriori==============================
#install.packages('Tertius', repos='http://cran.us.r-project.org')
#ter_clus1 <- Tertius(Trans.cluster_1, control = NULL)  
###Playing with Cluster 1
## turning into transaction  
  
  
  
Trans.cluster_1 <- as(as.matrix(cluster_1), "transactions") ## converting into transactionsal form
Trans.cluster_2 <- as(as.matrix(cluster_2), "transactions") ## converting into transactionsal form
#summary(Trans.cluster_1)

##Applying Apriori Rules
apriori_rules_c1 <- apriori(Trans.cluster_1, parameter = list(supp=0.005, conf=0.3,minlen=2))## applying apriori
apriori_rules_c2 <- apriori(Trans.cluster_2, parameter = list(supp=0.003, conf=0.2,minlen=2))## applying apriori
##Pruning rules
apriori_rules_c1 <- apriori_rules_c1[!is.redundant(apriori_rules_c1)] ##pruning redundant rule
apriori_rules_c2 <- apriori_rules_c2[!is.redundant(apriori_rules_c2)] ##pruning redundant rule

subset.rules <- which(colSums(is.subset(apriori_rules_c1, apriori_rules_c1)) > 1) ##Removing subset rules
apriori_rules_c1 <- apriori_rules_c1[-subset.rules] # remove subset rules.

subset.rules <- which(colSums(is.subset(apriori_rules_c2, apriori_rules_c2)) > 1) ##Removing subset rules
apriori_rules_c2 <- apriori_rules_c2[-subset.rules] # remove subset rules.

## adding 1 quality indexes to the rules for cluster 1
quality(apriori_rules_c1) <- cbind(quality(apriori_rules_c1),
                                   yuleQ = interestMeasure(apriori_rules_c1, measure = "yuleQ",
                                                           transactions = as.matrix(cluster_1)))

## adding 1 quality indexes to the rules for cluster 1
quality(apriori_rules_c1) <- cbind(quality(apriori_rules_c1),
                                   oddsRatio = interestMeasure(apriori_rules_c1, measure = "oddsRatio",
                                                               transactions = as.matrix(cluster_1)))
----------------
## adding 1 quality indexes to the rules for cluster 2
quality(apriori_rules_c2) <- cbind(quality(apriori_rules_c2),
                                   yuleQ = interestMeasure(apriori_rules_c2, measure = "yuleQ",
                                                           transactions = as.matrix(cluster_2)))

## ## adding 1 quality indexes to the rules for cluster 2
quality(apriori_rules_c2) <- cbind(quality(apriori_rules_c2),
                                   oddsRatio = interestMeasure(apriori_rules_c2, measure = "oddsRatio",
                                                               transactions = as.matrix(cluster_2)))


## Subsetting the rules in the RHS to get only some specific rules

final_rules_subset_c1 <- subset(apriori_rules_c1, (rhs %in% c("intellectual_disability","autism_Spectrum","developmental_Disability","alcohol_Related_Disorder","drug_Substance_Disorder",
                                                              "Mobility_Impairment_Disorder","hearing_Visual_Impairment","hyperlipidemia","high_Blood_Pressure","diabetes",
                                                              "obesity","heart_Attack","stroke","other_Cardiac","pulmonary_Asthma","alzheimer","kidney_Disease","liver_Disease","endocrine_Condition",
                                                              "neurological_Condition","joint_Disease","cancer","other_Chronic_Med_Condition")))

final_rules_subset_c2 <- subset(apriori_rules_c2, (rhs %in% c("intellectual_disability","autism_Spectrum","developmental_Disability","alcohol_Related_Disorder","drug_Substance_Disorder",
                                                              "Mobility_Impairment_Disorder","hearing_Visual_Impairment","hyperlipidemia","high_Blood_Pressure","diabetes",
                                                              "obesity","heart_Attack","stroke","other_Cardiac","pulmonary_Asthma","alzheimer","kidney_Disease","liver_Disease","endocrine_Condition",
                                                              "neurological_Condition","joint_Disease","cancer","other_Chronic_Med_Condition")))

## conversion of transactions into dataframe for better visuals 

m1 <- as(final_rules_subset_c1, "data.frame")
dim(m1)
m2 <- as(final_rules_subset_c2, "data.frame")
dim(m2)


## Sorting of transactios on lift in descending order for both the clusters

m1 <- as(final_rules_subset_c1, "data.frame")
m1 <- m1[order(-m1$lift),]
m1 <- m1[1:40,]

m2 <- as(final_rules_subset_c2, "data.frame")
m2 <- m2[order(-m2$lift),]
m2 <- m2[1:30,]


### For making link graph  
subrules2 <- head(sort(l2, by="confidence"),20)
ig <- plot( subrules2, method="graph", control=list(type="items") )



#subRules<-final_rules_subset_c2[quality(final_rules_subset_c2)$confidence>0.1]
#plot(subRules)


### For exporting tables into Html format in browser
newobject <- xtable::xtable(m1)
xtable::print.xtable(newobject, type="html", file="filename2.html")

---------------------------------------Plotting--------------------------------------------
## For making plot on various distributions of rules at differernet confidence and support in both the clusters

subRules<-final_rules_subset_c1[quality(final_rules_subset_c1)$confidence>0.4]
plot(subRules)

subRules<-final_rules_subset_c2[quality(final_rules_subset_c2)$confidence>0.2]
plot(subRules)

-------------------------------------------------------------------------------------------  
    
-------------------------------------Hierarchichal clustering ends here---------------------------------------
--------------------------------------------------------------------------------------------------------------
  
**********************************************Rock clustering starts here*************************************

##executing rock clustering algorithm    
distm <- as.matrix(sampled_completeData_impu)
rock <- rockCluster(distm, 3, theta = .02)


-----------------------------------------------------------------------------------------------
## making data frame for rock cluster
clusterCut_imp_DF <- data.frame(rock$cl)

## merging dataset with clusters
Clustered_dataFrame <- merge(sampled_completeData_impu, clusterCut_imp_DF, by=0, all=TRUE)

dim(Clustered_dataFrame)

Clustered_dataFrame <- Clustered_dataFrame[,-c(1)]
str(Clustered_dataFrame)

#####Making 3 clusters now

cluster_1 <- Clustered_dataFrame[Clustered_dataFrame$rock.cl %in% c(1), ]
cluster_1 <- cluster_1[,-c(36,35,9)]
#cluster_1 <- data.frame(sapply(cluster_1,as.factor))
cluster_2 <- Clustered_dataFrame[Clustered_dataFrame$rock.cl %in% c(2), ]
cluster_2 <- cluster_2[,-c(36,35,9)]
#cluster_2 <- data.frame(sapply(cluster_2,as.factor))
#cluster_3 <- Clustered_dataFrame[Clustered_dataFrame$rock.cl %in% c(3), ]
#cluster_3 <- cluster_3[,-c(36,35,9)]

--------------------------------------------------------------------------------------------------
  
# conversion into treansactions  
Trans.cluster_1 <- as(as.matrix(cluster_1), "transactions") 
Trans.cluster_2 <- as(as.matrix(cluster_2), "transactions") 


## Same process as for the mining apriori rules on the hierarcchichal clustering
## Different combinations of support and confidence can be used used for different use cases mentioned in project report
apriori_rules_c1 <- apriori(Trans.cluster_1, parameter = list(supp=0.004, conf=0.3,minlen=2))##rules generaton
apriori_rules_c2 <- apriori(Trans.cluster_2, parameter = list(supp=0.002, conf=0.1,minlen=2))##rules generation
##Pruning rules
apriori_rules_c1 <- apriori_rules_c1[!is.redundant(apriori_rules_c1)]##redundant rules removal
apriori_rules_c2 <- apriori_rules_c2[!is.redundant(apriori_rules_c2)]##redudant  rules removal

subset.rules <- which(colSums(is.subset(apriori_rules_c1, apriori_rules_c1)) > 1)
apriori_rules_c1 <- apriori_rules_c1[-subset.rules] # remove subset rules.

subset.rules <- which(colSums(is.subset(apriori_rules_c2, apriori_rules_c2)) > 1)
apriori_rules_c2 <- apriori_rules_c2[-subset.rules] # remove subset rules.

## adding 1 quality indexes to the rules
quality(apriori_rules_c1) <- cbind(quality(apriori_rules_c1),
                                   yuleQ = interestMeasure(apriori_rules_c1, measure = "yuleQ",
                                                           transactions = as.matrix(cluster_1)))

## adding 2 quality indexes to the rules
quality(apriori_rules_c1) <- cbind(quality(apriori_rules_c1),
                                   oddsRatio = interestMeasure(apriori_rules_c1, measure = "oddsRatio",
                                                               transactions = as.matrix(cluster_1)))
----------------Add rules for cluster 2-------------

quality(apriori_rules_c2) <- cbind(quality(apriori_rules_c2),
                                     yuleQ = interestMeasure(apriori_rules_c2, measure = "yuleQ",
                                                             transactions = as.matrix(cluster_2)))

## adding 2 quality indexes to the rules
quality(apriori_rules_c2) <- cbind(quality(apriori_rules_c2),
                                   oddsRatio = interestMeasure(apriori_rules_c2, measure = "oddsRatio",
                                                               transactions = as.matrix(cluster_2)))




final_rules_subset_c1 <- subset(apriori_rules_c1, (rhs %in% c("intellectual_disability","autism_Spectrum","developmental_Disability",
                                                              "Mobility_Impairment_Disorder","diabetes",
                                                              "obesity","heart_Attack","stroke","other_Cardiac","pulmonary_Asthma","alzheimer","kidney_Disease","liver_Disease","endocrine_Condition",
                                                              "neurological_Condition","joint_Disease","cancer","other_Chronic_Med_Condition")))
final_rules_subset_c2 <- subset(final_rules_subset_c2, (rhs %in% c("intellectual_disability","autism_Spectrum","developmental_Disability",
                                                                   "Mobility_Impairment_Disorder","hearing_Visual_Impairment",
                                                                   "obesity","stroke","pulmonary_Asthma","alzheimer","kidney_Disease","liver_Disease","endocrine_Condition",
                                                                   "neurological_Condition","joint_Disease","cancer","other_Chronic_Med_Condition")))
m1 <- as(final_rules_subset_c1, "data.frame")
dim(m1)
m2 <- as(final_rules_subset_c2, "data.frame")
dim(m2)
m1 <- m2

m3 <- as(final_rules_subset_c2, "data.frame")
m3 <- m3[order(-m3$lift),]
m3 <- m3[1:50,]

  
************************************************************Ends here***********************************************
********************************************************************************************************************  
  
  
  
  
  
  












  





































































































