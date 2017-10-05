####Naive Bayse Implementation 
#
#Input the Data into Data Frame
#

Collection_data <- read.table("data.csv",header=TRUE,sep=",")

#
#75%
#
traindata <- as.data.frame(Collection_data[1:76325,]) 

#
#25%
#
testdata <- as.data.frame(Collection_data[76326:101766,])

#
#
#
t_readmited <- table(traindata$readmitted)
t_readmited < t_readmited/sum(t_readmited)
t_readmited
#
#Calculating of Count in 
# Confitional Probability
#
race_count <- table(traindata[,c("readmitted", "race")]) 
race_count <- race_count/rowSums(race_count) 
race_count
#
#Calculating of Count in 
# Confitional Probability
#
gender_count <- table(traindata[,c("readmitted", "gender")]) 
gender_count <- gender_count/rowSums(gender_count) 
gender_count
#
#Calculating of Count in 
# Confitional Probability
#
age_count <- table(traindata[,c("readmitted", "age")]) 
age_count <- age_count/rowSums(age_count) 
age_count
#
#Calculating of Count in 
# Confitional Probability
#
admission_type_id_count <- table(traindata[,c("readmitted", "admission_type_id")]) 
admission_type_id_count <- admission_type_id_count/rowSums(admission_type_id_count) 
admission_type_id_count
#
#Calculating of Count in 
# Confitional Probability
#
discharge_disposition_id_count <- table(traindata[,c("readmitted", "discharge_disposition_id")]) 
discharge_disposition_id_count <- discharge_disposition_id_count/rowSums(discharge_disposition_id_count) 
discharge_disposition_id_count
#
#Calculating of Count in 
# Confitional Probability
#
admission_source_id_count <- table(traindata[,c("readmitted", "admission_source_id")]) 
admission_source_id_count <- admission_source_id_count/rowSums(admission_source_id_count) 
admission_source_id_count
#
#Calculating of Count in 
# Confitional Probability
#
time_in_hospital_count <- table(traindata[,c("readmitted", "time_in_hospital")]) 
time_in_hospital_count <- time_in_hospital_count/rowSums(time_in_hospital_count) 
time_in_hospital_count
#
#Calculating of Count in 
# Confitional Probability
#
num_lab_procedures_count <- table(traindata[,c("readmitted", "num_lab_procedures")]) 
num_lab_procedures_count <- num_lab_procedures_count/rowSums(num_lab_procedures_count) 
num_lab_procedures_count
#
#Calculating of Count in 
# Confitional Probability
#
num_procedures_count <- table(traindata[,c("readmitted", "num_procedures")]) 
num_procedures_count <- num_procedures_count/rowSums(num_procedures_count) 
num_procedures_count
#
#Calculating of Count in 
# Confitional Probability
#
num_medications_count <- table(traindata[,c("readmitted", "num_medications")]) 
num_medications_count <- num_medications_count/rowSums(num_medications_count) 
num_medications_count
#
#Calculating of Count in 
# Confitional Probability
#
diag_1_count <- table(traindata[,c("readmitted", "diag_1")]) 
diag_1_count <- diag_1_count/rowSums(diag_1_count) 
diag_1_count
#
#Calculating of Count in 
# Confitional Probability
#
diag_2_count <- table(traindata[,c("readmitted", "diag_2")]) 
diag_2_count <- diag_2_count/rowSums(diag_2_count) 
diag_2_count
#
#Calculating of Count in 
# Confitional Probability
#
diag_3_count <- table(traindata[,c("readmitted", "diag_3")]) 
diag_3_count <- diag_3_count/rowSums(diag_3_count) 
diag_3_count
#
#Calculating of Count in 
# Confitional Probability
#
number_diagnoses_count <- table(traindata[,c("readmitted", "number_diagnoses")]) 
number_diagnoses_count <- number_diagnoses_count/rowSums(number_diagnoses_count) 
number_diagnoses_count
#
#Calculating of Count in 
# Confitional Probability
#
A1Cresult_count <- table(traindata[,c("readmitted", "A1Cresult")]) 
A1Cresult_count <- A1Cresult_count/rowSums(A1Cresult_count) 
A1Cresult_count
#
#Calculating of Count in 
# Confitional Probability
#
metformin_count <- table(traindata[,c("readmitted", "metformin")]) 
metformin_count <- metformin_count/rowSums(metformin_count) 
metformin_count
#
#Calculating of Count in 
# Confitional Probability
#
glipizide_count <- table(traindata[,c("readmitted", "glipizide")]) 
glipizide_count <- glipizide_count/rowSums(glipizide_count) 
glipizide_count
#
#Calculating of Count in 
# Confitional Probability
#
glyburide_count <- table(traindata[,c("readmitted", "glyburide")]) 
glyburide_count <- glyburide_count/rowSums(glyburide_count) 
glyburide_count
#
#Calculating of Count in 
# Confitional Probability
#
insulin_count <- table(traindata[,c("readmitted", "insulin")]) 
insulin_count <- insulin_count/rowSums(insulin_count) 
insulin_count
#
#Calculating of Count in 
# Confitional Probability
#
change_count <- table(traindata[,c("readmitted", "change")]) 
change_count <- change_count/rowSums(change_count) 
change_count
#
#Calculating of Count in 
# Confitional Probability
#
diabetesMed_count <- table(traindata[,c("readmitted", "diabetesMed")]) 
diabetesMed_count <- diabetesMed_count/rowSums(diabetesMed_count) 
diabetesMed_count
#
# Propality of readmitted in less than 30 days 
#
pL30 <- 
  race_count["<30","AfricanAmerican"]*
  gender_count["<30","Female"]*
  age_count["<30","[20-30)"]*
  admission_type_id_count["<30","1"]*
  discharge_disposition_id_count["<30","1"]*
  admission_source_id_count["<30","7"]*
  time_in_hospital_count["<30","2"]*
  num_lab_procedures_count["<30","11"]*
  num_procedures_count["<30","5"]*
  num_medications_count["<30","13"]*
  diag_1_count["<30","648"]*
  diag_2_count["<30","250"]*
  diag_3_count["<30","V27"]*
  number_diagnoses_count["<30","6"]*
  A1Cresult_count["<30","None"]*
  metformin_count["<30","No"]*
  glipizide_count["<30","Steady"]*
  glyburide_count["<30","No"]*
  insulin_count["<30","No"]*
  change_count["<30","No"]*
  diabetesMed_count["<30","Yes"]
#
# Propality of readmitted in greater than 30 days 
#
pG30 <-
  race_count[">30","AfricanAmerican"]*
  gender_count[">30","Female"]*
  age_count[">30","[20-30)"]*
  admission_type_id_count[">30","1"]*
  discharge_disposition_id_count[">30","1"]*
  admission_source_id_count[">30","7"]*
  time_in_hospital_count[">30","2"]*
  num_lab_procedures_count[">30","11"]*
  num_procedures_count[">30","5"]*
  num_medications_count[">30","13"]*
  diag_1_count[">30","648"]*
  diag_2_count[">30","250"]*
  diag_3_count[">30","V27"]*
  number_diagnoses_count[">30","6"]*
  A1Cresult_count[">30","None"]*
  metformin_count[">30","No"]*
  glipizide_count[">30","Steady"]*
  glyburide_count[">30","No"]*
  insulin_count[">30","No"]*
  change_count[">30","No"]*
  diabetesMed_count[">30","Yes"]
#
# Propality of NO readmitted  
#
pNO <- 
  race_count["NO","AfricanAmerican"]*
  gender_count["NO","Female"]*
  age_count["NO","[20-30)"]*
  admission_type_id_count["NO","1"]*
  discharge_disposition_id_count["NO","1"]*
  admission_source_id_count["NO","7"]*
  time_in_hospital_count["NO","2"]*
  num_lab_procedures_count["NO","11"]*
  num_procedures_count["NO","5"]*
  num_medications_count["NO","13"]*
  diag_1_count["NO","648"]*
  diag_2_count["NO","250"]*
  diag_3_count["NO","V27"]*
  number_diagnoses_count["NO","6"]*
  A1Cresult_count["NO","None"]*
  metformin_count["NO","No"]*
  glipizide_count["NO","Steady"]*
  glyburide_count["NO","No"]*
  insulin_count["NO","No"]*
  change_count["NO","No"]*
  diabetesMed_count["NO","Yes"]
#
# Model Building with the Naive Bayes Technique with out Laplace Smoothing
#
Model_Naive_noLaplace <- naiveBayes(readmitted ~.,traindata)
Model_Naive_noLaplace
#
# Model Building with the Naive Bayes Technique with  Laplace Smoothing
# If the probility goes zero then product goes zero, to avoid that we use 
# Laplace Smoothing which add's a small amount of the value numerator and
# denominator which dosen't make lot of difference in main probability 
Model_Naive_Laplace <- naiveBayes(readmitted ~.,traindata, laplace=.01)
Model_Naive_Laplace
##
## Prediction Model Development 
##
Prediction_Result <- predict(Model_Naive_noLaplace,testdata)
Prediction_Redult <- predict(Model_Naive_Laplace,testdata)

