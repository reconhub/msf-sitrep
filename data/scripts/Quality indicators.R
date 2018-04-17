#1 Like with the MMR, save the patient and visit tabs as tab delimited (patient.txt and visit.txt) files
#apply 3 conversion steps for date columns in the patient file (Admission date & Date of exit),the visit file (Date of visit).
#2 Like in step 1, save the lab investigation files: as lab.txt
#3 Change the column of Exam date into the right format with the 3 conversion steps. First:  =TEXT(A1,"dd/mm/yyyy")   Then: =RIGHT(C1,4) &"/"&MID(C1,4,2)&"/"&LEFT(C1,2)
#subsequently paste with 123 and set the column on general
#4 Make sure for date of exit, all exited patients have an exit date. And all not exited patients have a date far in the future like: 2020/01/00
dir <- "D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017"
setwd(dir)
#5 loads files
patient <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017/patient.txt",stringsAsFactors=FALSE)
visit <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017/visit.txt",stringsAsFactors=FALSE)
lab <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017/lab.txt",stringsAsFactors=FALSE)
#6 installs packages
install.packages("lubridate") 
library(lubridate)
install.packages("data.table") 
library(data.table)
#7 converts dates to right format
patient$Admission.date <- as.Date(patient$Admission.date)
patient$Date.of.exit <- as.Date(patient$Date.of.exit)
visit$Date.of.visit <- as.Date(visit$Date.of.visit)
lab$Exam.date <- as.Date(lab$Exam.date)

#8 first encodes NCD's in the visit file
visit$Diabetes.I[visit$Diabetes.I =="New"] <- 1; visit$Diabetes.I[visit$Diabetes.I =="Old (verified)"] <- 1;visit$Diabetes.I[visit$Diabetes.I =="Old (unverified)"] <- 1;visit$Diabetes.I[visit$Diabetes.I =="No"] <- 0 
visit$Diabetes.II[visit$Diabetes.II =="New"] <- 1; visit$Diabetes.II[visit$Diabetes.II =="Old (verified)"] <- 1;visit$Diabetes.II[visit$Diabetes.II =="Old (unverified)"] <- 1;visit$Diabetes.II[visit$Diabetes.II =="No"] <- 0 
visit$Hypertension[visit$Hypertension =="New"] <- 1; visit$Hypertension[visit$Hypertension =="Old (verified)"] <- 1;visit$Hypertension[visit$Hypertension =="Old (unverified)"] <- 1;visit$Hypertension[visit$Hypertension =="No"] <- 0 
visit$COPD[visit$COPD =="New"] <- 1; visit$COPD[visit$COPD =="Old (verified)"] <- 1;visit$COPD[visit$COPD =="Old (unverified)"] <- 1;visit$COPD[visit$COPD =="No"] <- 0 
visit$Asthma[visit$Asthma =="New"] <- 1; visit$Asthma[visit$Asthma =="Old (verified)"] <- 1;visit$Asthma[visit$Asthma =="Old (unverified)"] <- 1;visit$Asthma[visit$Asthma =="No"] <- 0 
visit$Hypothyroidism[visit$Hypothyroidism =="New"] <- 1; visit$Hypothyroidism[visit$Hypothyroidism =="Old (verified)"] <- 1;visit$Hypothyroidism[visit$Hypothyroidism =="Old (unverified)"] <- 1;visit$Hypothyroidism[visit$Hypothyroidism =="No"] <- 0 
visit$Stable.angina[visit$Stable.angina =="New"] <- 1; visit$Stable.angina[visit$Stable.angina =="Old (verified)"] <- 1;visit$Stable.angina[visit$Stable.angina =="Old (unverified)"] <- 1;visit$Stable.angina[visit$Stable.angina =="No"] <- 0 
visit$Unstable.angina[visit$Unstable.angina =="New"] <- 1; visit$Unstable.angina[visit$Unstable.angina =="Old (verified)"] <- 1;visit$Unstable.angina[visit$Unstable.angina =="Old (unverified)"] <- 1;visit$Unstable.angina[visit$Unstable.angina =="No"] <- 0 
visit$Angioplasty.CABG[visit$Angioplasty.CABG =="New"] <- 1; visit$Angioplasty.CABG[visit$Angioplasty.CABG =="Old (verified)"] <- 1;visit$Angioplasty.CABG[visit$Angioplasty.CABG =="Old (unverified)"] <- 1;visit$Angioplasty.CABG[visit$Angioplasty.CABG =="No"] <- 0 
visit$Myocardial.Infarction[visit$Myocardial.Infarction =="New"] <- 1; visit$Myocardial.Infarction[visit$Myocardial.Infarction =="Old (verified)"] <- 1;visit$Myocardial.Infarction[visit$Myocardial.Infarction =="Old (unverified)"] <- 1;visit$Myocardial.Infarction[visit$Myocardial.Infarction =="No"] <- 0 
visit$Congestive.Heart.Failure[visit$Congestive.Heart.Failure =="New"] <- 1; visit$Congestive.Heart.Failure[visit$Congestive.Heart.Failure =="Old (verified)"] <- 1;visit$Congestive.Heart.Failure[visit$Congestive.Heart.Failure =="Old (unverified)"] <- 1;visit$Congestive.Heart.Failure[visit$Congestive.Heart.Failure =="No"] <- 0 
visit$Peripheral.Vascular.Disease[visit$Peripheral.Vascular.Disease =="Old (verified)"] <- 1;visit$Peripheral.Vascular.Disease[visit$Peripheral.Vascular.Disease =="No"] <- 0 
visit$Stroke[visit$Stroke =="New"] <- 1; visit$Stroke[visit$Stroke =="Old (verified)"] <- 1;visit$Stroke[visit$Stroke =="Old (unverified)"] <- 1;visit$Stroke[visit$Stroke =="No"] <- 0 
visit$Musculoskeletal.disorder[visit$Musculoskeletal.disorder =="New"] <- 1; visit$Musculoskeletal.disorder[visit$Musculoskeletal.disorder =="Old (verified)"] <- 1;visit$Musculoskeletal.disorder[visit$Musculoskeletal.disorder =="Old (unverified)"] <- 1;visit$Musculoskeletal.disorder[visit$Musculoskeletal.disorder =="No"] <- 0 
visit$Neurological.disorder[visit$Neurological.disorder =="New"] <- 1; visit$Neurological.disorder[visit$Neurological.disorder =="Old (verified)"] <- 1;visit$Neurological.disorder[visit$Neurological.disorder =="Old (unverified)"] <- 1;visit$Neurological.disorder[visit$Neurological.disorder =="No"] <- 0 
visit$Diabetes.I <- as.numeric(as.character(visit$Diabetes.I))
visit$Diabetes.II <- as.numeric(as.character(visit$Diabetes.II))
visit$COPD <- as.numeric(as.character(visit$COPD))
visit$Hypertension <- as.numeric(as.character(visit$Hypertension))
visit$Asthma <- as.numeric(as.character(visit$Asthma))
visit$Hypothyroidism <- as.numeric(as.character(visit$Hypothyroidism))
visit$Stable.angina <- as.numeric(as.character(visit$Stable.angina))
visit$Unstable.angina <- as.numeric(as.character(visit$Unstable.angina))
visit$Angioplasty.CABG <- as.numeric(as.character(visit$Angioplasty.CABG))
visit$Myocardial.Infarction <- as.numeric(as.character(visit$Myocardial.Infarction))
visit$Congestive.Heart.Failure <- as.numeric(as.character(visit$Congestive.Heart.Failure))
visit$Peripheral.Vascular.Disease <- as.numeric(as.character(visit$Peripheral.Vascular.Disease))
visit$Stroke <- as.numeric(as.character(visit$Stroke))
visit$Musculoskeletal.disorder <- as.numeric(as.character(visit$Musculoskeletal.disorder))
visit$Neurological.disorder <- as.numeric(as.character(visit$Neurological.disorder))

colnames(patient)[colnames(patient)=="Internal.ID"] <- "Patient.ID"
+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#_____________Indicators to be run 1 by 1_______________#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#
  
  #Number / % of hypertensive patients with a BP recorded at their last visit /BP recorded at last visit <140-<90
  
Firstday <- "2018-01-01" # Note that this is the upper limit and Exitday is the lower limit
Firstday <- as.Date(Firstday)
Exitday <- "2014-09-30"
Exitday <- as.Date(Exitday)                                      
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
# next part selects all hypertensives with admission date in a certain period (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_Hyp <- visitP[ visitP$Hypertension.x == 1 & visitP$Admission.date <Firstday & visitP$Date.of.visit <Firstday &visitP$Sys.BP!="" &visitP$Dia.BP!="",]
Total_Hyp_min <- setkey(setDT(Total_Hyp), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Total_Hyp_denom_N <-NROW(Total_Hyp_min) 
#from this selection it selects the hypertensives that had BP recorded
#Total_HypBP_min_denom <- Total_Hyp_min[Total_Hyp_min$Sys.BP != "" & Total_Hyp_min$Dia.BP != "",]
# OR from this selection the hypertensives that had a BP 140-90 on last visit
Total_HypBP_min_denom <- Total_Hyp_min[Total_Hyp_min$Sys.BP <140 & Total_Hyp_min$Dia.BP <90,]

Total_HypBP_min_denom_N <-NROW(Total_HypBP_min_denom)
(perc_Hyp_BP<-(Total_HypBP_min_denom_N/Total_Hyp_denom_N))


#---------------------------------------------------------------------------------------------------------------------#
#Number / % of diabetes 2 patients with a BP recorded at their last visit 

Firstday <- "2017-10-01" # Note that this is the upper limit and Exitday is the lower limit
Firstday <- as.Date(Firstday)
Exitday <- "2016-08-31"
Exitday <- as.Date(Exitday)                                         
visitP <- merge(visit, all.x = TRUE, patient, c("MSF.ID"))
# next part selects all diabetics with admission date in a certain period (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM2 <- visitP[visitP$Clinic== "Ibn Sina" & visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Date.of.exit >Exitday & visitP$Date.of.visit <Firstday,]
Total_DM2_max <- setkey(setDT(Total_DM2), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Total_DM2_denom_N <-NROW(Total_DM2_max) 
#from this selection it selects the hypertensives that had BP recorded
Total_DM2BP_min_denom <- Total_DM2_max[Total_DM2_max$Sys.BP != "" & Total_DM2_max$Dia.BP != "",]
Total_DM2BP_min_denom_N <-NROW(Total_DM2BP_min_denom)
(perc_DM2_BP<-(Total_DM2BP_min_denom_N/Total_DM2_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number/ % of new diagnoses (DM Type I, DM Type II, hypertension, other cardiovascular disease, asthma, COPD, hypothyroidism, other) as a proportion of active cohort 

Firstday <- "2017-10-01"
Firstday <- as.Date(Firstday)
Exitday <- "2017-08-31"
Exitday <- as.Date(Exitday)
#select data !!use the right file name and directory!!
patient <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017/patient.txt",stringsAsFactors=FALSE)
visit <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Dec2017/visit.txt",stringsAsFactors=FALSE)
patient$Admission.date <- as.Date(patient$Admission.date)
patient$Date.of.exit <- as.Date(patient$Date.of.exit)
visit$Date.of.visit <- as.Date(visit$Date.of.visit)
#select all active patients (not exited; maybe >90days not seen) within the reporting period 
visitP <- merge(visit, all.x = TRUE, patient, c("MSF.ID"))
Total_visitP_Denom <- visitP[visitP$Date.of.exit > Exitday & visitP$Admission.date < Firstday & visitP$Date.of.visit <Firstday, ]
Total_active_visitP_Denom<-setkey(setDT(Total_visitP_Denom), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Total_active_visitP_N<-NROW(Total_active_visitP_Denom)
#select all visits of active patients (also multiple visits in the period) in the reporting period  
Total_active_visitP <- visitP[visitP$Date.of.exit > Firstday & visitP$Admission.date < Firstday & visitP$Date.of.visit <Firstday & visitP$Date.of.visit > Exitday, ]

Total_Diabetes1 <- Total_active_visitP[Total_active_visitP$Diabetes.I == "New" ,]; Total_Diabetes1_N<-NROW(Total_Diabetes1);perc_Total_Diabetes1 <- Total_Diabetes1_N/Total_active_visitP_N
Total_Diabetes2 <- Total_active_visitP[Total_active_visitP$Diabetes.II == "New" ,]; Total_Diabetes2_N<-NROW(Total_Diabetes2);perc_Total_Diabetes2 <- Total_Diabetes2_N/Total_active_visitP_N
Total_Hypertension <- Total_active_visitP[Total_active_visitP$Hypertension.x == "New" ,]; Total_Hypertension_N<-NROW(Total_Hypertension);perc_Total_Hypertension <- Total_Hypertension_N/Total_active_visitP_N
Total_COPD <- Total_active_visitP[Total_active_visitP$COPD.x == "New" ,]; Total_COPD_N<-NROW(Total_COPD);perc_Total_COPD <- Total_COPD_N/Total_active_visitP_N
Total_Asthma <- Total_active_visitP[Total_active_visitP$Asthma.x == "New" ,]; Total_Asthma_N<-NROW(Total_Asthma);perc_Total_Asthma <- Total_Asthma_N/Total_active_visitP_N
Total_Hypothyroidism <- Total_active_visitP[Total_active_visitP$Hypothyroidism == "New" ,]; Total_Hypothyroidism_N<-NROW(Total_Hypothyroidism);perc_Total_Hypothyroidism <- Total_Hypothyroidism_N/Total_active_visitP_N
Total_Musculoskeletal.disorder <- Total_active_visitP[Total_active_visitP$Musculoskeletal.disorder == "New" ,]; (Total_Musculoskeletal.disorder_N<-NROW(Total_Musculoskeletal.disorder));(perc_Total_Musculoskeletal.disorder <- Total_Musculoskeletal.disorder_N/Total_active_visitP_N)
Total_Neurological.disorder <- Total_active_visitP[Total_active_visitP$Neurological.disorder == "New" ,]; (Total_Neurological.disorder_N<-NROW(Total_Neurological.disorder));(perc_Total_Neurological.disorder <- Total_Neurological.disorder_N/Total_active_visitP_N)

Total_active_visitP$Stable.angina[Total_active_visitP$Stable.angina =="New"] <- 1; Total_active_visitP$Stable.angina[Total_active_visitP$Stable.angina =="Old (verified)"] <- 0;Total_active_visitP$Stable.angina[Total_active_visitP$Stable.angina =="Old (unverified)"] <- 0;Total_active_visitP$Stable.angina[Total_active_visitP$Stable.angina =="No"] <- 0 
Total_active_visitP$Unstable.angina[Total_active_visitP$Unstable.angina =="New"] <- 1; Total_active_visitP$Unstable.angina[Total_active_visitP$Unstable.angina =="Old (verified)"] <- 0;Total_active_visitP$Unstable.angina[Total_active_visitP$Unstable.angina =="Old (unverified)"] <- 0;Total_active_visitP$Unstable.angina[Total_active_visitP$Unstable.angina =="No"] <- 0 
Total_active_visitP$Angioplasty.CABG[Total_active_visitP$Angioplasty.CABG =="New"] <- 1; Total_active_visitP$Angioplasty.CABG[Total_active_visitP$Angioplasty.CABG =="Old (verified)"] <- 0;Total_active_visitP$Angioplasty.CABG[Total_active_visitP$Angioplasty.CABG =="Old (unverified)"] <- 0;Total_active_visitP$Angioplasty.CABG[Total_active_visitP$Angioplasty.CABG =="No"] <- 0 
Total_active_visitP$Myocardial.Infarction[Total_active_visitP$Myocardial.Infarction =="New"] <- 1; Total_active_visitP$Myocardial.Infarction[Total_active_visitP$Myocardial.Infarction =="Old (verified)"] <- 0;Total_active_visitP$Myocardial.Infarction[Total_active_visitP$Myocardial.Infarction =="Old (unverified)"] <- 0;Total_active_visitP$Myocardial.Infarction[Total_active_visitP$Myocardial.Infarction =="No"] <- 0 
Total_active_visitP$Congestive.Heart.Failure[Total_active_visitP$Congestive.Heart.Failure =="New"] <- 1; Total_active_visitP$Congestive.Heart.Failure[Total_active_visitP$Congestive.Heart.Failure =="Old (verified)"] <- 0;Total_active_visitP$Congestive.Heart.Failure[Total_active_visitP$Congestive.Heart.Failure =="Old (unverified)"] <- 1;Total_active_visitP$Congestive.Heart.Failure[Total_active_visitP$Congestive.Heart.Failure =="No"] <- 0 
Total_active_visitP$Peripheral.Vascular.Disease[Total_active_visitP$Peripheral.Vascular.Disease =="New"] <- 1; Total_active_visitP$Peripheral.Vascular.Disease[Total_active_visitP$Peripheral.Vascular.Disease =="Old (verified)"] <- 0;Total_active_visitP$Peripheral.Vascular.Disease[Total_active_visitP$Peripheral.Vascular.Disease =="Old (unverified)"] <- 1;Total_active_visitP$Peripheral.Vascular.Disease[Total_active_visitP$Peripheral.Vascular.Disease =="No"] <- 0 
Total_active_visitP$Stroke[Total_active_visitP$Stroke =="New"] <- 1; Total_active_visitP$Stroke[Total_active_visitP$Stroke =="Old (verified)"] <- 0;Total_active_visitP$Stroke[Total_active_visitP$Stroke =="Old (unverified)"] <- 0;Total_active_visitP$Stroke[Total_active_visitP$Stroke =="No"] <- 0 
Total_active_visitP$Cardiovascular.other[Total_active_visitP$Cardiovascular.other =="New"] <- 1; Total_active_visitP$Cardiovascular.other[Total_active_visitP$Cardiovascular.other =="No"] <- 0 ; Total_active_visitP$Cardiovascular.other[Total_active_visitP$Cardiovascular.other =="Old (verified)"]<-0; Total_active_visitP$Cardiovascular.other[Total_active_visitP$Cardiovascular.other =="Old (unverified)"] <-0
Total_active_visitP$Stable.angina <- as.numeric(as.character(Total_active_visitP$Stable.angina))
Total_active_visitP$Unstable.angina <- as.numeric(as.character(Total_active_visitP$Unstable.angina))
Total_active_visitP$Angioplasty.CABG <- as.numeric(as.character(Total_active_visitP$Angioplasty.CABG))
Total_active_visitP$Myocardial.Infarction <- as.numeric(as.character(Total_active_visitP$Myocardial.Infarction))
Total_active_visitP$Congestive.Heart.Failure <- as.numeric(as.character(Total_active_visitP$Congestive.Heart.Failure))
Total_active_visitP$Peripheral.Vascular.Disease <- as.numeric(as.character(Total_active_visitP$Peripheral.Vascular.Disease))
Total_active_visitP$Stroke <- as.numeric(as.character(Total_active_visitP$Stroke))
Total_active_visitP$Cardiovascular.other <- as.numeric(as.character(Total_active_visitP$Cardiovascular.other))
Total_active_visitP$CVD <-rowSums(Total_active_visitP[,c("Stable.angina", "Unstable.angina","Angioplasty.CABG","Myocardial.Infarction","Congestive.Heart.Failure","Peripheral.Vascular.Disease","Stroke", "Cardiovascular.other" )])
CVD <- Total_active_visitP[Total_active_visitP$CVD !=0,]
CVD_N<-NROW(CVD) ; perc_CVD_N<- CVD_N/Total_active_visitP_N

Total_Morbidities<- data.frame()
DM1 = c(Total_Diabetes1_N,perc_Total_Diabetes1); Total_Morbidities = rbind(Total_Morbidities,DM1)
Dm2 = c(Total_Diabetes2_N,perc_Total_Diabetes2);Total_Morbidities = rbind(Total_Morbidities,Dm2)
Hypert = c(Total_Hypertension_N,perc_Total_Hypertension);Total_Morbidities = rbind(Total_Morbidities,Hypert)
COPD = c(Total_COPD_N,perc_Total_COPD);Total_Morbidities = rbind(Total_Morbidities,COPD)
Asthma = c(Total_Asthma_N,perc_Total_Asthma);Total_Morbidities = rbind(Total_Morbidities,Asthma)
Hypother = c(Total_Hypothyroidism_N,perc_Total_Hypothyroidism);Total_Morbidities = rbind(Total_Morbidities,Hypother)
CVD = c(CVD_N,perc_CVD_N);Total_Morbidities = rbind(Total_Morbidities,CVD)
Musco = c(Total_Musculoskeletal.disorder_N,perc_Total_Musculoskeletal.disorder);Total_Morbidities = rbind(Total_Morbidities,Musco)
Neuro = c(Total_Neurological.disorder_N,perc_Total_Neurological.disorder);Total_Morbidities = rbind(Total_Morbidities,Neuro)
# table of all morbidities
colnames(Total_Morbidities) <- c("Freq.x","Freq.y")
Total_Morbidities$Var1<-c("DM1","Dm2", "Hypert", "COPD", "Asthma", "Hypother", "CVD", "Musco", "Neuro")
Total_Morbidities<-Total_Morbidities[,c("Var1","Freq.x","Freq.y")]

#Make internal ID into patient.ID
colnames(patient)[colnames(patient)=="Internal.ID"] <- "Patient.ID"
#---------------------------------------------------------------------------------------------------------------------#
# Number / % of DM patients that have  MALB testing in the last period (3 months/year)
# !! Run step 8(line 25-54) first again !!!!!!!!!!

#As denominator here we take all the diabetic actives between: 3 months before the end of reporting period and 3 month before the beginning of the reporting period.
#As nominator we take all MALB tests in 2017, then take every last MALB test in this period if a patient did multiple. Finally non diabetic patients with a MALB are filtered out.
Firstday <- "2017-10-01"
Firstday <- as.Date(Firstday)
Firstday2 <- "2018-01-01"
Firstday2 <- as.Date(Firstday2)
Exitday <- "2016-10-01"
Exitday <- as.Date(Exitday)
Exitday2 <- "2016-09-30"
Exitday2 <- as.Date(Exitday2)
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
# next part selects all diabetics with admission date in a certain period (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
#Total_DM2 <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Date.of.exit >Exitday & visitP$Date.of.visit <Firstday,]
Total_DM2 <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Date.of.exit >Firstday & visitP$Date.of.visit <Firstday & visitP$Date.of.visit >Exitday,]
Total_DM2_max <- setkey(setDT(Total_DM2), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_DM2_denom_N <-NROW(Total_DM2_max) 
#from this selection it selects the diabetics that had microalbumin recorded
Total_Microalbumin <- lab[lab$Microalbumin != "" & lab$Exam.date < Firstday2,] 
Total_Microalbumin_max<-setkey(setDT(Total_Microalbumin), Patient.ID)[, .SD[which.max(Exam.date)], Patient.ID] # take last dipstick done
Total_Microalbumin_maxP <-Total_Microalbumin_max[Total_Microalbumin_max$Exam.date > Exitday2 &Total_Microalbumin_max$Exam.date < Firstday2,]
Total_Microalbumin_denom <- merge(Total_Microalbumin_maxP, all = FALSE, Total_DM2_max, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_Microalbumin_denom_N <-NROW(Total_Microalbumin_denom) 
(perc_DM2_Microalbumin<-(Total_Microalbumin_denom_N/Total_DM2_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of DM patients that have HbA1c testing in the last period (3 months/year)
Firstday <- "2017-10-01"
Firstday <- as.Date(Firstday)
Exitday <- "2017-08-31"
Exitday <- as.Date(Exitday)
colnames(patient)[colnames(patient)=="Internal.ID"] <- "Patient.ID"
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
# next part selects all diabetics with admission date in a certain period (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM2 <- visitP[visitP$Clinic== "Ibn Sina" & visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Date.of.exit >Exitday & visitP$Date.of.visit <Firstday,]
Total_DM2_max <- setkey(setDT(Total_DM2), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_DM2_denom_N <-NROW(Total_DM2_max) 
#from this selection it selects the diabetics that had urine dipstick recorded recorded
Total_HbA1C <- lab[lab$HbA1C != "" & lab$Exam.date < Firstday,] 
Total_HbA1C_max<-setkey(setDT(Total_HbA1C), Patient.ID)[, .SD[which.max(Exam.date)], Patient.ID]
Total_HbA1C_maxP <-Total_HbA1C_max[Total_HbA1C_max$Exam.date > Exitday &Total_HbA1C_max$Exam.date < Firstday,]
Total_HbA1C_denom <- merge(Total_HbA1C_maxP, all = FALSE, Total_DM2_max, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_HbA1C_denom_N <-NROW(Total_HbA1C_denom) 
(perc_DM2_HbA1C<-(Total_HbA1C_denom_N/Total_DM2_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of patients with DM or on ACE inhibitor (ACEi) with Creatinine testing in last year 

Firstday <- "2017-10-01" # Note that this is the upper limit and Exitday is the lower limit
Firstday <- as.Date(Firstday)
Exitday <- "2017-08-31"
Exitday <- as.Date(Exitday) 
# Make a variable that says if the visit has a ACEi prescribed
visit$ACEi<-apply(visit, 1, function(r) any(r %in% c("ENALAPRIL maleate, 5 mg, tab.")))
# Select the visits that have DM2 or ACEi 
colnames(patient)[colnames(patient)=="Internal.ID"] <- "Patient.ID"
# if you want to do the analysis on one clinic, add in the line below for instance: visit$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DiabetesII_ACEi<-visit[visit$Diabetes.II == 1 & visit$Date.of.visit<Firstday,]
Total_DiabetesII_ACEi_min <- setkey(setDT(Total_DiabetesII_ACEi), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_DiabetesII_ACEi_minP <- merge(patient, all.x = TRUE, Total_DiabetesII_ACEi_min, c("Patient.ID"))
Total_DiabetesII_ACEi_denom <- Total_DiabetesII_ACEi_minP[Total_DiabetesII_ACEi_minP$Admission.date < Firstday & Total_DiabetesII_ACEi_minP$Date.of.exit > Exitday & Total_DiabetesII_ACEi_minP$Date.of.visit <Firstday & !is.na(Total_DiabetesII_ACEi_minP$Date.of.visit),]
Total_DiabetesII_ACEi_denom_N <-NROW(Total_DiabetesII_ACEi_denom) 
# if you want to do the analysis on one clinic, add in the line below for instance: visit$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DiabetesII_ACEi2<-visit[visit$ACEi =="TRUE" & visit$Date.of.visit<Firstday & visit$Date.of.visit>Exitday,]
Total_DiabetesII_ACEi_min2 <- setkey(setDT(Total_DiabetesII_ACEi2), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_DiabetesII_ACEi_minP2 <- merge(patient, all.x = TRUE, Total_DiabetesII_ACEi_min2, c("Patient.ID"))
Total_DiabetesII_ACEi_denom2 <- Total_DiabetesII_ACEi_minP2[Total_DiabetesII_ACEi_minP2$Admission.date < Firstday & Total_DiabetesII_ACEi_minP2$Date.of.exit > Exitday & Total_DiabetesII_ACEi_minP2$Date.of.visit <Firstday & !is.na(Total_DiabetesII_ACEi_minP2$Date.of.visit),]
Total_DiabetesII_ACEi_denom_N2 <-NROW(Total_DiabetesII_ACEi_denom2)
Total_DiabetesII_ACEi_denom_tot <- merge(Total_DiabetesII_ACEi_denom, all = TRUE, Total_DiabetesII_ACEi_denom2, c("Patient.ID"))
Total_DiabetesII_ACEi_denom_tot_N<-NROW(Total_DiabetesII_ACEi_denom_tot)

Total_Creatinine <- lab[lab$Creatinine.serum != "" & !is.na(lab$Creatinine.serum) & lab$Exam.date < Firstday,]
Total_Creatinine_max<-setkey(setDT(Total_Creatinine), Patient.ID)[, .SD[which.max(Exam.date)], Patient.ID]
Total_Creatinine_maxP <-Total_Creatinine_max[Total_Creatinine_max$Exam.date > Exitday &Total_Creatinine_max$Exam.date < Firstday,]
Total_Creatinine_denom <- merge(Total_Creatinine_maxP, all = FALSE, Total_DiabetesII_ACEi_denom_tot, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_Creatinine_denom_N <-NROW(Total_Creatinine_denom)
(perc_DM2_Creatinintest<-(Total_Creatinine_denom_N/Total_DiabetesII_ACEi_denom_tot_N)) 

#---------------------------------------------------------------------------------------------------------------------#
# Number/ % asthma patients who had acute exacerbations
Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)
Total_Asthma <- visit[visit$Asthma == 1 & visit$Date.of.visit<Firstday ,]
Total_Asthma_min <- setkey(setDT(Total_Asthma), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_Asthma_minP <- merge(patient, all.x = TRUE, Total_Asthma_min, c("Patient.ID")) 
# if you want to do the analysis on one clinic, add in the line below for instance: Total_Asthma_minP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_Asthma_denom <- Total_Asthma_minP[ Total_Asthma_minP$Admission.date < Firstday & Total_Asthma_minP$Date.of.exit > Exitday & Total_Asthma_minP$Date.of.visit <Firstday & !is.na(Total_Asthma_minP$Date.of.visit),]
Total_Asthma_denom_N <-NROW(Total_Asthma_denom) 

Total_AEAsthma <- visit[visit$AE..Asthma== "Yes" & !is.na(visit$AE..Asthma) & visit$Date.of.visit < Firstday,]
Total_AEAsthma_max<-setkey(setDT(Total_AEAsthma), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_AEAsthma_maxP <-Total_AEAsthma_max[Total_AEAsthma_max$Date.of.visit > Exitday &Total_AEAsthma_max$Date.of.visit < Firstday,]
Total_AEAsthma_denom <- merge(Total_AEAsthma_maxP, all = FALSE, Total_Asthma_denom, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_AEAsthma_denom_N <-NROW(Total_AEAsthma_denom)
(perc_Asthma_AE<-(Total_AEAsthma_denom_N/Total_Asthma_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of DM patients with mean HbA1C < 8.0 % (or average FPG<150) in last 12 months (Target = 80%) 
Firstday <- "2017-10-01"
Firstday <- as.Date(Firstday)
Exitday <- "2017-07-31"
Exitday <- as.Date(Exitday)
Total_DiabetesII <- visit[visit$Diabetes.II == 1 &visit$Date.of.visit<Firstday ,]
Total_DiabetesII_min <- setkey(setDT(Total_DiabetesII), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_DiabetesII_minP <- merge(patient, all.x = TRUE, Total_DiabetesII_min, c("Patient.ID")) 
# if you want to do the analysis on one clinic, add in the line below for instance: Total_DiabetesII_minP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DiabetesII_denom <- Total_DiabetesII_minP[ Total_DiabetesII_minP$Admission.date < Firstday & Total_DiabetesII_minP$Date.of.exit > Exitday & Total_DiabetesII_minP$Date.of.visit <Firstday & !is.na(Total_DiabetesII_minP$Date.of.visit),]
Total_DiabetesII_denom_N <-NROW(Total_DiabetesII_denom) 

labhba1cfgpmean <- lab[lab$Exam.date <Firstday &lab$Exam.date>Exitday,] 
meanhba1c<-aggregate(HbA1C ~ Patient.ID, labhba1cfgpmean, mean) 
meanfgp<-aggregate(Fasting.Plasma.Glucose ~ Patient.ID, labhba1cfgpmean, mean)
patientmeanhba1c <- merge(patient, all.x = TRUE, meanhba1c, c("Patient.ID")) 
patientmeanhba1cfgp <- merge(patientmeanhba1c, all.x = TRUE, meanfgp, c("Patient.ID")) 
# if you want to do the analysis on one clinic, add in the line below for instance: visit$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_hba1cfgpmean <- patientmeanhba1cfgp[patientmeanhba1cfgp$HbA1C < 8 | patientmeanhba1cfgp$Fasting.Plasma.Glucose <8 ,]
Total_hf <- lab[lab$HbA1C <8 & lab$Exam.date <Firstday & lab$Exam.date>Exitday,]
Total_hf_min<-setkey(setDT(Total_hf), Patient.ID)[, .SD[which.min(Exam.date)], Patient.ID]
Total_hf_min2 <- merge(Total_hf_min, all = FALSE, Total_hba1cfgpmean, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_hf_minP <-Total_hf_min2[Total_hf_min2$Exam.date > Exitday &Total_hf_min2$Exam.date < Firstday,]
Total_hf_denom <- merge(Total_hf_minP, all = FALSE, Total_DiabetesII_denom, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_hf_denom_N <-NROW(Total_hf_denom)
(perc_DM2_hf<-(Total_hf_denom_N/Total_DiabetesII_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with hypertension that have a most recent BP <= 140/90 at baseline

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)                                          
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#selects all hypertensives with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_Hyp <- visitP[visitP$Hypertension.x == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_Hyp_min <- setkey(setDT(Total_Hyp), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_Hyp_denom_N <-NROW(Total_Hyp_min) 
#from this selection selects the hypertensives that had BP <= 140/90 in the first visit
Total_HypBP_min_denom <- Total_Hyp_min[Total_Hyp_min$Sys.BP < 140 & Total_Hyp_min$Dia.BP < 90,]
Total_HypBP_min_denom_N <-NROW(Total_HypBP_min_denom)
(perc_Hyp_BP<-(Total_HypBP_min_denom_N/Total_Hyp_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with hypertension that have a most recent BP <= 140/90 at 4-8 months after admission

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)  
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Date.of.visit , units = c("days"))
#selects all hypertensives with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_Hyp <- visitP[visitP$Hypertension.x == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_Hyp_sec <- Total_Hyp[Total_Hyp$diff_adm_vis < -130 & Total_Hyp$diff_adm_vis > -250,]
Total_Hyp_sec_min <- setkey(setDT(Total_Hyp_sec), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_Hyp_sec_min_N <-NROW(Total_Hyp_sec_min) 

#from this selection selects the hypertensives that had BP <= 140/90 in the first visit
Total_HypBP_min_denom <- Total_Hyp_sec_min[Total_Hyp_sec_min$Sys.BP < 140 & Total_Hyp_sec_min$Dia.BP < 90,]
Total_HypBP_min_denom_N <-NROW(Total_HypBP_min_denom)
(perc_Hyp_BP<-(Total_HypBP_min_denom_N/Total_Hyp_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with hypertension that have a most recent BP <= 140/90 at 9-12 months after admission

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Date.of.visit , units = c("days"))
#select all hypertensives with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_Hyp <- visitP[visitP$Hypertension.x == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_Hyp_sec <- Total_Hyp[Total_Hyp$diff_adm_vis < -250 & Total_Hyp$diff_adm_vis > -370,]
Total_Hyp_sec_min <- setkey(setDT(Total_Hyp_sec), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_Hyp_sec_min_N <-NROW(Total_Hyp_sec_min) 
#from this selection selects the hypertensives that had BP <= 140/90 in the first visit
Total_HypBP_min_denom <- Total_Hyp_sec_min[Total_Hyp_sec_min$Sys.BP < 140 & Total_Hyp_sec_min$Dia.BP < 90,]
Total_HypBP_min_denom_N <-NROW(Total_HypBP_min_denom)
(perc_Hyp_BP<-(Total_HypBP_min_denom_N/Total_Hyp_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with DM2 that have a most recent BP <= 140/90 at baseline

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)                                          
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#select all DM2 with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM2 <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_DM2_min <- setkey(setDT(Total_DM2), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_DM2_denom_N <-NROW(Total_DM2_min) 
#from this selection selects the DM2ertensives that had BP <= 140/90 in the first visit
Total_DM2BP_min_denom <- Total_DM2_min[Total_DM2_min$Sys.BP < 140 & Total_DM2_min$Dia.BP < 90,]
Total_DM2BP_min_denom_N <-NROW(Total_DM2BP_min_denom)
(perc_DM2_BP<-(Total_DM2BP_min_denom_N/Total_DM2_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with DM2 that have a most recent BP <= 140/90 at 4-8 months after admission

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)  
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Date.of.visit , units = c("days"))
#selects all DM2 with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM2 <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_DM2_sec <- Total_DM2[Total_DM2$diff_adm_vis < -130 & Total_DM2$diff_adm_vis > -250,]
Total_DM2_sec_min <- setkey(setDT(Total_DM2_sec), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_DM2_sec_min_N <-NROW(Total_DM2_sec_min) 

#from this selection selects the DM2ertensives that had BP <= 140/90 in the first visit
Total_DM2BP_min_denom <- Total_DM2_sec_min[Total_DM2_sec_min$Sys.BP < 140 & Total_DM2_sec_min$Dia.BP < 90,]
Total_DM2BP_min_denom_N <-NROW(Total_DM2BP_min_denom)
(perc_DM2_BP<-(Total_DM2BP_min_denom_N/Total_DM2_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
#No./% patients with DM2 that have a most recent BP <= 140/90 at 9-12 months after admission

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)
visitP <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Date.of.visit , units = c("days"))
#select all DM2 with admission date in 2016 (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM2 <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday,]
Total_DM2_sec <- Total_DM2[Total_DM2$diff_adm_vis < -250 & Total_DM2$diff_adm_vis > -370,]
Total_DM2_sec_min <- setkey(setDT(Total_DM2_sec), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_DM2_sec_min_N <-NROW(Total_DM2_sec_min) 
#from this selection selects the DM2ertensives that had BP <= 140/90 in the first visit
Total_DM2BP_min_denom <- Total_DM2_sec_min[Total_DM2_sec_min$Sys.BP < 140 & Total_DM2_sec_min$Dia.BP < 90,]
Total_DM2BP_min_denom_N <-NROW(Total_DM2BP_min_denom)
(perc_DM2_BP<-(Total_DM2BP_min_denom_N/Total_DM2_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of patients with diabetes with  HbA1c < 8.0 %/ 7.0 %  at baseline
Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)                                          
visitPa <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
visitP <- merge(lab, all.x = TRUE, visitPa, c("Patient.ID"))
#selects all diabetics with admission date in 2016, with a HbA1C test (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday & visitP$HbA1C != "",]
Total_DM_min <- setkey(setDT(Total_DM), Patient.ID)[, .SD[which.min(Exam.date)], Patient.ID]
Total_DM_denom_N <-NROW(Total_DM_min) 
#from this selection selects the hypertensives that had BP <= 140/90 in the first visit
Total_DMHbA1C_min_denom <- Total_DM_min[Total_DM_min$HbA1C < 8 ,]
Total_DMHbA1C_min_denom_N <-NROW(Total_DMHbA1C_min_denom)
(perc_DM_HbA1C<-(Total_DMHbA1C_min_denom_N/Total_DM_denom_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of patients with diabetes with last HbA1c < 8.0 %/ 7.0 %  6  months post enrolment and trend from baseline

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)

visitPa <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
visitP <- merge(lab, all.x = TRUE, visitPa, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Exam.date , units = c("days"))

#selects all diabetics with admission date in 2016, with a HbA1C test (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday & visitP$HbA1C != "",]
Total_DM_sec <- Total_DM[Total_DM$diff_adm_vis < -130 & Total_DM$diff_adm_vis > -250,]
Total_DM_sec_min <- setkey(setDT(Total_DM_sec), Patient.ID)[, .SD[which.max(Exam.date)], Patient.ID]
Total_DM_sec_min_N <-NROW(Total_DM_sec_min) 

#from this selection selects the diabetics that had HbA1C <= 8 in the second visit
Total_DM_sec_min_denom <- Total_DM_sec_min[Total_DM_sec_min$HbA1C < 8 ,]
Total_DM_sec_min_denom_N <-NROW(Total_DM_sec_min_denom)
(perc_Hyp_BP<-(Total_DM_sec_min_denom_N/Total_DM_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number / % of patients with diabetes with last HbA1c < 8.0 %/ 7.0 %  12  months post enrolment and trend from baseline

Firstday <- "2017-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2015-12-31"
Exitday <- as.Date(Exitday)

visitPa <- merge(visit, all.x = TRUE, patient, c("Patient.ID"))
visitP <- merge(lab, all.x = TRUE, visitPa, c("Patient.ID"))
#calculates difference in days between admission date and visit dates
visitP$diff_adm_vis<- difftime(visitP$Admission.date ,visitP$Exam.date , units = c("days"))

#selects all diabetics with admission date in 2016, with a HbA1C test (denominator)
# if you want to do the analysis on one clinic, add in the line below for instance: visitP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DM <- visitP[visitP$Diabetes.II == 1 & visitP$Admission.date <Firstday & visitP$Admission.date>Exitday & visitP$HbA1C != "",]
Total_DM_sec <- Total_DM[Total_DM$diff_adm_vis < -250 & Total_DM$diff_adm_vis > -370,]
Total_DM_sec_min <- setkey(setDT(Total_DM_sec), Patient.ID)[, .SD[which.max(Exam.date)], Patient.ID]
Total_DM_sec_min_N <-NROW(Total_DM_sec_min) 

#from this selection selects the diabetics that had HbA1C <= 8 in the second visit
Total_DM_sec_min_denom <- Total_DM_sec_min[Total_DM_sec_min$HbA1C < 8 ,]
Total_DM_sec_min_denom_N <-NROW(Total_DM_sec_min_denom)
(perc_Hyp_BP<-(Total_DM_sec_min_denom_N/Total_DM_sec_min_N))

#---------------------------------------------------------------------------------------------------------------------#
# Number/ % of eligible patients with diabetes that have had an annual foot check/ eye check performed during the reporting period
visit$Foot.examination[visit$Foot.examination =="Yes"] <- 1; visit$Foot.examination[visit$Foot.examination =="No"] <- 0;
visit$Foot.examination <- as.numeric(as.character(visit$Foot.examination))

Firstday <- "2016-01-01"
Firstday <- as.Date(Firstday)
Exitday <- "2014-12-31"
Exitday <- as.Date(Exitday)
Total_DiabetesII <- visit[visit$Diabetes.II == 1 &visit$Date.of.visit<Firstday ,]
Total_DiabetesII_min <- setkey(setDT(Total_DiabetesII), Patient.ID)[, .SD[which.min(Date.of.visit)], Patient.ID]
Total_DiabetesII_minP <- merge(patient, all.x = TRUE, Total_DiabetesII_min, c("Patient.ID")) 
# if you want to do the analysis on one clinic, add in the line below for instance: Total_DiabetesII_minP$Clinic== "Ibn Sina" &. If all clinics together, remove this part. 
Total_DiabetesII_denom <- Total_DiabetesII_minP[Total_DiabetesII_minP$Admission.date < Firstday & Total_DiabetesII_minP$Date.of.exit2 > Exitday & Total_DiabetesII_minP$Date.of.visit <Firstday & !is.na(Total_DiabetesII_minP$Date.of.visit),]
Total_DiabetesII_denom_N <-NROW(Total_DiabetesII_denom) 

Total_FE <- visit[visit$Foot.examination== 1 & !is.na(visit$Foot.examination) & visit$Date.of.visit < Firstday,]
Total_FE_max<-setkey(setDT(Total_FE), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
Total_FE_maxP <-Total_FE_max[Total_FE_max$Date.of.visit > Exitday &Total_FE_max$Date.of.visit < Firstday,]
Total_FE_denom <- merge(Total_FE_maxP, all = FALSE, Total_DiabetesII_denom, c("Patient.ID")) # use FALSE to get only DM patients with testing, use TRUE to get all tests
Total_FE_denom_N <-NROW(Total_FE_denom)
(perc_DM2_FE<-(Total_FE_denom_N/Total_DiabetesII_denom_N))

