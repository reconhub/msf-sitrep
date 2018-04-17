
#----> Some of the exited patients don't have an exit date. First add that date in the tool for those patients. Use their last visit date as exit date

# 1) First save the patient, visit and investigation tabs/sheets as 3 separate .txt (tab delimited) files. 
# Then open them with Excel.

# 2) Because excel always formats dates wrongly, you have to apply 3 conversion steps for date columns in the patient file (Admission date & Date of exit),
# the visit file (Date of visit) and the investigation file (Exam date).
# 2a) Create a new column adjacent to the date column and apply the following excel formula  =TEXT(A1,"dd/mm/yyyy"). Change A1 here to
# reflect the date column in question. Apply this to the whole column.
# 2b) Create a new column adjacent to the column created under 2a and apply the following excel formula =RIGHT(B1,4) &"/"&MID(B1,4,2)&"/"&LEFT(B1,2)
# Change the B1's here to reflect the column created under 2a. Apply this to the whole column.

#=============================================================================================================================
# Alternatively, copy the following formula into a new column and replace C2 with the date field to be converted.
# =IF(C2="","2020/01/01",RIGHT(TEXT(C2,"dd/mm/yyyy"),4)&"/"&MID(TEXT(C2,"dd/mm/yyyy"),4,2)&"/"&LEFT(TEXT(C2,"dd/mm/yyyy"),2))
#=============================================================================================================================

# 2c) Copy the whole column created under 2b and paste it in the same column using "paste values" (the 123 paste button). Change dataformat of the column into "General"
# Keep the same column header name. Your date in excel should look like year-month-day: 2016/05/26
# !!Don't edit the txt files anymore with excel (opening no problem though), or the dates will be formatted wrongly again!!

# 3)Loads the final databases into R. Make sure you enter the right path where you keep the .txt files. 
# For instance D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/02_Cohort_Analysis/03_Data_backup/2017/Oct2017/visit.txt
# RUN these 2 lines
patient <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/03_Routine work/03_Data_backup/2018/Jan2018/patient.txt",stringsAsFactors=FALSE)
visit <- read.delim("D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/03_Routine work/03_Data_backup/2018/Jan2018/visit.txt",stringsAsFactors=FALSE)
# Set default directory, for MMR results to be printed to
dir <- "D:/Users/Jordan-Epi/Documents/Epi_2016-17/03_Irbid/03_Routine work/03_Data_backup/2018/Jan2018/"
setwd(dir)

# 4) Indicate the time range of your reporting period. Then RUN these 3 lines:
Upperlimitmonth <- "2018-02-01" # 1 day after the last day in the reporting period
Lowerlimitmonth <- "2017-12-31" # 1 day before the first day in the reporting period
Workingdaysmonth <- 23 # number of clinic working days in reporting period (calendar days minus days off)

# 5) Install needed packages 
install.packages("lubridate") # run only the first time script is used on a new computer
library(lubridate) # ignore message about masked fields that appear the first time this is run in a session.
install.packages("data.table") # run only the first time script is used on a new computer
library(data.table) # ignore message about masked fields that appear the first time this is run in a session.

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#_______Run the script from here________#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

# Converts all date columns into R date format
patient$Admission.date <- as.Date(patient$Admission.date)
patient$Date.of.exit <- as.Date(patient$Date.of.exit)
visit$Date.of.visit <- as.Date(visit$Date.of.visit)
visit$Next.visit.date <- as.Date(visit$Next.visit.date)
Upperlimitmonth <- as.Date(Upperlimitmonth)
Lowerlimitmonth <- as.Date(Lowerlimitmonth)

# Converts current age column to numeric format
patient$Age <- as.numeric(as.character(patient$Age)) # will generate an error if any patients are missing this data.

# First change 'Internal.ID' variable into 'Patient.ID'. Then Merge patient data to visit data.
visitP <- merge(patient, visit,by.x= c("Internal.ID"), by.y = c("Patient.ID")); visitP <- visitP[visitP$Date.of.visit < Upperlimitmonth,]

# Total # of patients ever enrolled (as of the end of the reporting period)
Total_enrolled <- patient[patient$Admission.date < Upperlimitmonth,] 
NROW(Total_enrolled)

# Number of patients newly enrolled in this reporting period 
Total_new_enrolled <- Total_enrolled[Total_enrolled$Admission.date > Lowerlimitmonth,] 
Total_new_enrolled_n <- NROW(Total_new_enrolled)

# Total consultations this reporting period 
Total_new_consults <- visit[visit$Date.of.visit > Lowerlimitmonth & visit$Date.of.visit < Upperlimitmonth,] 
Total_new_consults_n <- NROW(Total_new_consults)
tail(visit$Date.of.visit)
# Average number of consultations per day, this month
Average_consultations_day<-Total_new_consults_n/Workingdaysmonth

# % of individual patients prescribed an antibiotic this month 
Antibiotic <- Total_new_consults[Total_new_consults$Antibiotic.count !=0,]
Antibiotic_n<-NROW(Antibiotic); perc_Total_Antibiotic <- Antibiotic_n/Total_new_consults_n

# Select most recent visit of patients of that month
Total_visitP<-setkey(setDT(visitP), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Active_visitP <- Total_visitP[Total_visitP$Admission.date < Upperlimitmonth & Total_visitP$Admission.date > Lowerlimitmonth,]

#Total_visitP_new <- visitP[visitP$Admission.date < Upperlimitmonth & visitP$Admission.date > Lowerlimitmonth,] # all visits of new patients
#Active_visitP <- setkey(setDT(Total_visitP_new), Internal.ID)[, .SD[which.min(Date.of.visit)], Internal.ID]

# Total # of new enrolled patients, this month, per clinc/home visit(hv)
Total_new_IbnSina <- Active_visitP[Active_visitP$Clinic == "Ibn Sina",]
Total_new_IbnSina_N <- NROW(Total_new_IbnSina); perc_Total_new_IbnSina<-Total_new_IbnSina_N/Total_new_enrolled_n
Total_new_IbnRushd <- Active_visitP[Active_visitP$Clinic == "Ibn Rushd",]
Total_new_IbnRushd_N <- NROW(Total_new_IbnRushd); perc_Total_new_IbnRushd<-Total_new_IbnRushd_N/Total_new_enrolled_n
Total_new_HomveVisit <- Active_visitP[Active_visitP$Clinic == "Home Visit",]
Total_new_HomeVisit_N <- NROW(Total_new_HomveVisit); perc_Total_new_HomeVisit<-Total_new_HomeVisit_N/Total_new_enrolled_n
New_enrolled<- data.frame()
New_enrolled_IbnSina = c(Total_new_IbnSina_N,perc_Total_new_IbnSina); New_enrolled = rbind(New_enrolled,New_enrolled_IbnSina)
New_enrolled_IbnRushd = c(Total_new_IbnRushd_N,perc_Total_new_IbnRushd); New_enrolled = rbind(New_enrolled,New_enrolled_IbnRushd)
New_enrolled_HomeVisit = c(Total_new_HomeVisit_N,perc_Total_new_HomeVisit); New_enrolled = rbind(New_enrolled,New_enrolled_HomeVisit)
colnames(New_enrolled) <- c("Freq.x","Freq.y")
New_enrolled$Var1<-c("Total_new_IbnSina", "Total_new_IbnRushd","Total_new_HomveVisit")
New_enrolled<-New_enrolled[,c("Var1","Freq.x","Freq.y")]

# Total # of new consultations, this month, per clinc/hv, 
Total_new_consults_N<-table(Total_new_consults$Clinic); Total_new_consults_N_df<-as.data.frame(Total_new_consults_N)
Total_new_consults_per<-prop.table(Total_new_consults_N); Total_new_consults_per_df<-as.data.frame(Total_new_consults_per)
(Total_new_consults_clinic <- merge(Total_new_consults_N_df, all.x = TRUE, Total_new_consults_per_df, c("Var1")))
Total_new_consults_clinic$Var1<-c("Total_newconsults_HomveVisit", "Total_newconsults_IbnRushd","Total_newconsults_IbnSina")

# Total # of new consultations, this month, per clinc/hv, per type of visit (nurse or doctor)
Total_new_consults_Doc <- Total_new_consults[Total_new_consults$Visit.type == "Physician",]
Total_new_consults_Doc_N<-table(Total_new_consults_Doc$Clinic); Total_new_consults_Doc_N_df<-as.data.frame(Total_new_consults_Doc_N)
Total_new_consults_Doc_per<-prop.table(Total_new_consults_Doc_N); Total_new_consults_Doc_per_df<-as.data.frame(Total_new_consults_Doc_per)
(Total_new_consults_Doc_clinic <- merge(Total_new_consults_Doc_N_df, all.x = TRUE, Total_new_consults_Doc_per_df, c("Var1")))
Total_new_consults_Doc_clinic$Var1<-c("Total_newDoctorconsults_HomveVisit", "Total_newDoctorconsults_IbnRushd","Total_newDoctorconsults_IbnSina")

Total_new_consults_Nurse <- Total_new_consults[Total_new_consults$Visit.type == "Nurse",]
Total_new_consults_Nurse_N<-table(Total_new_consults_Nurse$Clinic); Total_new_consults_Nurse_N_df<-as.data.frame(Total_new_consults_Nurse_N)
Total_new_consults_Nurse_per<-prop.table(Total_new_consults_Nurse_N); Total_new_consults_Nurse_per_df<-as.data.frame(Total_new_consults_Nurse_per)
(Total_new_consults_Nurse_clinic <- merge(Total_new_consults_Nurse_N_df, all.x = TRUE, Total_new_consults_Nurse_per_df, c("Var1")))

# Total number of new referrals, and referrals to whom

Total_new_Referral <- Total_new_consults[Total_new_consults$Referral == "Yes",]; New_referral<-NROW(Total_new_Referral)
Total_new_HospitalEr <- Total_new_consults[Total_new_consults$Hospital.ER == "Yes",]; New_hospitalEr<-NROW(Total_new_HospitalEr);perc_New_hospitalEr <- New_hospitalEr/New_referral
Total_new_Podiatry <- Total_new_consults[Total_new_consults$Podiatry == "Yes",]; New_podiatry<-NROW(Total_new_Podiatry);perc_New_podiatry <- New_podiatry/New_referral
Total_new_Ophthalmology <- Total_new_consults[Total_new_consults$Ophthalmology == "Yes",];New_ophthalmology<-(NROW(Total_new_Ophthalmology));perc_New_ophthalmology <- New_ophthalmology/New_referral
Total_new_Surgeon <- Total_new_consults[Total_new_consults$Surgeon == "Yes",];New_surgeon<-(NROW(Total_new_Surgeon));perc_New_surgeon <- New_surgeon/New_referral
Total_new_Cardiologist <- Total_new_consults[Total_new_consults$Cardiologist == "Yes",]; New_cardiologist<-(NROW(Total_new_Cardiologist));perc_New_cardiologist <- New_cardiologist/New_referral
Total_new_Endocrine <- Total_new_consults[Total_new_consults$Endocrine == "Yes",];New_endocrine<-(NROW(Total_new_Endocrine));perc_New_endocrine <- New_endocrine/New_referral
Total_new_Respiratory <- Total_new_consults[Total_new_consults$Respiratory == "Yes",];New_respiratory<-(NROW(Total_new_Respiratory));perc_New_respiratory <- New_respiratory/New_referral
Total_new_Nephrology <- Total_new_consults[Total_new_consults$Nephrology == "Yes",];New_nephrology<-(NROW(Total_new_Nephrology));perc_New_nephrology <- New_nephrology/New_referral
Total_new_Other <- Total_new_consults[Total_new_consults$Other == "Yes",];New_other<-(NROW(Total_new_Other));perc_New_other <- New_other/New_referral
perc_New_Referral<-New_referral/Total_new_consults_n

Total_Referral<- data.frame()
NewReferral_total = c(New_referral,perc_New_Referral); Total_Referral = rbind(Total_Referral,NewReferral_total)
HospitalEr = c(New_hospitalEr,perc_New_hospitalEr); Total_Referral = rbind(Total_Referral,HospitalEr)
Podiatry = c(New_podiatry,perc_New_podiatry);Total_Referral = rbind(Total_Referral,Podiatry)
Opthalmology = c(New_ophthalmology,perc_New_ophthalmology);Total_Referral = rbind(Total_Referral,Opthalmology)
Surgeon = c(New_surgeon,perc_New_surgeon);Total_Referral = rbind(Total_Referral,Surgeon)
Cardio = c(New_cardiologist,perc_New_cardiologist);Total_Referral = rbind(Total_Referral,Cardio)
Endocrine = c(New_endocrine,perc_New_endocrine);Total_Referral = rbind(Total_Referral,Endocrine)
Respiratory = c(New_respiratory,perc_New_respiratory);Total_Referral = rbind(Total_Referral,Respiratory)
Nephrology = c(New_nephrology,perc_New_nephrology);Total_Referral = rbind(Total_Referral,Nephrology)
Other = c(New_other,perc_New_other);Total_Referral = rbind(Total_Referral,Other)
colnames(Total_Referral) <- c("Freq.x","Freq.y")
Total_Referral$Var1<-c("NewReferral_total", "HospitalEr","Podiatry", "Opthalmology", "Surgeon", "Cardio", "Endocrine", "Respiratory", "Nephrology", "Other")
(Total_Referral<-Total_Referral[,c("Var1","Freq.x","Freq.y")])

#Total # of actively enrolled patients, clinic 
Active_visitP_clinic <- Total_visitP[Total_visitP$Exit == "",]
Total_active_N<-NROW(Active_visitP_clinic)
Total_active_tab<-table(Active_visitP_clinic$Clinic); Total_active_N_df<-as.data.frame(Total_active_tab)
Total_active_per<-prop.table(Total_active_tab); Total_active_per_df<-as.data.frame(Total_active_per)
(Total_enrolled_active <- merge(Total_active_N_df, all.x = TRUE, Total_active_per_df, c("Var1")))
Total_enrolled_active$Var1<-c("TotalActivePatient_HomeVisit", "TotalActivePatient_IbnRushd","TotalActivePatient_IbnSina")

# Total # of actively enrolled patients
Active_last_visitP <- Total_enrolled[Total_enrolled$Exit == "", ]
Active_last_visitP_N<-NROW(Active_last_visitP)


#==================================Taking into account 90 days-is-inactive guideline===========================

# Number of patients active at end of period (ie not exited or defaulted).
# Change from old code here (was previously any patient ever whose last visit was not marked as an exit)
#Last_visitP<-setkey(setDT(visitP), Internal.ID)[, .SD[which.max(Date.of.visit)], Internal.ID] # Last visit of all patients up to end of period
#Active_last_visitP <- Last_visitP[Upperlimitmonth-Last_visitP$Date.of.visit <= 91 & Last_visitP$Exit == "",] # Patients not exited or defaulted by end of reporting period (91 due to Upperlimit being 1 day after end of reporting period)

# Code if change for defaulter definitions is changed. Instead of 90 days from last visit would be 90 days from last appointment or, if no appointment was set, 120 from last visit.
#Total_active <- Last_visitP[Upperlimitmonth-ifelse(Last_visitP$Next.visit.nr.of.days.months.years == 0,Last_visitP$Date.of.visit,Last_visitP$Next.visit.date) < ifelse(Last_visitP$Next.visit.nr.of.days.months.years==0,120,90),]
#Total_active_N<-NROW(Active_last_visitP) # Number of patients in program at end of period

# Total # of actively enrolled patients, clinic 
#Total_active_tab<-table(Active_last_visitP$Clinic); Total_active_N_df<-as.data.frame(Total_active_tab)
#Total_active_per<-prop.table(Total_active_tab); Total_active_per_df<-as.data.frame(Total_active_per)
#(Total_enrolled_active <- merge(Total_active_N_df, all.x = TRUE, Total_active_per_df, c("Var1")))
#Total_enrolled_active$Var1 <- c(paste("Active patients at end of period",Total_enrolled_active$Var1,sep=" - "))

#======================================exit taking into account 90 days-is-inactive guideline =========================

# Cohort exits in this reporting period 
#Total_exit <- Last_visitP[(Last_visitP$Date.of.exit>Lowerlimitmonth & Last_visitP$Date.of.exit<Upperlimitmonth) | (Upperlimitmonth - Last_visitP$Date.of.visit >= 91  & Lowerlimitmonth-89 <= Last_visitP$Date.of.visit & Last_visitP$Exit==""       ), ]
#(NROW(Total_exit))
#Total_exit_N <- table(Total_exit$Exit); Total_exit_N_df<-as.data.frame(Total_exit_N)
#Total_exit_per <- prop.table(Total_exit_N); Total_exit_per_df<-as.data.frame(Total_exit_per)
#(Total_exit <- merge(Total_exit_N_df, all.x = TRUE, Total_exit_per_df, c("Var1")))
#Total_exit$Var1 <- c(paste("Exits this period",Total_exit$Var1,sep=" - "))
#=========================================================================================================================

# Total number of cohort exits

Last_visitP_Exit<-setkey(setDT(visitP), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Total_exit <- Last_visitP_Exit[Last_visitP_Exit$Date.of.exit > Lowerlimitmonth & Last_visitP_Exit$Date.of.exit < Upperlimitmonth & Last_visitP_Exit$Exit != "",]
(NROW(Total_exit))
Total_exit_N<-table(Total_exit$Exit); Total_exit_N_df<-as.data.frame(Total_exit_N)
Total_exit_per<-prop.table(Total_exit_N); Total_exit_per_df<-as.data.frame(Total_exit_per)
(Total_exit <- merge(Total_exit_N_df, all.x = TRUE, Total_exit_per_df, c("Var1")))

Total_gender_N <- table(Active_last_visitP$Gender); Total_gender_N_df<-as.data.frame(Total_gender_N)
Total_gender_per <- prop.table(Total_gender_N); Total_gender_per_df<-as.data.frame(Total_gender_per)
(Total_gender <- merge(Total_gender_N_df, all.x = TRUE, Total_gender_per_df, c("Var1")))

#Age, total # of actively enrolled patients 
age5_ <- Active_last_visitP[Active_last_visitP$Age <6,]; age5_N <- nrow(age5_); perc_age5 <- age5_N/Total_active_N
age5to15 <- Active_last_visitP[Active_last_visitP$Age >5 & Active_last_visitP$Age<16,]; age5to15_N <- nrow(age5to15); perc_age5to15 <- age5to15_N/Total_active_N
age16to40 <- Active_last_visitP[Active_last_visitP$Age >15 & Active_last_visitP$Age<41,];age16to40_N <- nrow(age16to40);perc_age16to40 <- age16to40_N/Total_active_N
age41to65 <- Active_last_visitP[Active_last_visitP$Age >40 & Active_last_visitP$Age<65,];age41to65_N <- nrow(age41to65);perc_age41to65 <- age41to65_N/Total_active_N
age65 <- Active_last_visitP[Active_last_visitP$Age >=65,];age65_N <- nrow(age65);perc_age65 <- age65_N/Total_active_N
Age<- data.frame()
Age5 = c(age5_N,perc_age5);Age = rbind(Age,Age5)
Age15 = c(age5to15_N,perc_age5to15);Age = rbind(Age,Age15)
Age40 = c(age16to40_N,perc_age16to40);Age = rbind(Age,Age40)
Age65 = c(age41to65_N,perc_age41to65);Age = rbind(Age,Age65)
Age66 = c(age65_N,perc_age65);Age = rbind(Age,Age66)
colnames(Age) <- c("Freq.x","Freq.y")
Age$Var1<-c("Age < 6yrs","Age 6-15yrs","Age 16-40yrs", "Age 41-64yrs", "Age >= 65yrs")
Age<-Age[,c("Var1","Freq.x","Freq.y")]

# Nationality, total # of actively enrolled patients 
Total_country_N<-table(Active_last_visitP$Country.of.origin); Total_country_N_df<-as.data.frame(Total_country_N)
Total_country_per<-prop.table(Total_country_N); Total_country_per_df<-as.data.frame(Total_country_per)
(Total_country <- merge(Total_country_N_df, all.x = TRUE, Total_country_per_df, c("Var1")))

# Morbidity based upon the last visit of all patients active at the end of the reporting period
# --> Converts NCD data into positive (1s) and negatives (0s) and turns these values into R numeric format.
# Added mental health disorders and revised Other CVDs to include Other CVD column (oct 2017)

Total_visitP<-setkey(setDT(visitP), MSF.ID)[, .SD[which.max(Date.of.visit)], MSF.ID]
Active_last_visitP <- Total_visitP[Total_visitP$Exit == "", ]
Active_last_visitP_N<-NROW(Active_last_visitP)

Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="New"] <- 1; Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="Old (verified)"] <- 1;Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="No"] <- 0 
Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="New"] <- 1; Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="Old (verified)"] <- 1;Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="Old (unverified)"] <- 1;Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="No"] <- 0 
Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="New"] <- 1; Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="Old (verified)"] <- 1;Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="Old (unverified)"] <- 1;Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="No"] <- 0 
Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="New"] <- 1; Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="Old (verified)"] <- 1;Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="Old (unverified)"] <- 1;Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="No"] <- 0 
Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="New"] <- 1; Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="Old (verified)"] <- 1;Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="Old (unverified)"] <- 1;Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="No"] <- 0 
Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="New"] <- 1; Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="Old (verified)"] <- 1;Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="Old (unverified)"] <- 1;Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="No"] <- 0 
Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="New"] <- 1; Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="Old (verified)"] <- 1;Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="Old (unverified)"] <- 1;Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="No"] <- 0 
Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="New"] <- 1; Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="Old (verified)"] <- 1;Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="Old (unverified)"] <- 1;Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="No"] <- 0 
Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="New"] <- 1; Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="Old (verified)"] <- 1;Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="Old (unverified)"] <- 1;Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="No"] <- 0 
Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="New"] <- 1; Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="Old (verified)"] <- 1;Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="Old (unverified)"] <- 1;Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="No"] <- 0 
Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="New"] <- 1; Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="Old (verified)"] <- 1;Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="Old (unverified)"] <- 1;Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="No"] <- 0 
Active_last_visitP$Peripheral.Vascular.Disease[Active_last_visitP$Peripheral.Vascular.Disease =="Old (verified)"] <- 1;Active_last_visitP$Peripheral.Vascular.Disease[Active_last_visitP$Peripheral.Vascular.Disease =="No"] <- 0 
Active_last_visitP$Stroke[Active_last_visitP$Stroke =="New"] <- 1; Active_last_visitP$Stroke[Active_last_visitP$Stroke =="Old (verified)"] <- 1;Active_last_visitP$Stroke[Active_last_visitP$Stroke =="Old (unverified)"] <- 1;Active_last_visitP$Stroke[Active_last_visitP$Stroke =="No"] <- 0 
Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="New"] <- 1; Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="Old (verified)"] <- 1;Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="Old (unverified)"] <- 1;Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="No"] <- 0 
Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="New"] <- 1; Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="Old (verified)"] <- 1;Active_last_visitP$Diabetes.I[Active_last_visitP$Diabetes.I =="No"] <- 0 
Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="New"] <- 1; Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="Old (verified)"] <- 1;Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="Old (unverified)"] <- 1;Active_last_visitP$Diabetes.II[Active_last_visitP$Diabetes.II =="No"] <- 0 
Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="New"] <- 1; Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="Old (verified)"] <- 1;Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="Old (unverified)"] <- 1;Active_last_visitP$Hypertension.y[Active_last_visitP$Hypertension.y =="No"] <- 0 
Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="New"] <- 1; Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="Old (verified)"] <- 1;Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="Old (unverified)"] <- 1;Active_last_visitP$COPD.y[Active_last_visitP$COPD.y =="No"] <- 0 
Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="New"] <- 1; Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="Old (verified)"] <- 1;Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="Old (unverified)"] <- 1;Active_last_visitP$Asthma.y[Active_last_visitP$Asthma.y =="No"] <- 0 
Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="New"] <- 1; Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="Old (verified)"] <- 1;Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="Old (unverified)"] <- 1;Active_last_visitP$Hypothyroidism[Active_last_visitP$Hypothyroidism =="No"] <- 0 
Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="New"] <- 1; Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="Old (verified)"] <- 1;Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="Old (unverified)"] <- 1;Active_last_visitP$Stable.angina[Active_last_visitP$Stable.angina =="No"] <- 0 
Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="New"] <- 1; Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="Old (verified)"] <- 1;Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="Old (unverified)"] <- 1;Active_last_visitP$Unstable.angina[Active_last_visitP$Unstable.angina =="No"] <- 0 
Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="New"] <- 1; Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="Old (verified)"] <- 1;Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="Old (unverified)"] <- 1;Active_last_visitP$Angioplasty.CABG[Active_last_visitP$Angioplasty.CABG =="No"] <- 0 
Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="New"] <- 1; Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="Old (verified)"] <- 1;Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="Old (unverified)"] <- 1;Active_last_visitP$Myocardial.Infarction[Active_last_visitP$Myocardial.Infarction =="No"] <- 0 
Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="New"] <- 1; Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="Old (verified)"] <- 1;Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="Old (unverified)"] <- 1;Active_last_visitP$Congestive.Heart.Failure[Active_last_visitP$Congestive.Heart.Failure =="No"] <- 0 
Active_last_visitP$Peripheral.Vascular.Disease[Active_last_visitP$Peripheral.Vascular.Disease =="Old (verified)"] <- 1;Active_last_visitP$Peripheral.Vascular.Disease[Active_last_visitP$Peripheral.Vascular.Disease =="No"] <- 0 
Active_last_visitP$Cardiovascular.other[Active_last_visitP$Cardiovascular.other =="New"] <- 1; Active_last_visitP$Cardiovascular.other[Active_last_visitP$Cardiovascular.other =="Old (verified)"] <- 1;Active_last_visitP$Cardiovascular.other[Active_last_visitP$Cardiovascular.other =="Old (unverified)"] <- 1;Active_last_visitP$Cardiovascular.other[Active_last_visitP$Cardiovascular.other =="No"] <- 0 
Active_last_visitP$Stroke[Active_last_visitP$Stroke =="New"] <- 1; Active_last_visitP$Stroke[Active_last_visitP$Stroke =="Old (verified)"] <- 1;Active_last_visitP$Stroke[Active_last_visitP$Stroke =="Old (unverified)"] <- 1;Active_last_visitP$Stroke[Active_last_visitP$Stroke =="No"] <- 0 
Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="New"] <- 1; Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="Old (verified)"] <- 1;Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="Old (unverified)"] <- 1;Active_last_visitP$Musculoskeletal.disorder[Active_last_visitP$Musculoskeletal.disorder =="No"] <- 0 
Active_last_visitP$Neurological.disorder[Active_last_visitP$Neurological.disorder =="New"] <- 1; Active_last_visitP$Neurological.disorder[Active_last_visitP$Neurological.disorder =="Old (verified)"] <- 1;Active_last_visitP$Neurological.disorder[Active_last_visitP$Neurological.disorder =="Old (unverified)"] <- 1;Active_last_visitP$Neurological.disorder[Active_last_visitP$Neurological.disorder =="No"] <- 0 
Active_last_visitP$Mental.Health.disorder[Active_last_visitP$Mental.Health.disorder =="New"] <- 1; Active_last_visitP$Mental.Health.disorder[Active_last_visitP$Mental.Health.disorder =="Old (verified)"] <- 1;Active_last_visitP$Mental.Health.disorder[Active_last_visitP$Mental.Health.disorder =="Old (unverified)"] <- 1;Active_last_visitP$Mental.Health.disorder[Active_last_visitP$Mental.Health.disorder =="No"] <- 0 

Active_last_visitP$Diabetes.I <- as.numeric(as.character(Active_last_visitP$Diabetes.I))
Active_last_visitP$Diabetes.II <- as.numeric(as.character(Active_last_visitP$Diabetes.II))
Active_last_visitP$COPD.y <- as.numeric(as.character(Active_last_visitP$COPD.y))
Active_last_visitP$Hypertension.y <- as.numeric(as.character(Active_last_visitP$Hypertension.y))
Active_last_visitP$Asthma.y <- as.numeric(as.character(Active_last_visitP$Asthma.y))
Active_last_visitP$Hypothyroidism <- as.numeric(as.character(Active_last_visitP$Hypothyroidism))
Active_last_visitP$Stable.angina <- as.numeric(as.character(Active_last_visitP$Stable.angina))
Active_last_visitP$Unstable.angina <- as.numeric(as.character(Active_last_visitP$Unstable.angina))
Active_last_visitP$Angioplasty.CABG <- as.numeric(as.character(Active_last_visitP$Angioplasty.CABG))
Active_last_visitP$Myocardial.Infarction <- as.numeric(as.character(Active_last_visitP$Myocardial.Infarction))
Active_last_visitP$Congestive.Heart.Failure <- as.numeric(as.character(Active_last_visitP$Congestive.Heart.Failure))
Active_last_visitP$Peripheral.Vascular.Disease <- as.numeric(as.character(Active_last_visitP$Peripheral.Vascular.Disease))
Active_last_visitP$Cardiovascular.other <- as.numeric(as.character(Active_last_visitP$Cardiovascular.other))
Active_last_visitP$Stroke <- as.numeric(as.character(Active_last_visitP$Stroke))
Active_last_visitP$Neurological.disorder <- as.numeric(as.character(Active_last_visitP$Neurological.disorder))
Active_last_visitP$Musculoskeletal.disorder <- as.numeric(as.character(Active_last_visitP$Musculoskeletal.disorder))
Active_last_visitP$Mental.Health.disorder <- as.numeric(as.character(Active_last_visitP$Mental.Health.disorder))

Total_Diabetes1 <- Active_last_visitP[Active_last_visitP$Diabetes.I == 1 ,]; Total_Diabetes1_N <- NROW(Total_Diabetes1);perc_Total_Diabetes1 <- Total_Diabetes1_N/Total_active_N
Total_Diabetes2 <- Active_last_visitP[Active_last_visitP$Diabetes.II ==1 ,]; Total_Diabetes2_N <- NROW(Total_Diabetes2);perc_Total_Diabetes2 <- Total_Diabetes2_N/Total_active_N
Total_Hypertension <- Active_last_visitP[Active_last_visitP$Hypertension.y ==1 ,]; Total_Hypertension_N <- NROW(Total_Hypertension);perc_Total_Hypertension <- Total_Hypertension_N/Total_active_N
Total_COPD <- Active_last_visitP[Active_last_visitP$COPD.y ==1 ,]; Total_COPD_N<-NROW(Total_COPD);perc_Total_COPD <- Total_COPD_N/Total_active_N
Total_Asthma <- Active_last_visitP[Active_last_visitP$Asthma.y ==1 ,]; Total_Asthma_N<-NROW(Total_Asthma);perc_Total_Asthma <- Total_Asthma_N/Total_active_N/Total_active_N
Total_Hypothyroidism <- Active_last_visitP[Active_last_visitP$Hypothyroidism ==1 ,]; Total_Hypothyroidism_N <- NROW(Total_Hypothyroidism);perc_Total_Hypothyroidism <- Total_Hypothyroidism_N/Total_active_N
Total_Neurological.disorder <- Active_last_visitP[Active_last_visitP$Neurological.disorder ==1 ,]; (Total_Neurological.disorder_N <- NROW(Total_Neurological.disorder));(perc_Total_Neurological.disorder <- Total_Neurological.disorder_N/Total_active_N)
Total_Musculoskeletal.disorder <- Active_last_visitP[Active_last_visitP$Musculoskeletal.disorder ==1 ,]; (Total_Musculoskeletal.disorder_N <- NROW(Total_Musculoskeletal.disorder));(perc_Total_Musculoskeletal.disorder <- Total_Musculoskeletal.disorder_N/Total_active_N)
Total_MH.disorder <- Active_last_visitP[Active_last_visitP$Mental.Health.disorder ==1,]; (Total_MH.disorder_N <- NROW (Total_MH.disorder)); perc_Total_MH.disorder <- Total_MH.disorder_N/Total_active_N

# Patients >16y/o with BMI>30, denominator only those that we have data on. 
Active_last_visitP_BMI <- Active_last_visitP[Active_last_visitP$BMI >=16 & Active_last_visitP$BMI <60 & Active_last_visitP$Age >16,];Active_last_visitP_BMI_N<-NROW(Active_last_visitP_BMI)
Total_BMI <- Active_last_visitP_BMI[Active_last_visitP_BMI$BMI >= 30 ,]; Total_BMI_N<-NROW(Total_BMI);perc_Total_BMI <- Total_BMI_N/Active_last_visitP_BMI_N

# Patients with diabetes and hypertension (other chronic disease can not be part of this, others like neuro, musco etc are)
Total_Diabetes2Hypertension <- Active_last_visitP[Active_last_visitP$Diabetes.II ==1 & Active_last_visitP$Hypertension.y == 1 & Active_last_visitP$Diabetes.I == 0& Active_last_visitP$COPD.y == 0  & Active_last_visitP$Asthma.y == 0 & Active_last_visitP$Hypothyroidism == 0
& Active_last_visitP$Stable.angina == 0 & Active_last_visitP$Unstable.angina == 0 & Active_last_visitP$Angioplasty.CABG == 0 & Active_last_visitP$Myocardial.Infarction == 0 & Active_last_visitP$Congestive.Heart.Failure == 0
& Active_last_visitP$Peripheral.Vascular.Disease == 0 & Active_last_visitP$Cardiovascular.other == 0 & Active_last_visitP$Stroke == 0,]
Total_Diabetes2Hypertension_N<-NROW(Total_Diabetes2Hypertension);perc_Total_Diabetes2Hypertension <- Total_Diabetes2Hypertension_N/Total_active_N

# Changed logic for hypertension only (now anyone with hypertension = 1 and number of conditions =1 is counted).
Total_Hypertensiononly <- Active_last_visitP[Active_last_visitP$Hypertension.y == 1 & Active_last_visitP$Diabetes.II == 0 & Active_last_visitP$Diabetes.I == 0 & Active_last_visitP$COPD.y == 0  & Active_last_visitP$Asthma.y == 0 & Active_last_visitP$Hypothyroidism == 0
                                             & Active_last_visitP$Stable.angina == 0 & Active_last_visitP$Unstable.angina == 0 & Active_last_visitP$Angioplasty.CABG == 0 & Active_last_visitP$Myocardial.Infarction == 0 & Active_last_visitP$Congestive.Heart.Failure == 0
                                             & Active_last_visitP$Peripheral.Vascular.Disease == 0 & Active_last_visitP$Cardiovascular.other == 0 & Active_last_visitP$Stroke == 0,] 
Total_Hypertensiononly_N<-NROW(Total_Hypertensiononly);perc_Total_Hypertensiononly <- Total_Hypertensiononly_N/Total_active_N

# Changed logic for diabetes 2 only (now anyone with DM2 =1 and number of conditions = 1 is counted).
Total_Diabetes2only <- Active_last_visitP[Active_last_visitP$Diabetes.II ==1 & Active_last_visitP$Hypertension.y == 0 & Active_last_visitP$Diabetes.I == 0 & Active_last_visitP$COPD.y == 0  & Active_last_visitP$Asthma.y == 0 & Active_last_visitP$Hypothyroidism == 0
                                          & Active_last_visitP$Stable.angina == 0 & Active_last_visitP$Unstable.angina == 0 & Active_last_visitP$Angioplasty.CABG == 0 & Active_last_visitP$Myocardial.Infarction == 0 & Active_last_visitP$Congestive.Heart.Failure == 0
                                          & Active_last_visitP$Peripheral.Vascular.Disease == 0 & Active_last_visitP$Cardiovascular.other == 0 & Active_last_visitP$Stroke == 0,]
Total_Diabetes2only_N<-NROW(Total_Diabetes2only);perc_Total_Diabetes2only <- Total_Diabetes2only_N/Total_active_N

# Patients with other CVD (asides from hypertension)
Active_last_visitP$CVD <-rowSums(Active_last_visitP[,c("Stable.angina", "Unstable.angina","Angioplasty.CABG","Myocardial.Infarction","Congestive.Heart.Failure","Peripheral.Vascular.Disease","Cardiovascular.other","Stroke" )])
CVD <- Active_last_visitP[Active_last_visitP$CVD !=0,]
CVD_N <- NROW(CVD) ; perc_CVD_N <- CVD_N/Total_active_N

# Patients with Hypertension and a CVD only
HypertensionCVD <- Active_last_visitP[Active_last_visitP$CVD !=0 & Active_last_visitP$Diabetes.II == 0 & Active_last_visitP$Hypertension.y ==1& Active_last_visitP$Diabetes.I == 0
                                                     & Active_last_visitP$Diabetes.I ==0 & Active_last_visitP$COPD.y ==0 & Active_last_visitP$Asthma.y ==0 & Active_last_visitP$Hypothyroidism ==0 ,]
HypertensionCVD_N <- NROW(HypertensionCVD) ; perc_Total_HypertensionCVD_N<- HypertensionCVD_N/Total_active_N

# Patients with Diabetes, hypertension and CVD only
Total_Diabetes2HypertensionCVD <- Active_last_visitP[Active_last_visitP$CVD !=0 & Active_last_visitP$Diabetes.II == 1 & Active_last_visitP$Hypertension.y ==1
                                                     & Active_last_visitP$Diabetes.I ==0 & Active_last_visitP$COPD.y ==0 & Active_last_visitP$Asthma.y ==0 & Active_last_visitP$Hypothyroidism ==0 
                                                     & Active_last_visitP$Neurological.disorder ==0 & Active_last_visitP$Musculoskeletal.disorder ==0,]
Total_Diabetes2HypertensionCVD_N <- NROW(Total_Diabetes2HypertensionCVD) ; perc_Total_Diabetes2HypertensionCVD_N<- Total_Diabetes2HypertensionCVD_N/Total_active_N

Total_Morbidities <- data.frame()
DM1 = c(Total_Diabetes1_N,perc_Total_Diabetes1); Total_Morbidities = rbind(Total_Morbidities,DM1)
Dm2 = c(Total_Diabetes2_N,perc_Total_Diabetes2);Total_Morbidities = rbind(Total_Morbidities,Dm2)
Hypert = c(Total_Hypertension_N,perc_Total_Hypertension);Total_Morbidities = rbind(Total_Morbidities,Hypert)
COPD = c(Total_COPD_N,perc_Total_COPD);Total_Morbidities = rbind(Total_Morbidities,COPD)
Asthma = c(Total_Asthma_N,perc_Total_Asthma);Total_Morbidities = rbind(Total_Morbidities,Asthma)
Hypothyr = c(Total_Hypothyroidism_N,perc_Total_Hypothyroidism);Total_Morbidities = rbind(Total_Morbidities,Hypothyr)
CVD = c(CVD_N,perc_CVD_N);Total_Morbidities = rbind(Total_Morbidities,CVD)
Neuro = c(Total_Neurological.disorder_N,perc_Total_Neurological.disorder);Total_Morbidities = rbind(Total_Morbidities,Neuro)
Musco = c(Total_Musculoskeletal.disorder_N,perc_Total_Musculoskeletal.disorder);Total_Morbidities = rbind(Total_Morbidities,Musco)
MH = c(Total_MH.disorder_N, perc_Total_MH.disorder); Total_Morbidities = rbind(Total_Morbidities,MH)
BMI = c(Total_BMI_N,perc_Total_BMI);Total_Morbidities = rbind(Total_Morbidities,BMI)
DM2Hypert = c(Total_Diabetes2Hypertension_N,perc_Total_Diabetes2Hypertension);Total_Morbidities = rbind(Total_Morbidities,DM2Hypert)
Hypertonly = c(Total_Hypertensiononly_N,perc_Total_Hypertensiononly);Total_Morbidities = rbind(Total_Morbidities,Hypertonly)
DM2only = c(Total_Diabetes2only_N,perc_Total_Diabetes2only);Total_Morbidities = rbind(Total_Morbidities,DM2only)
DM2HypertCVD = c(Total_Diabetes2HypertensionCVD_N,perc_Total_Diabetes2HypertensionCVD_N);Total_Morbidities = rbind(Total_Morbidities,DM2HypertCVD)
HypertCVD = c(HypertensionCVD_N,perc_Total_HypertensionCVD_N);Total_Morbidities = rbind(Total_Morbidities,HypertCVD)

colnames(Total_Morbidities) <- c("Freq.x","Freq.y")
Total_Morbidities$Var1<-c("DM1 in active patients","DM2 in active patients", "Hypertension in active patients", "COPD in active patients", "Asthma in active patients", "Hypothyroidism in active patients", "Other CVDs in active patients (all but hypertension)", "Neurological disorders in active patients","Musculoskeletal disorders in active patients", "Mental health disorders in active patients", "BMI>30 and age>16yrs in active patients", "DM2 & hypertension in active patients", "Hypertension only in active patients", "DM2 only in active patients", "DM2 & hypertension & Other CVD in active patients", "Hypertension & Other CVD in active patients")
Total_Morbidities<-Total_Morbidities[,c("Var1","Freq.x","Freq.y")]

# Next visit distribution
Active_last_visitP$Next.visit.date <- as.Date(Active_last_visitP$Next.visit.date)
Active_last_visitP$diff_in_days<- difftime(Active_last_visitP$Next.visit.date ,Active_last_visitP$Date.of.visit , units = c("days"))

Total_diff_N<-NROW(Active_last_visitP$diff_in_days)

Day29_ <- Active_last_visitP[Active_last_visitP$diff_in_days<30,]; Day29_N <- nrow(Day29_); perc_Day29 <- Day29_N/Total_diff_N
Day3040_ <- Active_last_visitP[Active_last_visitP$diff_in_days>29 & Active_last_visitP$diff_in_days<41,]; Day3040_N <- nrow(Day3040_); perc_Day3040 <- Day3040_N/Total_diff_N
Day4050_ <- Active_last_visitP[Active_last_visitP$diff_in_days>40 & Active_last_visitP$diff_in_days<51,]; Day4050_N <- nrow(Day4050_); perc_Day4050 <- Day4050_N/Total_diff_N
Day5060_ <- Active_last_visitP[Active_last_visitP$diff_in_days>50 & Active_last_visitP$diff_in_days<61,]; Day5060_N <- nrow(Day5060_); perc_Day5060 <- Day5060_N/Total_diff_N
Day6090_ <- Active_last_visitP[Active_last_visitP$diff_in_days>60 & Active_last_visitP$diff_in_days<91,]; Day6090_N <- nrow(Day6090_); perc_Day6090 <- Day6090_N/Total_diff_N
Day90_ <- Active_last_visitP[Active_last_visitP$diff_in_days >90,];Day90_N <- nrow(Day90_);perc_Day90 <- Day90_N/Total_diff_N
Nextvisit<- data.frame()
Day29 = c(Day29_N,perc_Day29);Nextvisit = rbind(Nextvisit,Day29)
Day3040 = c(Day3040_N,perc_Day3040);Nextvisit = rbind(Nextvisit,Day3040)
Day4050 = c(Day4050_N,perc_Day4050);Nextvisit = rbind(Nextvisit,Day4050)
Day5060 = c(Day5060_N,perc_Day5060);Nextvisit = rbind(Nextvisit,Day5060)
Day6090 = c(Day6090_N,perc_Day6090);Nextvisit = rbind(Nextvisit,Day6090)
Day90 = c(Day90_N,perc_Day90);Nextvisit = rbind(Nextvisit,Day90)

colnames(Nextvisit) <- c("Freq.x","Freq.y")
Nextvisit$Var1<-c("<30days","3040days","4050days","5060days","6090days","90days")
Nextvisit<-Nextvisit[,c("Var1","Freq.x","Freq.y")]

# Bind table and print
MMR <- rbind(New_enrolled, Total_new_consults_clinic, Total_new_consults_Doc_clinic, Total_new_consults_Nurse_clinic,Total_Referral, Total_enrolled_active,  Total_exit,Total_gender,Age,Total_country,Total_Morbidities,Nextvisit)
write.csv(MMR, file = "MMR.csv")

#--------------------------------Denominator values & single values (run manually)-------------------------------------#

# Patients with more than 1 NCD
Active_last_visitP$More1NCD <-rowSums(Active_last_visitP[,c("Diabetes.I", "Diabetes.II","Hypertension.y","COPD.y","Asthma.y", "Hypothyroidism","Stable.angina", "Unstable.angina","Angioplasty.CABG","Myocardial.Infarction","Congestive.Heart.Failure","Peripheral.Vascular.Disease", "Cardiovascular.other", "Stroke", "Neurological.disorder","Musculoskeletal.disorder","Mental.Health.disorder" )])
More1NCD <- Active_last_visitP[Active_last_visitP$More1NCD >1,]
More1NCD_N<-NROW(More1NCD) ; perc_More1NCD_N<- More1NCD_N/Total_active_N

# Number of visits in period where antibiotics were prescribed
Antibiotic <- Total_new_consults[Total_new_consults$Antibiotic.count !=0,]
Antibiotic_n<-NROW(Antibiotic); perc_Total_Antibiotic <- Antibiotic_n/Total_new_consults_n

# Average number of consultations per day, this month
Average_consultations_day<-Total_new_consults_n/Workingdaysmonth

#Total number of patient in whom a lab request was made
Total_unique_new_consults<-setkey(setDT(Total_new_consults), Patient.ID)[, .SD[which.max(Date.of.visit)], Patient.ID]
(Total_unique_new_consults_n <- NROW(Total_unique_new_consults)) # number of unique IDs with a consult in last month
Total_unique_new_consults_lab <- Total_unique_new_consults[Total_unique_new_consults$Lab.request == "Yes" ,] 
(Total_unique_new_consults_lab_n <- NROW(Total_unique_new_consults_lab)) # number of unique IDs with a lab request in last month
(Total_unique_new_consults_n_per<-Total_unique_new_consults_lab_n/Total_unique_new_consults_n)# percentage of patients with a lab request last month


