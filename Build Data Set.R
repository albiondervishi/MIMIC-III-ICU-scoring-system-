install.packages("sqldf")
install.packages("lubridate")
install.packages("forcats")
install.packages("BiocGenerics")
library(sqldf)
library(dplyr)
library(lubridate)
library(plyr)
library(forcats)
library(BiocGenerics)

#---------JOIN ICUSTAY AND ADMISSIONS DATA-------------
setwd("C:/Users/amber/Desktop/Data_Practicum/DATA")
icustays<-read.csv("ICUSTAYS.csv")
admissions<-read.csv("ADMISSIONS.csv")
admissions<-sqldf('select A.*, B.* FROM icustays A left join admissions B on A.HADM_ID=B.HADM_ID')
names(admissions)
admissions<-admissions[,c(1:12, 16:31)] # remove duplicate variables of hadm_id, subject_id, and row_id
sum(is.na(admissions$HADM_ID)) # make sure there are no missing values for the hadm_id column
admissions<-admissions[!duplicated(admissions$HADM_ID),] # keep only the first icu visit of each patient visit
anyDuplicated(admissions$HADM_ID) # make sure all the duplicates have been removed (ie subsequent visits to the icu)
admissions<-admissions[admissions$HAS_CHARTEVENTS_DATA==1,] # remove rows where patients have no chartevents
names(admissions)

#----------JOIN DATASET AND DIAGNOSES------------------
diagnoses_icd<-read.csv("DIAGNOSES_ICD.csv")
d_icd_diagnoses<-read.csv("D_ICD_DIAGNOSES.csv")
head(diagnoses_icd)
head(d_icd_diagnoses)
summary(diagnoses_icd)


table(as.factor(diagnoses_icd$SEQ_NUM))

# subset to only include diagnoses 1, 2, etc
diagnoses_1<-diagnoses_icd[diagnoses_icd$SEQ_NUM==1,]
diagnoses_2<-diagnoses_icd[diagnoses_icd$SEQ_NUM==2,]
diagnoses_3<-diagnoses_icd[diagnoses_icd$SEQ_NUM==3,]
diagnoses_4<-diagnoses_icd[diagnoses_icd$SEQ_NUM==4,]
diagnoses_5<-diagnoses_icd[diagnoses_icd$SEQ_NUM==5,]
diagnoses_6<-diagnoses_icd[diagnoses_icd$SEQ_NUM==6,]
diagnoses_7<-diagnoses_icd[diagnoses_icd$SEQ_NUM==7,]
diagnoses_8<-diagnoses_icd[diagnoses_icd$SEQ_NUM==8,]
diagnoses_9<-diagnoses_icd[diagnoses_icd$SEQ_NUM==9,]
diagnoses_10<-diagnoses_icd[diagnoses_icd$SEQ_NUM==10,]
diagnoses_11<-diagnoses_icd[diagnoses_icd$SEQ_NUM==11,]
diagnoses_12<-diagnoses_icd[diagnoses_icd$SEQ_NUM==12,]
diagnoses_13<-diagnoses_icd[diagnoses_icd$SEQ_NUM==13,]
diagnoses_14<-diagnoses_icd[diagnoses_icd$SEQ_NUM==14,]
diagnoses_15<-diagnoses_icd[diagnoses_icd$SEQ_NUM==15,]
diagnoses_16<-diagnoses_icd[diagnoses_icd$SEQ_NUM==16,]
diagnoses_17<-diagnoses_icd[diagnoses_icd$SEQ_NUM==17,]
diagnoses_18<-diagnoses_icd[diagnoses_icd$SEQ_NUM==18,]
diagnoses_19<-diagnoses_icd[diagnoses_icd$SEQ_NUM==19,]
diagnoses_20<-diagnoses_icd[diagnoses_icd$SEQ_NUM==20,]

# combine admissions and diagnoses tables
admissions<-sqldf('select A.*, B.ICD9_CODE AS DIAGNOSES_1, 
                  C.ICD9_CODE AS DIAGNOSES_2, D.ICD9_CODE AS DIAGNOSES_3, E.ICD9_CODE AS DIAGNOSES_4, 
                  F.ICD9_CODE AS DIAGNOSES_5, G.ICD9_CODE AS DIAGNOSES_6, H.ICD9_CODE AS DIAGNOSES_7, 
                  I.ICD9_CODE AS DIAGNOSES_8, J.ICD9_CODE AS DIAGNOSES_9, K.ICD9_CODE AS DIAGNOSES_10,
                  L.ICD9_CODE AS DIAGNOSES_11, M.ICD9_CODE AS DIAGNOSES_12, N.ICD9_CODE AS DIAGNOSES_13,
                  O.ICD9_CODE AS DIAGNOSES_14, P.ICD9_CODE AS DIAGNOSES_15, Q.ICD9_CODE AS DIAGNOSES_16,
                  R.ICD9_CODE AS DIAGNOSES_17, S.ICD9_CODE AS DIAGNOSES_18, T.ICD9_CODE AS DIAGNOSES_19,
                  U.ICD9_CODE AS DIAGNOSES_20
                  from admissions A left join diagnoses_1 B on A.HADM_ID=B.HADM_ID 
                  left join diagnoses_2 C on A.HADM_ID=C.HADM_ID 
                  left join diagnoses_3 D on A.HADM_ID=D.HADM_ID
                  left join diagnoses_4 E on A.HADM_ID=E.HADM_ID
                  left join diagnoses_5 F on A.HADM_ID=F.HADM_ID 
                  left join diagnoses_6 G on A.HADM_ID=G.HADM_ID
                  left join diagnoses_7 H on A.HADM_ID=H.HADM_ID
                  left join diagnoses_8 I on A.HADM_ID=I.HADM_ID 
                  left join diagnoses_9 J on A.HADM_ID=J.HADM_ID
                  left join diagnoses_10 K on A.HADM_ID=K.HADM_ID
                  left join diagnoses_11 L on A.HADM_ID=L.HADM_ID 
                  left join diagnoses_12 M on A.HADM_ID=M.HADM_ID
                  left join diagnoses_13 N on A.HADM_ID=N.HADM_ID
                  left join diagnoses_14 O on A.HADM_ID=O.HADM_ID 
                  left join diagnoses_15 P on A.HADM_ID=P.HADM_ID
                  left join diagnoses_16 Q on A.HADM_ID=Q.HADM_ID
                  left join diagnoses_17 R on A.HADM_ID=R.HADM_ID 
                  left join diagnoses_18 S on A.HADM_ID=S.HADM_ID
                  left join diagnoses_19 T on A.HADM_ID=T.HADM_ID
                  left join diagnoses_20 U on A.HADM_ID=U.HADM_ID
                  ')
head(admissions)
head(diagnoses_icd, 20)
#verify changes were done correctly
admissions[admissions$HADM_ID==172335,]

DATA<-admissions
rm(admissions)
rm(diagnoses_1)
rm(diagnoses_2)
rm(diagnoses_3)
rm(diagnoses_10)
rm(diagnoses_4)
rm(diagnoses_5)
rm(diagnoses_6)
rm(diagnoses_7)
rm(diagnoses_8)
rm(diagnoses_9)
rm(diagnoses_11)
rm(diagnoses_12)
rm(diagnoses_13)
rm(diagnoses_14)
rm(diagnoses_15)
rm(diagnoses_16)
rm(diagnoses_17)
rm(diagnoses_18)
rm(diagnoses_19)
rm(diagnoses_20)

pattern <- "^46|^47|^48|^49|^50|^51" # all the icd-9 codes for respiratory illness fall between 460 and 519

# subset so that seperate diagnoses columns can be created
a<-DATA[(grep(pattern, DATA$DIAGNOSES_1)),]
b<-DATA[(grep(pattern, DATA$DIAGNOSES_2)),]
c<-DATA[(grep(pattern, DATA$DIAGNOSES_3)),]
d<-DATA[(grep(pattern, DATA$DIAGNOSES_4)),]
e<-DATA[(grep(pattern, DATA$DIAGNOSES_5)),]
f<-DATA[(grep(pattern, DATA$DIAGNOSES_6)),]
g<-DATA[(grep(pattern, DATA$DIAGNOSES_7)),]
h<-DATA[(grep(pattern, DATA$DIAGNOSES_8)),]
i<-DATA[(grep(pattern, DATA$DIAGNOSES_9)),]
j<-DATA[(grep(pattern, DATA$DIAGNOSES_10)),]
k<-DATA[(grep(pattern, DATA$DIAGNOSES_11)),]
l<-DATA[(grep(pattern, DATA$DIAGNOSES_12)),]
m<-DATA[(grep(pattern, DATA$DIAGNOSES_13)),]
n<-DATA[(grep(pattern, DATA$DIAGNOSES_14)),]
o<-DATA[(grep(pattern, DATA$DIAGNOSES_15)),]
p<-DATA[(grep(pattern, DATA$DIAGNOSES_16)),]
q<-DATA[(grep(pattern, DATA$DIAGNOSES_17)),]
r<-DATA[(grep(pattern, DATA$DIAGNOSES_18)),]
s<-DATA[(grep(pattern, DATA$DIAGNOSES_19)),]
t<-DATA[(grep(pattern, DATA$DIAGNOSES_20)),]

# combine all rows with respiratory illness
DATA<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)
rm(a)
rm(b)
rm(c)
rm(d)
rm(e)
rm(f)
rm(g)
rm(h)
rm(i)
rm(j)
rm(k)
rm(l)
rm(m)
rm(n)
rm(o)
rm(p)
rm(q)
rm(r)
rm(s)
rm(t)

nrow(DATA)

dir()
patients<-read.csv("PATIENTS.CSV")
head(patients)

# VERIFY THAT THE SUBJECT_ID IS NOT DUPLICATED IN THE DATA TABLE
sqldf('select SUBJECT_ID, count(distinct (SUBJECT_ID)) FROM DATA GROUP BY SUBJECT_ID HAVING COUNT(DISTINCT(SUBJECT_ID))>1 ')

# add the dob and gender from the patient table
DATA<-sqldf('SELECT A.*, B.GENDER, B.DOB FROM DATA A left join patients B on A.SUBJECT_ID=B.SUBJECT_ID')
head(DATA)

# add the short title based on the icd9 on the diagnosis lines 
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_1_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_1=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_2_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_2=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_3_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_3=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_4_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_4=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_5_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_5=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_6_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_6=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_7_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_7=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_8_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_8=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_9_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_9=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_10_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_10=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_11_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_11=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_12_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_12=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_13_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_13=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_14_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_14=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_15_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_15=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_16_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_16=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_17_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_17=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_18_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_18=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_19_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_19=B.ICD9_CODE')
DATA<-sqldf('select A.*, B.SHORT_TITLE AS DIAGNOSES_20_TITLE FROM DATA A left join d_icd_diagnoses B on A.DIAGNOSES_20=B.ICD9_CODE')

head(DATA)

#reorganize DATA for easier examiniation so that diagnoses titles are beside their icd9 codes and gender and dob are with the demographic DATA
names(DATA)

# load microevents table
microevents<-read.csv("MICROBIOLOGYEVENTS.csv")
head(microevents)
microevents$ORG_NAME<-as.character(microevents$ORG_NAME)
microevents<-microevents[!microevents$ORG_NAME=="",] # remove rows where there were a negative result
sort(table(as.factor(microevents$ORG_NAME)))
anyDuplicated(microevents$HADM_ID) # check to see if each row has a unique hadm_id 
nrow(microevents[duplicated(microevents$HADM_ID),]) # look at rows with duplicate hadm_id
microevents<-microevents[!duplicated(microevents$HADM_ID),] # remove all duplicated rows 
microevents$ORG_NAME<-as.factor(microevents$ORG_NAME)
DATA<-sqldf("select A.*, B.ORG_NAME FROM DATA A left join microevents B on A.HADM_ID=B.HADM_ID")
names(DATA)

# load the additional variables from postgresql

# get all the urine output for the first 24 hours in the icu for each patient
dir()
uo<-read.csv("urine_output.csv")
head(uo)
names(uo) <- toupper(names(uo))
nrow(uo[duplicated(uo$ICUSTAY_ID),]) # look at rows with duplicate icustay_id
DATA<-sqldf('select A.*, B.URINEOUTPUT AS URINE_OUTPUT from DATA A left join uo B on A.ICUSTAY_ID=B.ICUSTAY_ID')
head(DATA)
sum(is.na(DATA$URINE_OUTPUT))

# add the weight to the DATAset
dir()
weight<-read.csv("weight_first_day.csv")
head(weight)
weight$weight<-(weight$weight * 2.2046226218488)
names(weight) <- toupper(names(weight))
DATA<-sqldf('select A.*, B.WEIGHT from DATA A left join weight B on A.ICUSTAY_ID=B.ICUSTAY_ID')

# add blood gas measure
blood_gas<-read.csv("blood_gas_first_day.csv")
blood_gas_arterial<-read.csv("blood_gas_first_day_arterial.csv")

# remove rows where all the important varialbes are missing
blood_gas_totalco2<-blood_gas[!is.na(blood_gas$totalco2),]
blood_gas_pco2<-blood_gas[!is.na(blood_gas$pco2),]
blood_gas_ph<-blood_gas[!is.na(blood_gas$ph),]
blood_gas_po2<-blood_gas[!is.na(blood_gas$po2),]
blood_gas_glucose<-blood_gas[!is.na(blood_gas$glucose),]
blood_gas<-rbind(blood_gas_totalco2, blood_gas_pco2, blood_gas_ph, blood_gas_po2, blood_gas_glucose)

blood_gas_arterial_totalco2<-blood_gas_arterial[!is.na(blood_gas_arterial$totalco2),]
blood_gas_arterial_pco2<-blood_gas_arterial[!is.na(blood_gas_arterial$pco2),]
blood_gas_arterial_ph<-blood_gas_arterial[!is.na(blood_gas_arterial$ph),]
blood_gas_arterial_po2<-blood_gas_arterial[!is.na(blood_gas_arterial$po2),]
blood_gas_arterial_glucose<-blood_gas_arterial[!is.na(blood_gas_arterial$glucose),]
blood_gas_arterial<-rbind(blood_gas_arterial_totalco2, blood_gas_arterial_pco2, blood_gas_arterial_ph, blood_gas_arterial_po2, blood_gas_arterial_glucose)

blood_gas<-blood_gas[!duplicated(blood_gas$hadm_id),]
blood_gas_arterial<-blood_gas_arterial[!duplicated(blood_gas_arterial$hadm_id),]

names(blood_gas)
names(blood_gas_arterial)
blood_gas<-blood_gas[,c(2,3,9,24,22,25,13)] # subset to include only totalc02, pc02, ph, p02, and glucose
blood_gas_arterial<-blood_gas_arterial[,c(2,3,10,11,17,20,31)] # subset to include only totalc02, pc02, ph, p02, and glucose
names(blood_gas) <- toupper(names(blood_gas))
names(blood_gas_arterial) <- toupper(names(blood_gas_arterial))
#combine the two sets of blood gas measures
blood_gas_all<-sqldf('SELECT A.*, B.HADM_ID, B.ICUSTAY_ID, B.TOTALCO2 AS TOTALCO2_ART,
                     B.PH AS PH_ART, B.PCO2 AS PCO2_ART, B.PO2 AS PO2_ART, B.GLUCOSE AS GLUCOSE_ART
                     FROM blood_gas A join blood_gas_arterial B ON A.HADM_ID=B.HADM_ID')

# replace the null values for the venous blood gas with the arterial blood gas measures
null_totalco2<-blood_gas_all[is.na(blood_gas_all$TOTALCO2),]
not_null_totalco2<-blood_gas_all[!is.na(blood_gas_all$TOTALCO2),]
null_totalco2$TOTALCO2<-null_totalco2$TOTALCO2_ART
blood_gas_all<-rbind(null_totalco2, not_null_totalco2)

null_ph<-blood_gas_all[is.na(blood_gas_all$PH),]
not_null_ph<-blood_gas_all[!is.na(blood_gas_all$PH),]
null_ph$PH<-null_ph$PH_ART
blood_gas_all<-rbind(null_ph, not_null_ph)

null_pco2<-blood_gas_all[is.na(blood_gas_all$PCO2),]
not_null_pco2<-blood_gas_all[!is.na(blood_gas_all$PCO2),]
null_pco2$PCO2<-null_pco2$PCO2_ART
blood_gas_all<-rbind(null_pco2, not_null_pco2)

null_po2<-blood_gas_all[is.na(blood_gas_all$PO2),]
not_null_po2<-blood_gas_all[!is.na(blood_gas_all$PO2),]
null_po2$PO2<-null_po2$PO2_ART
blood_gas_all<-rbind(null_po2, not_null_po2)

null_glucose<-blood_gas_all[is.na(blood_gas_all$GLUCOSE),]
not_null_glucose<-blood_gas_all[!is.na(blood_gas_all$GLUCOSE),]
null_glucose$GLUCOSE<-null_glucose$GLUCOSE_ART
blood_gas_all<-rbind(null_glucose, not_null_glucose)

# remove unwanted variables from the blood gas
blood_gas_all<-blood_gas_all[,c(1:7)]
blood_gas_all<-blood_gas_all[!duplicated(blood_gas_all$ICUSTAY_ID),]

# add blood gas to the DATA set
DATA<-sqldf('select A.*, B.TOTALCO2, B.PH, B.PCO2, B.PO2, B.GLUCOSE FROM DATA A left join blood_gas_all B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# load rrt (renal replacement therapy) DATA and add it to the DATA set
rrt<-read.csv("rrt_first_day.csv")
names(rrt)<-toupper(names(rrt))
DATA<-sqldf('SELECT A.*, B.RRT FROM DATA A LEFT JOIN rrt B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
DATA$RRT<-as.factor(DATA$RRT)

# add the vitals DATA to the DATA set
vitals<-read.csv("vitals_first_day.csv")
anyDuplicated(vitals$hadm_id)
anyDuplicated(vitals$icustay_id)
vitals<-vitals[, c(2:5, 7,8,10,11, 13,14, 16,17, 19,20, 22, 23, 25, 26)]
names(vitals)<-toupper(names(vitals))
DATA<-sqldf('SELECT A.*, B.* FROM DATA A LEFT JOIN vitals B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
names(DATA)
DATA<-DATA[,-80]
DATA<-DATA[,-81]

# add gcs (glasgow coma score) to DATA set
gcs<-read.csv('gcs_first_day.csv')
head(gcs)
names(gcs)<-toupper(names(gcs))
DATA<-sqldf('SELECT A.*, B.GCSMOTOR, B.GCSVERBAL, B.GCSEYES FROM DATA A LEFT JOIN gcs B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# add ventilator durations
dir()
vent<-read.csv("ventdurations.csv")
head(vent)
names(vent)<-toupper(names(vent))
anyDuplicated(vent$ICUSTAY_ID)
nrow(vent[duplicated(vent$ICUSTAY_ID),])
vent<-vent[!duplicated(vent$ICUSTAY_ID),]
DATA<-sqldf('SELECT A.*, B.DURATION_HOURS AS VENT_DURATION FROM DATA A LEFT JOIN vent B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SAPS DATA
saps<-read.csv("saps_output.csv")
head(saps)
names(saps)<-toupper(names(saps))
nrow(duplicated(saps$ICUSTAY_ID))
DATA<-sqldf('SELECT A.*, B.SAPS FROM DATA A LEFT JOIN saps B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SAPS II DATA
sapsii<-read.csv("sapsii_output.csv")
head(sapsii)
names(sapsii)<-toupper(names(sapsii))
DATA<-sqldf('SELECT A.*, B.SAPSII, B.SAPSII_PROB FROM DATA A LEFT JOIN sapsii B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD SOFA DATA
SOFA<-read.csv("sofa.csv")
head(SOFA)
names(SOFA)<-toupper(names(SOFA))
DATA<-sqldf('SELECT A.*, B.SOFA, B.RESPIRATION AS RESP_SOFA FROM DATA A LEFT JOIN SOFA B ON A.ICUSTAY_ID=B.ICUSTAY_ID')

# ADD OASIS DATA
OASIS<-read.csv("oasis_output.csv")
head(OASIS)
names(OASIS)<-toupper(names(OASIS))
anyDuplicated(OASIS$ICUSTAY_ID)
DATA<-sqldf('SELECT A.*, B.OASIS, B.OASIS_PROB, B.ICUSTAY_AGE_GROUP FROM DATA A LEFT JOIN OASIS B ON A.ICUSTAY_ID=B.ICUSTAY_ID')
table(as.factor(DATA$ICUSTAY_AGE_GROUP))
DATA<-DATA[(DATA$ICUSTAY_AGE_GROUP=="adult"),] # subset to include only adult patients
DATA<-DATA[,-107]

# CREATE LENGTH OF STAY VARIABLE
head(DATA)
DATA$LOS <- as.Date(as.character(DATA$DISCHTIME), format="%Y-%m-%d %H:%M:%S")-
  as.Date(as.character(DATA$ADMITTIME), format="%Y-%m-%d %H:%M:%S")
head(DATA)

head(DATA)
callout<-read.csv("CALLOUT.csv")
head(callout)

caregivers<-read.csv("CAREGIVERS.csv")

table(as.factor(caregivers$DESCRIPTION))

# try to fill in the blanks for the missing description in caregivers table
missing<-caregivers[caregivers$DESCRIPTION=="",]
notmissing<-caregivers[!caregivers$DESCRIPTION=="",]
head(missing)
head(notmissing)
table(as.factor(missing$LABEL))
missing$LABEL<-tolower(missing$LABEL)
table(as.factor(missing$LABEL))
missing$LABEL[grepl("stud", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("stu", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("ms", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("di", missing$LABEL, ignore.case=FALSE)] <- "dietician"
missing$LABEL[grepl("res", missing$LABEL, ignore.case=FALSE)] <- "resident"
missing$LABEL[grepl("dr", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("cow", missing$LABEL, ignore.case=FALSE)] <- "nursing assistant"
missing$LABEL[grepl("rn", missing$LABEL, ignore.case=FALSE)] <- "nurse"
missing$LABEL[grepl("md", missing$LABEL, ignore.case=FALSE)] <- "doctor"
missing$LABEL[grepl("co-wk", missing$LABEL, ignore.case=FALSE)] <- "nursing assistant"
missing$LABEL[grepl("adm", missing$LABEL, ignore.case=FALSE)] <- "administrator"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("st", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("pst", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("ph", missing$LABEL, ignore.case=FALSE)] <- "pharmacist"
missing$LABEL[grepl("med. s", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("lpn", missing$LABEL, ignore.case=FALSE)] <- "nurse"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("rt", missing$LABEL, ignore.case=FALSE)] <- "respiratory therapist"
missing$LABEL[grepl("pa", missing$LABEL, ignore.case=FALSE)] <- "physician assistant"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("np", missing$LABEL, ignore.case=FALSE)] <- "nurse practitioner"
missing$LABEL[grepl("int", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("sn", missing$LABEL, ignore.case=FALSE)] <- "student"
missing$LABEL[grepl("uc", missing$LABEL, ignore.case=FALSE)] <- "uco"

df<-rbind(missing, notmissing)


df$LABEL<-as.character(df$LABEL)
df$DESCRIPTION<-as.character(df$DESCRIPTION)
df$DESCRIPTION[df$LABEL=="doctor"]<-"Attending"
df$DESCRIPTION[df$LABEL=="student"]<-"Student"
df$DESCRIPTION[df$LABEL=="dietician"]<-"Dietitian"
df$DESCRIPTION[df$LABEL=="coord"]<-"Case Manager"
df$DESCRIPTION[df$LABEL=="nurse"]<-"RN"
df$DESCRIPTION[df$LABEL=="nurse practitioner"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="pharmacist"]<-"Pharmacist"
df$DESCRIPTION[df$LABEL=="physician assistant"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="ra"]<-"Research Assistant"
df$DESCRIPTION[df$LABEL=="resident"]<-"Resident/Fellow/PA/NP"
df$DESCRIPTION[df$LABEL=="respiratory therapist"]<-"Respiratory"
nrow(df[df$DESCRIPTION=="",]) # reduced number of missing in description from 2411 to 220
df$LABEL<-as.factor(df$LABEL)
df$DESCRIPTION<-as.factor(df$DESCRIPTION)


# create age variable

DATA$admit_year <- as.POSIXct(DATA$ADMITTIME)
DATA$admit_year<-format(DATA$admit_year, "%Y")
DATA$admit_year<-as.integer(DATA$admit_year)

DATA$dob_year <- as.POSIXct(DATA$DOB)
DATA$dob_year<-format(DATA$dob_year, "%Y")
DATA$dob_year<-as.integer(DATA$dob_year)

DATA$AGE <- DATA$admit_year-DATA$dob_year

# drop unused levels and rearrange variables so that age is with demographic DATA
DATA$dob_year<-NULL
DATA$admit_year<-NULL
droplevels(DATA)

names(DATA)
# reorganize the variables or easier viewing
DATA<-DATA[, c(3:27,29,51,30, 52, 49, 50,      
              71:106, 29, 51, 30, 52, 31,53, 
              32,54, 33,55, 34,56, 35,57, 36,
              58, 37,59, 38,60, 39,61, 40, 62, 
              41, 63, 42, 64, 43, 65, 44, 66, 
              45, 67, 46, 68, 47, 69, 48, 70)]
summary(DATA)
sort(sapply(X = DATA, FUN = function(x) sum(is.na(x))))
# drop variables with more than 20,000 missing values
DATA$GLUCOSE<-NULL
DATA$DIAGNOSES_20<-NULL
DATA$DIAGNOSES_20_TITLE<-NULL
DATA$DIAGNOSES_19<-NULL
DATA$DIAGNOSES_19_TITLE<-NULL
DATA$DIAGNOSES_18<-NULL
DATA$DIAGNOSES_18_TITLE<-NULL
DATA$DIAGNOSES_17<-NULL
DATA$DIAGNOSES_17_TITLE<-NULL
DATA$DIAGNOSES_16<-NULL
DATA$DIAGNOSES_16_TITLE<-NULL
DATA$DIAGNOSES_15<-NULL
DATA$DIAGNOSES_18_TITLE<-NULL
DATA$DIAGNOSES_14<-NULL
DATA$DIAGNOSES_14_TITLE<-NULL

droplevels(DATA)
summary(DATA)
# see if it is practical to convert the diagnoses columns to a factor variable
sort(table(as.factor(DATA$DIAGNOSES_1_TITLE)), decreasing=TRUE)
write.csv(DATA, "DATA.csv")
