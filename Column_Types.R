# Tim Dawes February 2023
# Paediatric IPAH risk stratification paper
# Code for detecting the data types in the Excel sheet with PH data


library(readxl)


# Read in data
    options(warn=-1)
    d<- read_excel("Spreadsheet.xlsx", na=character(0))
    c<- colnames(d)
    no.rows<- min(which(is.na(d$Demo_HRN)==TRUE)) - 1

# Set wildcards to look for, to identify columns which include dates and text
    date.names<- c("DoB","DoD","Date","date")
    text.names<- c("Name","Notes","Dx","Has PH_SMsheet","Anaesthetist","Consultant","Procedure","Comorbidities","Cardiac diagnoses",
                   "Planned discharge destination","PVR Study?","Has PH","Q_","Gender", "assessment",
                   "Proc_PVRStudy","Proc_Name","Proc_DoneWith6mFU","Description","Echo_TRseverity", "Anaes_Premed_name",
                   "Anaes_Induction_Needed", "Anaes_Induction_Sevo", "Anaes_Induction_N2O",
                   "Echo_RVdys","Echo_RVdil","Echo_RVhyp","Echo_Rad.dysfunc","Echo_Long.dysfunc","Echo_PRseverity",
                   "Echo_RHfunction","Echo_LHfunction")

# Find column names which match the wildcards
    date.cols<- unique(unlist(sapply(date.names, function(x) {grep(x,c)})))
    text.cols<- unique(unlist(sapply(text.names, function(x) {grep(x,c)})))

# Create a vector - 'col_types' - indicating data types in each column 
    col_types<- rep("numeric",ncol(d))
    col_types[date.cols]<- "date"
    col_types[text.cols]<- "text"

# Write the vector to external file
    write.csv(col_types, file="Column_Types.csv", row.names=FALSE)
    write.csv(no.rows, file="Number_of_rows.csv", row.names=FALSE)
