## Density Plot data from Spinalcord Mapper
## Script written by Nicolas Stifani contact nstifani@gmail.com
## Note: This Plugin Assume that Y coordinates are Inverted
## Manual CellCounter does not report "inverted Y". So the X=0 Y=0 is the top left corner.
## To Invert Non-Inverted Y Coordinates one must take the absolute of the Y Value minus Maximum Y value
## Y2=abs(Y-Max(Y))


# Functions ---------------------------------------------------------------
rm(list=ls()) # Clear the workspace just in case you have old stuff there

# Functions to install the required packages
InstallRequiredPackage.Function<-function(ListPackage){
  for (PackageI in 1:length(ListPackage)){ 
    RequiredPackageI<-ListPackage[PackageI]
    if (!is.element(RequiredPackageI, installed.packages()[,1])){
      install.packages(RequiredPackageI)
    }
    library(RequiredPackageI, character.only=TRUE) # Load the required packages
  } # Download required packages if there are not already there and load them
}

# Function to display an Error dialog
ErrorDialog.Function<-function(ErrorMessage, FixMessage){
  ErrorDialogChoice<-tk_messageBox(type = "retrycancel", message=paste0(ErrorMessage,"\n\n", FixMessage, "\n\n", "Click Retry to Try Again\n or \nCancel to Abort."), caption = "KinemaR Information", icon="question")
  if(ErrorDialogChoice=="cancel"){
    stop(paste0("Function Stopped because ", ErrorMessage))
  }
}

# Function to select a directory containing CSV file with only one . in their file name
SelectCSVDir.Function <- function(DialogMessage, DirPathObjectName, DirNameObjectName, ListFilePathObjectName, ParentDirPathObjectName){
  CSVDirPass=0
  
  while(CSVDirPass!=1){
    DirPath<-tk_choose.dir(default=getwd(), caption=DialogMessage) # Prompt the user to select an inputdirectory
    DirName<-basename(DirPath) # Defines Name of Input directory
    ListFilePath<-list.files(path=DirPath, pattern=".csv", all.files=FALSE, full.names=TRUE, ignore.case = TRUE) # Get the list of CSV filepath within InputDir
    
    if(length(ListFilePath)==0){
      ErrorMessageInputFile=paste0("Sorry, the folder ",DirName," does not contain any .CSV file.")
      FixMessageInputFile="Please select a folder containing at least one CSV file."
      ErrorDialog.Function(ErrorMessage=ErrorMessageInputFile, FixMessage=FixMessageInputFile)
      NbCSVFilePass=0
    } else {
      NbCSVFilePass=1
    }
    if(length(ListFilePath)>0){
      NameCSVFilePass=1
      for(CSVFileI in 1: length(ListFilePath)){ ## Screen all CSV files to make sure they have only one .
        FilePathCSVFileI <- ListFilePath[CSVFileI]
        FilenameCSVFileI <- basename(FilePathCSVFileI)
        FilenameCSVFileIComponents <- unlist(strsplit(as.character(FilenameCSVFileI),".", fixed=TRUE))
        if(length(FilenameCSVFileIComponents)!=2){ # If more than one . make an error
          NameCSVFilePass=0
          ErrorMessageInputFile=paste0("Sorry, the file ",FilenameCSVFileI," contains more than one \".\" character.")
          FixMessageInputFile="Please ensure that all CSV File contain only one \".\" for the file extension."
          ErrorDialog.Function(ErrorMessage=paste0("Sorry ",FilenameCSVFileI," contains more than one \".\" character."), FixMessage="Please ensure that all CSV File contain only one \".\" for the file extension.")
        }
      }
    }
    if(NbCSVFilePass==1 && NameCSVFilePass==1){
      CSVDirPass=1
    }
  }
  assign(DirPathObjectName, DirPath, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign(DirNameObjectName, DirName, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign(ListFilePathObjectName, ListFilePath, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign(ParentDirPathObjectName, dirname(DirPath), envir = .GlobalEnv)
}

## Function to select a Directory
SelectDir.Function <- function(DialogMessage, DirPathObjectName, DirNameObjectName, ParentDirPathObjectName){
  DirPath<-tk_choose.dir(default=getwd(), caption=DialogMessage) # Prompt the user to select an inputdirectory
  DirName<-basename(DirPath) # Defines Name of Input directory
  assign(DirPathObjectName, DirPath, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign(DirNameObjectName, DirName, envir = .GlobalEnv) # Assign the Variable to the global environment
  assign(ParentDirPathObjectName, dirname(DirPath), envir = .GlobalEnv)
}

## Function to Merge CSV fils from the Select Input Directory
MergeCSVFileList.Function <- function(ListCSVFilePath, MergedObjectName){
  for (FileI in 1:length(ListCSVFilePath)){
    CSVFilePathI <- ListCSVFilePath[FileI] # Defines the Path of the File to be processed
    CSVFilenameI <- basename(CSVFilePathI) # Get the Filename of the File being processed
    CSVFilenameICompoments <- unlist(strsplit(as.character(CSVFilenameI),".", fixed=TRUE))
    CSVFilenameINoExt<-CSVFilenameICompoments[1]
    #  CSVFilenameINoExt <- gsub(".csv","", CSVFilenameI, ignore.case = TRUE) # Create a filename without extension
    DataI <- read.table(CSVFilePathI, sep = ",", header = TRUE, nrows = 100000)
    DataI$File_ID<-rep(CSVFilenameINoExt, dim(DataI)[1])
    if(FileI==1){
      MergedData<-DataI
    } else {
      MergedData<-rbind(MergedData, DataI)
    }
  }
  assign(MergedObjectName, MergedData, envir=.GlobalEnv)
}

# Function to Select a given CSV File
SelectCSVFile.Function <- function(DialogMessage, DataObjectName){
  DataFilePath<-tk_choose.files(default = getwd(), caption = DialogMessage, filters=matrix(c("CSV File",".csv"),1,2, byrow=TRUE), multi=FALSE)
  Data<-read.table(DataFilePath, header=TRUE, sep=",")
  assign(DataObjectName, Data, envir=.GlobalEnv)
}

# Function to Create an Output Directory
CreateOutputDir.Function <- function(OutputDirLocation, OutputDirName, SubDirList){
  
  OutputDirName2=OutputDirName
  n=1 
  while(dir.exists(file.path(OutputDirLocation, OutputDirName2))==TRUE){
    n=n+1
    OutputDirName2=paste0(OutputDirName,"_", n)
  }
  dir.create(file.path(OutputDirLocation, OutputDirName2))
  
  assign("OutputDirPath", file.path(OutputDirLocation, OutputDirName2), envir = .GlobalEnv)
  for(SubDirI in 1:length(SubDirList)){
    dir.create(file.path(OutputDirPath,SubDirList[SubDirI]))
  }
}


# HouseKeeping ------------------------------------------------------------
# Install Required Packages
ListRequiredPackage=c("zoo", "tcltk", "MASS", "Hotelling")
InstallRequiredPackage.Function(ListPackage=ListRequiredPackage)

# Select and Read the Data ------------------------------------------------------------
# Select the Input Data and Get list of CSV files
SelectCSVDir.Function(DialogMessage = "Choose the folder containing the CSV Data Files.",
                      DirPathObjectName="InputDirPath",
                      DirNameObjectName="InputDirName",
                      ListFilePathObjectName="ListInputFilePath",
                      ParentDirPathObjectName="ParentInputDirPath"
)

# Merge all Input CSV Files into one
MergeCSVFileList.Function(ListCSVFilePath=ListInputFilePath, MergedObjectName="MergedInputData")


# Create OuputDirectory and Subdirectory
CreateOutputDir.Function(OutputDirLocation=ParentInputDirPath, 
                         OutputDirName=paste0(InputDirName,"_Coordinates_Processed"),
                         SubDirList=c("Graphs by File", "Tables by File","Graphs by Subject", "Tables by Subject","Graphs by Group", "Tables by Group") )

# Select the MetaData and Registrtion coordinates
SelectCSVFile.Function(DialogMessage="Select the MetaData and Registration Coordinates CSV File", DataObjectName = "MetaData")





# Process MetaData  ---------------------------------------------------
# Get the Resolution Unit as character
MetaData$Resolution_Unit<-as.character(MetaData$Resolution_Unit)

# Center the Metadata for each Row according to the CC position
# If coordinates are already in pixel just copy them
# If coordinates are in scaled distance convert them to pixels

for(RowI in 1:dim(MetaData)[1]){
  if(MetaData$Resolution_Unit[RowI]=="pixels") {  
    MetaData$CC_X.Pixel[RowI]<- MetaData$CC_X[RowI]
    MetaData$CC_Y.Pixel[RowI]<- MetaData$CC_Y[RowI]
    
    MetaData$DE_R_X.Pixel[RowI]<- MetaData$DE_R_X[RowI]
    MetaData$DE_R_Y.Pixel[RowI]<- MetaData$DE_R_Y[RowI]
    
    MetaData$LE_R_X.Pixel[RowI]<- MetaData$LE_R_X[RowI]
    MetaData$LE_R_Y.Pixel[RowI]<- MetaData$LE_R_Y[RowI]
    
    MetaData$VE_R_X.Pixel[RowI]<- MetaData$VE_R_X[RowI]
    MetaData$VE_R_Y.Pixel[RowI]<- MetaData$VE_R_Y[RowI]
    
    MetaData$VE_L_X.Pixel[RowI]<- MetaData$VE_L_X[RowI]
    MetaData$VE_L_Y.Pixel[RowI]<- MetaData$VE_L_Y[RowI]
    
    MetaData$LE_L_X.Pixel[RowI]<- MetaData$LE_L_X[RowI]
    MetaData$LE_L_Y.Pixel[RowI]<- MetaData$LE_L_Y[RowI]
    
    MetaData$DE_L_X.Pixel[RowI]<- MetaData$DE_L_X[RowI]
    MetaData$DE_L_Y.Pixel[RowI]<- MetaData$DE_L_Y[RowI]
    
  } else if(MetaData$Resolution_Unit[RowI]!="pixels") { ## If coordinates are NOT in pixel
    ImageResolution<-as.numeric(MetaData$Resolution_Pixels_per_Unit[RowI])
    # If Resolution_Unit is not pixels then we need to convert coordinates in pixels
    MetaData$CC_X.Pixel[RowI]<-MetaData$CC_X[RowI]*ImageResolution
    MetaData$CC_Y.Pixel[RowI]<-MetaData$CC_Y[RowI]*ImageResolution
    
    MetaData$DE_R_X.Pixel[RowI]<-MetaData$DE_R_X[RowI]*ImageResolution
    MetaData$DE_R_Y.Pixel[RowI]<-MetaData$DE_R_Y[RowI]*ImageResolution
    
    MetaData$LE_R_X.Pixel[RowI]<-MetaData$LE_R_X[RowI]*ImageResolution
    MetaData$LE_R_Y.Pixel[RowI]<-MetaData$LE_R_Y[RowI]*ImageResolution
    
    MetaData$VE_R_X.Pixel[RowI]<-MetaData$VE_R_X[RowI]*ImageResolution
    MetaData$VE_R_Y.Pixel[RowI]<-MetaData$VE_R_Y[RowI]*ImageResolution
    
    MetaData$VE_L_X.Pixel[RowI]<-MetaData$VE_L_X[RowI]*ImageResolution
    MetaData$VE_L_Y.Pixel[RowI]<-MetaData$VE_L_Y[RowI]*ImageResolution
    
    MetaData$LE_L_X.Pixel[RowI]<-MetaData$LE_L_X[RowI]*ImageResolution
    MetaData$LE_L_Y.Pixel[RowI]<-MetaData$LE_L_Y[RowI]*ImageResolution
    
    MetaData$DE_L_X.Pixel[RowI]<-MetaData$DE_L_X[RowI]*ImageResolution
    MetaData$DE_L_Y.Pixel[RowI]<-MetaData$DE_L_Y[RowI]*ImageResolution
  } ## End of create Pixel Coordinates
  
  # Center the registration coordinates according to the CC position
  MetaData$CC_X.Pixel.Centered[RowI]<-scale(MetaData$CC_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$DE_R_X.Pixel.Centered[RowI]<-scale(MetaData$DE_R_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$LE_R_X.Pixel.Centered[RowI]<-scale(MetaData$LE_R_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$VE_R_X.Pixel.Centered[RowI]<-scale(MetaData$VE_R_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$VE_L_X.Pixel.Centered[RowI]<-scale(MetaData$VE_L_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$LE_L_X.Pixel.Centered[RowI]<-scale(MetaData$LE_L_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  MetaData$DE_L_X.Pixel.Centered[RowI]<-scale(MetaData$DE_L_X.Pixel[RowI], center=MetaData$CC_X.Pixel[RowI], scale=FALSE)
  
  MetaData$CC_Y.Pixel.Centered[RowI]<-scale(MetaData$CC_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$DE_R_Y.Pixel.Centered[RowI]<-scale(MetaData$DE_R_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$LE_R_Y.Pixel.Centered[RowI]<- 0 ## By default we use the lateral edge a Y=0
  #MetaData$LE_R_Y.Pixel.Centered[RowI]<-scale(MetaData$LE_R_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$VE_R_Y.Pixel.Centered[RowI]<-scale(MetaData$VE_R_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$VE_L_Y.Pixel.Centered[RowI]<-scale(MetaData$VE_L_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$LE_L_Y.Pixel.Centered[RowI]<- 0 ## By default we use the lateral edge a Y=0
  #MetaData$LE_L_Y.Pixel.Centered[RowI]<-scale(MetaData$LE_L_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  MetaData$DE_L_Y.Pixel.Centered[RowI]<-scale(MetaData$DE_L_Y.Pixel[RowI], center=MetaData$CC_Y.Pixel[RowI], scale=FALSE)
  
  
  
  # Scale the Registration coordinates
  MetaData$CC_X.Scaled[RowI]<- scale(MetaData$CC_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$DE_R_X.Pixel.Centered[RowI])
  MetaData$DE_R_X.Scaled[RowI]<- scale(MetaData$DE_R_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_R_X.Pixel.Centered[RowI])
  MetaData$LE_R_X.Scaled[RowI]<- scale(MetaData$LE_R_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_R_X.Pixel.Centered[RowI])
  MetaData$VE_R_X.Scaled[RowI]<- scale(MetaData$VE_R_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_R_X.Pixel.Centered[RowI])
  MetaData$VE_L_X.Scaled[RowI]<- - scale(MetaData$VE_L_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_L_X.Pixel.Centered[RowI])
  MetaData$LE_L_X.Scaled[RowI]<- - scale(MetaData$LE_L_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_L_X.Pixel.Centered[RowI])
  MetaData$DE_L_X.Scaled[RowI]<- - scale(MetaData$DE_L_X.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_L_X.Pixel.Centered[RowI])
  
  MetaData$CC_Y.Scaled[RowI]<- scale(MetaData$CC_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$DE_R_Y.Pixel.Centered[RowI])
  MetaData$DE_R_Y.Scaled[RowI]<- scale(MetaData$DE_R_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$DE_R_Y.Pixel.Centered[RowI])
  MetaData$LE_R_Y.Scaled[RowI]<- 0 # Force the Lateral point to be in the dorsal quadrant
  # MetaData$LE_R_Y.Scaled[RowI]<- scale(MetaData$LE_R_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_R_Y.Pixel.Centered[RowI])
  MetaData$VE_R_Y.Scaled[RowI]<- - scale(MetaData$VE_R_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$VE_R_Y.Pixel.Centered[RowI])
  MetaData$VE_L_Y.Scaled[RowI]<- - scale(MetaData$VE_L_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$VE_L_Y.Pixel.Centered[RowI])
  MetaData$LE_L_Y.Scaled[RowI]<- 0 # Force the Lateral point to be in the dorsal quadrant
  #MetaData$LE_L_Y.Scaled[RowI]<- scale(MetaData$LE_L_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$LE_L_Y.Pixel.Centered[RowI])
  MetaData$DE_L_Y.Scaled[RowI]<- scale(MetaData$DE_L_Y.Pixel.Centered[RowI], center=FALSE, scale=MetaData$DE_L_Y.Pixel.Centered[RowI])
  
  # Extract the Variables from the File name convention
  List_Filename_Variables<- unlist(strsplit(as.character(MetaData$File_ID[RowI]),"_", fixed = TRUE))
  MetaData$Date[RowI]<-List_Filename_Variables[1]
  MetaData$Subject_ID[RowI]<-List_Filename_Variables[2]
  MetaData$Group[RowI]<-List_Filename_Variables[3]
  
  if (length(List_Filename_Variables)>3){
    for (VariableI in 4:length(List_Filename_Variables)){
      MetaData[[paste0("Filename_Variable_", sprintf("%03d", as.numeric(VariableI)))]][RowI]  <- as.character(List_Filename_Variables[VariableI])
    }# Add the Variable from the filename to the Metadata table
  }  # If 
} ## End of for each Row and center and scale the data

write.table(MetaData, file=file.path(OutputDirPath, "MetaData_Coordinates_Processed.csv"), row.names=FALSE, sep = ",")


# Process Data File -------------------------------------------------------
# Transform File_ID into factor
MergedInputData$File_ID<-as.factor(MergedInputData$File_ID)

# Process each level of File_ID aka process each File separately
for (FileI in 1:nlevels(MergedInputData$File_ID)){
  File_IDI<-levels(MergedInputData$File_ID)[FileI]
  
  InputDataI<-MergedInputData[MergedInputData$File_ID==File_IDI,] # Get the Data of a given Image
  InputDataI$Channel<-as.character(InputDataI$Label) # Copy the Label and create a Channel in case markers are channel specific
  
  #Create the Channel by removing the File_ID from the Label and then removing the .tif: extension if any so the channel will be 1 2 3 etc...
  for(RowI in 1:length(InputDataI$Label)){
    InputDataI$Channel[RowI]<- gsub(as.character(InputDataI$File_ID[RowI]), "", as.character(InputDataI$Label[RowI]))
    InputDataI$Channel[RowI]<- gsub(".tif:", "", as.character(InputDataI$Channel[RowI]))
  }
  
  
  ## Get the registered coordinates from the MetaData
  ## If a perfect match on fileIDs
  #MetaDataI<-MetaData[MetaData$File_ID==File_IDI,]
  ## If a partial match on fileIDs
  MetaDataI<-subset(MetaData, pmatch(MetaData$File_ID, File_IDI)==1)
  
  if(MetaDataI$Resolution_Unit!="pixels") {
    ImageResolutionI<-as.numeric(MetaDataI$Resolution_Pixels_per_Unit)
    InputDataI$X.Pixel<- InputDataI$X * ImageResolutionI
    InputDataI$Y.Pixel<- InputDataI$Y * ImageResolutionI
  } else { ## Else data is already in pixels
    InputDataI$X.Pixel<- InputDataI$X
    InputDataI$Y.Pixel<- InputDataI$Y
  }
  
  # Center the data on the central canal
  InputDataI$X.Pixel.Centered<-scale(InputDataI$X.Pixel, center=MetaDataI$CC_X.Pixel, scale=FALSE)
  InputDataI$Y.Pixel.Centered<-scale(InputDataI$Y.Pixel, center=MetaDataI$CC_Y.Pixel, scale=FALSE)
  
  # Invert the Y because ImageJ gives inverted coordinates
  ##InputDataI$Y.Pixel.Centered <- -InputDataI$Y.Pixel.Centered
  
  # Divide the Data into 4 quadrants Dorso-Ventral Right_Left
  InputDataI_D_R<- InputDataI[InputDataI$X.Pixel.Centered>=0 & InputDataI$Y.Pixel.Centered>=0,]
  InputDataI_V_R<- InputDataI[InputDataI$X.Pixel.Centered>=0 & InputDataI$Y.Pixel.Centered<0,]
  InputDataI_V_L<- InputDataI[InputDataI$X.Pixel.Centered<0 & InputDataI$Y.Pixel.Centered<0,]
  InputDataI_D_L<- InputDataI[InputDataI$X.Pixel.Centered<0 & InputDataI$Y.Pixel.Centered>=0,]
  
  
  # Scale each quadrant
  InputDataI_D_R$X.Scaled<- scale(InputDataI_D_R$X.Pixel.Centered, center=FALSE, scale=MetaDataI$LE_R_X.Pixel.Centered)
  InputDataI_D_R$Y.Scaled<- scale(InputDataI_D_R$Y.Pixel.Centered, center=FALSE, scale=MetaDataI$DE_R_Y.Pixel.Centered)
  
  InputDataI_V_R$X.Scaled<- scale(InputDataI_V_R$X.Pixel.Centered, center=FALSE, scale=MetaDataI$LE_R_X.Pixel.Centered)
  InputDataI_V_R$Y.Scaled<- - scale(InputDataI_V_R$Y.Pixel.Centered, center=FALSE, scale=MetaDataI$VE_R_Y.Pixel.Centered)
  
  InputDataI_V_L$X.Scaled<- - scale(InputDataI_V_L$X.Pixel.Centered, center=FALSE, scale=MetaDataI$LE_L_X.Pixel.Centered)
  InputDataI_V_L$Y.Scaled<- - scale(InputDataI_V_L$Y.Pixel.Centered, center=FALSE, scale=MetaDataI$VE_L_Y.Pixel.Centered)
  
  InputDataI_D_L$X.Scaled<-  - scale(InputDataI_D_L$X.Pixel.Centered, center=FALSE, scale=MetaDataI$LE_L_X.Pixel.Centered)
  InputDataI_D_L$Y.Scaled<- scale(InputDataI_D_L$Y.Pixel.Centered, center=FALSE, scale=MetaDataI$DE_R_Y.Pixel.Centered)
  
  
  # Bind the quadrants back together
  OutputDataI<-rbind(InputDataI_D_R,InputDataI_V_R,InputDataI_V_L,InputDataI_D_L)
  
  ## Add the MetaData to the OutputData
  MissingVars<- setdiff(colnames(MetaDataI), colnames(OutputDataI)) # Get the Missing Columns
 
  if (length(MissingVars)>0){ # Compare the Nb Of Columns if Merge file has more columns
    for (MissingVariableI in 1: length(MissingVars)){
      OutputDataI[[paste0(MissingVars[MissingVariableI])]] <- rep(MetaDataI[[paste0(MissingVars[MissingVariableI])]], dim(OutputDataI)[1])
    } # Add Missing Variables to OutputDataI
  }
  
  # Merge Output Files together
    if(FileI==1){
    OutputData<-OutputDataI
  } else {
    OutputData<-rbind(OutputData, OutputDataI)
  }
    write.table(OutputData, file=file.path(OutputDirPath, "Data_Coordinates_Processed.csv"), row.names=FALSE, sep = ",")
}


# Plot by File ---------------------------------------------------------
# Process each level of File_ID aka process each File separately
for (FileI in 1:nlevels(OutputData$File_ID)){
  File_IDI<-levels(OutputData$File_ID)[FileI]
  OutputDataI<-OutputData[OutputData$File_ID==File_IDI,] # Get the Data of a given Image
  write.table(OutputDataI, file=file.path(OutputDirPath, "Tables by File",paste0(File_IDI,".csv")), row.names=FALSE, sep = ",")
  
  TotalCountOutputDataI<-dim(OutputDataI)[1]
  LeftCountOutputDataI<-dim(OutputDataI[OutputDataI$X.Scaled<0,])[1]
  RightCountOutputDataI<-dim(OutputDataI[OutputDataI$X.Scaled>=0,])[1]
  
  # Plot RAW coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by File", paste0(File_IDI,"_Raw_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X.Pixel.Centered),mean(OutputDataI$LE_R_X.Pixel.Centered),max(abs(OutputDataI$X.Pixel.Centered))))),-1)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y.Pixel.Centered),mean(OutputDataI$DE_R_Y.Pixel.Centered),mean(OutputDataI$VE_L_Y.Pixel.Centered),mean(OutputDataI$VE_R_Y.Pixel.Centered), max(abs(OutputDataI$Y.Pixel.Centered))))),-1)
  plot(OutputDataI$X.Pixel.Centered, OutputDataI$Y.Pixel.Centered,
       pch=16,
       bty="n",
     yaxp=c(-Ylim,Ylim,4),
     xaxp=c(-Xlim,Xlim,4),
      ylim=c(-Ylim,Ylim),
      xlim=c(-Xlim,Xlim),
       type="p", col="deepskyblue",
       main=File_IDI, ylab="Relative position to CC (pixel)", xlab="Relative position to CC (pixel)", lwd=1)
  points(mean(OutputDataI$CC_X.Pixel.Centered),mean(OutputDataI$CC_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$DE_R_X.Pixel.Centered),mean(OutputDataI$DE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$LE_R_X.Pixel.Centered),mean(OutputDataI$LE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$VE_R_X.Pixel.Centered),mean(OutputDataI$VE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$DE_L_X.Pixel.Centered),mean(OutputDataI$DE_L_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$LE_L_X.Pixel.Centered),mean(OutputDataI$LE_L_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$VE_L_X.Pixel.Centered),mean(OutputDataI$VE_L_Y.Pixel.Centered), col="black", pch=3)
  legend("top",
         c("Left ; Total ; Right",paste0(LeftCountOutputDataI," ; ",TotalCountOutputDataI," ; ",RightCountOutputDataI)),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
      dev.off() # Close and save the graph
  
  # Plot scaled coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by File", paste0(File_IDI,"_Scaled_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X.Scaled),mean(OutputDataI$LE_R_X.Scaled),max(abs(OutputDataI$X.Scaled))))),2)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y.Scaled),mean(OutputDataI$DE_R_Y.Scaled),mean(OutputDataI$VE_L_Y.Scaled),mean(OutputDataI$VE_R_Y.Scaled), max(abs(OutputDataI$Y.Scaled))))),2)
  plot(OutputDataI$X.Scaled, OutputDataI$Y.Scaled,
       pch=16,
       bty="n",
       yaxp=c(-Ylim,Ylim,4),
       xaxp=c(-Xlim,Xlim,4),
       ylim=c(-Ylim,Ylim),
       xlim=c(-Xlim,Xlim),
       type="p", col="deepskyblue",
       main=File_IDI, ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)
  points(mean(OutputDataI$CC_X.Scaled),mean(OutputDataI$CC_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$DE_R_X.Scaled),mean(OutputDataI$DE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$LE_R_X.Scaled),mean(OutputDataI$LE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$VE_R_X.Scaled),mean(OutputDataI$VE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$DE_L_X.Scaled),mean(OutputDataI$DE_L_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$LE_L_X.Scaled),mean(OutputDataI$LE_L_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$VE_L_X.Scaled),mean(OutputDataI$VE_L_Y.Scaled), col="black", pch=3)
  legend("top",
         c("Left ; Total ; Right",paste0(LeftCountOutputDataI," ; ",TotalCountOutputDataI," ; ",RightCountOutputDataI)),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  dev.off() # Close and save the graph
}







# Plot By Subject ID ----------------------------------------------------
OutputData$Subject_ID<-as.factor(OutputData$Subject_ID)
for (FileI in 1:nlevels(OutputData$Subject_ID)){
  Animal_IDI<-levels(OutputData$Subject_ID)[FileI]
  OutputDataI<-OutputData[OutputData$Subject_ID==Animal_IDI,] # Get the Data of a given Subject
  write.table(OutputDataI, file=file.path(OutputDirPath, "Tables by Subject",paste0(Animal_IDI,".csv")), row.names=FALSE, sep = ",")
  
  OutputDataI$File_ID<-factor(OutputDataI$File_ID)
  LeftCountsPerImage<-c()
  RightCountsPerImage<-c()
  for(ImageI in 1:length(levels(OutputDataI$File_ID))){
    Image_IDI<-levels(OutputDataI$File_ID)[ImageI]
    DataImageI<-OutputDataI[OutputDataI$File_ID==Image_IDI,]
    LeftDataImageI<-DataImageI[DataImageI$X.Scaled<0,]
    RightDataImageI<-DataImageI[DataImageI$X.Scaled>=0,]
    LeftCountsPerImage<-c(LeftCountsPerImage,dim(LeftDataImageI)[1])
    RightCountsPerImage<-c(RightCountsPerImage,dim(RightDataImageI)[1])
  }
  NbImages<-length(levels(OutputDataI$File_ID))
  TotalCountOutputDataI<-dim(OutputDataI)[1]
  LeftCountOutputDataI<-dim(OutputDataI[OutputDataI$X.Scaled<0,])[1]
  RightCountOutputDataI<-dim(OutputDataI[OutputDataI$X.Scaled>=0,])[1]
  
  
  
  
  # Plot RAW coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", paste0(Animal_IDI,"_Raw_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X.Pixel.Centered),mean(OutputDataI$LE_R_X.Pixel.Centered),max(abs(OutputDataI$X.Pixel.Centered))))),-1)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y.Pixel.Centered),mean(OutputDataI$DE_R_Y.Pixel.Centered),mean(OutputDataI$VE_L_Y.Pixel.Centered),mean(OutputDataI$VE_R_Y.Pixel.Centered), max(abs(OutputDataI$Y.Pixel.Centered))))),-1)
  plot(OutputDataI$X.Pixel.Centered, OutputDataI$Y.Pixel.Centered,
       pch=16,
       bty="n",
       yaxp=c(-Ylim,Ylim,4),
       xaxp=c(-Xlim,Xlim,4),
       ylim=c(-Ylim,Ylim),
       xlim=c(-Xlim,Xlim),
       type="p", col="deepskyblue",
       main=Animal_IDI, ylab="Relative position to CC (pixel)", xlab="Relative position to CC (pixel)", lwd=1)
  points(mean(OutputDataI$CC_X.Pixel.Centered),mean(OutputDataI$CC_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$DE_R_X.Pixel.Centered),mean(OutputDataI$DE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$LE_R_X.Pixel.Centered),mean(OutputDataI$LE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$VE_R_X.Pixel.Centered),mean(OutputDataI$VE_R_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$DE_L_X.Pixel.Centered),mean(OutputDataI$DE_L_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$LE_L_X.Pixel.Centered),mean(OutputDataI$LE_L_Y.Pixel.Centered), col="black", pch=3)
  points(mean(OutputDataI$VE_L_X.Pixel.Centered),mean(OutputDataI$VE_L_Y.Pixel.Centered), col="black", pch=3)
  legend("top",
         c("Left ; Total ; Right",paste0(LeftCountOutputDataI," ; ",TotalCountOutputDataI," ; ",RightCountOutputDataI)),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  legend("topleft",
         c("Mean ; SD ; Nb Images",paste0(signif(mean(LeftCountsPerImage),4)," ; ",signif(sd(LeftCountsPerImage),4)," ; ",length(LeftCountsPerImage))),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  legend("topright",
         c("Mean ; SD ; Nb Images",paste0(signif(mean(RightCountsPerImage),4)," ; ",signif(sd(RightCountsPerImage),4)," ; ",length(RightCountsPerImage))),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  dev.off() # Close and save the graph
  
  # Plot scaled coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", paste0(Animal_IDI,"_Scaled_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X.Scaled),mean(OutputDataI$LE_R_X.Scaled),max(abs(OutputDataI$X.Scaled))))),2)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y.Scaled),mean(OutputDataI$DE_R_Y.Scaled),mean(OutputDataI$VE_L_Y.Scaled),mean(OutputDataI$VE_R_Y.Scaled), max(abs(OutputDataI$Y.Scaled))))),2)
  plot(OutputDataI$X.Scaled, OutputDataI$Y.Scaled,
       pch=16,
       bty="n",
       yaxp=c(-Ylim,Ylim,4),
       xaxp=c(-Xlim,Xlim,4),
       ylim=c(-Ylim,Ylim),
       xlim=c(-Xlim,Xlim),
       type="p", col="deepskyblue",
       main=Animal_IDI, ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)
  points(mean(OutputDataI$CC_X.Scaled),mean(OutputDataI$CC_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$DE_R_X.Scaled),mean(OutputDataI$DE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$LE_R_X.Scaled),mean(OutputDataI$LE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$VE_R_X.Scaled),mean(OutputDataI$VE_R_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$DE_L_X.Scaled),mean(OutputDataI$DE_L_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$LE_L_X.Scaled),mean(OutputDataI$LE_L_Y.Scaled), col="black", pch=3)
  points(mean(OutputDataI$VE_L_X.Scaled),mean(OutputDataI$VE_L_Y.Scaled), col="black", pch=3)
  legend("top",
         c("Left ; Total ; Right",paste0(LeftCountOutputDataI," ; ",TotalCountOutputDataI," ; ",RightCountOutputDataI)),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  legend("topleft",
         c("Mean ; SD ; Nb Images",paste0(signif(mean(LeftCountsPerImage),4)," ; ",signif(sd(LeftCountsPerImage),4)," ; ",length(LeftCountsPerImage))),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
  legend("topright",
         c("Mean ; SD ; Nb Images",paste0(signif(mean(RightCountsPerImage),4)," ; ",signif(sd(RightCountsPerImage),4)," ; ",length(RightCountsPerImage))),
         text.col=c("black"),
         bty="n", xjust=0.5, yjust=0.5,
         cex=0.75) # Add legend
   dev.off() # Close and save the graph
}


# Plot By Group ----------------------------------------------------





OutputData$Group<-as.factor(as.character(OutputData$Group))
OutputData$File_ID<-as.factor(OutputData$File_ID)

# Subset the data into control and test
ControlData<-OutputData[OutputData$Group=="Control",]
write.table(ControlData, file=file.path(OutputDirPath, "Tables","Control Data.csv"), row.names=FALSE, sep = ",")
MetaDataControl<-MetaData[MetaData$Group=="Control",]
SummaryTableControl<-as.data.frame(table(ControlData$X.Scaled))
SummaryTableControl$Var1<- as.numeric(as.character(SummaryTableControl$Var1))
SummaryControlLeft<-SummaryTableControl[SummaryTableControl$Var1<0,]
SummaryControlRight<-SummaryTableControl[SummaryTableControl$Var1>=0,]






TestData<-OutputData[OutputData$Group=="Test",]
write.table(TestData, file=file.path(OutputDirPath, "Tables","Test Data.csv"), row.names=FALSE, sep = ",")
MetaDataTest<-MetaData[MetaData$Group=="Test",]

SummaryTableTest<-as.data.frame(table(TestData$X.Scaled))
SummaryTableTest$Var1<- as.numeric(as.character(SummaryTableTest$Var1))
SummaryTestLeft<-SummaryTableTest[SummaryTableTest$Var1<0,]
SummaryTestRight<-SummaryTableTest[SummaryTableTest$Var1>=0,]



cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Control_Pixel_Centered_Graph.pdf"))) # Open the graph as pdf
plot(ControlData$X.Pixel.Centered, ControlData$Y.Pixel.Centered,
     pch=1, cex=0.5,
     bty="n",
     #yaxt="n",
     #xaxp=c(0,600,3),
     ylim=c(-2000,2000),
     xlim=c(-2000,2000),
     type="p", col="deepskyblue",
     main="Control_Centered_Graph", ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)

legend("topleft",
       paste0("Total Left: ",round(sum(SummaryControlLeft$Freq))),
       text.col=c("blue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
legend("topright",
       paste0("Total Right: ",round(sum(SummaryControlRight$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend

points(MetaDataControl$CC_X.Pixel.Centered,MetaDataControl$CC_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$DE_R_X.Pixel.Centered,MetaDataControl$DE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$LE_R_X.Pixel.Centered,MetaDataControl$LE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$VE_R_X.Pixel.Centered,MetaDataControl$VE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$DE_L_X.Pixel.Centered,MetaDataControl$DE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$LE_L_X.Pixel.Centered,MetaDataControl$LE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataControl$VE_L_X.Pixel.Centered,MetaDataControl$VE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)

dev.off() # Close and save the graph









cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Control_Scaled_Graph.pdf"))) # Open the graph as pdf
plot(ControlData$X.Scaled, ControlData$Y.Scaled,
     pch=1, cex=0.5,
     bty="n",
     #yaxt="n",
     #xaxp=c(0,600,3),
     ylim=c(-1,1),
     xlim=c(-1,1),
     type="p", col="deepskyblue",
     main="Control_Scaled_Graph", ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)

legend("topleft",
       paste0("Total Left: ",round(sum(SummaryControlLeft$Freq))),
       text.col=c("blue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
legend("topright",
       paste0("Total Right: ",round(sum(SummaryControlRight$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
points(MetaDataControl$CC_X.Scaled,MetaDataControl$CC_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$DE_R_X.Scaled,MetaDataControl$DE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$LE_R_X.Scaled,MetaDataControl$LE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$VE_R_X.Scaled,MetaDataControl$VE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$DE_L_X.Scaled,MetaDataControl$DE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$LE_L_X.Scaled,MetaDataControl$LE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataControl$VE_L_X.Scaled,MetaDataControl$VE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(mean(ControlData$X.Scaled[ControlData$X.Scaled<0]),mean(ControlData$Y.Scaled[ControlData$X.Scaled<0]) , col="deepskyblue4", pch=3, cex=1, lwd=2)
points(mean(ControlData$X.Scaled[ControlData$X.Scaled>0]),mean(ControlData$Y.Scaled[ControlData$X.Scaled>=0]) , col="deepskyblue4", pch=3, cex=1, lwd=2)

dev.off() # Close and save the graph






cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Test_Pixel_Centered_Graph.pdf"))) # Open the graph as pdf
plot(TestData$X.Pixel.Centered, TestData$Y.Pixel.Centered,
     pch=1, cex=0.5,
     bty="n",
     #yaxt="n",
     #xaxp=c(0,600,3),
     ylim=c(-2000,2000),
     xlim=c(-2000,2000),
     type="p", col="red",
     main="Test_Centered_Graph", ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)


legend("topleft",
       paste0("Total Left: ",round(sum(SummaryTestLeft$Freq))),
       text.col=c("blue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
legend("topright",
       paste0("Total Right: ",round(sum(SummaryTestRight$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend


points(MetaDataTest$CC_X.Pixel.Centered,MetaDataTest$CC_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$DE_R_X.Pixel.Centered,MetaDataTest$DE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$LE_R_X.Pixel.Centered,MetaDataTest$LE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$VE_R_X.Pixel.Centered,MetaDataTest$VE_R_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$DE_L_X.Pixel.Centered,MetaDataTest$DE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$LE_L_X.Pixel.Centered,MetaDataTest$LE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)
points(MetaDataTest$VE_L_X.Pixel.Centered,MetaDataTest$VE_L_Y.Pixel.Centered, col="black", pch=3, cex=0.5)



dev.off() # Close and save the graph





cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Test_Scaled_Graph.pdf"))) # Open the graph as pdf
plot(TestData$X.Scaled, TestData$Y.Scaled,
     pch=1, cex=0.5,
     bty="n",
     #yaxt="n",
     #xaxp=c(0,600,3),
     ylim=c(-1,1),
     xlim=c(-1,1),
     type="p", col="red",
     main="Test_Scaled_Graph", ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)


legend("topleft",
       paste0("Total Left: ",round(sum(SummaryTestLeft$Freq))),
       text.col=c("blue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
legend("topright",
       paste0("Total Right: ",round(sum(SummaryTestRight$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
points(MetaDataTest$CC_X.Scaled,MetaDataTest$CC_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$DE_R_X.Scaled,MetaDataTest$DE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$LE_R_X.Scaled,MetaDataTest$LE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$VE_R_X.Scaled,MetaDataTest$VE_R_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$DE_L_X.Scaled,MetaDataTest$DE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$LE_L_X.Scaled,MetaDataTest$LE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(MetaDataTest$VE_L_X.Scaled,MetaDataTest$VE_L_Y.Scaled, col="black", pch=3, cex=0.5)
points(mean(TestData$X.Scaled[TestData$X.Scaled<0]),mean(TestData$Y.Scaled[TestData$X.Scaled<0]) , col="red4", pch=3, cex=1, lwd=2)
points(mean(TestData$X.Scaled[TestData$X.Scaled>0]),mean(TestData$Y.Scaled[TestData$X.Scaled>=0]) , col="red4", pch=3, cex=1, lwd=2)

dev.off() # Close and save the graph





NbOfBin=100



# Use Bandwidth.nrd as a default
Density.Total<-kde2d(OutputData$X.Scaled, OutputData$Y.Scaled, h=c(bandwidth.nrd(OutputData$X.Scaled),bandwidth.nrd(OutputData$Y.Scaled)), n=NbOfBin, lims=c(-1.2,1.2,-1.2,1.2))
write.table(Density.Total, file=file.path(OutputDirPath, "Tables","Density_Total.csv"), row.names=FALSE, sep = ",")
#Alternative use width.SJ() for defining the h
#Density.Total.ScaledSJ<-kde2d(OutputData$X.Scaled, OutputData$Y.Scaled, h=c(width.SJ(OutputData$X.Scaled),width.SJ(OutputData$Y.Scaled)), n=100, lims=c(-1.2,1.2,-1.2,1.2))

Density.Total.Norm<-Density.Total
Density.Total.Norm$z<- round(  100*scale(  Density.Total$z, center=FALSE, scale=rep(      max(        Density.Total$z        ), dim(Density.Total$z)[2]      )    )   )
write.table(Density.Total.Norm, file=file.path(OutputDirPath, "Tables","Density_Total_Norm.csv"), row.names=FALSE, sep = ",")
cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Density_Total_Norm.pdf"))) # Open the graph as pdf
filled.contour(Density.Total.Norm, main="Density.Total.Norm",
               levels=seq(0,max(c(Density.Total.Norm$z)), by=2),
               col= c("white", heat.colors(length(seq(0,max(c(Density.Total.Norm$z)), by=2))-1)) ,
               frame.plot=FALSE)
dev.off()

Density.Control<-kde2d(ControlData$X.Scaled, ControlData$Y.Scaled, h=c(bandwidth.nrd(ControlData$X.Scaled),bandwidth.nrd(ControlData$Y.Scaled)), n=NbOfBin, lims=c(-1.2,1.2,-1.2,1.2))
write.table(Density.Control, file=file.path(OutputDirPath, "Tables","Density_Control.csv"), row.names=FALSE, sep = ",")

Density.Control.Norm<-Density.Control
Density.Control.Norm$z<- round(  100*scale(  Density.Control$z, center=FALSE, scale=rep(      max(        Density.Control$z        ), dim(Density.Control$z)[2]      )    )   )
write.table(Density.Control.Norm, file=file.path(OutputDirPath, "Tables","Density_Control_Norm.csv"), row.names=FALSE, sep = ",")

cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Density_Control_Norm.pdf"))) # Open the graph as pdf
filled.contour(Density.Control.Norm, main="Density.Control.Norm",
               levels=seq(0,max(c(Density.Control.Norm$z)), by=2),
               col= c("white", heat.colors(length(seq(0,max(c(Density.Control.Norm$z)), by=2))-1)) ,
               frame.plot=FALSE)
dev.off()

Density.Test<-kde2d(TestData$X.Scaled, TestData$Y.Scaled, h=c(bandwidth.nrd(TestData$X.Scaled),bandwidth.nrd(TestData$Y.Scaled)), n=NbOfBin, lims=c(-1.2,1.2,-1.2,1.2))
write.table(Density.Test, file=file.path(OutputDirPath, "Tables","Density_Test.csv"), row.names=FALSE, sep = ",")

Density.Test.Norm<-Density.Test
Density.Test.Norm$z<- round(  100*scale(  Density.Test$z, center=FALSE, scale=rep(      max(        Density.Test$z        ), dim(Density.Test$z)[2]      )    )   )
write.table(Density.Test.Norm, file=file.path(OutputDirPath, "Tables","Density_Test_Norm.csv"), row.names=FALSE, sep = ",")
cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Density_Test_Norm.pdf"))) # Open the graph as pdf
filled.contour(Density.Test.Norm, main="Density.Test.Norm",
               levels=seq(0,max(c(Density.Test.Norm$z)), by=2),
               col= c("white", heat.colors(length(seq(0,max(c(Density.Test.Norm$z)), by=2))-1)) ,
               frame.plot=FALSE)
dev.off()


#PLOT WEIGHTED DATA
Density.Control.Weighted<-Density.Control.Norm
Density.Control.Weighted$z<-Density.Control.Norm$z * (dim(ControlData)[1])/100
write.table(Density.Control.Weighted, file=file.path(OutputDirPath, "Tables","Density_Control_Weighted.csv"), row.names=FALSE, sep = ",")
Density.Test.Weighted<-Density.Test.Norm
Density.Test.Weighted$z<-Density.Test.Norm$z * (dim(TestData)[1])/100
write.table(Density.Test.Weighted, file=file.path(OutputDirPath, "Tables","Density_Test_Weighted.csv"), row.names=FALSE, sep = ",")

cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Density_Control_Weighted.pdf"))) # Open the graph as pdf
filled.contour(Density.Control.Weighted,
               col= c("white", heat.colors(length(seq(0,max(c(Density.Control.Weighted$z, Density.Test.Weighted$z)), by=20))-1)) ,
               main="Density.Control.Weighted",
               zlim=c(0,max(c(Density.Control.Weighted$z, Density.Test.Weighted$z))),
               levels=seq(0,max(c(Density.Control.Weighted$z,  Density.Test.Weighted$z)), by=20),frame.plot=FALSE)
dev.off()




cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Density_Test_Weighted.pdf"))) # Open the graph as pdf
filled.contour(Density.Test.Weighted,
               col= c("white", heat.colors(length(seq(0,max(c(Density.Control.Weighted$z, Density.Test.Weighted$z)), by=20))-1)) ,
               main="Density.Test.Weighted",
               zlim=c(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z))),
               levels=seq(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z)), by=20),frame.plot=FALSE)

dev.off()




cairo_pdf(file.path(OutputDirPath, "Graphs", paste0("Contour_Density_Control_vs_Test.pdf"))) # Open the graph as pdf
contour(Density.Control.Weighted, col="deepskyblue",labcex =0.5, drawlabels=TRUE, method="edge", zlim=c(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z))), levels=seq(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z)), by=50),frame.plot=FALSE, lty="solid",lwd=1)
contour(Density.Test.Weighted, col="red",add = TRUE, zlim=c(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z))),levels=seq(0,max(c(Density.Control.Weighted$z,Density.Test.Weighted$z)), by=50),
        labels=NULL, labcex =0.5, drawlabels=TRUE, method="edge", lty="solid", lwd=1)
title("Space Normalized density of cFos expressing cells following\nunilateral (Right hindpaw) Capsaicin injection", font = 1)

legend("topleft",
       paste0("Total Left"),
       text.col=c("black"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
legend("topleft",
       paste0("\nControl ",round(sum(SummaryControlLeft$Freq))),
       text.col=c("deepskyblue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend

legend("topleft",
       paste0("\n \nTest ",round(sum(SummaryTestLeft$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend


legend("topright",
       paste0("Total Right"),
       text.col=c("black"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend

legend("topright",
       paste0("\nControl ",round(sum(SummaryControlRight$Freq))),
       text.col=c("deepskyblue"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend

legend("topright",
       paste0("\n \nTest ",round(sum(SummaryTestRight$Freq))),
       text.col=c("red"),
       inset = .0,
       bty="n",
       cex=0.75,
       y.intersp=1.4) # Add legend
abline(v=0, col="darkgrey")
abline(h=0, col="darkgrey")

points(mean(ControlData$X.Scaled[ControlData$X.Scaled<0]),mean(ControlData$Y.Scaled[ControlData$X.Scaled<0]) , col="deepskyblue4", pch=3, cex=1, lwd=2)
points(mean(ControlData$X.Scaled[ControlData$X.Scaled>0]),mean(ControlData$Y.Scaled[ControlData$X.Scaled>=0]) , col="deepskyblue4", pch=3, cex=1, lwd=2)

points(mean(TestData$X.Scaled[TestData$X.Scaled<0]),mean(TestData$Y.Scaled[TestData$X.Scaled<0]) , col="red4", pch=3, cex=1, lwd=2)
points(mean(TestData$X.Scaled[TestData$X.Scaled>0]),mean(TestData$Y.Scaled[TestData$X.Scaled>=0]) , col="red4", pch=3, cex=1, lwd=2)

dev.off()


hist(ControlData$X.Scaled[ControlData$X.Scaled>0])
abline(v=mean(ControlData$X.Scaled[ControlData$X.Scaled>0]),col="red")
abline(v=mean(ControlData$X.Scaled[ControlData$X.Scaled>0])-2*sd(ControlData$X.Scaled[ControlData$X.Scaled>0]),col="grey")
abline(v=mean(ControlData$X.Scaled[ControlData$X.Scaled>0])+2*sd(ControlData$X.Scaled[ControlData$X.Scaled>0]),col="grey")

# To compare the distribution use ks.test()
# To test for normal distribution shapiro.test() if p value >0.05 distribution is NOT normal

data1<-TestData$X.Scaled[TestData$X.Scaled>0]
data2<-TestData$Y.Scaled[TestData$X.Scaled>0]
data3<-ControlData$X.Scaled[ControlData$X.Scaled>0]
data4<-ControlData$Y.Scaled[ControlData$X.Scaled>0]
data<-data1

boxplot(data)
var(data)
sd(data)



if( shapiro.test(ControlData$Y.Scaled[ControlData$X.Scaled>0])$p.value>0.05){
  print("Data is not normally distruted")
}
shapiro.test(TestData$X.Scaled[TestData$X.Scaled<0])

hist(ControlData$X.Scaled[ControlData$X.Scaled>=0])

hist(ControlData$Y.Scaled[ControlData$X.Scaled>0])

summary(ControlData$X.Scaled[ControlData$X.Scaled>0])
sd(ControlData$X.Scaled[ControlData$X.Scaled>0])
summary(ControlData$Y.Scaled[ControlData$X.Scaled>0])
sd(ControlData$Y.Scaled[ControlData$X.Scaled>0])
summary(TestData$X.Scaled[TestData$X.Scaled>0])
sd(TestData$X.Scaled[TestData$X.Scaled>0])
summary(TestData$Y.Scaled[TestData$X.Scaled>0])
sd(TestData$Y.Scaled[TestData$X.Scaled>0])

### Do the Hoteling test

## Remove constant variable
# To be used as Dataframe<-Dataframe[,-RemoveConstantVariable.Function(Dataframe)]

Density.Control.Weighted.Unique<- data.frame(Density.Control.Weighted$z)
Density.Control.Weighted.Unique<- Density.Control.Weighted.Unique[,-RemoveConstantVariable.Function(Density.Control.Weighted.Unique)]

Density.Test.Weighted.Unique<- data.frame(Density.Test.Weighted$z)
Density.Test.Weighted.Unique<- Density.Test.Weighted.Unique[,-RemoveConstantVariable.Function(Density.Test.Weighted.Unique)]

fit = hotelling.test(Density.Control.Weighted.Unique, Density.Test.Weighted.Unique)

JustScaledCoordinates<-data.frame("DUMMY"=1:dim(OutputData)[1])
JustScaledCoordinates$DUMMY<-NULL
JustScaledCoordinates$Group<-OutputData$Group
JustScaledCoordinates$X.Scaled<-OutputData$X.Scaled
JustScaledCoordinates$Y.Scaled<-OutputData$Y.Scaled

JustScaledCoordinatesLeft<-JustScaledCoordinates[JustScaledCoordinates$X.Scaled<0,]
JustScaledCoordinatesRight<-JustScaledCoordinates[JustScaledCoordinates$X.Scaled>=0,]

JustScaledCoordinatesLeftFlippedtoRight<-JustScaledCoordinatesLeft
JustScaledCoordinatesLeftFlippedtoRight$X.Scaled<- - JustScaledCoordinatesLeftFlippedtoRight$X.Scaled

ResultControlLeftvsRight=hotelling.test(JustScaledCoordinatesLeftFlippedtoRight[JustScaledCoordinatesLeftFlippedtoRight$Group=="Control",-1], JustScaledCoordinatesRight[JustScaledCoordinatesRight$Group=="Control",-1], perm = TRUE, B=500,progBar = TRUE)
ResultControlLeftvsRight

ResultTestLeftvsRight=hotelling.test(JustScaledCoordinatesLeftFlippedtoRight[JustScaledCoordinatesLeftFlippedtoRight$Group=="Test",-1], JustScaledCoordinatesRight[JustScaledCoordinatesRight$Group=="Test",-1], perm = TRUE, B=500,progBar = TRUE)
ResultTestLeftvsRight

ResultLeftControlvsTest=hotelling.test(JustScaledCoordinatesLeft[JustScaledCoordinatesLeft$Group=="Control",-1], JustScaledCoordinatesLeft[JustScaledCoordinatesLeft$Group=="Test",-1], perm = TRUE, B=10000,progBar = TRUE)
ResultLeftControlvsTest

ResultRightControlvsTest=hotelling.test(JustScaledCoordinatesRight[JustScaledCoordinatesRight$Group=="Control",-1], JustScaledCoordinatesRight[JustScaledCoordinatesRight$Group=="Test",-1], perm = TRUE, B=500,progBar = TRUE)
ResultRightControlvsTest


Test=hotelling.test(JustScaledCoordinates[JustScaledCoordinates$Group=="Control",-1], JustScaledCoordinates[JustScaledCoordinates$Group=="Test",-1], perm = TRUE ,progBar = TRUE,B=1000, shrinkage = TRUE)
Test$results
