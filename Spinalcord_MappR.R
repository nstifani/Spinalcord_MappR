## Density Plot data from Spinalcord Mapper
## Script written by Nicolas Stifani contact nstifani@gmail.com
## Note: This Plugin Assume that Y coordinates are Inverted
## Manual CellCounter does not report "inverted Y". So the X=0 Y=0 is the top left corner.
## To Invert Non-Inverted Y Coordinates one must take the absolute of the Y Value minus Maximum Y value
## Y2=abs(Y-Max(Y))
## Requires Ghosscript brew install ghostscript
## The SC Layout is generated from a AI file to generate a postscript XML 
## PostScriptTrace("Mouse_SC_L4_Normalized.ai")


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

# Function to select a directory containing  file with only one . in their file name and the FileExt as a file extension
SelectInputDir.Function <- function(DialogMessage, DirPathObjectName, DirNameObjectName, ListFilePathObjectName, ParentDirPathObjectName, FileEXT){
  InputDirPass=0
  
  while(InputDirPass!=1){
    DirPath<-tk_choose.dir(default=getwd(), caption=DialogMessage) # Prompt the user to select an inputdirectory
    DirName<-basename(DirPath) # Defines Name of Input directory
    ListFilePath<-list.files(path=DirPath, pattern=paste0(".",FileEXT), all.files=FALSE, full.names=TRUE, ignore.case = TRUE) # Get the list of TXT filepath within InputDir
    
    if(length(ListFilePath)==0){
      ErrorMessageInputFile=paste0("Sorry, the folder ",DirName," does not contain any ", FileEXT," file.")
      FixMessageInputFile=paste0("Please select a folder containing at least one ",FileEXT," file.")
      ErrorDialog.Function(ErrorMessage=ErrorMessageInputFile, FixMessage=FixMessageInputFile)
      NbFilePass=0
    } else {
      NbFilePass=1
    }
    if(length(ListFilePath)>0){
      FilenamePass=1
      for(FileI in 1:length(ListFilePath)){ ## Screen all files to make sure they have only one .
        FilePathFileI <- ListFilePath[FileI]
        FilenameFileI <- basename(FilePathFileI)
        FilenameFileIComponents <- unlist(strsplit(as.character(FilenameFileI),".", fixed=TRUE))
        if(length(FilenameFileIComponents)!=2){ # If more than one . make an error
          FilenamePass=0
          ErrorMessageInputFile=paste0("Sorry, the file ",FilenameFileI," contains more than one \".\" character.")
          FixMessageInputFile="Please ensure that all Files contain only one \".\" for the file extension."
          ErrorDialog.Function(ErrorMessage=ErrorMessageInputFile, FixMessage=FixMessageInputFile)
        }
      }
    }
    if(NbFilePass==1 && FilenamePass==1){
      InputDirPass=1
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

## Function to Merge files from the Select Input Directory
MergeInputFile.Function <- function(ListFilePath, MergedObjectName){
  for (FileI in 1:length(ListFilePath)){
    FilePathI <- ListFilePath[FileI] # Defines the Path of the File to be processed
    FilenameI <- basename(FilePathI) # Get the Filename of the File being processed
    FilenameICompoments <- unlist(strsplit(as.character(FilenameI),".", fixed=TRUE))
    FilenameINoExt<-FilenameICompoments[1]
    #  FilenameINoExt <- gsub(".txt","", FilenameI, ignore.case = TRUE) # Create a filename without extension
    DataI <- read.table(FilePathI, sep = "\t", header = TRUE, nrows = 100000)
    DataI$File_ID<-rep(FilenameINoExt, dim(DataI)[1])
    if(FileI==1){
      MergedData<-DataI
    } else {
      MergedData<-rbind(MergedData, DataI)
    }
  }
  assign(MergedObjectName, MergedData, envir=.GlobalEnv)
}

# Function to Select a given File with FileExt
SelectFile.Function <- function(DialogMessage, DataObjectName, FileExt){
 # DataFilePath<-tk_choose.files(default = getwd(), caption = DialogMessage, filters=matrix(c(paste0(FileExt," File"),paste0(".",FileExt)),1,2, byrow=TRUE), multi=FALSE)
  DataFilePath<-tk_choose.files(default = getwd(), caption = DialogMessage, multi=FALSE)
  Data<-read.table(DataFilePath, header=TRUE, sep = "\t", colClasses = "character")
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

## Create a Color Palette From Blue to Red
BlueToRedPalette<-function(NbOfColor, Transparency){
  rev(rainbow(n=NbOfColor, s=1, v=1, start=0, end=4/6, alpha=Transparency))}


# HouseKeeping ------------------------------------------------------------
# Install Required Packages
ListRequiredPackage=c("zoo", "tcltk", "MASS", "Hotelling","ggplot2", "car", "grImport")
InstallRequiredPackage.Function(ListPackage=ListRequiredPackage)


# Select and Read the Data ------------------------------------------------------------
# Select the Input Data and Get list of Data files
SelectInputDir.Function(DialogMessage = "Choose the folder containing the Data Files.",
                      DirPathObjectName="InputDirPath",
                      DirNameObjectName="InputDirName",
                      ListFilePathObjectName="ListInputFilePath",
                      ParentDirPathObjectName="ParentInputDirPath",
                      FileEXT="TXT"
)
setwd(InputDirPath)
# Merge all Input data Files into one
MergeInputFile.Function(ListFilePath=ListInputFilePath, MergedObjectName="MergedInputData")


# Select the RegistrationData and Registrtion coordinates
SelectFile.Function(DialogMessage="Select the Registration Coordinates File", DataObjectName = "RegistrationData", FileExt="TXT")


# Create OuputDirectory and Subdirectory
CreateOutputDir.Function(OutputDirLocation=ParentInputDirPath, 
                         OutputDirName=paste0(InputDirName,"_Coordinates_Processed"),
                         SubDirList=c("Graphs by File", "Tables by File","Graphs by Subject", "Tables by Subject","Graphs by Group", "Tables by Group") )

# Select the XML SC Layout
#Create a XML Layout from a Vector File
#SC_Layout_Vector_File<-file.choose("Select the file of the SC Layout")
# Create the 
#setwd(dirname(SC_Layout_Vector_File))
#PostScriptTrace(SC_Layout_Vector_File)

SC_Layout_File<-file.choose("Select the XML file of the SC Layout")
SpinalCordLayout<-readPicture(SC_Layout_File)

# Pre-Process RegistrationData  ---------------------------------------------------
# Center the RegistrationData for each Row according to the CC position
# If coordinates are already in pixel just copy them
# If coordinates are in scaled distance convert them to pixels
for(RowI in 1:dim(RegistrationData)[1]){
  if(RegistrationData$Resolution_Unit[RowI]=="pixels") {  
    RegistrationData$CC_X_Pixel[RowI]<- as.numeric(RegistrationData$CC_X[RowI])
    RegistrationData$CC_Y_Pixel[RowI]<- as.numeric(RegistrationData$CC_Y[RowI])
    
    RegistrationData$DE_R_X_Pixel[RowI]<- as.numeric(RegistrationData$DE_R_X[RowI])
    RegistrationData$DE_R_Y_Pixel[RowI]<- as.numeric(RegistrationData$DE_R_Y[RowI])
    
    RegistrationData$LE_R_X_Pixel[RowI]<- as.numeric(RegistrationData$LE_R_X[RowI])
    RegistrationData$LE_R_Y_Pixel[RowI]<- as.numeric(RegistrationData$LE_R_Y[RowI])
    
    RegistrationData$VE_R_X_Pixel[RowI]<- as.numeric(RegistrationData$VE_R_X[RowI])
    RegistrationData$VE_R_Y_Pixel[RowI]<- as.numeric(RegistrationData$VE_R_Y[RowI])
    
    RegistrationData$VE_L_X_Pixel[RowI]<- as.numeric(RegistrationData$VE_L_X[RowI])
    RegistrationData$VE_L_Y_Pixel[RowI]<- as.numeric(RegistrationData$VE_L_Y[RowI])
    
    RegistrationData$LE_L_X_Pixel[RowI]<- as.numeric(RegistrationData$LE_L_X[RowI])
    RegistrationData$LE_L_Y_Pixel[RowI]<- as.numeric(RegistrationData$LE_L_Y[RowI])
    
    RegistrationData$DE_L_X_Pixel[RowI]<- as.numeric(RegistrationData$DE_L_X[RowI])
    RegistrationData$DE_L_Y_Pixel[RowI]<- as.numeric(RegistrationData$DE_L_Y[RowI])
    
  } else if(RegistrationData$Resolution_Unit[RowI]!="pixels") { ## If coordinates are NOT in pixel
    ImageResolution<-as.numeric(RegistrationData$Resolution_Pixels_per_Unit[RowI])
    # If Resolution_Unit is not pixels then we need to convert coordinates in pixels
    RegistrationData$CC_X_Pixel[RowI]<-as.numeric(RegistrationData$CC_X[RowI])*ImageResolution
    RegistrationData$CC_Y_Pixel[RowI]<-as.numeric(RegistrationData$CC_Y[RowI])*ImageResolution
    
    RegistrationData$DE_R_X_Pixel[RowI]<-as.numeric(RegistrationData$DE_R_X[RowI])*ImageResolution
    RegistrationData$DE_R_Y_Pixel[RowI]<-as.numeric(RegistrationData$DE_R_Y[RowI])*ImageResolution
    
    RegistrationData$LE_R_X_Pixel[RowI]<-as.numeric(RegistrationData$LE_R_X[RowI])*ImageResolution
    RegistrationData$LE_R_Y_Pixel[RowI]<-as.numeric(RegistrationData$LE_R_Y[RowI])*ImageResolution
    
    RegistrationData$VE_R_X_Pixel[RowI]<-as.numeric(RegistrationData$VE_R_X[RowI])*ImageResolution
    RegistrationData$VE_R_Y_Pixel[RowI]<-as.numeric(RegistrationData$VE_R_Y[RowI])*ImageResolution
    
    RegistrationData$VE_L_X_Pixel[RowI]<-as.numeric(RegistrationData$VE_L_X[RowI])*ImageResolution
    RegistrationData$VE_L_Y_Pixel[RowI]<-as.numeric(RegistrationData$VE_L_Y[RowI])*ImageResolution
    
    RegistrationData$LE_L_X_Pixel[RowI]<-as.numeric(RegistrationData$LE_L_X[RowI])*ImageResolution
    RegistrationData$LE_L_Y_Pixel[RowI]<-as.numeric(RegistrationData$LE_L_Y[RowI])*ImageResolution
    
    RegistrationData$DE_L_X_Pixel[RowI]<-as.numeric(RegistrationData$DE_L_X[RowI])*ImageResolution
    RegistrationData$DE_L_Y_Pixel[RowI]<-as.numeric(RegistrationData$DE_L_Y[RowI])*ImageResolution
  } ## End of create Pixel Coordinates
  
  # Center the registration coordinates according to the CC position
  RegistrationData$CC_X_Pixel_Centered[RowI]<-scale(RegistrationData$CC_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$DE_R_X_Pixel_Centered[RowI]<-scale(RegistrationData$DE_R_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$LE_R_X_Pixel_Centered[RowI]<-scale(RegistrationData$LE_R_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$VE_R_X_Pixel_Centered[RowI]<-scale(RegistrationData$VE_R_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$VE_L_X_Pixel_Centered[RowI]<-scale(RegistrationData$VE_L_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$LE_L_X_Pixel_Centered[RowI]<-scale(RegistrationData$LE_L_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  RegistrationData$DE_L_X_Pixel_Centered[RowI]<-scale(RegistrationData$DE_L_X_Pixel[RowI], center=RegistrationData$CC_X_Pixel[RowI], scale=FALSE)
  
  RegistrationData$CC_Y_Pixel_Centered[RowI]<-scale(RegistrationData$CC_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$DE_R_Y_Pixel_Centered[RowI]<-scale(RegistrationData$DE_R_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$LE_R_Y_Pixel_Centered[RowI]<- 0 ## By default we use the lateral edge a Y=0
  #RegistrationData$LE_R_Y_Pixel_Centered[RowI]<-scale(RegistrationData$LE_R_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$VE_R_Y_Pixel_Centered[RowI]<-scale(RegistrationData$VE_R_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$VE_L_Y_Pixel_Centered[RowI]<-scale(RegistrationData$VE_L_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$LE_L_Y_Pixel_Centered[RowI]<- 0 ## By default we use the lateral edge a Y=0
  #RegistrationData$LE_L_Y_Pixel_Centered[RowI]<-scale(RegistrationData$LE_L_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  RegistrationData$DE_L_Y_Pixel_Centered[RowI]<-scale(RegistrationData$DE_L_Y_Pixel[RowI], center=RegistrationData$CC_Y_Pixel[RowI], scale=FALSE)
  
  
  
  # Scale the Registration coordinates
  RegistrationData$CC_X_Scaled[RowI]<- scale(RegistrationData$CC_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$DE_R_X_Pixel_Centered[RowI])
  RegistrationData$DE_R_X_Scaled[RowI]<- scale(RegistrationData$DE_R_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_R_X_Pixel_Centered[RowI])
  RegistrationData$LE_R_X_Scaled[RowI]<- scale(RegistrationData$LE_R_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_R_X_Pixel_Centered[RowI])
  RegistrationData$VE_R_X_Scaled[RowI]<- scale(RegistrationData$VE_R_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_R_X_Pixel_Centered[RowI])
  RegistrationData$VE_L_X_Scaled[RowI]<- - scale(RegistrationData$VE_L_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_L_X_Pixel_Centered[RowI])
  RegistrationData$LE_L_X_Scaled[RowI]<- - scale(RegistrationData$LE_L_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_L_X_Pixel_Centered[RowI])
  RegistrationData$DE_L_X_Scaled[RowI]<- - scale(RegistrationData$DE_L_X_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_L_X_Pixel_Centered[RowI])
  
  RegistrationData$CC_Y_Scaled[RowI]<- scale(RegistrationData$CC_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$DE_R_Y_Pixel_Centered[RowI])
  RegistrationData$DE_R_Y_Scaled[RowI]<- scale(RegistrationData$DE_R_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$DE_R_Y_Pixel_Centered[RowI])
  RegistrationData$LE_R_Y_Scaled[RowI]<- 0 # Force the Lateral point to be in the dorsal quadrant
  # RegistrationData$LE_R_Y_Scaled[RowI]<- scale(RegistrationData$LE_R_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_R_Y_Pixel_Centered[RowI])
  RegistrationData$VE_R_Y_Scaled[RowI]<- - scale(RegistrationData$VE_R_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$VE_R_Y_Pixel_Centered[RowI])
  RegistrationData$VE_L_Y_Scaled[RowI]<- - scale(RegistrationData$VE_L_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$VE_L_Y_Pixel_Centered[RowI])
  RegistrationData$LE_L_Y_Scaled[RowI]<- 0 # Force the Lateral point to be in the dorsal quadrant
  #RegistrationData$LE_L_Y_Scaled[RowI]<- scale(RegistrationData$LE_L_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$LE_L_Y_Pixel_Centered[RowI])
  RegistrationData$DE_L_Y_Scaled[RowI]<- scale(RegistrationData$DE_L_Y_Pixel_Centered[RowI], center=FALSE, scale=RegistrationData$DE_L_Y_Pixel_Centered[RowI])
  
  # Extract the Variables from the File name convention
  List_Filename_Variables<- unlist(strsplit(RegistrationData$File_ID[RowI],"_", fixed = TRUE))
  RegistrationData$Date[RowI]<-List_Filename_Variables[1]
  RegistrationData$Subject_ID[RowI]<-List_Filename_Variables[2]
  RegistrationData$Group[RowI]<-List_Filename_Variables[3]
  
  if (length(List_Filename_Variables)>3){
    for (VariableI in 4:length(List_Filename_Variables)){
      RegistrationData[[paste0("Filename_Variable_", sprintf("%03d", as.numeric(VariableI)))]][RowI]  <- as.character(List_Filename_Variables[VariableI])
    }# Add the Variable from the filename to the RegistrationData table
  }  # If 
} ## End of for each Row and center and scale the data

# Pre-Process the Data ----------------------------------------------------

# Transform File_ID into factor
MergedInputData$File_ID<-factor(MergedInputData$File_ID)

## Three cases: when Channel is present, Channel is absent and Counter is present OR absent
if((any(colnames(MergedInputData)=="Channel")==TRUE )) { # Marker Name is factor of Channel
  MergedInputData$Marker_Name<-as.factor(as.character(MergedInputData$Channel))
  MergedInputData$Marker_Name_x_Channel<- MergedInputData$Marker_Name
} else { # Channel is absent
  if((any(colnames(MergedInputData)=="Counter")==FALSE )){ ## and Counter is absent because only one has been used then Create One
    MergedInputData$Counter<-as.factor(rep(paste0(000),dim(MergedInputData)[1]))
  }
  # Create Marker_ID
  MergedInputData$Marker_ID<-as.factor(sprintf("%03d", as.numeric(as.character(MergedInputData$Counter))))
  
  if(nlevels(MergedInputData$Marker_ID)==1){
    # Prompt String Marker_Name
    PromptNameDialog <- tktoplevel()
    Name_Var <- tclVar("Marker_Name")
    PromptNameDialog$env$Entered_Name <-tkentry(PromptNameDialog, width = "25", textvariable = Name_Var)
    tkgrid(tklabel(PromptNameDialog, text = "Please enter the Marker Name:", justify = "left"),
           padx = 10, pady = c(15, 5), sticky = "w")
    tkgrid(PromptNameDialog$env$Entered_Name, padx = 10, pady = c(0, 15))
    ClickOK <- function() {
      Name_Var2 <- tclvalue(Name_Var)
      tkdestroy(PromptNameDialog)
      assign("Marker_Name", Name_Var2, envir=.GlobalEnv)
    }
    PromptNameDialog$env$Button_OK <-tkbutton(PromptNameDialog, text = "OK", width = -6, command = ClickOK)
    tkgrid(PromptNameDialog$env$Button_OK, padx = 10, pady = c(5, 15))
    tkbind(PromptNameDialog$env$Entered_Name, "<Return>", ClickOK)
    tkfocus(PromptNameDialog)
    tkwait.window(PromptNameDialog)
      MergedInputData$Marker_Name<-rep(Marker_Name, length(MergedInputData$File_ID))
  #Create dataframe for MarkerData wiht only one row
      MarkerData<-data.frame("Marker_ID"=sprintf("%03d", 0), "Marker_Name"=Marker_Name)
  } else  if(nlevels(MergedInputData$Marker_ID)>1){
    # Prompt for Marker Information
    SelectFile.Function(DialogMessage="Select the Marker Information File", DataObjectName = "MarkerData", FileExt="txt")
    MarkerData$Marker_ID<-as.factor(sprintf("%03d", as.numeric(as.character(MarkerData$Marker_ID))))
    MergedInputData$Marker_Name<-""
    # Get the Marker_Name from the marker_ID in the MarkerData
    for(RowI in 1:length(MergedInputData$File_ID)){
      Marker_IDI<-as.character(MergedInputData$Marker_ID[RowI])
      MergedInputData$Marker_Name[RowI]<-as.character(MarkerData$Marker_Name[MarkerData$Marker_ID==Marker_IDI])          
    }
    
  } ## End of Get MarkerName
  
  
  MergedInputData$Marker_Name<-factor( MergedInputData$Marker_Name)
  # Create a Channel by the combination of MarkerName and Channel
  
  if((any(colnames(MergedInputData)=="Ch")==TRUE )){ ## and Counter is absent because only one has been used then Create One
    MergedInputData$Channel<-as.factor(MergedInputData$Ch)
  } else if((any(colnames(MergedInputData)=="Slice")==TRUE )){
    MergedInputData$Channel<-as.factor(MergedInputData$Slice)
  } else if((any(colnames(MergedInputData)=="Type")==TRUE )){
    MergedInputData$Channel<-as.factor(MergedInputData$Type)
  } else {
    # Prompt for Channel Name
    PromptNameDialog <- tktoplevel()
    Name_Var <- tclVar("Channel_Name")
    PromptNameDialog$env$Entered_Name <-tkentry(PromptNameDialog, width = "25", textvariable = Name_Var)
    tkgrid(tklabel(PromptNameDialog, text = "Please enter the Channel Name:", justify = "left"),
           padx = 10, pady = c(15, 5), sticky = "w")
    tkgrid(PromptNameDialog$env$Entered_Name, padx = 10, pady = c(0, 15))
    ClickOK <- function() {
      Name_Var2 <- tclvalue(Name_Var)
      tkdestroy(PromptNameDialog)
      assign("Channel_Name", Name_Var2, envir=.GlobalEnv)
    }
    PromptNameDialog$env$Button_OK <-tkbutton(PromptNameDialog, text = "OK", width = -6, command = ClickOK)
    tkgrid(PromptNameDialog$env$Button_OK, padx = 10, pady = c(5, 15))
    #  tkbind(PromptNameDialog$env$Entered_Name, "<Return>", ClickOK)
    #  tkfocus(PromptNameDialog)
    tkwait.window(PromptNameDialog)
    MergedInputData$Channel<-as.factor(as.character(rep(Channel_Name,length(MergedInputData$File_ID))))
  }
  
  MergedInputData$Marker_Name_x_Channel<- paste0(as.character(MergedInputData$Marker_Name),"_x_Ch",as.character(MergedInputData$Channel))
  
  MergedInputData$Marker_Name_x_Channel<-factor( MergedInputData$Marker_Name_x_Channel)
} ## end of else

# Gather the Data into MetaData Table -------------------------------------

##Bring the Marker Data
for (FileI in 1:length(MergedInputData$File_ID)){
  File_IDI<-MergedInputData$File_ID[FileI]
  InputDataI<-MergedInputData[MergedInputData$File_ID==File_IDI,] # Get the Data of a given Image
  #Refresh the Factor
  InputDataI$Marker_Name<-factor(InputDataI$Marker_Name)
  Nb_Marker_Types<-nlevels(InputDataI$Marker_Name)
  RegistrationData$Nb_Marker_Types[RegistrationData$File_ID==File_IDI]<-Nb_Marker_Types
  
}

for(MarkerI in 1:length(MarkerData$Marker_Name)){
  Marker_IDI<-MarkerData$Marker_ID[MarkerI]
  Marker_NameI<-MarkerData$Marker_Name[MarkerI]
  RegistrationData$MarkerI<-NA
  names(RegistrationData)[(dim(RegistrationData)[2])]<-paste0("Marker_",  sprintf("%03d", as.numeric(Marker_IDI)))
  RegistrationData$CounterNameI<-NA
  names(RegistrationData)[(dim(RegistrationData)[2])]<-paste0(Marker_NameI)
}

## Add the Counts of each marker
for (FileI in 1:length(RegistrationData$File_ID)){
  File_IDI<-RegistrationData$File_ID[FileI]
  InputDataI<-MergedInputData[MergedInputData$File_ID==File_IDI,] # Get the Data of a given Image
  #Refresh the Factor
  InputDataI$Marker_ID<-factor(InputDataI$Marker_ID)
  InputDataI$Marker_Name<-factor(InputDataI$Marker_Name)
  
  SummaryTable_Marker_ID<-as.data.frame(table(InputDataI$Marker_ID))
  SummaryTable_Marker_ID$Marker_ID<-SummaryTable_Marker_ID$Var1
  SummaryTable_Marker_ID$Counts<-SummaryTable_Marker_ID$Freq
  SummaryTable_Marker_ID$Var1<-NULL
  SummaryTable_Marker_ID$Freq<-NULL
  
  SummaryTable_Marker_Name<-as.data.frame(table(InputDataI$Marker_Name))
  SummaryTable_Marker_Name$Marker_Name<-SummaryTable_Marker_Name$Var1
  SummaryTable_Marker_Name$Counts<-SummaryTable_Marker_Name$Freq
  SummaryTable_Marker_Name$Var1<-NULL
  SummaryTable_Marker_Name$Freq<-NULL
  
  for(MarkerI in 1:length(SummaryTable_Marker_ID$Marker_ID)){
    Marker_IDI<-as.character(SummaryTable_Marker_ID$Marker_ID[MarkerI])
    Counts_Marker_IDI<-as.integer(SummaryTable_Marker_ID$Counts[MarkerI])
    RegistrationData[RegistrationData$File_ID==File_IDI, names(RegistrationData)==paste0("Marker_",Marker_IDI)]<-Counts_Marker_IDI
  }
  
  for(MarkerI in 1:length(SummaryTable_Marker_Name$Marker_Name)){
    Marker_NameI<-as.character(SummaryTable_Marker_Name$Marker_Name[MarkerI])
    Counts_Marker_NameI<-as.integer(SummaryTable_Marker_Name$Counts[MarkerI])
    RegistrationData[RegistrationData$File_ID==File_IDI, names(RegistrationData)==Marker_NameI]<-Counts_Marker_NameI
  }
  
  
}
MetaData<-RegistrationData
write.table(MetaData, file=file.path(OutputDirPath, "Metadata.txt"), row.names=FALSE, sep = "\t")


# Process each File separatly -------------------------------------------------------
for (FileI in 1:nlevels(MergedInputData$File_ID)){
  File_IDI<-levels(MergedInputData$File_ID)[FileI]
  InputDataI<-MergedInputData[MergedInputData$File_ID==File_IDI,] # Get the Data of a given Image
  
  ## Get the registered coordinates from the RegistrationData
  ## If a perfect match on fileIDs use the following line
  #RegistrationDataI<-RegistrationData[RegistrationData$File_ID==File_IDI,]
  ## If a partial match on fileIDs
  RegistrationDataI<-subset(RegistrationData, pmatch(RegistrationData$File_ID, File_IDI)==1)
  if(dim(RegistrationDataI)[1]==1){
    if(RegistrationDataI$Resolution_Unit!="pixels") {
      ImageResolutionI<-as.numeric(RegistrationDataI$Resolution_Pixels_per_Unit)
      InputDataI$X_Pixel<- as.numeric(InputDataI$X) * ImageResolutionI
      InputDataI$Y_Pixel<- as.numeric(InputDataI$Y) * ImageResolutionI
    } else { ## Else data is already in pixels
      InputDataI$X_Pixel<- as.numeric(InputDataI$X)
      InputDataI$Y_Pixel<- as.numeric(InputDataI$Y)
    }
  } else {
    stop(paste0("Registartion Data for the file ",File_IDI," is missing."))
  }
  
  # Center the data on the central canal
  InputDataI$X_Pixel_Centered<-scale(InputDataI$X_Pixel, center=RegistrationDataI$CC_X_Pixel, scale=FALSE)
  InputDataI$Y_Pixel_Centered<-scale(InputDataI$Y_Pixel, center=RegistrationDataI$CC_Y_Pixel, scale=FALSE)
  
  # Divide the Data into 4 quadrants Dorso-Ventral Right_Left
  InputDataI_D_R<- InputDataI[InputDataI$X_Pixel_Centered>=0 & InputDataI$Y_Pixel_Centered>=0,]
  InputDataI_V_R<- InputDataI[InputDataI$X_Pixel_Centered>=0 & InputDataI$Y_Pixel_Centered<0,]
  InputDataI_V_L<- InputDataI[InputDataI$X_Pixel_Centered<0 & InputDataI$Y_Pixel_Centered<0,]
  InputDataI_D_L<- InputDataI[InputDataI$X_Pixel_Centered<0 & InputDataI$Y_Pixel_Centered>=0,]
  
  
  # Scale each quadrant
  InputDataI_D_R$X_Scaled<- scale(InputDataI_D_R$X_Pixel_Centered, center=FALSE, scale=RegistrationDataI$LE_R_X_Pixel_Centered)
  InputDataI_D_R$Y_Scaled<- scale(InputDataI_D_R$Y_Pixel_Centered, center=FALSE, scale=RegistrationDataI$DE_R_Y_Pixel_Centered)
  
  InputDataI_V_R$X_Scaled<- scale(InputDataI_V_R$X_Pixel_Centered, center=FALSE, scale=RegistrationDataI$LE_R_X_Pixel_Centered)
  InputDataI_V_R$Y_Scaled<- - scale(InputDataI_V_R$Y_Pixel_Centered, center=FALSE, scale=RegistrationDataI$VE_R_Y_Pixel_Centered)
  
  InputDataI_V_L$X_Scaled<- - scale(InputDataI_V_L$X_Pixel_Centered, center=FALSE, scale=RegistrationDataI$LE_L_X_Pixel_Centered)
  InputDataI_V_L$Y_Scaled<- - scale(InputDataI_V_L$Y_Pixel_Centered, center=FALSE, scale=RegistrationDataI$VE_L_Y_Pixel_Centered)
  
  InputDataI_D_L$X_Scaled<-  - scale(InputDataI_D_L$X_Pixel_Centered, center=FALSE, scale=RegistrationDataI$LE_L_X_Pixel_Centered)
  InputDataI_D_L$Y_Scaled<- scale(InputDataI_D_L$Y_Pixel_Centered, center=FALSE, scale=RegistrationDataI$DE_R_Y_Pixel_Centered)
  
  
  # Bind the quadrants back together
  OutputDataI<-rbind(InputDataI_D_R,InputDataI_V_R,InputDataI_V_L,InputDataI_D_L)
  
  ## Add the RegistrationData to the OutputData
  MissingVars<- setdiff(colnames(RegistrationDataI), colnames(OutputDataI)) # Get the Missing Columns
  
  if (length(MissingVars)>0){ # Compare the Nb Of Columns if Merge file has more columns
    for (MissingVariableI in 1: length(MissingVars)){
      OutputDataI[[paste0(MissingVars[MissingVariableI])]] <- rep(RegistrationDataI[[paste0(MissingVars[MissingVariableI])]], dim(OutputDataI)[1])
    } # Add Missing Variables to OutputDataI
  }
  
  # Merge Output Files together
  if(FileI==1){
    OutputData<-OutputDataI
  } else {
    OutputData<-rbind(OutputData, OutputDataI)
  }
  write.table(OutputData, file=file.path(OutputDirPath, "Data_Coordinates_Processed.txt"), row.names=FALSE, sep = "\t")
}


# Plot by File ---------------------------------------------------------
# Process  each File separately
OutputData$File_ID<-factor(OutputData$File_ID)
for (FileI in 1:nlevels(OutputData$File_ID)){
  File_IDI<-levels(OutputData$File_ID)[FileI]
  OutputDataI<-OutputData[OutputData$File_ID==File_IDI,] # Get the Data of a given Image
  write.table(OutputDataI, file=file.path(OutputDirPath, "Tables by File",paste0(File_IDI,".txt")), row.names=FALSE, sep = "\t")
  
  if(FileI==1){
    dir.create(file.path(OutputDirPath, "Graphs by File","Raw"))
    dir.create(file.path(OutputDirPath, "Graphs by File","Scaled"))
  }
  
  
  # Plot RAW coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by File", "Raw", paste0(File_IDI,"_Raw_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Pixel_Centered),mean(OutputDataI$LE_R_X_Pixel_Centered),max(abs(OutputDataI$X_Pixel_Centered))))),-1)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Pixel_Centered),mean(OutputDataI$DE_R_Y_Pixel_Centered),mean(OutputDataI$VE_L_Y_Pixel_Centered),mean(OutputDataI$VE_R_Y_Pixel_Centered), max(abs(OutputDataI$Y_Pixel_Centered))))),-1)
  NbMarkers=nlevels(OutputDataI$Marker_Name_x_Channel)
  par(xpd=TRUE)
  plot(OutputDataI$X_Pixel_Centered, OutputDataI$Y_Pixel_Centered,
       type="p", bty="n",
       pch=1,lwd=0.5, cex=0.5, col=BlueToRedPalette(NbMarkers,1)[OutputDataI$Marker_Name_x_Channel],
       xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
       xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
       main=File_IDI,
       xlab="Relative position to CC (pixel)",  ylab="Relative position to CC (pixel)"
  )
  
  
  points(mean(OutputDataI$CC_X_Pixel_Centered),mean(OutputDataI$CC_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_R_X_Pixel_Centered),mean(OutputDataI$DE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_R_X_Pixel_Centered),mean(OutputDataI$LE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_R_X_Pixel_Centered),mean(OutputDataI$VE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_L_X_Pixel_Centered),mean(OutputDataI$DE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_L_X_Pixel_Centered),mean(OutputDataI$LE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_L_X_Pixel_Centered),mean(OutputDataI$VE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  
  # Add Marker Legend
  legend("bottomleft",
         bty="n",
         pch=1, cex=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Marker",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  
  # Add Cell Counts Title
  LegendTop <- legend("top",
                      inset=c(0,-0.05),
                      bty="n",
                      xjust =0.5, yjust = 0.5,
                      cex=0.5,
                      col="black",
                      title="Nb of Cells",
                      legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
                      text.width = strwidth("Marker: Left + Right = Total")
  )
  # Add Cell Counts SubTitle
  text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
       c("Marker: Left + Right = Total",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  
  # Add Counts for each Marker
  for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
    Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
    OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
    TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
    LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
    RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
    
    # Add the Marker Data to the Legend
    text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": " ,LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
  }## End of for Marker I
  dev.off() # Close and save the graph
  
  
  
  
  
  
  # Plot SCALED coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs by File", "Scaled", paste0(File_IDI,"_Scaled_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_R_X_Scaled),max(abs(OutputDataI$X_Scaled))))),2)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Scaled),mean(OutputDataI$DE_R_Y_Scaled),mean(OutputDataI$VE_L_Y_Scaled),mean(OutputDataI$VE_R_Y_Scaled), max(abs(OutputDataI$Y_Scaled))))),2)
  NbMarkers<-nlevels(OutputDataI$Marker_Name_x_Channel)
  par(xpd=TRUE)
  plot(OutputDataI$X_Scaled, OutputDataI$Y_Scaled,
       type="p", bty="n",
       pch=1,lwd=0.5, cex=0.5, col=BlueToRedPalette(NbMarkers,1)[OutputDataI$Marker_Name_x_Channel],
       xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
       xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
       main=File_IDI,
       xlab="Relative position to CC (Scaled)",  ylab="Relative position to CC (Scaled)"
       ,pannel.first={
         XCenter= ( par()$mai[2] + (par()$pin[1])/2)/(par()$din[1])
         YCenter= ( par()$mai[1] + (par()$pin[2])/2)/(par()$din[2])
         WidthSC=  ((par()$pin[1])/2)/(par()$din[1])
         HeightSC= ((par()$pin[2])/2)/(par()$din[2])
         grid.picture(SpinalCordLayout, x=XCenter, y=YCenter,
                      width=2*WidthSC+0.1*WidthSC, height=2*HeightSC+0.1*HeightSC
                      ,distort=TRUE)
         
       }
  )
  
  points(mean(OutputDataI$CC_X_Scaled),mean(OutputDataI$CC_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_R_X_Scaled),mean(OutputDataI$DE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_R_X_Scaled),mean(OutputDataI$LE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_R_X_Scaled),mean(OutputDataI$VE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_L_X_Scaled),mean(OutputDataI$DE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_L_X_Scaled),mean(OutputDataI$VE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  
  ## Add Marker Legend
  legend("bottomleft",
         bty="n",
         pch=1, cex=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Marker",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  
  ## Add Count Title
  LegendTop <- legend("top",
                      inset=c(0,-0.05),
                      bty="n",
                      xjust =0.5, yjust = 0.5,
                      cex=0.5,
                      col="black",
                      title="Nb of Cells",
                      legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
                      text.width = strwidth("Marker: Left + Right = Total")
  )
  ## Add Count SubTitle
  text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
       c("Marker: Left + Right = Total",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  
  # Add Counts for each Marker
  for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
    Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
    OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
    NbImages<-length(levels(OutputDataI$File_ID))
    TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
    LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
    RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
    
    # Add the Counts for each marker
    text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": " ,LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
  }## End of for Marker I
  dev.off() # Close and save the graph
}







# Plot By Subject ID ----------------------------------------------------
OutputData$Subject_ID<-factor(OutputData$Subject_ID)
for (SubjectI in 1:nlevels(OutputData$Subject_ID)){
  Subject_IDI<-levels(OutputData$Subject_ID)[SubjectI]
  OutputDataI<-OutputData[OutputData$Subject_ID==Subject_IDI,] # Get the Data of a given Subject
  # Refresh Factors
  OutputDataI$File_ID<-factor(OutputDataI$File_ID)
  OutputDataI$Subject_ID<-factor(OutputDataI$Subject_ID)
  
  write.table(OutputDataI, file=file.path(OutputDirPath, "Tables by Subject",paste0(Subject_IDI,".txt")), row.names=FALSE, sep = "\t")
  
  if(SubjectI==1){
    dir.create(file.path(OutputDirPath, "Graphs by Subject","Raw"))
    dir.create(file.path(OutputDirPath, "Graphs by Subject","Scaled"))
  }
  
  
  
  # Plot RAW coordinates for each SUBJECT
  cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", "Raw", paste0(Subject_IDI,"_Raw_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Pixel_Centered),mean(OutputDataI$LE_R_X_Pixel_Centered),max(abs(OutputDataI$X_Pixel_Centered))))),-1)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Pixel_Centered),mean(OutputDataI$DE_R_Y_Pixel_Centered),mean(OutputDataI$VE_L_Y_Pixel_Centered),mean(OutputDataI$VE_R_Y_Pixel_Centered), max(abs(OutputDataI$Y_Pixel_Centered))))),-1)
  NbMarkers=nlevels(OutputDataI$Marker_Name_x_Channel)
  par(xpd=TRUE)
  plot(OutputDataI$X_Pixel_Centered, OutputDataI$Y_Pixel_Centered,
       type="p", bty="n",
       pch=1,lwd=0.5, cex=0.5, col=BlueToRedPalette(NbMarkers,1)[OutputDataI$Marker_Name_x_Channel],
       xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
       xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
       main=Subject_IDI,
       xlab="Relative position to CC (pixel)",  ylab="Relative position to CC (pixel)"
  )
  
  
  points(mean(OutputDataI$CC_X_Pixel_Centered),mean(OutputDataI$CC_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_R_X_Pixel_Centered),mean(OutputDataI$DE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_R_X_Pixel_Centered),mean(OutputDataI$LE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_R_X_Pixel_Centered),mean(OutputDataI$VE_R_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_L_X_Pixel_Centered),mean(OutputDataI$DE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_L_X_Pixel_Centered),mean(OutputDataI$LE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_L_X_Pixel_Centered),mean(OutputDataI$VE_L_Y_Pixel_Centered), col="black", pch=3, cex=0.5)
  
  ## Add Marker Legend
  legend("bottomleft",
         bty="n",
         pch=1, cex=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Marker",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  
  ## Add Count Title
  LegendTop <- legend("top",
                      inset=c(0,-0.05),
                      bty="n",
                      xjust =0.5, yjust = 0.5,
                      cex=0.5,
                      col="black",
                      title="Nb Of Cells",
                      legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
                      text.width = strwidth("Marker: Left + Right = Total")
  )
  
  ## Add Count Subtitle
  text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
       c("Marker: Left + Right = Total",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  #Add Left Count Title
  LegendLeft <- legend("topleft",
                       inset=c(0,-0.05),
                       bty="n",
                       xjust =0, yjust = 0,
                       cex=0.5,
                       col="black",
                       title="Left Side Counts",
                       text.width = strwidth("Marker: Avg Cell/Section (+/- StDev) ; n Sections")/2,
                       legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  #Add Left Count SubTitle
  text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  #Add Right Count Title
  LegendRight <- legend("topright",
                        inset=c(0,-0.05),
                        bty="n",
                        xjust =0., yjust = 0,
                        cex=0.5,
                        col="black",
                        title="Right Side Counts",
                        text.width = strwidth("Avg Cell/Section (+/- SD) ; n Sections")/2,
                        legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  
  
  #Add Right Count SubTitle
  text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  
  
  # If you want to plot only the markers that are present in this animals then uncomment the next slide to refresh the factor of Marker_Name x Channel
  # OutputDataI$Marker_Name_x_Channel<-factor(OutputDataI$Marker_Name_x_Channel)
  # Add Counts for each Marker
  for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
    Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
    OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
    NbImages<-length(levels(OutputDataI$File_ID))
    LeftCountsPerImage_MarkerI<-c()
    RightCountsPerImage_MarkerI<-c()
    for(ImageI in 1:length(levels(OutputDataI$File_ID))){
      Image_IDI<-levels(OutputDataI$File_ID)[ImageI]
      DataImageI<-OutputDataI[OutputDataI$File_ID==Image_IDI,]
      LeftDataImageI<-DataImageI[DataImageI$X_Scaled<0,]
      RightDataImageI<-DataImageI[DataImageI$X_Scaled>=0,]
      LeftDataImageI_MarkerI<-LeftDataImageI[LeftDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      RightDataImageI_MarkerI<-RightDataImageI[RightDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      LeftCountsPerImage_MarkerI<-c(LeftCountsPerImage_MarkerI,dim(LeftDataImageI_MarkerI)[1])
      RightCountsPerImage_MarkerI<-c(RightCountsPerImage_MarkerI,dim(RightDataImageI_MarkerI)[1])
    }
    TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
    LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
    RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
    
    
    # Add the Marker Counts to the Legend
    text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": " ,LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    # Add Left Counts
    text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(LeftCountsPerImage_MarkerI),3)," (+/- ",signif(sd(LeftCountsPerImage_MarkerI),3),") ; n = ",length(LeftCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
           
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    
    # Add Right Counts
    text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(RightCountsPerImage_MarkerI),3)," (+/- ",signif(sd(RightCountsPerImage_MarkerI),3),") ; n = ",length(RightCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
  }## End of for Marker I
  dev.off() # Close and save the graph
  
  
  
  
  
  
  
  
  # Plot SCALED coordinates for each SUBJECT
  cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", "Scaled", paste0(Subject_IDI,"_Scaled_Graph.pdf"))) # Open the graph as pdf
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_R_X_Scaled),max(abs(OutputDataI$X_Scaled))))),2)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Scaled),mean(OutputDataI$DE_R_Y_Scaled),mean(OutputDataI$VE_L_Y_Scaled),mean(OutputDataI$VE_R_Y_Scaled), max(abs(OutputDataI$Y_Scaled))))),2)
  NbMarkers=nlevels(OutputDataI$Marker_Name_x_Channel)
  par(xpd=TRUE)
  plot(OutputDataI$X_Scaled, OutputDataI$Y_Scaled,
       type="p", bty="n",
       pch=1,lwd=0.5, cex=0.5, col=BlueToRedPalette(NbMarkers,1)[OutputDataI$Marker_Name_x_Channel],
       xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
       xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
       main=Subject_IDI,
       xlab="Relative position to CC (Scaled)",  ylab="Relative position to CC (Scaled)"
       ,pannel.first={
         XCenter= ( par()$mai[2] + (par()$pin[1])/2)/(par()$din[1])
         YCenter= ( par()$mai[1] + (par()$pin[2])/2)/(par()$din[2])
         WidthSC=  ((par()$pin[1])/2)/(par()$din[1])
         HeightSC= ((par()$pin[2])/2)/(par()$din[2])
         grid.picture(SpinalCordLayout, x=XCenter, y=YCenter,
                      width=2*WidthSC+0.1*WidthSC, height=2*HeightSC+0.1*HeightSC
                      ,distort=TRUE)
         
       }
  )
  
  points(mean(OutputDataI$CC_X_Scaled),mean(OutputDataI$CC_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_R_X_Scaled),mean(OutputDataI$DE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_R_X_Scaled),mean(OutputDataI$LE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_R_X_Scaled),mean(OutputDataI$VE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_L_X_Scaled),mean(OutputDataI$DE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_L_X_Scaled),mean(OutputDataI$VE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  
  ## add marker legend
  legend("bottomleft",
         bty="n",
         pch=1, cex=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Marker",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  
  ## Add Counts Title
  LegendTop <- legend("top",
                      inset=c(0,-0.05),
                      bty="n",
                      xjust =0.5, yjust = 0.5,
                      cex=0.5,
                      col="black",
                      title="Nb Of Cells",
                      legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
                      text.width = strwidth("Marker: Left + Right = Total")
  )
  
  # Add Coutn subtitles
  text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
       c("Marker: Left + Right = Total",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  # Add Left Count Title
  LegendLeft <- legend("topleft",
                       inset=c(0,-0.05),
                       bty="n",
                       xjust =0, yjust = 0,
                       cex=0.5,
                       col="black",
                       title="Left Side Counts",
                       text.width = strwidth("Marker: Avg Cell/Section (+/- StDev) ; n Sections")/2,
                       legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  
  # Add left coutns subtitle
  text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  # Add right count title
  LegendRight <- legend("topright",
                        inset=c(0,-0.05),
                        bty="n",
                        xjust =0., yjust = 0,
                        cex=0.5,
                        col="black",
                        title="Right Side Counts",
                        text.width = strwidth("Avg Cell/Section (+/- SD) ; n Sections")/2,
                        legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  #add right count subtitle
  text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  
  
  
  
  # Add Counts for each Marker
  for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
    Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
    OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
    NbImages<-length(levels(OutputDataI$File_ID))
    LeftCountsPerImage_MarkerI<-c()
    RightCountsPerImage_MarkerI<-c()
    for(ImageI in 1:length(levels(OutputDataI$File_ID))){
      Image_IDI<-levels(OutputDataI$File_ID)[ImageI]
      DataImageI<-OutputDataI[OutputDataI$File_ID==Image_IDI,]
      LeftDataImageI<-DataImageI[DataImageI$X_Scaled<0,]
      RightDataImageI<-DataImageI[DataImageI$X_Scaled>=0,]
      LeftDataImageI_MarkerI<-LeftDataImageI[LeftDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      RightDataImageI_MarkerI<-RightDataImageI[RightDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      LeftCountsPerImage_MarkerI<-c(LeftCountsPerImage_MarkerI,dim(LeftDataImageI_MarkerI)[1])
      RightCountsPerImage_MarkerI<-c(RightCountsPerImage_MarkerI,dim(RightDataImageI_MarkerI)[1])
    }
    TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
    LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
    RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
    
    
    # Add the Marker counts to the Legend
    text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": " ,LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    
    #Add left counts
    text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(LeftCountsPerImage_MarkerI),3)," (+/- ",signif(sd(LeftCountsPerImage_MarkerI),3),") ; n = ",length(LeftCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
           
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    
    ## Add Right Counts
    text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(RightCountsPerImage_MarkerI),3)," (+/- ",signif(sd(RightCountsPerImage_MarkerI),3),") ; n = ",length(RightCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
  }## End of for Marker I
  #Add 0 0 lines
  par(xpd=FALSE)
  abline(v=0, col="grey", lwd=0.5)
  abline(h=0, col="grey", lwd=0.5)
  par(xpd=TRUE)
  
  dev.off() # Close and save the graph
  
} # End of for SubjectI



# Calculate the Density and Plot Density per Subject ----------------------------------------------------
OutputData$Subject_ID<-factor(OutputData$Subject_ID)
for (SubjectI in 1:nlevels(OutputData$Subject_ID)){
  Subject_IDI<-levels(OutputData$Subject_ID)[SubjectI]
  OutputDataI<-OutputData[OutputData$Subject_ID==Subject_IDI,] # Get the Data of a given Subject
  # Refresh factors
  OutputDataI$File_ID<-factor(OutputDataI$File_ID)
  
  if(SubjectI==1){
    dir.create(file.path(OutputDirPath, "Graphs by Subject","Contours"))
    dir.create(file.path(OutputDirPath, "Graphs by Subject","Filled Density"))
    dir.create(file.path(OutputDirPath, "Tables by Subject","Density Raw"))
    dir.create(file.path(OutputDirPath, "Tables by Subject","Density Normalized"))
    dir.create(file.path(OutputDirPath, "Tables by Subject","Density Weighted"))
  }
  
   # Plot SCALED coordinates for each SUBJECT and ADD THE CONTOURS
  cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", "Contours", paste0(Subject_IDI,"_Contours_Normalized_Graph.pdf"))) # Open the graph as pdf
  par(xpd=TRUE)
  Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_R_X_Scaled),max(abs(OutputDataI$X_Scaled))))),2)
  Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Scaled),mean(OutputDataI$DE_R_Y_Scaled),mean(OutputDataI$VE_L_Y_Scaled),mean(OutputDataI$VE_R_Y_Scaled), max(abs(OutputDataI$Y_Scaled))))),2)
  NbMarkers=nlevels(OutputDataI$Marker_Name_x_Channel)
  plot(OutputDataI$X_Scaled, OutputDataI$Y_Scaled,
       type="p", bty="n",
       pch=1,lwd=0.5, cex=0.5, col=BlueToRedPalette(NbMarkers,1)[OutputDataI$Marker_Name_x_Channel],
       xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
       xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
       main=Subject_IDI,
       xlab="Relative position to CC (Scaled)",  ylab="Relative position to CC (Scaled)"
       ,pannel.first={
         XCenter= ( par()$mai[2] + (par()$pin[1])/2)/(par()$din[1])
         YCenter= ( par()$mai[1] + (par()$pin[2])/2)/(par()$din[2])
         WidthSC=  ((par()$pin[1])/2)/(par()$din[1])
         HeightSC= ((par()$pin[2])/2)/(par()$din[2])
         grid.picture(SpinalCordLayout, x=XCenter, y=YCenter,
                      width=2*WidthSC+0.1*WidthSC, height=2*HeightSC+0.1*HeightSC
                      ,distort=TRUE)
         
       }
  )
  
  points(mean(OutputDataI$CC_X_Scaled),mean(OutputDataI$CC_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_R_X_Scaled),mean(OutputDataI$DE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_R_X_Scaled),mean(OutputDataI$LE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_R_X_Scaled),mean(OutputDataI$VE_R_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$DE_L_X_Scaled),mean(OutputDataI$DE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  points(mean(OutputDataI$VE_L_X_Scaled),mean(OutputDataI$VE_L_Y_Scaled), col="black", pch=3, cex=0.5)
  
  # Add Marker type legend
  legend("bottomleft",
         bty="n",
         pch=1, cex=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Marker",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  # Add contour legend
  legend("bottomright",
         bty="n",
         lty=1, cex=0.5, lwd=0.5,
         col=BlueToRedPalette(NbMarkers,1),
         title="Density",
         legend=levels(OutputDataI$Marker_Name_x_Channel),
         xjust = 0.5, yjust = 0.5
  )
  
  # Add Counts Title
  LegendTop <- legend("top",
                      inset=c(0,-0.05),
                      bty="n",
                      xjust =0.5, yjust = 0.5,
                      cex=0.5,
                      col="black",
                      title="Nb Of Cells",
                      legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
                      text.width = strwidth("Marker: Left + Right = Total")
  )
  # Add Counts SubTitle
  text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
       c("Marker: Left + Right = Total",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  # Add Left Count Title
  LegendLeft <- legend("topleft",
                       inset=c(0,-0.05),
                       bty="n",
                       xjust =0, yjust = 0,
                       cex=0.5,
                       col="black",
                       title="Left Side Counts",
                       text.width = strwidth("Marker: Avg Cell/Section (+/- StDev) ; n Sections")/2,
                       legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  # Add Left Count SubTitle
  text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  # Add Right Count Title
  LegendRight <- legend("topright",
                        inset=c(0,-0.05),
                        bty="n",
                        xjust =0., yjust = 0,
                        cex=0.5,
                        col="black",
                        title="Right Side Counts",
                        text.width = strwidth("Avg Cell/Section (+/- SD) ; n Sections")/2,
                        legend=c(" ", rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel)))
  )
  # Add Right Count subTitle
  text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
       c("Marker: Avg Cell/Section (+/- StDev) ; n Sections",rep(" ",nlevels(OutputDataI$Marker_Name_x_Channel))),
       cex=0.5)
  
  
  
  
  # Add Data for each Marker Counts and Contours
  for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
    Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
    OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
    NbImages<-length(levels(OutputDataI$File_ID))
    LeftCountsPerImage_MarkerI<-c()
    RightCountsPerImage_MarkerI<-c()
    for(ImageI in 1:length(levels(OutputDataI$File_ID))){
      Image_IDI<-levels(OutputDataI$File_ID)[ImageI]
      DataImageI<-OutputDataI[OutputDataI$File_ID==Image_IDI,]
      LeftDataImageI<-DataImageI[DataImageI$X_Scaled<0,]
      RightDataImageI<-DataImageI[DataImageI$X_Scaled>=0,]
      LeftDataImageI_MarkerI<-LeftDataImageI[LeftDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      RightDataImageI_MarkerI<-RightDataImageI[RightDataImageI$Marker_Name_x_Channel==Marker_NameI,]
      LeftCountsPerImage_MarkerI<-c(LeftCountsPerImage_MarkerI,dim(LeftDataImageI_MarkerI)[1])
      RightCountsPerImage_MarkerI<-c(RightCountsPerImage_MarkerI,dim(RightDataImageI_MarkerI)[1])
    }
    TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
    LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
    RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
    
    
    # Add the Marker Count to the Legend
    text(LegendTop$rect$left + LegendTop$rect$w/2, LegendTop$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": " ,LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    ## Add the Left Counts
    text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(LeftCountsPerImage_MarkerI),3)," (+/- ",signif(sd(LeftCountsPerImage_MarkerI),3),") ; n = ",length(LeftCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    ## Add the Right Counts
    text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
         c(rep(" ",MarkerI),
           paste0(Marker_NameI,": ",signif(mean(RightCountsPerImage_MarkerI),3)," (+/- ",signif(sd(RightCountsPerImage_MarkerI),3),") ; n = ",length(RightCountsPerImage_MarkerI)),
           rep(" ",(nlevels(OutputDataI$Marker_Name_x_Channel))-MarkerI)
         ),
         cex=0.5, col=BlueToRedPalette(NbMarkers,1)[MarkerI]
    )
    
    
    ### ADD THE CONTOURS
    OutputDataI_MarkerI_Left<-OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,]
    OutputDataI_MarkerI_Right<-OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,]
    ### The Left
    if(dim(OutputDataI_MarkerI_Left)[1]>2){
      Density_OutputDataI_MarkerI_Left<-kde2d(OutputDataI_MarkerI_Left$X_Scaled, OutputDataI_MarkerI_Left$Y_Scaled,
                                              n=100, lims=c(-1.5,1.5,-1.5,1.5))
      Normalized_Density_OutputDataI_MarkerI_Left<-Density_OutputDataI_MarkerI_Left
      Normalized_Density_OutputDataI_MarkerI_Left$z<- ((Density_OutputDataI_MarkerI_Left$z-min(Density_OutputDataI_MarkerI_Left$z))/(max(Density_OutputDataI_MarkerI_Left$z)-min(Density_OutputDataI_MarkerI_Left$z)))
      Weighted_Density_OutputDataI_MarkerI_Left<-Normalized_Density_OutputDataI_MarkerI_Left
      Weighted_Density_OutputDataI_MarkerI_Left$z<- dim(OutputDataI_MarkerI_Left)[1] * Normalized_Density_OutputDataI_MarkerI_Left$z
      
      write.table(Density_OutputDataI_MarkerI_Left, file=file.path(OutputDirPath, "Tables by Subject", "Density Raw",paste0(Subject_IDI,"_",Marker_NameI,"_Left.txt")), row.names=FALSE, sep = "\t")
      write.table(Normalized_Density_OutputDataI_MarkerI_Left, file=file.path(OutputDirPath, "Tables by Subject", "Density Normalized",paste0(Subject_IDI,"_",Marker_NameI,"_Left.txt")), row.names=FALSE, sep = "\t")
      write.table(Weighted_Density_OutputDataI_MarkerI_Left, file=file.path(OutputDirPath, "Tables by Subject", "Density Weighted",paste0(Subject_IDI,"_",Marker_NameI,"_Left.txt")), row.names=FALSE, sep = "\t")
      
      
      contour(Normalized_Density_OutputDataI_MarkerI_Left,
              add = TRUE, drawlabels = FALSE,
              lty=1, lwd=0.5,
              col=BlueToRedPalette(NbMarkers,1)[MarkerI],
              zlim = c(0,1), nlevels = 10) # add the contours
    } # end of if
    
    ### The Right
    if(dim(OutputDataI_MarkerI_Right)[1]>2){
      Density_OutputDataI_MarkerI_Right<-kde2d(OutputDataI_MarkerI_Right$X_Scaled, OutputDataI_MarkerI_Right$Y_Scaled,
                                               n=100, lims=c(-1.5,1.5,-1.5,1.5))
      Normalized_Density_OutputDataI_MarkerI_Right<-Density_OutputDataI_MarkerI_Right
      Normalized_Density_OutputDataI_MarkerI_Right$z<- ((Density_OutputDataI_MarkerI_Right$z-min(Density_OutputDataI_MarkerI_Right$z))/(max(Density_OutputDataI_MarkerI_Right$z)-min(Density_OutputDataI_MarkerI_Right$z)))
      Weighted_Density_OutputDataI_MarkerI_Right<- Normalized_Density_OutputDataI_MarkerI_Right
      Weighted_Density_OutputDataI_MarkerI_Right$z<- dim(OutputDataI_MarkerI_Right)[1] * Weighted_Density_OutputDataI_MarkerI_Right$z
      write.table(Density_OutputDataI_MarkerI_Right, file=file.path(OutputDirPath, "Tables by Subject", "Density Raw",paste0(Subject_IDI,"_",Marker_NameI,"_Right.txt")), row.names=FALSE, sep = "\t")
      write.table(Normalized_Density_OutputDataI_MarkerI_Right, file=file.path(OutputDirPath, "Tables by Subject", "Density Normalized",paste0(Subject_IDI,"_",Marker_NameI,"_Right.txt")), row.names=FALSE, sep = "\t")
      write.table(Weighted_Density_OutputDataI_MarkerI_Right, file=file.path(OutputDirPath, "Tables by Subject", "Density Weighted",paste0(Subject_IDI,"_",Marker_NameI,"_Right.txt")), row.names=FALSE, sep = "\t")
      
      contour(Normalized_Density_OutputDataI_MarkerI_Right,
              add = TRUE, drawlabels = FALSE,
              lty=1, lwd=0.5,
              col=BlueToRedPalette(NbMarkers,1)[MarkerI],
              zlim = c(0,1), nlevels = 10) # add the contours
    } # end of if
  } # end of for MarkerI
    # Add 0 0 lines
    par(xpd=FALSE)
    abline(v=0, col="grey", lwd=0.5)
    abline(h=0, col="grey", lwd=0.5)
    par(xpd=TRUE)
    dev.off() # Close and save the graph
    
    ## Plot Filled Density
    for(MarkerI in 1:nlevels(OutputDataI$Marker_Name_x_Channel)){
      Marker_NameI<-levels(OutputDataI$Marker_Name_x_Channel)[MarkerI]
      OutputDataI_MarkerI<-OutputDataI[OutputDataI$Marker_Name_x_Channel==Marker_NameI,]
      NbImages<-length(levels(OutputDataI$File_ID))
      LeftCountsPerImage_MarkerI<-c()
      RightCountsPerImage_MarkerI<-c()
      for(ImageI in 1:length(levels(OutputDataI$File_ID))){
        Image_IDI<-levels(OutputDataI$File_ID)[ImageI]
        DataImageI<-OutputDataI[OutputDataI$File_ID==Image_IDI,]
        LeftDataImageI<-DataImageI[DataImageI$X_Scaled<0,]
        RightDataImageI<-DataImageI[DataImageI$X_Scaled>=0,]
        LeftDataImageI_MarkerI<-LeftDataImageI[LeftDataImageI$Marker_Name_x_Channel==Marker_NameI,]
        RightDataImageI_MarkerI<-RightDataImageI[RightDataImageI$Marker_Name_x_Channel==Marker_NameI,]
        LeftCountsPerImage_MarkerI<-c(LeftCountsPerImage_MarkerI,dim(LeftDataImageI_MarkerI)[1])
        RightCountsPerImage_MarkerI<-c(RightCountsPerImage_MarkerI,dim(RightDataImageI_MarkerI)[1])
      }
      TotalCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI)[1]
      LeftCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,])[1]
      RightCountOutputDataI_MarkerI<-dim(OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,])[1]
     
       OutputDataI_MarkerI_Left<-OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled<0,]
      OutputDataI_MarkerI_Right<-OutputDataI_MarkerI[OutputDataI_MarkerI$X_Scaled>=0,]
      
    
    
    cairo_pdf(file.path(OutputDirPath, "Graphs by Subject", "Filled Density", paste0(Subject_IDI,"_",Marker_NameI,"_Filled_Density_Graph.pdf"))) # Open the graph as pdf
    par(xpd=TRUE)
    Xlim=round(max(abs(c(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_R_X_Scaled),max(abs(OutputDataI$X_Scaled))))),2)
    Ylim=round(max(abs(c(mean(OutputDataI$DE_L_Y_Scaled),mean(OutputDataI$DE_R_Y_Scaled),mean(OutputDataI$VE_L_Y_Scaled),mean(OutputDataI$VE_R_Y_Scaled), max(abs(OutputDataI$Y_Scaled))))),2)
    ## Plot the background
    plot(OutputDataI_MarkerI$X_Scaled, OutputDataI_MarkerI$Y_Scaled,
         type="n", bty="n",
         pch=1,lwd=0.5, cex=0.5, col="black",
         xlim=c(-Xlim,Xlim), ylim=c(-Ylim,Ylim),
         xaxp=c(-Xlim,Xlim,4), yaxp=c(-Ylim,Ylim,4),
         main=paste0(Subject_IDI,"_",Marker_NameI),
         xlab="Relative position to CC (Scaled)",  ylab="Relative position to CC (Scaled)"
         ,pannel.first={
           XCenter= ( par()$mai[2] + (par()$pin[1])/2)/(par()$din[1])
           YCenter= ( par()$mai[1] + (par()$pin[2])/2)/(par()$din[2])
           WidthSC=  ((par()$pin[1])/2)/(par()$din[1])
           HeightSC= ((par()$pin[2])/2)/(par()$din[2])
           grid.picture(SpinalCordLayout, x=XCenter, y=YCenter,
                        width=2*WidthSC+0.1*WidthSC, height=2*HeightSC+0.1*HeightSC
                        ,distort=TRUE)
           }
    )

    ###Add the Density for each Marker
    if(dim(OutputDataI_MarkerI_Left)[1]>2){
      Density_OutputDataI_MarkerI_Left<-kde2d(OutputDataI_MarkerI_Left$X_Scaled, OutputDataI_MarkerI_Left$Y_Scaled,
                                              n=100, lims=c(-1.5,1.5,-1.5,1.5))
      Normalized_Density_OutputDataI_MarkerI_Left<-Density_OutputDataI_MarkerI_Left
      Normalized_Density_OutputDataI_MarkerI_Left$z<- ((Density_OutputDataI_MarkerI_Left$z-min(Density_OutputDataI_MarkerI_Left$z))/(max(Density_OutputDataI_MarkerI_Left$z)-min(Density_OutputDataI_MarkerI_Left$z)))
      Weighted_Density_OutputDataI_MarkerI_Left<-Normalized_Density_OutputDataI_MarkerI_Left
      Weighted_Density_OutputDataI_MarkerI_Left$z<- mean(LeftCountsPerImage_MarkerI) * Normalized_Density_OutputDataI_MarkerI_Left$z
      }
      if(dim(OutputDataI_MarkerI_Right)[1]>2){
        Density_OutputDataI_MarkerI_Right<-kde2d(OutputDataI_MarkerI_Right$X_Scaled, OutputDataI_MarkerI_Right$Y_Scaled,
                                                 n=100, lims=c(-1.5,1.5,-1.5,1.5))
        Normalized_Density_OutputDataI_MarkerI_Right<-Density_OutputDataI_MarkerI_Right
        Normalized_Density_OutputDataI_MarkerI_Right$z<- ((Density_OutputDataI_MarkerI_Right$z-min(Density_OutputDataI_MarkerI_Right$z))/(max(Density_OutputDataI_MarkerI_Right$z)-min(Density_OutputDataI_MarkerI_Right$z)))
        Weighted_Density_OutputDataI_MarkerI_Right<-Normalized_Density_OutputDataI_MarkerI_Right
        Weighted_Density_OutputDataI_MarkerI_Right$z<- mean(RightCountsPerImage_MarkerI) * Normalized_Density_OutputDataI_MarkerI_Right$z
      }
      if(dim(OutputDataI_MarkerI_Left)[1]>2 && dim(OutputDataI_MarkerI_Right)[1]>2){
      ColorRangeMax<-max(c(Weighted_Density_OutputDataI_MarkerI_Left$z,Weighted_Density_OutputDataI_MarkerI_Right$z))
      .filled.contour(Weighted_Density_OutputDataI_MarkerI_Left$x,
                      Weighted_Density_OutputDataI_MarkerI_Left$y,
                      Weighted_Density_OutputDataI_MarkerI_Left$z,
                      levels = seq(from=0, to=ColorRangeMax, by=ColorRangeMax/21), col=c(rgb(red=1,green=1,blue=1,alpha=0), BlueToRedPalette(20,0.5))
      )
      .filled.contour(Weighted_Density_OutputDataI_MarkerI_Right$x,
                      Weighted_Density_OutputDataI_MarkerI_Right$y,
                      Weighted_Density_OutputDataI_MarkerI_Right$z,
                      levels = seq(from=0, to=ColorRangeMax, by=ColorRangeMax/21), col=c(rgb(red=1,green=1,blue=1,alpha=0), BlueToRedPalette(20,0.5))
      )
      } else if(dim(OutputDataI_MarkerI_Left)[1]>2 && dim(OutputDataI_MarkerI_Right)[1]<=2){
        ColorRangeMax<-max(c(Weighted_Density_OutputDataI_MarkerI_Left$z))
        .filled.contour(Weighted_Density_OutputDataI_MarkerI_Left$x,
                        Weighted_Density_OutputDataI_MarkerI_Left$y,
                        Weighted_Density_OutputDataI_MarkerI_Left$z,
                        levels = seq(from=0, to=ColorRangeMax, by=ColorRangeMax/21), col=c(rgb(red=1,green=1,blue=1,alpha=0), BlueToRedPalette(20,0.5))
        )
      } else if(dim(OutputDataI_MarkerI_Left)[1]<=2 && dim(OutputDataI_MarkerI_Right)[1]>2){
        ColorRangeMax<-max(c(Weighted_Density_OutputDataI_MarkerI_Right$z))
        .filled.contour(Weighted_Density_OutputDataI_MarkerI_Right$x,
                        Weighted_Density_OutputDataI_MarkerI_Right$y,
                        Weighted_Density_OutputDataI_MarkerI_Right$z,
                        levels = seq(from=0, to=ColorRangeMax, by=ColorRangeMax/21), col=c(rgb(red=1,green=1,blue=1,alpha=0), BlueToRedPalette(20,0.5))
        )
      }
      
    points(mean(OutputDataI$CC_X_Scaled),mean(OutputDataI$CC_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$DE_R_X_Scaled),mean(OutputDataI$DE_R_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$LE_R_X_Scaled),mean(OutputDataI$LE_R_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$VE_R_X_Scaled),mean(OutputDataI$VE_R_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$DE_L_X_Scaled),mean(OutputDataI$DE_L_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$LE_L_X_Scaled),mean(OutputDataI$LE_L_Y_Scaled), col="black", pch=3, cex=0.5)
    points(mean(OutputDataI$VE_L_X_Scaled),mean(OutputDataI$VE_L_Y_Scaled), col="black", pch=3, cex=0.5)
    
    # add legend title Left
    LegendLeft <- legend("topleft", legend = c(" "),inset=c(-0,-0.075),
                         xjust =0, yjust = 0,
                         title = "Avg Cell/Section (+/- SD) ; n Sections", cex=0.7, bty="n");
    
    # add legend Count Left
    text(LegendLeft$rect$left + LegendLeft$rect$w/2, LegendLeft$text$y,
         c( paste0(signif(mean(LeftCountsPerImage_MarkerI),3)," (+/- ",signif(sd(LeftCountsPerImage_MarkerI),3),") ; n = ",length(LeftCountsPerImage_MarkerI))),
         cex=0.7);
    
    # add legend title Right
    LegendRight <- legend("topright", legend = c(" "),inset=c(-0,-0.075),
                          xjust =0, yjust = 0,
                          title = "Avg Cell/Section (+/- SD) ; n Sections", cex=0.7, bty="n");
    
    # add legend Count right
    text(LegendRight$rect$left + LegendRight$rect$w/2, LegendRight$text$y,
         c( paste0(signif(mean(RightCountsPerImage_MarkerI),3)," (+/- ",signif(sd(RightCountsPerImage_MarkerI),3),") ; n = ",length(RightCountsPerImage_MarkerI))),
         cex=0.7);
    
    # add Count Title
    LegendBottom <- legend("bottom", legend = c(" ", " "),
                           text.width = strwidth("Left + Right = Total"),
                           xjust = 0.5, yjust = 0.5,
                           title = "Nb of Cells", cex=0.7, bty="n");
    # add Count subTitle
    text(LegendBottom$rect$left + LegendBottom$rect$w/2, LegendBottom$text$y,
         c("Left + Right = Total",""), cex=0.7);
    # add Count
    text(LegendBottom$rect$left + LegendBottom$rect$w/2, LegendBottom$text$y,
         c("",paste0(LeftCountOutputDataI_MarkerI," + ",RightCountOutputDataI_MarkerI," = ",TotalCountOutputDataI_MarkerI)),
         cex=0.7);
    # add marker legend
    legend("bottomleft", title="Marker",
           legend=Marker_NameI,bty="n",
           col="black",
           pch=1, cex=0.5)
    # add 0 0 axis
    par(xpd=FALSE)
    abline(v=0, lty=1, lwd=0.5, col="grey");
    abline(h=0, lty=1, lwd=0.5, col="grey");
    par(xpd=TRUE)
    # add data points 
    points(OutputDataI_MarkerI$X_Scaled, OutputDataI_MarkerI$Y_Scaled,
           pch=1,lwd=0.5, cex=0.5,
           col="black")
    dev.off()
  }## End of for Marker I
  
} ## End of for SubjectI



# Summarize Data Per Subject ----------------------------------------------
Subjects<-data.frame(Subject_ID=levels(OutputData$Subject_ID))
Markers<-data.frame(Marker_Name=levels(OutputData$Marker_Name_x_Channel))
Sides<-data.frame(Side=c("Left","Right"))
SummaryData<-merge(Subjects, Markers)
SummaryData<-merge(SummaryData, Sides)

for (RowI in 1:length(SummaryData$Subject_ID)){
Subject_IDI<-SummaryData$Subject_ID[RowI]
SummaryData$Group[RowI]<-unique(OutputData$Group[OutputData$Subject_ID==Subject_IDI])
}

for (RowI in 1:length(SummaryData$Subject_ID)){
  Subject_IDI<-SummaryData$Subject_ID[RowI]
  Marker_NameI<-SummaryData$Marker_Name[RowI]
  SideI<-SummaryData$Side[RowI]
  
  OutputData_SubjectI<-OutputData[OutputData$Subject_ID==Subject_IDI ,] # Get the Data of a given Subject
  OutputData_SubjectI$File_ID<-factor(OutputData_SubjectI$File_ID)
  Nb_Section<-length(levels(OutputData_SubjectI$File_ID))
  SummaryData$Nb_Section[RowI]<-Nb_Section
  
  OutputData_SubjectI_MarkerI<-OutputData_SubjectI[OutputData_SubjectI$Marker_Name_x_Channel==Marker_NameI ,] # Get the Data of a given Subject
  if(SideI=="Left"){
  OutputData_SubjectI_MarkerI_SideI<-OutputData_SubjectI_MarkerI[OutputData_SubjectI_MarkerI$X_Scaled<0,]
  } else {
    OutputData_SubjectI_MarkerI_SideI <-OutputData_SubjectI_MarkerI[OutputData_SubjectI_MarkerI$X_Scaled>=0,]
  }
  CountsPerImage_SubjectI_MarkerI_SideI<-c()
  for(FileI in 1:length(levels(OutputData_SubjectI_MarkerI_SideI$File_ID))){
    File_IDI<-levels(OutputData_SubjectI_MarkerI$File_ID)[FileI]
    Data_SectionI<-OutputData_SubjectI_MarkerI_SideI[OutputData_SubjectI_MarkerI_SideI$File_ID==File_IDI,]
    CountsPerImage_SubjectI_MarkerI_SideI<-c(CountsPerImage_SubjectI_MarkerI_SideI,dim(Data_SectionI)[1])
  }
  SummaryData$Nb_Cell_Total[RowI]<- mean(CountsPerImage_SubjectI_MarkerI_SideI)
  SummaryData$Nb_Cell_Section_Avg[RowI]<- mean(CountsPerImage_SubjectI_MarkerI_SideI)
  SummaryData$Nb_Cell_Section_Sd[RowI]<- sd(CountsPerImage_SubjectI_MarkerI_SideI)
  SummaryData$Center_X[RowI] <- mean(OutputData_SubjectI_MarkerI_SideI$X_Scaled)
  SummaryData$Center_Y[RowI] <- mean(OutputData_SubjectI_MarkerI_SideI$Y_Scaled)
  SummaryData$Spread_X[RowI] <- sd(OutputData_SubjectI_MarkerI_SideI$X_Scaled)
  SummaryData$Spread_Y[RowI] <- sd(OutputData_SubjectI_MarkerI_SideI$X_Scaled)
  
} ## End of Rowi

write.table(SummaryData, file=file.path(OutputDirPath, paste0("Summary_Data.txt")), row.names=FALSE, sep = "\t")

  


