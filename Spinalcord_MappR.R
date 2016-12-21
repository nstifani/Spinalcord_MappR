## Density Plot data from Spinalcord Mapper
## Script written by Nicolas Stifani contact nstifani@gmail.com
## Note: This Plugin Assume that Y coordinates are Inverted
## Manual CellCounter does not report "inverted Y". So the X=0 Y=0 is the top left corner.
## To Invert Non-Inverted Y Coordinates one must take the absolute of the Y Value minus Maximum Y value
## Y2=abs(Y-Max(Y))


# Functions ---------------------------------------------------------------
rm(list=ls()) # Clear the workspace just in case you have old stuff there

# Functions
InstallRequiredPackage.Function<-function(ListPackage){
  for (PackageI in 1:length(ListPackage)){ 
    RequiredPackageI<-ListPackage[PackageI]
    if (!is.element(RequiredPackageI, installed.packages()[,1])){
      install.packages(RequiredPackageI)
    }
    library(RequiredPackageI, character.only=TRUE) # Load the required packages
  } # Download required packages if there are not already there and load them
}

ErrorDialog.Function<-function(ErrorMessage, FixMessage){
  ErrorDialogChoice<-tk_messageBox(type = "retrycancel", message=paste0(ErrorMessage,"\n\n", FixMessage, "\n\n", "Click Retry to Try Again\n or \nCancel to Abort."), caption = "KinemaR Information", icon="question")
  if(ErrorDialogChoice=="cancel"){
    stop(paste0("Function Stopped because ", ErrorMessage))
  }
}

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

SelectDir.Function <- function(DialogMessage, DirPathObjectName, DirNameObjectName, ParentDirPathObjectName){
  DirPath<-tk_choose.dir(default=getwd(), caption=DialogMessage) # Prompt the user to select an inputdirectory
  DirName<-basename(DirPath) # Defines Name of Input directory
    assign(DirPathObjectName, DirPath, envir = .GlobalEnv) # Assign the Variable to the global environment
    assign(DirNameObjectName, DirName, envir = .GlobalEnv) # Assign the Variable to the global environment
    assign(ParentDirPathObjectName, dirname(DirPath), envir = .GlobalEnv)
}

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

SelectCSVFile.Function <- function(DialogMessage, DataObjectName){
  DataFilePath<-tk_choose.files(default = getwd(), caption = DialogMessage, filters=matrix(c("CSV File",".csv"),1,2, byrow=TRUE), multi=FALSE)
  Data<-read.table(DataFilePath, header=TRUE, sep=",")
  assign(DataObjectName, Data, envir=.GlobalEnv)
}

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




# Reading Data ------------------------------------------------------------
# Select the Input Data  
SelectCSVDir.Function(DialogMessage = "Choose the folder containing the CSV Data Files.",
                        DirPathObjectName="InputDirPath",
                        DirNameObjectName="InputDirName",
                        ListFilePathObjectName="ListInputFilePath",
                        ParentDirPathObjectName="ParentInputDirPath"
  )

# Merge all InputFiles into one
MergeCSVFileList.Function(ListCSVFilePath=ListInputFilePath, MergedObjectName="MergedInputData")


# Create OuputDirectory and Subdirectory
CreateOutputDir.Function(OutputDirLocation=ParentInputDirPath, 
                         OutputDirName=paste0(InputDirName,"_Coordinates_Processed"),
                         SubDirList=c("Graphs", "Tables") )

# Select the MetaData and Registrtion coordinates
SelectCSVFile.Function(DialogMessage="Select the MetaData and Registration Coordinates CSV File", DataObjectName = "MetaData")



# Process MetaData  ---------------------------------------------------
# Get the Resolution Unit as character
MetaData$Resolution_Unit<-as.character(MetaData$Resolution_Unit)

# Center the Metadata for each Row according to the CC position
for(RowI in 1:dim(MetaData)[1]){
  if(MetaData$Resolution_Unit[RowI]=="pixels") { # If coordinates are already in pixel just copy them 
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
    
    # Now we can center the Data on the CC coordinnates
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
    
    # Now that the coordinates are in pixel and centered we can scaled them
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
    
    
    List_Filename_Variables<- unlist(strsplit(as.character(MetaData$File_ID[RowI]),"_", fixed = TRUE))
    MetaData$Date<-List_Filename_Variables[1]
    MetaData$Subject_ID<-List_Filename_Variables[2]
    MetaData$Group<-List_Filename_Variables[3]
  
    if (length(List_Filename_Variables)>3){
      for (VariableI in 4:length(List_Filename_Variables)){
        MetaData[[paste0("Filename_Variable_", sprintf("%03d", as.numeric(VariableI)))]][RowI]  <- as.character(List_Filename_Variables[VariableI])
      }# Add the Variable from the filename to the Metadata table
    }  # If 
  } ## End of for each Row and center and scale the data


write.table(MetaData, file=file.path(OutputDirPath, "MetaData_Coordinates_Processed_v0.csv"), row.names=FALSE, sep = ",")

  
  
  
  
  
  
  
# Process Each File -------------------------------------------------------
# transform File_ID into factor
MergedInputData$File_ID<-as.factor(MergedInputData$File_ID)

# Process each level of File_ID aka process each File separately
for (FileI in 1:nlevels(MergedInputData$File_ID)){
  File_IDI<-levels(MergedInputData$File_ID)[FileI]
  
InputDataI<-MergedInputData[MergedInputData$File_ID==File_IDI,]
InputDataI$Channel<-as.character(InputDataI$Label)
for(RowI in 1:length(InputDataI$Label)){
  InputDataI$Channel[RowI]<- gsub(as.character(InputDataI$File_ID[RowI]), "", as.character(InputDataI$Label[RowI]))
  InputDataI$Channel[RowI]<- gsub(".tif:", "", as.character(InputDataI$Channel[RowI]))
}

  ## Get the coordinates from the MetaData
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
  
  
  # Bind the rows together
  OutputDataI<-rbind(InputDataI_D_R,InputDataI_V_R,InputDataI_V_L,InputDataI_D_L)
  
  
  if(FileI==1){
    OutputData<-OutputDataI
  } else {
    OutputData<-rbind(OutputData, OutputDataI)
  }
  
  write.table(OutputDataI, file=file.path(OutputDirPath, "Tables",paste0(File_IDI,".csv")), row.names=FALSE, sep = ",")
  
  
  # Plot scaled coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs", paste0(File_IDI,"_Scaled_Graph.pdf"))) # Open the graph as pdf
  plot(OutputDataI$X.Scaled, OutputDataI$Y.Scaled,
       pch=16,
       bty="n",
       #yaxt="n",
       #xaxp=c(0,600,3),
       ylim=c(-1,1),
       xlim=c(-1,1),
       type="p", col="deepskyblue",
       main=File_IDI, ylab="Relative position to CC", xlab="Relative position to CC", lwd=1)
  
  SummaryTable<-as.data.frame(table(OutputDataI$X.Scaled))
  SummaryTable$Var1<- as.numeric(as.character(SummaryTable$Var1))
  SummaryLeft<-SummaryTable[SummaryTable$Var1<0,]
  SummaryRight<-SummaryTable[SummaryTable$Var1>=0,]
  
  legend("topleft",
         paste0("Total Left: ",round(sum(SummaryLeft$Freq))),
         text.col=c("blue"),
         inset = .0,
         bty="n",
         cex=0.75,
         y.intersp=1.4) # Add legend
  legend("topright",
         paste0("Total Right: ",round(sum(SummaryRight$Freq))),
         text.col=c("red"),
         inset = .0,
         bty="n",
         cex=0.75,
         y.intersp=1.4) # Add legend
  
  points(MetaDataI$CC_X.Scaled,MetaDataI$CC_Y.Scaled, col="black", pch=3)
  points(MetaDataI$DE_R_X.Scaled,MetaDataI$DE_R_Y.Scaled, col="black", pch=3)
  points(MetaDataI$LE_R_X.Scaled,MetaDataI$LE_R_Y.Scaled, col="black", pch=3)
  points(MetaDataI$VE_R_X.Scaled,MetaDataI$VE_R_Y.Scaled, col="black", pch=3)
  points(MetaDataI$DE_L_X.Scaled,MetaDataI$DE_L_Y.Scaled, col="black", pch=3)
  points(MetaDataI$LE_L_X.Scaled,MetaDataI$LE_L_Y.Scaled, col="black", pch=3)
  points(MetaDataI$VE_L_X.Scaled,MetaDataI$VE_L_Y.Scaled, col="black", pch=3)
  
  
  dev.off() # Close and save the graph
  
  
  # Plot RAW coordinates for each file
  cairo_pdf(file.path(OutputDirPath, "Graphs", paste0(File_IDI,"_Raw_Graph.pdf"))) # Open the graph as pdf
  plot(OutputDataI$X.Pixel.Centered, OutputDataI$Y.Pixel.Centered,
       pch=16,
       bty="n",
       #yaxt="n",
       #xaxp=c(0,600,3),
      ylim=c(-MetaDataI$Total_Height_Pixels/2,MetaDataI$Total_Height_Pixels/2),
      xlim=c(-MetaDataI$Total_Width_Pixels/2,MetaDataI$Total_Width_Pixels/2),
      type="p", col="deepskyblue",
       main=File_IDI, ylab="Relative position to CC (pixel)", xlab="Relative position to CC (pixel)", lwd=1)
    
  points(MetaDataI$CC_X.Pixel.Centered,MetaDataI$CC_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$DE_R_X.Pixel.Centered,MetaDataI$DE_R_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$LE_R_X.Pixel.Centered,MetaDataI$LE_R_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$VE_R_X.Pixel.Centered,MetaDataI$VE_R_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$DE_L_X.Pixel.Centered,MetaDataI$DE_L_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$LE_L_X.Pixel.Centered,MetaDataI$LE_L_Y.Pixel.Centered, col="black", pch=3)
  points(MetaDataI$VE_L_X.Pixel.Centered,MetaDataI$VE_L_Y.Pixel.Centered, col="black", pch=3)
  
  
  
  dev.off() # Close and save the graph
  

  
  
} # End of process each file



# Bring the Group and resolution from the Metadata to the output data
OutputData$File_ID<-as.character(OutputData$File_ID)
MetaData$Group<-as.character(MetaData$Group)

for(RowI in 1: dim(OutputData)[1]){
  File_IDRowI<-OutputData$File_ID[RowI]
  Group_RowI<-subset(MetaData, pmatch(MetaData$File_ID,File_IDRowI)==1, Group)
  OutputData$Group[RowI]<-as.character(Group_RowI)
}

OutputData$Group<-as.factor(as.character(OutputData$Group))
OutputData$File_ID<-as.factor(OutputData$File_ID)
MetaData$Group<-as.factor(MetaData$Group)

write.table(OutputData, file=file.path(OutputDirPath, "Data_Coordinates_Processed.csv"), row.names=FALSE, sep = ",")

write.table(MetaData, file=file.path(OutputDirPath, "MetaData_Coordinates_Processed.csv"), row.names=FALSE, sep = ",")



