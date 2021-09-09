library(IATTCassessment)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
FstdDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Fstd)/"
FlimitDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Flimit)/"
dMSYDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(dmsy)/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")

steepness <- seq(1,0.7,-0.1)

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (s in 1:length(steepness)) {
  for (m in 1:length(model)) {
    Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
    FstdPath <- paste0(FstdDir,model[m],"-",toString(steepness[s]),"/")
    FlimitPath <- paste0(FlimitDir,model[m],"-",toString(steepness[s]),"/")
    dMSYPath <- paste0(dMSYDir,model[m],"-",toString(steepness[s]),"/")
    
    if(converge[m,s]) {
      print(Path)
      
      MT <- makeManagTable.new(Path, FFleets=1:23, FstdPath, FlimitPath, dMSYPath)
      if(m==1&s==1) {
        BET_Table <- data.frame(MT$ManagTable)
        names(BET_Table)[2] <- paste0(model_name[m],"-",toString(steepness[s]))
      }
      else {
        MT1 <- data.frame(MT$ManagTable[,2])
        names(MT1) <- paste0(model_name[m],"-",toString(steepness[s]))
        BET_Table <- cbind(BET_Table,MT1)
        # names(BET_Table)[1] <- paste0(model_name[m],"-",toString(steepness[s]))
      }
    }
    else {
      MT1 <- data.frame(matrix(NA,ncol=1,nrow = nrow(MT$ManagTable)))
      names(MT1) <- paste0(model_name[m],"-",toString(steepness[s]))
      BET_Table <- cbind(BET_Table,MT1)
    }
  }
}

BET_Table <- BET_Table[c(1,2,6,5,4,13,14,17,18,12,20,15,21,9,22,23),]

write.csv(BET_Table,file=paste0(Dir,"BET_Management_Table.csv"),row.names = FALSE)