library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
NewDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Fstd)/"
SSDir <- "D:/OneDrive - IATTC/Git/UpdateAssessment/Document/Flimit/"
model <- c("R","RG","RM","RS","G","M1","M2","S","L","LG","LM","LS")
model_name <- c("R","R-GC","R-MA","R-DS","GC","MJ","MA","DS","L","L-GC","L-MA","L-DS")
model_name2 <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1:1) {
    Path <- paste0(Dir,model[m],"-",toString(steepness[s]))
    NewPath <- paste0(NewDir,model[m],"-",toString(steepness[s]))
    print(NewPath)
    
    if (converge[m, s]) {
      # copy old SS files to the new folder
      unlink(NewPath, recursive = TRUE, force = TRUE)
      dir.create(NewPath)
      
      print("Change the starter file to 1 (from par)!!!")
      files = c(paste0(Path, "/ss.par"),
                paste0(Path, "/go.bat"),
                paste0(SSDir, "/starter.ss"),
                paste0(Path, "/forecast.ss"),
                paste0(Path, "/BET-EPO.ctl"),
                paste0(Path, "/BET-EPO.dat"),
                paste0(SSDir, "/ss.exe"))
      
      file.copy(from = files, to = NewPath)
    }
    
    # run the model
    
    setwd(NewPath)
    command <- paste("cd", NewPath, "& go.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
  }
}
