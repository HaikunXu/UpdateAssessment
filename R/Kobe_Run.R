# tested on Sep. 8 2021
# the starter file needs to be put in the Kobe folder before running this code

library(IATTCassessment)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
KobeDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/Kobe/"
DynamicDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(dmsy)/"
SSDir <- "D:/OneDrive - IATTC/Git/UpdateAssessment/Document/Kobe/"

# lyear <- 2019 # last year
FFleets <- c(1:23) # fishery fleets
STD_only <- FALSE # Kobe table is generated
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

dir.create(KobeDir)

for (m in 1:3) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      KobePath <- paste0(KobeDir,model[m],"-",toString(steepness[s]),"/")
      DynamicPath <- paste0(DynamicDir,model[m],"-",toString(steepness[s]),"/")
      print(Path)
      # fyear <- ifelse(m %in% seq(5,8), 2000, 1979) # first year
      # step 1: copy ss file
      unlink(KobePath, recursive = TRUE, force = TRUE)
      dir.create(KobePath)
      files = c(paste0(Path, "/go_nohess.bat"),
                paste0(SSDir, "/starter.ss"),
                paste0(Path, "/forecast.ss"),
                paste0(Path, "/BET-EPO.ctl"),
                paste0(Path, "/BET-EPO.dat"),
                paste0(Path, "/Report.sso"),
                paste0(Path, "/CompReport.sso"),
                paste0(Path, "/ss.par"),
                paste0(Path, "/ss.exe"))
      file.copy(from = files, to = KobePath)
      
      # step 2: run trajectory
      Kobe.Out <- make_kobetable_SAC11(Path,KobePath,FFleets,STD_only,newSS=TRUE, Path, Path, DynamicPath)
    }
  }
}
