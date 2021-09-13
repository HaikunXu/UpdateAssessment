library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
RetroDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/Retro/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

dir.create(RetroDir)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      RetroPath <- paste0(RetroDir,model[m],"-",toString(steepness[s]),"/")
      
      # unlink(JitterPath, recursive = TRUE, force = TRUE)
      dir.create(RetroPath)
      files = c(
        paste0(Path, "/go_nohess.bat"),
        paste0(Path, "/starter.ss"),
        paste0(Path, "/forecast.ss"),
        paste0(Path, "/control.ss_new"),
        paste0(Path, "/BET-EPO.dat"),
        paste0(Path, "/ss.exe"),
        paste0(Path, "/ss.par")
      )
      file.copy(from = files, to = RetroPath)
      file.rename(paste0(RetroPath, "/control.ss_new"),
                  paste0(RetroPath, "/BET-EPO.ctl"))
      # start from the par file
      # starterFile <- readLines(paste0(RetroPath, "/starter.ss"), warn = F)
      # starterFile[6] <- toString(1) # start from initial condition
      # writeLines(starterFile, paste0(RetroPath, "/starter.ss"))
      
      SS_doRetro(masterdir=RetroPath, 
                 oldsubdir="", 
                 newsubdir="retrospectives", 
                 years=seq(0,-20,-4),
                 CallType = "shell",
                 extras = "-nox -cbs 4000000000 -gbs 4000000000 -ams 400000000 -maxfn 20000 -nohess")
      
      retroModels <- SSgetoutput(dirvec=file.path(RetroPath, "retrospectives",paste("retro",seq(0,-20,-4),sep="")))
      retroSummary <- SSsummarize(retroModels)
      endyrvec <- retroSummary$endyrs + seq(0,-20,-4)
      SSplotComparisons(retroSummary, endyrvec=endyrvec, legendlabels=paste("Data",seq(0,-20,-4),"years"),
                        plot = FALSE, print = TRUE, plotdir = RetroPath)
      
      SSmohnsrho(retroSummary, endyrvec = endyrvec, startyr = retroSummary$startyrs, verbose = TRUE)
      
      }
  }
}