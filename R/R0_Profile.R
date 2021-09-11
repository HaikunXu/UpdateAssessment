library(r4ss)

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
R0Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/R0/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

dir.create(R0Dir)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      R0Path <- paste0(R0Dir,model[m],"-",toString(steepness[s]),"/")
      
      # unlink(JitterPath, recursive = TRUE, force = TRUE)
      dir.create(R0Path)
      files = c(
        paste0(Path, "/go_nohess.bat"),
        paste0(Path, "/starter.ss"),
        paste0(Path, "/forecast.ss"),
        paste0(Path, "/control.ss_new"),
        paste0(Path, "/BET-EPO.dat"),
        paste0(Path, "/ss.exe"),
        paste0(Path, "/ss.par")
      )
      file.copy(from = files, to = R0Path)
      
      starter <- SS_readstarter(paste0(R0Path, 'starter.ss'))
      starter$ctlfile <- "control_modified.ss"
      SS_writestarter(starter, dir=mydir, overwrite=TRUE)
      
      SS_profile(dir=R0Path, 
                 oldsubdir="", 
                 newsubdir="retrospectives", 
                 years=seq(0,-20,-4),
                 CallType = "shell",
                 extras = "-nox -cbs 4000000000 -gbs 4000000000 -ams 400000000 -maxfn 20000 -nohess")
      
    }
  }
}