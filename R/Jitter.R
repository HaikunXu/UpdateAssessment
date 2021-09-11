library(r4ss)

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
JitterDir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/Jitter/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

numjitter <- 10
dir.create(JitterDir)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:1) {
  for (s in 1:1) {
    if(converge[m,s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
      JitterPath <- paste0(JitterDir,model[m],"-",toString(steepness[s]),"/")
      
      # unlink(JitterPath, recursive = TRUE, force = TRUE)
      dir.create(JitterPath)
      files = c(
        paste0(Path, "/go_nohess.bat"),
        paste0(Path, "/starter.ss"),
        paste0(Path, "/forecast.ss"),
        paste0(Path, "/BET-EPO.ctl"),
        paste0(Path, "/BET-EPO.dat"),
        paste0(Path, "/ss.exe"),
        paste0(Path, "/ss.par")
      )
      file.copy(from = files, to = JitterPath)
      
      # start from the par file
      # starterFile <- readLines(paste0(JitterPath, "/starter.ss"), warn = F)
      # starterFile[6] <- toString(1) # start from initial condition
      # writeLines(starterFile, paste0(JitterPath, "/starter.ss"))
      
      jit.likes <- SS_RunJitter(
        mydir = JitterPath,
        Njitter = numjitter,
        extras = "-nox -cbs 4000000000 -gbs 4000000000 -ams 400000000 -maxfn 20000 -nohess",
        jitter_fraction = 0.05, init_values_src = 1)
      
      }
  }
}
