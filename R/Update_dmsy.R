library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
NewDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(dmsy)/"
SSDir <- "D:/OneDrive - IATTC/Git/UpdateAssessment/Document/dmsy/"
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
    NewPath <- paste0(NewDir,model_name[m],"-",toString(steepness[s]))
    print(NewPath)
    
    if (converge[m, s]) {

      # step 1: creata a new folder for each run
      unlink(NewPath, recursive = TRUE, force = TRUE)
      dir.create(NewPath)
      files = c(paste0(Path, "/go_nohess.bat"),
                paste0(SSDir, "/starter.ss"),
                paste0(Path, "/BET-EPO.ctl"),
                paste0(Path, "/BET-EPO.dat"),
                paste0(SSDir, "/ss.exe"))
      file.copy(from = files, to = NewPath)
      
      # step 2: change par file
      print("Change the starter file to 1 (from par)!!!")
      
      ParDir <- paste0(Path, "ss.par")
      ParFile <- readLines(ParDir, warn = F)
      
      Rep <- r4ss::SS_output(dir = Path, ncols = 400, covar = T, printstats = F, verbose = FALSE)
      Recruit <- Rep$recruit$dev[which(Rep$recruit$era=="Main")]
      bias_adjust <- -Rep$recruit$biasadjuster[which(Rep$recruit$era=="Main")]*0.6^2/2
      Recruit_forecast <- c(Recruit + bias_adjust,0)
      
      Line <- match("# Fcast_recruitments:", ParFile)
      Line_error <- match("# Fcast_impl_error:", ParFile)
      ParFile[Line + 1] <- gsub(",", "", toString(Recruit_forecast))
      ParFile[Line_error + 1] <- gsub(",", "", toString(Recruit_forecast*0.0))
      writeLines(ParFile, paste0(NewPath, "/ss.par"))
      
      # step 3: change forecast file
      ForecastDir <- paste0(SSDir, "forecast.ss")
      ForecastFile <- readLines(ForecastDir, warn = F)
      
      ForecastFile[13] <- toString(ifelse(m %in% seq(5,8),81,165))
      writeLines(ForecastFile, paste0(NewPath, "/forecast.ss"))
      
      # step 4: run ss
      setwd(NewPath)
      command <- paste("cd", NewPath, "& go_noHess.bat", sep = " ")
      ss <- shell(cmd = command, intern = T, wait = T)
  }
}
