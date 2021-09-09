library(r4ss)

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/"
NewDir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
SSDir <- "C:/Users/hkxu/OneDrive - IATTC/Git/UpdateAssessment/Document/Update/"
# Dir <- "D:/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/"
# NewDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
# SSDir <- "D:/OneDrive - IATTC/Git/UpdateAssessment/Document/Update/"
model <- c("R","RG","RM","RS","G","M1","M2","S","L","LG","LM","LS")
model_name <- c("R","R-GC","R-MA","R-DS","GC","MJ","MA","DS","L","L-GC","L-MA","L-DS")
model_name2 <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 3) {
  for (s in 3) {
    if (converge[m, s]) {
      Path <- paste0(Dir,model[m],"-",toString(steepness[s]))
      NewPath <- paste0(NewDir,model_name2[m],"-",toString(steepness[s]))
      print(NewPath)
    
      # copy old SS files to the new folder
      unlink(NewPath, recursive = TRUE, force = TRUE)
      dir.create(NewPath)
      files = c(
        paste0(SSDir, "/go_nohess.bat"),
        paste0(SSDir, "/go.bat"),
        paste0(SSDir, "/starter.ss"),
        paste0(SSDir, "/forecast.ss"),
        paste0(Path, "/control.ss_new"),
        paste0(Path, "/BET-EPO.dat"),
        paste0(SSDir, "/ss.exe")
      )
      file.copy(from = files, to = NewPath)
    
    # update data file
    # data <- SS_readdat_3.30(file = paste0(NewPath, "/BET-EPO.dat"), verbose = FALSE)
    # 
    # data$catch
    
    dat <- SS_readdat_3.30(file = paste0(NewPath, "/BET-EPO.dat"), verbose = FALSE)
    
    ctl <- SS_readctl_3.30(
      file = paste0(NewPath, "/control.ss_new"),
      verbose = FALSE,
      datlist = dat,
      use_datlist = TRUE
    )

    Selex <- ctl$size_selex_parms
    rows <- which(row.names(Selex) %in% c("SizeSel_Spine_Val_3_A1-LL-n(1)",
                                          "SizeSel_Spine_Val_3_A5-OBJ(16)",
                                          "SizeSel_Spine_Val_3_A2-NOADEL(19)"))
    Selex[rows,"PHASE"] <- 2
    
    ctl$size_selex_parms <- Selex
    SS_writectl_3.30(ctl,outfile = paste0(NewPath, "/BET-EPO.ctl"),overwrite = TRUE,verbose = FALSE)
    
    # run the model
    
    setwd(NewPath)
    command <- paste("cd", NewPath, "& go_noHess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    # check the max gradient
    myreplist <- SS_output(dir=NewPath,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
    print(paste0("Max gradient = ",myreplist$maximum_gradient_component))
    # }
    # else {
    #   starterFile <- readLines(paste0(NewPath, "/starter.ss"), warn = F)
    #   starterFile[6] <- toString(1) # start from initial condition
    #   writeLines(starterFile, paste0(NewPath, "/starter.ss"))
    #   
    #   print(paste0("Try 1: Max gradient = ",myreplist$maximum_gradient_component))
    #   
    #   command <- paste("cd", NewPath, "& go_noHess.bat", sep = " ")
    #   ss <- shell(cmd = command, intern = T, wait = T)
    #   
    #   myreplist <- SS_output(dir=NewPath,ncols=400,covar=F,verbose = FALSE, printstats = FALSE)
    #   print(paste0("Try 2: Max gradient = ",myreplist$maximum_gradient_component))
    # }
    
    if(myreplist$maximum_gradient_component < 0.1) {
      print("Hessian Run")
      command <- paste("cd", NewPath, "& go.bat", sep = " ")
      ss <- shell(cmd = command, intern = T, wait = T)
    }

    }
  }
}
