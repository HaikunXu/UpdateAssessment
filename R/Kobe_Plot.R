library(IATTCassessment)
library(r4ss)

Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
FstdDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Fstd)/"
FlimitDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(Flimit)/"
dMSYDir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS(dmsy)/"
model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")

steepness <- seq(1,0.7,-0.1)
converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0
fyear <- 1979 # first year
lyear <- 2019 # last year
FFleets <- c(1:23) # fishery fleets
STD_only <- TRUE # no Kobe table is generated

for (m in 1:length(model)) {
  if(m %in% 9:12) fyear <- 2000 # first year
  for (s in 1:length(steepness)) {
    BasePath <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
    FstdPath <- paste0(FstdDir,model_name[m],"-",toString(steepness[s]),"/")
    FlimitPath <- paste0(FlimitDir,model_name[m],"-",toString(steepness[s]),"/")
    DynamicPath <- paste0(dMSYDir,model_name[m],"-",toString(steepness[s]),"/")
    KobePath=""
    print(BasePath)
    if(converge[m,s]) {
      Kobe.Out <- make_kobetable_SAC11(BasePath,KobePath="",FFleets,STD_only=TRUE,newSS=TRUE,FstdPath = FstdPath,FlimitPath = FlimitPath, DynamicPath = DynamicPath)
      if(m==1&s==1) {
        Mgmt <- data.frame("F_low"=Kobe.Out$STD$FrecentFmsy[1],
                           "F"=Kobe.Out$STD$FrecentFmsy[2],
                           "F_high"=Kobe.Out$STD$FrecentFmsy[3],
                           "SB_low"=Kobe.Out$STD$SB[1],
                           "SB"=Kobe.Out$STD$SB[2],
                           "SB_high"=Kobe.Out$STD$SB[3],
                           "Model"=model_name[m],
                           "Steepness"=steepness[s])
      }
      else {
        Mgmt <- rbind(Mgmt,data.frame("F_low"=Kobe.Out$STD$FrecentFmsy[1],
                                      "F"=Kobe.Out$STD$FrecentFmsy[2],
                                      "F_high"=Kobe.Out$STD$FrecentFmsy[3],
                                      "SB_low"=Kobe.Out$STD$SB[1],
                                      "SB"=Kobe.Out$STD$SB[2],
                                      "SB_high"=Kobe.Out$STD$SB[3],
                                      "Model"=model_name[m],
                                      "Steepness"=steepness[s]))
      }
    }
  }
}

Mgmt$Steepness <- as.factor(Mgmt$Steepness)

d=data.frame(x1=c(0,0,1,1), x2=c(1,1,4,4), y1=c(0,1,1,0), y2=c(1,4,4,1), r=c(1,2,3,4))
limit <- data.frame("F"=1.787855, "SB"=0.353868)

ggplot(data=Mgmt) +
  geom_rect(data=d %>% filter(r==1), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="yellow", color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==2), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="red", color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==3), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="yellow",color="white", alpha=0.5) +
  geom_rect(data=d %>% filter(r==4), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),  fill="green",color="white", alpha=0.5) +
  geom_point(aes(x=SB,y=F,color=Steepness,shape=Steepness),stroke = 2,size=2) +
  scale_shape_manual(values=c(1,2,4,5)) +
  geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high,color=Steepness),width=0.01,alpha=0.5) +
  geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F,color=Steepness),alpha=0.5) +
  geom_hline(yintercept = 1.787855,linetype="dashed",size=1) +
  geom_vline(xintercept = 0.353868,linetype="dashed",size=1) +
  scale_colour_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
  theme_bw(18) +
  xlab("S/Smsy") +
  ylab("F/Fmsy") +
  coord_cartesian(xlim = c(0,3), ylim = c(0,3), expand = FALSE)

ggsave(file = paste0(Dir, "Kobe plot.png"), width = 10, height = 8)
ggsave(file = paste0(Dir, "Kobe plot.eps"), width = 10, height = 8, device = cairo_ps)
write.csv(Mgmt,file = paste0(Dir,"Mgmt.csv"),row.names = FALSE)

# ##### add overall values
# target <-  data.frame("F"=0.998, "F_low"=0.395,"F_high"=2.042,"SB"=0.917,"SB_low"=0.251,"SB_high"=2.515)
# limit <- data.frame("F"=1.787855, "SB"=0.353868)
# 
# ggplot(data=Mgmt) +
#   geom_rect(data=d %>% filter(r==1), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="yellow", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==2), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="red", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==3), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="yellow",color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==4), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),  fill="green",color="white", alpha=0.5) +
#   geom_point(aes(x=SB,y=F,color=Steepness,shape=Steepness),stroke = 2,size=2) +
#   scale_shape_manual(values=c(1,2,4,5)) +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high,color=Steepness),width=0.01,alpha=0.5) +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F,color=Steepness),alpha=0.5) +
#   geom_hline(yintercept = 1.787855,linetype="dashed",size=1) +
#   geom_vline(xintercept = 0.353868,linetype="dashed",size=1) +
#   scale_colour_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
#   theme_bw(18) +
#   xlab("S/Smsy") +
#   ylab("F/Fmsy") +
#   coord_cartesian(xlim = c(0,3), ylim = c(0,3), expand = FALSE) +
#   geom_point(aes(x=SB,y=F),data=target,size=6) +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high),size=1,width=0.2,data=target) +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F),size=1,height=0.2,data=target)
# 
# ggsave(file = paste0(Dir, "Kobe plot2.png"), width = 10, height = 8)
# ggsave(file = paste0(Dir, "Kobe plot2.eps"), width = 10, height = 8, device = cairo_ps)
# 
# 
# pessimistic <-  data.frame("F"=1.432, "F_low"=0.95,"F_high"=2.193,"SB"=0.521,"SB_low"=0.182,"SB_high"=0.871)
# opotimistic <-  data.frame("F"=0.693, "F_low"=0.354,"F_high"=1.231,"SB"=1.507,"SB_low"=0.73,"SB_high"=2.696)
# 
# ggplot(data=Mgmt) +
#   geom_rect(data=d %>% filter(r==1), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="yellow", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==2), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="red", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==3), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="yellow",color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==4), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),  fill="green",color="white", alpha=0.5) +
#   geom_point(aes(x=SB,y=F,color=Steepness,shape=Steepness),stroke = 2,size=2) +
#   scale_shape_manual(values=c(1,2,4,5)) +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high,color=Steepness),width=0.01,alpha=0.5) +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F,color=Steepness),alpha=0.5) +
#   geom_hline(yintercept = 1.787855,linetype="dashed",size=1) +
#   geom_vline(xintercept = 0.353868,linetype="dashed",size=1) +
#   scale_colour_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
#   theme_bw(18) +
#   xlab("S/Smsy") +
#   ylab("F/Fmsy") +
#   coord_cartesian(xlim = c(0,3), ylim = c(0,3), expand = FALSE) +
#   geom_point(aes(x=SB,y=F),data=target,size=6) +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high),size=1,width=0.2,data=target) +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F),size=1,height=0.2,data=target) +
#   geom_point(aes(x=SB,y=F),data=pessimistic,size=6,color="purple") +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high),size=1,width=0.2,data=pessimistic,color="purple") +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F),size=1,height=0.2,data=pessimistic,color="purple") +
#   geom_point(aes(x=SB,y=F),data=opotimistic,size=6,color="darkgreen") +
#   geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high),size=1,width=0.2,data=opotimistic,color="darkgreen") +
#   geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F),size=1,height=0.2,data=opotimistic,color="darkgreen")
# 
# ggsave(file = paste0(Dir, "Kobe plot3.png"), width = 10, height = 8)
# ggsave(file = paste0(Dir, "Kobe plot3.eps"), width = 10, height = 8, device = cairo_ps)


# ###### Kobe trajectory
# Dir_Kobe <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Kobe/"
# for (m in 1:length(model)) {
#   if(m %in% 9:12) fyear <- 2000 # first year
#   else fyear <- 1979
#   for (s in 1:length(steepness)) {
#     Path <- paste0(Dir_Kobe,model_name[m],"-",toString(steepness[s]),"/")
#     # print(Path)
#     if(converge[m,s]) {
#       Kobe <- read.csv(paste0(Path,"KobePlotOut.csv"))
#       if(m==1&s==1) {
#         F_ts <- data.frame("F_mult"=as.numeric(Kobe[9,2:ncol(Kobe)]),
#                            "Year"=seq(fyear+2,lyear),
#                            "Model"=model_name2[m],
#                            "Steepness"=steepness[s])
#         SB_ts <- data.frame("SB"=as.numeric(Kobe[6,2:ncol(Kobe)]),
#                             "Year"=seq(fyear+2,lyear),
#                             "Model"=model_name2[m],
#                             "Steepness"=steepness[s])
#       }
#       else {
#         F_ts <- rbind(F_ts,data.frame("F_mult"=as.numeric(Kobe[9,2:ncol(Kobe)]),
#                                       "Year"=seq(fyear+2,lyear),
#                                       "Model"=model_name2[m],
#                                       "Steepness"=steepness[s]))
#         SB_ts <- rbind(SB_ts,data.frame("SB"=as.numeric(Kobe[6,2:ncol(Kobe)]),
#                                         "Year"=seq(fyear+2,lyear),
#                                         "Model"=model_name2[m],
#                                         "Steepness"=steepness[s]))
#       }
#     }
#   }
# }
# 
# F_ts$Steepness <- factor(F_ts$Steepness)
# SB_ts$Steepness <- factor(SB_ts$Steepness)
# 
# ggplot(data=F_ts) +
#   geom_line(aes(x=Year,y=log10(F_mult),color=Steepness)) +
#   # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
#   geom_hline(yintercept=0,linetype="dashed") +
#   theme_bw(15) +
#   ylab("Fmultiplier") +xlab("") +
#   facet_wrap(~Model,nrow=4) +
#   scale_y_continuous(breaks = seq(-2,1.5,0.25),labels = round(10^seq(-2,1.5,0.25),1))
# 
# ggsave(file = paste0(Dir,"F_mult_trend.png"), width = 10, height = 10)
# ggsave(file = paste0(Dir,"F_mult_trend.eps"), width = 10, height = 10)
# 
# ggplot(data=SB_ts) +
#   geom_line(aes(x=Year,y=log10(SB),color=Steepness)) +
#   # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
#   geom_hline(yintercept=0,linetype="dashed") +
#   theme_bw(15) +
#   ylab("SB/SBmsy") +xlab("") +
#   facet_wrap(~Model,nrow=4) +
#   scale_y_continuous(breaks = seq(-1,0.5,0.25),labels = round(10^seq(-1,0.5,0.25),1))
# ggsave(file = paste0(Dir,"SB_trend.png"), width = 10, height = 10)
# ggsave(file = paste0(Dir,"SB_trend.eps"), width = 10, height = 10)
# 
# Kobe <- left_join(F_ts,SB_ts) %>% mutate(Decade=floor(Year/10)*10)
# d=data.frame(x1=c(0,0,1,1), x2=c(1,1,12,12), y1=c(0,1,1,0), y2=c(1,12,12,1), r=c(1,2,3,4))
# 
# ggplot(data=Kobe) +
#   geom_rect(data=d %>% filter(r==1), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="yellow", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==2), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),fill="red", color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==3), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill="yellow",color="white", alpha=0.5) +
#   geom_rect(data=d %>% filter(r==4), mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2),  fill="green",color="white", alpha=0.5) +
#   geom_line(aes(x=SB,y=1/F_mult,color=Steepness,alpha=Decade),size=1.5) +
#   # geom_point(aes(x=SB,y=1/F_mult,color=Steepness,alpha=Year),size=1.5) +
#   geom_point(aes(x=SB,y=1/F_mult,fill=Steepness),color="white",stroke=2,size=4,shape=22,data=Kobe%>%filter(Year==ifelse(substring(Model,1,3) == "Srt",2002,1981))) +
#   geom_point(aes(x=SB,y=1/F_mult,fill=Steepness),color="white",stroke=2,size=4,shape=21,data=Kobe%>%filter(Year==lyear)) +
#   # geom_errorbar(aes(x=SB,ymin=F_low, ymax=F_high,color=Steepness),width=0.001,data=Mgmt,size=1) +
#   # geom_errorbarh(aes(xmin=SB_low,xmax=SB_high,y=F,color=Steepness),data=Mgmt,size=1) +
#   # geom_hline(yintercept = 1.6,linetype="dashed") +
#   # geom_vline(xintercept = 0.38,linetype="dashed") +
#   scale_fill_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
#   scale_color_manual(values = c("cyan","deepskyblue","blue","darkblue")) + # https://www.r-graph-gallery.com/ggplot2-color.html
#   theme_bw(20) +
#   facet_wrap(~Model,nrow=4) +
#   xlab("S/Smsy") +
#   ylab("F/Fmsy") +
#   coord_cartesian(xlim = c(0,7), ylim = c(0,3), expand = FALSE)
# 
# ggsave(file = paste0(Dir, "Kobe trajactory.png"), width = 14, height = 14)
# ggsave(file = paste0(Dir, "Kobe trajactory.eps"), width = 14, height = 14, device = cairo_ps)
# 


### MSY and SMSY
Dir_Kobe <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Kobe/"
for (m in 1:length(model)) {
  if(m %in% 9:12) fyear <- 2000 # first year
  else fyear <- 1979
  for (s in 1:length(steepness)) {
    Path <- paste0(Dir_Kobe,model_name[m],"-",toString(steepness[s]),"/")
    # print(Path)
    if(converge[m,s]) {
      Kobe <- read.csv(paste0(Path,"KobePlotOut.csv"))
      if(m==1&s==1) {
        MSY_ts <- data.frame("MSY"=as.numeric(Kobe[1,2:ncol(Kobe)]),
                             "Year"=seq(fyear+2,lyear),
                             "Model"=model_name2[m],
                             "Steepness"=steepness[s])
        SMSY_ts <- data.frame("SMSY"=as.numeric(Kobe[3,2:ncol(Kobe)]),
                              "Year"=seq(fyear+2,lyear),
                              "Model"=model_name2[m],
                              "Steepness"=steepness[s])
      }
      else {
        MSY_ts <- rbind(MSY_ts,data.frame("MSY"=as.numeric(Kobe[1,2:ncol(Kobe)]),
                                          "Year"=seq(fyear+2,lyear),
                                          "Model"=model_name2[m],
                                          "Steepness"=steepness[s]))
        SMSY_ts <- rbind(SMSY_ts,data.frame("SMSY"=as.numeric(Kobe[3,2:ncol(Kobe)]),
                                            "Year"=seq(fyear+2,lyear),
                                            "Model"=model_name2[m],
                                            "Steepness"=steepness[s]))
      }
    }
  }
}

MSY_ts$Steepness <- factor(MSY_ts$Steepness)
SMSY_ts$Steepness <- factor(SMSY_ts$Steepness)

f1 <- ggplot(data=MSY_ts %>% filter(Steepness==1)) +
  geom_line(aes(x=Year,y=MSY,color=Model)) +
  # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
  # geom_hline(yintercept=0,linetype="dashed") +
  theme_bw(15) +
  ylab("MSY (tons)") +xlab("") +
  coord_cartesian(ylim=c(0,4e5),expand = FALSE)


f2 <- ggplot(data=SMSY_ts %>% filter(Steepness==1)) +
  geom_line(aes(x=Year,y=SMSY,color=Model)) +
  # geom_point(aes(x=Year,y=F_mult,shape=Steepness,color=Model)) +
  # geom_hline(yintercept=0,linetype="dashed") +
  theme_bw(15) +
  ylab("SMSY (tons)") +xlab("")+
  # scale_y_continuous(breaks = seq(4.5,6,0.25),labels = round(10^seq(4.5,6,0.25),1)) +
  coord_cartesian(ylim=c(0,1.5e5),expand = FALSE)

f_all <- gridExtra::grid.arrange(f1, f2, nrow = 2)

ggsave(f_all, file = paste0(Dir,"MSY_trend.png"), width = 8, height = 8)
ggsave(f_all, file = paste0(Dir,"MSY_trend.eps"), width = 8, height = 8)
