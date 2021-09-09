library(tidyverse)
library(r4ss)

model <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel",
           "Gro","Mov","Mrt","Sel",
           "Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")

for (m in 1:12) {
  if(m==1) Dir <- paste0("D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/",
                         model[m],"-",toString(1),"/")
  else Dir <- c(Dir,paste0("D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/",
                           model[m],"-",toString(1),"/"))
}

Save_Dir <- "D:/OneDrive - IATTC/IATTC/2021/UpdateAssessment/SS/"
Last_Year <- 2019
xlim <- c(1978,2021)
ylim <- c(0,1)

# FAA
for (i in 1:length(model)) {
  print(i)
  myreplist = r4ss::SS_output(dir = Dir[i], ncols = 500, covar = F, verbose = FALSE, printstats = FALSE)
  
  Z <- myreplist$Z_at_age
  M <- myreplist$M_at_age
  # M_Matrix <- rbind(matrix(rep(data.matrix(M[1, ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T), matrix(rep(data.matrix(M[2, 
  #     ]), nrow(Z)/2), nrow = nrow(Z)/2, byrow = T))
  
  F_M <- Z
  F_M[, 4:43] <- Z[, 4:43] - data.matrix(M[, 4:43])
  
  F_Matrix <- F_M %>% gather("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                             "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                             "34", "35", "36", "37", "38", "39", "40", key = "Age", value = "FAA")
  F_Matrix$Age <- as.numeric(F_Matrix$Age)
  F_Matrix$Year2 <- ceiling(F_Matrix$Yr/4) + 1974
  
  # F_Matrix <- na.omit(F_Matrix %>% mutate('Group'=cut(Age, breaks = c(-1,4,8,12,20,39))))
  
  F_vector <- F_Matrix %>% group_by(Sex, Year2, Age) %>% summarise(F_annual = sum(FAA)) %>% mutate(Age = cut(Age, 
                                                                                                             breaks = c(0, 4, 8, 12, 19, 40), labels = c("1-4 quarters", "5-8 quarters", "9-12 quarters", "13-19 quarters", "20+ quarters")))
  F_vector <- na.omit(F_vector)
  
  if(i==1) {
    FAA <- F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=Model[i])
  } else {
    FAA <- rbind(FAA, F_vector %>% group_by(Age, Year2) %>% summarise(F_group = mean(F_annual)) %>% mutate(Model=Model[i]))
  }
}

Weight <- data.frame("Model"=Model,"Weight"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
data <- left_join(FAA,Weight) %>% group_by(Age,Year2) %>%
  mutate(Weight2=Weight/sum(Weight))
Data <- data %>% filter(Year2 <= Last_Year) %>% group_by(Age,Year2) %>%
  mutate(Combined=sum(F_group*Weight2))

f <- ggplot(data = Data) +
  geom_line(aes(x = Year2, y = F_group, color=Model)) +
  geom_point(aes(x = Year2, y = Combined),data=Data) +
  # geom_line(aes(x = Year2, y = Combined),data=Data %>% filter(Year2>1999),size=1.5) +
  facet_wrap(~Age,nrow=5) +
  theme_bw(20) + ylab("Average annual F") + xlab("Year") +
  coord_cartesian(xlim=xlim,ylim=ylim,expand=FALSE)

ggsave(f,file = paste0(Save_Dir, "faa_a.png"), width = 12, height = 15)
ggsave(f,file = paste0(Save_Dir, "faa_a.pdf"), width = 12, height = 15)


# SBR
lyear <- rep(2019,12) # last assessment year
fyear <- c(rep(1979,8),rep(2000,4)) # first assessment year
legend <- Model
SS_Dir <- Dir
Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Comparison/"
ymax=1.25

for (i in 1:length(lyear)) {
  cor_mat <- read.table(paste0(SS_Dir[i], "ss.std"), skip = 1, fill = NA, header = FALSE)
  names(cor_mat) <- c("index","name","value","std.dev")
  SB_est <- cor_mat$value[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
  SB_std <- cor_mat$std.dev[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
  # if(sum(SB_std)==0) SB_std <- cor_mat$std_dev[which(cor_mat$name == "depletion")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
  SB <- data.frame(est = SB_est, std = SB_std, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))
  
  if(i==1) SB_A <- SB %>% mutate(Model=legend[i])
  else SB_A <- rbind(SB_A,SB %>% mutate(Model=legend[i]))
}

SB_A <- SB_A %>% data.frame() %>% mutate(Model=factor(Model))

Weight <- data.frame("Model"=Model,
                     "Weight"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11),
                     "Group"=c("Pessimistic","Optimistic","Pessimistic","Optimistic","Optimistic","Optimistic","Optimistic","Optimistic","Pessimistic","Pessimistic","Pessimistic","Pessimistic"))

data <- left_join(SB_A,Weight) %>% group_by(yq) %>%
  mutate(Weight2=Weight/sum(Weight))

Data <- data %>% group_by(yq) %>%
  mutate(Combined=sum(est*Weight2))

data2 <- left_join(SB_A,Weight) %>% group_by(Group,yq) %>%
  mutate(Weight2=Weight/sum(Weight))

Data2 <- data2 %>% group_by(Group,yq) %>%
  mutate(Combined=sum(est*Weight2))

ggplot(data = Data) + 
  geom_ribbon(aes(x = yq, ymin = est - 1.96 * std, ymax = est + 1.96 * std, fill = Model), alpha=0.1) +
  geom_line(aes(x = yq, y = est, color = Model,linetype=Group), size = 1.2) +
  geom_line(aes(x = yq, y = Combined,linetype=Group), size = 2,data=Data2) +
  # geom_line(aes(x = yq, y = Combined), size = 1.5,data=Data2 %>% filter(Group=="Optimistic"),alpha=0.5) +
  # geom_point(aes(x = yq, y = est, color = Model), size = 2,data = SB_A %>% filter(yq==year)) +
  theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(aes(x = yq, y = Combined), size = 3, shape = 17) +
  coord_cartesian(ylim = c(0,1.25),xlim=c(1978,2021),expand = FALSE)

ggsave(file = paste0(Save_Dir, "SBR.png"), width = 14, height = 12)

ggsave(file = paste0(Save_Dir, "SBR.pdf"), width = 14, height = 12)  


# Relative recruitment
lyear <- rep(2019,12) # last assessment year
fyear <- c(rep(1979,8),rep(2000,4)) # first assessment year
legend <- Model
SS_Dir <- Dir
Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Comparison/"
# ymax=1.25

for (i in 1:length(lyear)) {
  cor_mat <- read.table(paste0(SS_Dir[i], "ss.cor"), skip = 1, fill = NA, header = TRUE)
  R_est <- cor_mat$value[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
  R_std <- cor_mat$std.dev[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
  if(sum(R_std)==0) R_std <- cor_mat$std_dev[which(cor_mat$name == "recr_std")[3:((lyear[i] - (fyear[i]-1)) * 4 + 2)]]
  R <- data.frame(est = R_est, std = R_std, year = rep(fyear[i]:lyear[i], each = 4), yq = seq(fyear[i], lyear[i] + 0.75, 0.25))
  R_annual <- R %>% group_by(year) %>% summarise(Est = sum(est), Std = NA)
  # cov_mat <- matrix(NA, nrow = length(R_est), ncol = 4)
  index_init <- which(cor_mat$name == "recr_std")[3]
  
  for (y in fyear[i]:lyear[i]) {
    index_year <- 0:3 + index_init + (y - fyear[i]) * 4
    cor_y <- cor_mat[index_year, index_year + 4]
    var_y <- R$std[((y - fyear[i]) * 4 + 1):((y - (fyear[i]-1)) * 4)] %*% t(R$std[((y - fyear[i]) * 4 + 1):((y - (fyear[i]-1)) * 4)]) * 
      cor_y
    var_y[1, 2:4] <- var_y[2:4, 1]
    var_y[2, 3:4] <- var_y[3:4, 2]
    var_y[3, 4] <- var_y[4, 3]
    var_y <- data.matrix(var_y)
    # cov_mat[((y-1975)*4+1):((y-1974)*4),1:4] <- var_y
    R_annual$Std[which(R_annual$year == y)] <- sqrt(matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) %*% var_y %*% matrix(c(1, 
                                                                                                                     1, 1, 1), nrow = 4, ncol = 1))
  }
  
  R_quarterly <- R %>% mutate(R = est/mean(est), STD = std/est, Model=legend[i])
  R_annual <- R_annual %>% mutate(R = Est/mean(Est), STD = Std/Est, Model=legend[i])
  
  if(i==1) {
    R_Q <- R_quarterly
    R_A <- R_annual
  }
  else
  {
    R_Q <- rbind(R_Q,R_quarterly)
    R_A <- rbind(R_A,R_annual)
  }
}

R_A <- R_A %>% data.frame() %>% mutate(Model=factor(Model))

Weight <- data.frame("Model"=Model,"Weight"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
data <- left_join(R_A,Weight) %>% group_by(year) %>%
  mutate(Weight2=Weight/sum(Weight))
Data <- data %>% group_by(year) %>%
  mutate(Combined=sum(R*Weight2))


ggplot(data = R_A) + geom_ribbon(aes(x = year, ymin = R * exp(-1.96 * STD), ymax = R * exp(1.96 * STD), fill = Model), alpha=0.1) +
  geom_line(aes(x = year, y = R, color = Model), size = 1.2) +
  theme_bw(20) + xlab("") + ylab("") + geom_hline(yintercept = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(0,3),xlim=c(1978,2021),expand = FALSE) +
  geom_line(aes(x = year, y = Combined),data=Data, size = 1.5)

ggsave(file = paste0(Save_Dir, "R.pdf"), width = 12, height = 10)




### Multi-model average
# R
Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/"

model <- c("R","RG","RM","RS","G","M1","M2","S","L","LG","LM","LS")
model_name <- c("R","R-GC","R-MA","R-DS","GC","MJ","MA","DS","L","L-GC","L-MA","L-DS")
model_name2 <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

lyear <- rep(2019,12) # last assessment year
fyear <- c(rep(1979,8),rep(2000,4)) # first assessment year
# legend <- Model
# SS_Dir <- Dir
Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Comparison/"

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:12) {
  for (s in 1:4) {
    SS_Dir <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
    # legend
    
    # print(Path)
    if(converge[m,s]) {
      cor_mat <- read.table(paste0(SS_Dir, "ss.cor"), skip = 1, fill = NA, header = TRUE)
      R_est <- cor_mat$value[which(cor_mat$name == "recr_std")[3:((lyear[m] - (fyear[m]-1)) * 4 + 2)]]
      R_std <- cor_mat$std.dev[which(cor_mat$name == "recr_std")[3:((lyear[m] - (fyear[m]-1)) * 4 + 2)]]
      if(sum(R_std)==0) R_std <- cor_mat$std_dev[which(cor_mat$name == "recr_std")[3:((lyear[m] - (fyear[m]-1)) * 4 + 2)]]
      R <- data.frame(est = R_est, std = R_std, year = rep(fyear[m]:lyear[m], each = 4), yq = seq(fyear[m], lyear[m] + 0.75, 0.25))
      R_annual <- R %>% group_by(year) %>% summarise(Est = sum(est), Std = NA)
      # cov_mat <- matrix(NA, nrow = length(R_est), ncol = 4)
      index_init <- which(cor_mat$name == "recr_std")[3]
      
      for (y in fyear[m]:lyear[m]) {
        index_year <- 0:3 + index_init + (y - fyear[m]) * 4
        cor_y <- cor_mat[index_year, index_year + 4]
        var_y <- R$std[((y - fyear[m]) * 4 + 1):((y - (fyear[m]-1)) * 4)] %*% t(R$std[((y - fyear[m]) * 4 + 1):((y - (fyear[m]-1)) * 4)]) * 
          cor_y
        var_y[1, 2:4] <- var_y[2:4, 1]
        var_y[2, 3:4] <- var_y[3:4, 2]
        var_y[3, 4] <- var_y[4, 3]
        var_y <- data.matrix(var_y)
        # cov_mat[((y-1975)*4+1):((y-1974)*4),1:4] <- var_y
        R_annual$Std[which(R_annual$year == y)] <- sqrt(matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) %*% var_y %*% matrix(c(1, 
                                                                                                                         1, 1, 1), nrow = 4, ncol = 1))
      }
      
      R_quarterly <- R %>% mutate(R = est/mean(est), STD = std/est, Model=model_name2[m], Steepness=steepness[s])
      R_annual <- R_annual %>% mutate(R = Est/mean(Est), STD = Std/Est, Model=model_name2[m], Steepness=steepness[s])
      
      if(m+s==2) {
        R_Q <- R_quarterly
        R_A <- R_annual
      }
      else
      {
        R_Q <- rbind(R_Q,R_quarterly)
        R_A <- rbind(R_A,R_annual)
      }
    }
  }
}

Weight_M <- data.frame("Model"=Model,"Weight_M"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
Weight_S <- data.frame("Steepness"=steepness,"Weight_S"=c(0.44,0.31,0.21,0.04))

R_All <- left_join(left_join(R_A,Weight_M),Weight_S)

R <- R_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999) %>% group_by(year) %>% mutate(Weight=weight/sum(weight))

R_average <- R %>% group_by(year) %>% summarise(Est=sum(Est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, R) {
  R_y <- R %>% filter(year==y)
  mean <- R_y$Est
  sd <- R_y$Std
  weight <- R_y$Weight
  
  for (m in 1:44) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

R_series <- seq(1,200000,0.1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(R_series,y,R)
  Low[y-1999] <- R_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- R_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

R_average$Low <- Low
R_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

R <- R %>% mutate(model=paste0(Model,"-",Steepness))

ggplot() +
  geom_line(aes(x=year,y=Est,color=model),data=R) +
  geom_ribbon(aes(x=year,ymin=Est-1.96*Std,ymax=Est+1.96*Std,fill=model,color=model),data=R,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=R_average,size=1) +
  geom_line(aes(x=year,y=Low),data=R_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=R_average,size=1,linetype="dashed")

write.csv(R_average,file=paste0(Save_Dir,"R_average.csv"),row.names = FALSE) 



# R optimistic

R <- R_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999,Model %in% model_name2[c(2,4:8)]) %>%
  group_by(year) %>% mutate(Weight=weight/sum(weight),
                            model=paste0(Model,"-",Steepness))

R_average <- R %>% group_by(year) %>% summarise(Est=sum(Est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, R) {
  R_y <- R %>% filter(year==y)
  mean <- R_y$Est
  sd <- R_y$Std
  weight <- R_y$Weight
  
  for (m in 1:length(unique(R_y$model))) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

R_series <- seq(1,200000,0.1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(R_series,y,R)
  Low[y-1999] <- R_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- R_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

R_average$Low <- Low
R_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

ggplot() +
  geom_line(aes(x=year,y=Est,color=model),data=R) +
  geom_ribbon(aes(x=year,ymin=Est-1.96*Std,ymax=Est+1.96*Std,fill=model,color=model),data=R,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=R_average,size=1) +
  geom_line(aes(x=year,y=Low),data=R_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=R_average,size=1,linetype="dashed")

write.csv(R_average,file=paste0(Save_Dir,"R_optimistic.csv"),row.names = FALSE) 




# R pessimistic

R <- R_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999,Model %in% model_name2[c(1,3,9:12)]) %>%
  group_by(year) %>% mutate(Weight=weight/sum(weight),
                            model=paste0(Model,"-",Steepness))

R_average <- R %>% group_by(year) %>% summarise(Est=sum(Est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, R) {
  R_y <- R %>% filter(year==y)
  mean <- R_y$Est
  sd <- R_y$Std
  weight <- R_y$Weight
  
  for (m in 1:length(unique(R_y$model))) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

R_series <- seq(1,200000,0.1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(R_series,y,R)
  Low[y-1999] <- R_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- R_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

R_average$Low <- Low
R_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

ggplot() +
  geom_line(aes(x=year,y=Est,color=model),data=R) +
  geom_ribbon(aes(x=year,ymin=Est-1.96*Std,ymax=Est+1.96*Std,fill=model,color=model),data=R,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=R_average,size=1) +
  geom_line(aes(x=year,y=Low),data=R_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=R_average,size=1,linetype="dashed")

write.csv(R_average,file=paste0(Save_Dir,"R_pesimistic.csv"),row.names = FALSE) 

# SB

Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/"

model <- c("R","RG","RM","RS","G","M1","M2","S","L","LG","LM","LS")
model_name <- c("R","R-GC","R-MA","R-DS","GC","MJ","MA","DS","L","L-GC","L-MA","L-DS")
model_name2 <- c("Env-Fix","Env-Gro","Env-Mrt","Env-Sel","Gro","Mov","Mrt","Sel","Srt-Fix","Srt-Gro","Srt-Mrt","Srt-Sel")
steepness <- seq(1,0.7,-0.1)

lyear <- rep(2019,12) # last assessment year
fyear <- c(rep(1979,8),rep(2000,4)) # first assessment year
# legend <- Model
# SS_Dir <- Dir
Save_Dir <- "C:/Users/hkxu/OneDrive - IATTC/IATTC/2020/BET assessment/SS Model/Comparison/"

converge <- matrix(1,nrow=length(model),ncol=length(steepness))
converge[1,2:4] <- 0
converge[9,4] <- 0

for (m in 1:12) {
  for (s in 1:4) {
    SS_Dir <- paste0(Dir,model[m],"-",toString(steepness[s]),"/")
    # legend
    
    # print(Path)
    if(converge[m,s]) {
      cor_mat <- read.table(paste0(SS_Dir, "ss.std"), skip = 1, fill = NA, header = FALSE)
      names(cor_mat) <- c("index","name","value","std.dev")
      SB_est <- cor_mat$value[which(cor_mat$name == "SSB_std")[3:((lyear[m] - (fyear[m]-1)) * 4 + 2)]]
      SB_std <- cor_mat$std.dev[which(cor_mat$name == "SSB_std")[3:((lyear[m] - (fyear[m]-1)) * 4 + 2)]]
      # if(sum(SB_std)==0) SB_std <- cor_mat$std_dev[which(cor_mat$name == "SSB_std")[1:((lyear[i] - (fyear[i]-1)) * 4)]]
      SB <- data.frame(est = SB_est, std = SB_std, year = rep(fyear[m]:lyear[m], each = 4), yq = seq(fyear[m], lyear[m] + 0.75, 0.25))
      
      if(m+s==2) SB_A <- SB %>% mutate(Model=model_name2[m], Steepness=steepness[s])
      else SB_A <- rbind(SB_A,SB %>% mutate(Model=model_name2[m], Steepness=steepness[s]))
    }
  }
}

Weight_M <- data.frame("Model"=Model,"Weight_M"=c(0.01,0.13,0.02,0.05,0.24,0.01,0.02,0.09,0.04,0.22,0.07,0.11))
Weight_S <- data.frame("Steepness"=steepness,"Weight_S"=c(0.44,0.31,0.21,0.04))

SB_All <- left_join(left_join(SB_A,Weight_M),Weight_S)

SB <- SB_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999,year==yq) %>% group_by(yq) %>% mutate(Weight=weight/sum(weight))

SB_average <- SB %>% group_by(year) %>% summarise(Est=sum(est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, SB) {
  SB_y <- SB %>% filter(year==y)
  mean <- SB_y$est
  sd <- SB_y$std
  weight <- SB_y$Weight
  
  for (m in 1:44) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

SB_series <- seq(1,800000,1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(SB_series,y,SB)
  Low[y-1999] <- SB_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- SB_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

SB_average$Low <- Low
SB_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

SB <- SB %>% mutate(model=paste0(Model,"-",Steepness))

ggplot() +
  geom_line(aes(x=year,y=est,color=model),data=SB) +
  geom_ribbon(aes(x=year,ymin=est-1.96*std,ymax=est+1.96*std,fill=model,color=model),data=SB,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=SB_average,size=1) +
  geom_line(aes(x=year,y=Low),data=SB_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=SB_average,size=1,linetype="dashed")

write.csv(SB_average,file=paste0(Save_Dir,"SB_average.csv"),row.names = FALSE) 


# SB optimistic

SB <- SB_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999,year==yq,Model %in% model_name2[c(2,4:8)]) %>%
  group_by(yq) %>% mutate(Weight=weight/sum(weight),
                          model=paste0(Model,"-",Steepness))

SB_average <- SB %>% group_by(year) %>% summarise(Est=sum(est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, SB) {
  SB_y <- SB %>% filter(year==y)
  mean <- SB_y$est
  sd <- SB_y$std
  weight <- SB_y$Weight
  
  for (m in 1:length(unique(SB_y$model))) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

SB_series <- seq(1,800000,1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(SB_series,y,SB)
  Low[y-1999] <- SB_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- SB_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

SB_average$Low <- Low
SB_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

ggplot() +
  geom_line(aes(x=year,y=est,color=model),data=SB) +
  geom_ribbon(aes(x=year,ymin=est-1.96*std,ymax=est+1.96*std,fill=model,color=model),data=SB,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=SB_average,size=1) +
  geom_line(aes(x=year,y=Low),data=SB_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=SB_average,size=1,linetype="dashed")

write.csv(SB_average,file=paste0(Save_Dir,"SB_optimistic.csv"),row.names = FALSE) 




# SB pessimistic

SB <- SB_All %>% data.frame() %>% mutate(weight=Weight_M*Weight_S) %>%
  filter(year>1999,year==yq,Model %in% model_name2[c(1,3,9:12)]) %>%
  group_by(yq) %>% mutate(Weight=weight/sum(weight),
                          model=paste0(Model,"-",Steepness))

SB_average <- SB %>% group_by(year) %>% summarise(Est=sum(est*Weight))

# find 2.5% and 97.5% quantiles

fun <- function (x, y, SB) {
  SB_y <- SB %>% filter(year==y)
  mean <- SB_y$est
  sd <- SB_y$std
  weight <- SB_y$Weight
  
  for (m in 1:length(unique(SB_y$model))) {
    if(m == 1) cdf <- pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
    else cdf <- cdf + pnorm(x,mean=mean[m],sd=sd[m]) * weight[m]
  }
  
  return(cdf)
}

SB_series <- seq(1,800000,1)
Low <- rep(NA,length(2000:2019))
High <- rep(NA,length(2000:2019))

for (y in 2000:2019) {
  print(y)
  CDF <- fun(SB_series,y,SB)
  Low[y-1999] <- SB_series[which(abs(CDF-0.025)==min(abs(CDF-0.025)))]
  High[y-1999] <- SB_series[which(abs(CDF-0.975)==min(abs(CDF-0.975)))]
}

SB_average$Low <- Low
SB_average$High <- High

# R_std <- R %>% group_by(year) %>% summarise(Low=)

ggplot() +
  geom_line(aes(x=year,y=est,color=model),data=SB) +
  geom_ribbon(aes(x=year,ymin=est-1.96*std,ymax=est+1.96*std,fill=model,color=model),data=SB,alpha=0.02) + 
  geom_line(aes(x=year,y=Est),data=SB_average,size=1) +
  geom_line(aes(x=year,y=Low),data=SB_average,size=1,linetype="dashed") +
  geom_line(aes(x=year,y=High),data=SB_average,size=1,linetype="dashed")

write.csv(SB_average,file=paste0(Save_Dir,"SB_pessimistic.csv"),row.names = FALSE) 
