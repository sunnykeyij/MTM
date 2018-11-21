rm(list=ls())
if(!require(tm)){ install.packages('tm')}; require(tm) 
if(!require(qdap)){ install.packages('qdap')}; require(qdap) 
if(!require(dplyr)){ install.packages('dplyr')}; require(dplyr) 
if(!require(pacman)) install.packages("pacman"); require(pacman)   # for rm_round

# setwd("C:\\Users\\MYCOM\\Dropbox\\DataMiningTeamProject")
setwd("C:\\Users\\UOS\\Dropbox\\데이터마이닝\\MTM")
script_galaxy1 <- readLines("Galaxy1.txt") # Warning?

# 숫자제거
script_galaxy1<-removeNumbers(script_galaxy1)
# 공백제거
script_galaxy1 <-stripWhitespace(script_galaxy1) 
script_galaxy1 <-script_galaxy1[unlist(lapply(1:length(script_galaxy1),function(i) script_galaxy1[i]!=" "))]
# 분석을위해제거
which(script_galaxy1 == "I swear. I don't know where they went! I swear.")
script_galaxy1 <- script_galaxy1[-c(2,793)]
######################################### scene, actor seperate ###############################################
# 대문자있는것만찾기
actor_sep <- function(x){
  seps <- which(sapply(1:length(x),function(ii) x[ii]==toupper(x[ii])))
  return(seps)
}
upper_ind_galaxy1 <- actor_sep(script_galaxy1)
upper_galaxy1 <- script_galaxy1[upper_ind_galaxy1]
upper_table <- table(upper_galaxy1)

# 괄호 안에 영어가 있는 경우 찾기
round_ind_galaxy1 <- !is.na(unlist(rm_round(script_galaxy1, extract=TRUE)))
round_galaxy1 <- script_galaxy1[round_ind_galaxy1]
round_table <- table(round_galaxy1)

# 장소 구분
in_galaxy1 <- script_galaxy1[c(grep("\\(In",script_galaxy1),grep("\\(in",script_galaxy1))][-15]       # (In ~) 이라고 되어있는 장소 찾기
in_galaxy2 <- names(upper_table[upper_table==1])[c(9,12,13,15,16,18,19,20,21,22)]
scens_seps <- c(in_galaxy1,in_galaxy2)

# 인물 구분
actor_seps <- c(names(upper_table[upper_table>1]),"FORTY K","XANDARIANs",names(round_table[round_table>1])[-c(15,16,17)])

scens_galaxy1 <- which(script_galaxy1 %in% scens_seps) # location of scene
actor_galaxy1 <- which(script_galaxy1 %in% actor_seps) # location of actor

###################################################################################################################

seps_galaxy1 <- sort(c(scens_galaxy1,actor_galaxy1))              # 대사 또는 장소가 있는 행 
diff_words <- diff(seps_galaxy1)-1                                # 사이에 있는 대사 (또는 지문) 수
reptimes <- diff_words[!(seps_galaxy1 %in% scens_galaxy1)]        # 대사있는 것만 추출

words <- script_galaxy1[-c(scens_galaxy1,actor_galaxy1)]          # 대사
actor <- rep(x=script_galaxy1[actor_galaxy1],times=reptimes)      # 배우 

scens_by_actor <- c()
for (i in 1:length(actor_galaxy1)){
  scens_by_actor[i] <- max(which(actor_galaxy1[i] > scens_galaxy1))
}
scens <- rep(x=script_galaxy1[scens_galaxy1[scens_by_actor]],times=reptimes)

# 최종 데이터프레임을 만듭니다.
dataframe_galaxy1 <- data.frame(scene=scens, actor=actor, words=words)
save(dataframe_galaxy1,file="dataframe_galaxy1.Rda")
