rm(list=ls())
if(!require(tm)){ install.packages('tm')}; require(tm) 
if(!require(qdap)){ install.packages('qdap')}; require(qdap) 
if(!require(dplyr)){ install.packages('dplyr')}; require(dplyr) 

# setwd("C:\\Users\\MYCOM\\Dropbox\\DataMiningTeamProject")
setwd("C:\\Users\\UOS\\Dropbox\\데이터마이닝\\MTM")
script_galaxy2 <- readLines("Galaxy2.txt")

# 구두점제거
# script_galaxy2<-removePunctuation(script_galaxy2)  # INT. EXT. 을 위해 미리 제거 X 
# 숫자제거
script_galaxy2<-removeNumbers(script_galaxy2)
# 괄호안 텍스트 제거
script_galaxy2<-bracketX(script_galaxy2) 
# 공백제거
script_galaxy2 <-stripWhitespace(script_galaxy2) 
script_galaxy2 <-script_galaxy2[unlist(lapply(1:length(script_galaxy2),function(i) script_galaxy2[i]!=""))]

# scene별로 잘라서 list로 만든다
scens_seps <- sort(c(grep("INT\\.",script_galaxy2),grep("EXT\\.",script_galaxy2)))
scens_seps <- scens_seps[-grep("POINT",script_galaxy2[scens_seps])]
script_galaxy2[scens_seps] # 확인

list_galaxy2 <- lapply(1:(length(scens_seps)-1),function(i) script_galaxy2[(scens_seps[i]+1):(scens_seps[i+1]-1)] )
list_galaxy2[[length(scens_seps)]] <- script_galaxy2[(scens_seps[length(scens_seps)]+1):length(script_galaxy2)]
names(list_galaxy2) <- script_galaxy2[scens_seps]

# 각 씬에서 actor 찾는 함수 (대문자로)
actor_sep <- function(x){
  seps <- which(sapply(1:length(x),function(ii) x[ii]==toupper(x[ii])))
  return(seps)
}

# 예
actor_sep(list_galaxy2[[1]])
actor_sep(list_galaxy2[[2]])

# 각 씬에서 나오는 인물이 없으면 그 씬 그냥 지워라
noactorvec <- c()
for (i in 1:length(list_galaxy2)){
  noactorvec[i] <- length(actor_sep(list_galaxy2[[i]]))==0
}
list_galaxy2[noactorvec] <- NULL


# actor별로 대사 붙이기 + 데이터프레임 만드는 함수
actor_word <- function(x){
  actor_seps <- actor_sep(x)                                            # x=씬에서 actor 찾아서
  if (length(actor_seps)==1){                                           # 그 씬에서 인물이 한번만 나오면
    actor_list <- paste(x[(actor_seps+1):length(x)],collapse = " ")     # 인물 이후 끝까지 다 대사로 paste 처리한다
  }
  else {
    actor_list <- lapply(1:(length(actor_seps)-1), function(i) paste(x[(actor_seps[i]+1):(actor_seps[i+1]-1)],collapse = " "))
    if (length(x)>1){
      actor_list [[length(actor_seps)]] <- x[(actor_seps[length(actor_seps)]+1):length(x)]  ### 왜 paste 없지?
  }
}
names(actor_list) <- x[actor_seps] 
actor_df <- data.frame( Actor = rep(names(actor_list), lapply(actor_list, length)),     # 배우,대사 들어가는 데이터프레임 만든다
                        Words = unlist(actor_list))
return(actor_df)
}
# 예
actor_word(list_galaxy2[[5]])

# 이렇게하면 모든 씬(list)에 대해서 배우,대사 데이터프레임을 만들고
wlist_galaxy2 <- lapply(list_galaxy2,actor_word) 

# 최종 데이터프레임을 만듭니다.
dataframe_galaxy2 <- data.frame()
for (i in 1:length(wlist_galaxy2 )){
  df <- data.frame(scene=rep(names(wlist_galaxy2)[i],nrow(wlist_galaxy2[[i]])), actor=wlist_galaxy2[[i]]$Actor, words=wlist_galaxy2[[i]]$Words)
  dataframe_galaxy2 <- rbind(dataframe_galaxy2,df)
}

View(dataframe_galaxy2)
save(dataframe_galaxy2,file="dataframe_galaxy2.Rda")

# dataframe_galaxy2 %>% select(actor,words) %>% filter(actor=="GROOT")










###########################################################################################
# mydt <- data.frame(
# lNames = rep(names(myList), lapply(myList, length)),
# lVal = unlist(myList))

# # space로 끊기
# script_galaxy2 <- strsplit(script_galaxy2,split=" ")

### if list가 scens_seps를 포함하면 scene이라는 변수에 그list를 넣어라.
if (any(scens_seps %in% script_galaxy2[[25]]))
scene =  script_galaxy2[[25]]

sapply(script_galaxy2, function(x) {}  )


scens_seps %in% script_galaxy2[[25]]


lapply(1:length(script_galaxy2),function(i) script_galaxy2[i]==toupper(script_galaxy2[i]))


# 대문자추출
ind <- lapply(1:length(script_galaxy2),function(i) script_galaxy2[i]==toupper(script_galaxy2[i]))
script_galaxy2[unlist(ind)]
table(script_galaxy2[unlist(ind)])
table1 <- table(script_galaxy2[unlist(ind)])
# names(table1[table1>1])
names(table1)


script_clean1 <- function(text){
text<-removeNumbers(text)                     # 숫자제거
text<-removePunctuation(text)                 # 괄호안에있는 텍스트 제거
text<-removeWords(text, stopwords("english")) # 불용어제거
text<-stripWhitespace(text)                   # 공백(whitespace)을 제거
}



script_galaxy2<-script_clean1(script_galaxy2)


ind <- lapply(1:length(script_galaxy2),function(i) script_galaxy2[i]==toupper(script_galaxy2[i]))
script_galaxy2[unlist(ind)]



