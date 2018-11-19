rm(list = ls())
if(!require(tm)){install.packages("tm")}; require(tm)
if(!require(qdap)){install.packages("qdap")}; require(qdap)
if(!require(magrittr)){install.packages("magrittr")}; require(magrittr) #extract2
if(!require(stringr)){install.packages("stringr")}; require(stringr)

#import data(txt)
 # thor = read.delim("C:/Users/UOS/Dropbox/2018-2/DATA_MINING/project/thor.txt",sep = "\n",stringsAsFactors = F)
# setwd("C:/Users/UOS/Dropbox/2018-2/DATA_MINING/project")
setwd("C:/Users/UOS/Documents/GitHub/MTM")
thor = readLines("C:/Users/UOS/Dropbox/2018-2/DATA_MINING/project/thor.txt")



#clean text
thor= removeNumbers(thor)
thor= bracketX(thor) #괄호안 삭제
thor =  removePunctuation(thor)
thor = stripWhitespace(thor)
head(thor,100)

del = which(thor == "FADE OUT" | thor == "FADE IN") 
if(length(del) > 0){
  thor = thor[-del]
} 

index = which(thor == "") #""delete
if(length(index) > 0){
  thor = thor[-index]
} 
thor = stripWhitespace(thor)

head(thor,100)
# thor1 = writeLines(thor,sep = "")

#divide scene
ext = grep("EXT",thor) # 87(89) thor[grep("EXT",thor)]
int = grep("INT",thor) # 50(51)
num = c(ext,int); num = sort(num) #137

scene = list() 
for (i in 1:(length(num)-1)) {
  scene[[i]]= thor[seq(num[i],(num[i+1]-1))]
}
scene[[137]] = thor[seq(num[137],4979)]

#divided by figure
# grep(('[:upper:]'),scene[[1]])
# sapply(str_extract_all(scene[[1]], all('\\b[A-Z]+\\b')), paste, collapse=' ')
# if(scene[[1]][1]== ("[:upper:]"))
# str_extract_all(scene[[1]][1],all("[:upper:]"))
# 
# 
# ind <- lapply(1:length(script_galaxy2),function(i) script_galaxy2[i]==toupper(script_galaxy2[i]))
# script_galaxy2[unlist(ind)]
# table(script_galaxy2[unlist(ind)])
# 
# string1 <- "THE QUEEN’S HAND"
# string2 <- "T"
# 
# scene[[2]][1] %>%
#   str_replace_all(., "[^a-zA-Z$]", "") 
# 
# 
# scene[[2]][3] %>%  str_extract(.,"[^a-zA-Z$]") %>%((str_detect(., "[[:upper:]]{2,}"))) 







#make corpus
thor = VectorSource(thor)
thor = VCorpus(thor)
thor = tm_map(thor,removeWords,stopwords(kind = "en"))
inspect(thor)
viewthor = function(d, n) {d %>% extract2(n) %>% as.character() %>% writeLines()} #줄별로 document
viewthor(thor,4981)






