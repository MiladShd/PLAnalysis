install.packages("httr")
install.packages("XML")
install.packages('xml2')
install.packages("RCurl")
library("XML")
library('xml2')
library("RCurl")
library("httr")
install.packages("purrr")
library(purrr)
install.packages("rvest")
library("rvest")

#================================================
Clean <- function(DirtyStr){
  if (!is.na(DirtyStr)){
    DirtyStr <- gsub("\\n","",DirtyStr)
    DirtyStr <- gsub("\\t","",DirtyStr)
    DirtyStr <- gsub("\\r","",DirtyStr)
    DirtyStr <- gsub(",,",",",DirtyStr)
    DirtyStr <- gsub(" ","",DirtyStr)
    if (!grepl("[a-z^A-Z]",DirtyStr)){
      DirtyStr <- gsub("\\s","",DirtyStr)
    }
    if (substr(DirtyStr,1,1)==","){
      DirtyStr <- substr(DirtyStr,2,nchar(DirtyStr))    
  }  
  return(DirtyStr)
  }
  else{
    return("")
  }
}
#================================================
CleanTimeL <- function(DirtyString){ # To clean the timeline data!
  CleanString =substr(DirtyString, 1 , (commaLoc[1]))
  subDirt =""
  for ( i in (1:(length(commaLoc)-1))){
    subDirt = substr(DirtyString, (commaLoc[i]+1) , (commaLoc[i+1]))
    if (!(grepl("'" , subDirt))){
      CleanString = paste0(CleanString , subDirt )
    }
  }
  CleanString = gsub("Substitution,Substitution,", "Substitution,",CleanString )
  CleanString = gsub("YellowCard,YellowCard,", "YellowCard,",CleanString )
  CleanString = gsub("Goal,label.penalty.scored,", "Goal,",CleanString )
  CleanString = gsub("Goal,Goal,", "Goal,",CleanString )
  
  return(CleanString)
}
#================================================
textParse <- function(eText, tempText){ #use this function to find character inside all tags
  
  fPos <- as.double(gregexpr("<", eText)[[1]])
  sPos <- as.double(gregexpr(">", eText)[[1]])
  for (iInform in 1:(length(sPos))){
    tempSubInner <- substr(eText,sPos[iInform]+1,fPos[iInform+1]-1)
    tempSubInner <- Clean(tempSubInner)
    if (grepl("[a-z^A-Z^0-9]", tempSubInner)){
      tempText = paste0(tempText,",",tempSubInner  )
    }
  }
  
  tempText =  paste0(tempText,"\n")
  return(tempText)
}
#================================================
#TO GET MATCH IDS 
# Link: https://www.premierleague.com/results
# webpage load data partially so to get it easier, run the linke; scroll down and save it as results.html
mLURL <- 'results.html' 
mLTable1 <-(read_html(mLURL))
uIndex =as.double(gregexpr('data-matchid', mLTable1)[[1]]) 

matchIDS <- NULL #instead of using {<- initial value}
#sample string we face: data-matchid="22602"
for (i in 1: length(uIndex)){
matchIDS = c(matchIDS , as.double(substr(mLTable1 ,uIndex[i]+14 ,  uIndex[i]+18)))
}
#================================================
#================================================
#matchDataList =""

for (OutI in 1:5){ #just for 5 matches!
  
  matchID = matchIDS[OutI]
  mURL<- paste0("https://www.premierleague.com/match/", matchID) 
  
  #teamStInf=""
  
  mTable1 <-as.character(read_html(mURL))
  write.csv(mTable1,"mTable.html" )
  
  #===============================================
  mURL2<- "mTable.html"
  
  x_path<- '/html/body/main/div/section/div[2]/section/div[3]/div[1]/div/div[7]'
  mTable2 <- as.character(mURL2 %>% read_html(mURL2) %>% html_nodes(xpath = x_path))
  write.csv(mTable2,"mTable2.html" )
  
  x_path = "/html/body/div/div/div["
  mURL3 = "mTable2.html"
  iEvents = 1
  matchTimeL = ""
  
  mTable3 <- as.character(mURL3 %>% read_html(mURL3) %>% html_nodes(xpath = paste0(x_path, iEvents , "]")))
  write.csv(mTable3,"mTable3.html" )
  while (length(mTable3) > 0){
    
  
    if (grepl("home", mTable3)){
      matchTimeL <- paste0(matchTimeL, "home" )
    }
    if (grepl("away", mTable3))
    {
      matchTimeL <- paste0(matchTimeL , "away")
    }
    matchTimeL <- textParse(mTable3, matchTimeL)
    
    iEvents <- iEvents + 1
    mTable3 <- as.character(mURL3 %>% read_html(mURL3) %>% html_nodes(xpath = paste0(x_path, iEvents , "]")))
  }
  commaLoc = as.double(gregexpr(',', matchTimeL)[[1]])
  matchTimeL = as.character(matchTimeL)
  matchTimeL = CleanTimeL(matchTimeL)          
  write.table(matchTimeL,eol = "\n", file = paste0( matchID, "_Time_Line.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )          
  
  #================================================
  #history
  x_path = '/html/body/main/div/section/div[2]/section/div[3]/div[3]'
  
  mTable2 <- as.character(mURL2 %>% read_html(mURL2) %>% html_nodes(xpath = x_path))
  write.csv(mTable2,"mTable2.html" )
  
  matchInf =NULL
  
  matchInf <- Clean(textParse(mTable2, matchInf))
  matchInf <- gsub(",RecentMeetings","\nRecentMeetings",matchInf) 
  matchInf <- gsub(",FormGuide","\nFormGuide",matchInf)
  
  write.table(substr(as.character(matchInf),1,nchar(as.character(matchInf))),eol = "\n", file = paste0(matchID, "_history.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )          
  #================================================
  #Formation - Home Awy - Name
  x_path <- '/html/body/main/div/section/div[2]/section/div[3]/div[4]/section[2]'
  mURL2 <- "mTable.html"
  mTable2 <- as.character(mURL2 %>% read_html(mURL2))
  write.csv(mTable2,"mTable2.html" )

  fPosInd <- as.double(gregexpr("matchteam", mTable2)[[1]])
  tempFStr<- substr(mTable2 , fPosInd[1],fPosInd[1] + 100)
  formStartInd <- as.double(gregexpr(">", tempFStr)[[1]])
  formEndInd <-as.double(gregexpr("<", tempFStr)[[1]])
  
  matchHF = (substr(tempFStr,formStartInd[1]+1,formEndInd[1]-1 ))
  
  tempFStr<- substr(mTable2 , fPosInd[2],fPosInd[2] + 100)
  formStartInd <- as.double(gregexpr(">", tempFStr)[[1]])
  formEndInd <-as.double(gregexpr("<", tempFStr)[[1]])
  
  matchAF = (substr(tempFStr,formStartInd[1]+1,formEndInd[1]-1 ))
  
  XHome = as.double(gregexpr('<div class="" team home' , mTable2)[[1]])
  XAway = as.double(gregexpr('<div class="" team away' , mTable2)[[1]])
  
  mathcHName=""
  mathcHNameStr= Clean(textParse(substr(mTable2, XHome[1], XHome[1]+400),""))
  mathcHName= "HT Name, HT Formation \n"
  
  mathcAName=""
  mathcANameStr= Clean(textParse(substr(mTable2, XAway[1], XAway[1]+400),""))
  mathcAName= "AT Name, AT Formation \n"
  
  Formations = ""
  Formations = paste0(mathcHName,mathcHNameStr,",", matchHF, " \n" ,mathcAName,mathcANameStr,",",matchAF )
  
 # matchDataList = paste0(matchDataList, matchID , " ", mathcHName," ",mathcAName, "\n")
  
  write.table(Formations,eol = "\n", file = paste(matchID, "_Formation.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )
  #================================================
  #================================================
  
  stlInd=as.double(gregexpr('startinglineupcontainer', mTable2)[[1]])
  subInd=as.double(gregexpr('Substitutes', mTable2)[[1]])
  
  mathcHLineup="HT Lineup:"
  mathcHLineupStr= paste0("<" , substr(mTable2, stlInd[1], subInd[1]), ">")
  mathcHLineup= Clean(textParse(mathcHLineupStr, mathcHLineup))

  hEndSubInd =as.double(gregexpr(' matchcentrepitchcontainer', mTable2)[[1]])
  
  mathcHSub="HT SUB:"
  mathcHSubStr= paste0(substr(mTable2, subInd[1], hEndSubInd[1]),">")
  mathcHSub= Clean(textParse(mathcHSubStr, mathcHSub))
  
  
  mathcALineup="AT Lineup:"
  mathcALineupStr=  paste0("<", substr(mTable2, stlInd[3], subInd[2]),">")
  mathcALineup= Clean(textParse(mathcALineupStr, mathcALineup))

  AEndSubInd =as.double(gregexpr('<!-- STATS TAB -->', mTable2)[[1]])
  
  mathcASub="AT SUB:"
  mathcASubStr= paste0(substr(mTable2, subInd[2], AEndSubInd))
  mathcASub= Clean(textParse(mathcASubStr, mathcASub))
  
  matchLS = paste0(mathcHLineup,"\n",mathcHSub,"\n",mathcALineup,"\n",mathcASub)
  
  write.table(matchLS,eol = "\n", file = paste0(matchID, "_Lineup_Subs.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )          
#====================================================
#Score
  #mURL <- "mTable.html"
  #mTable <- as.character(mURL2 %>% read_html(mURL))
  scoreI = as.double(gregexpr('score fulltime' , mTable2)[[1]])
  
  matchScore=""
  matchScoreStr= paste0("<",substr(mTable2, scoreI, scoreI+50))
  matchScore= Clean(textParse(matchScoreStr, ""))
  write.table(matchScore, eol = "\n", file = paste(matchID, "_Result.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )
}

#write.table(Clean(matchDataList),eol = "\n", file = paste("MatchDataList.csv"), quote = FALSE, col.names = FALSE,row.names = FALSE )


