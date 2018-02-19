#Standing Information from PL
#==============================================
Clean <- function(DirtyStr){ #remove redundants
  DirtyStr <- gsub("\\n","",DirtyStr)
  DirtyStr <- gsub("\\t","",DirtyStr)
  DirtyStr <- gsub("\\r","",DirtyStr)
  # ... 
  return(DirtyStr)
}
#=================================
tURL<- "https://www.premierleague.com/tables" #the url that we use
stTable1 <- tURL %>% read_html(tURL) %>% 
  html_nodes(xpath='/html/body/main/div[2]/div[3]/div/div/div/table')
stTable1 <- as.character (stTable1[[1]])
write.csv(stTable1,"stTable.html" ) #like before we want to reach deep in the document so write and read it again is a good idea since the file is small

nTeams = c(2*(1:20)-1)
teamStInf=""
for (iTeams in nTeams){
  tURL2<-"stTable.html"
  test <- tURL2 %>% read_html(tURL2) %>%
    html_nodes(xpath=paste0('/html/body/table/tbody/tr[',iTeams,']'))
  test <- as.character (test[[1]])
  tempRow <- test
  tdBPos <- (as.double(gregexpr("<td", tempRow)[[1]]))
  tdEPos <- (as.double(gregexpr("</td>", tempRow)[[1]]))
  for (itd in 1:11)
  {
    tempSubL <- substr(tempRow,tdBPos[itd],tdEPos[itd])
    fPos <- as.double(gregexpr("<", tempSubL)[[1]]) #Condition NEEDED
    sPos <- as.double(gregexpr(">", tempSubL)[[1]])
    for (iInform in 1:(length(sPos))){
      tempSubInner <- substr(tempSubL,sPos[iInform]+1,fPos[iInform+1]-1)
      tempSubInner <- Clean(tempSubInner)
      tempSubInner <- gsub("[[:space:]]", "", tempSubInner)
      if (grepl("[a-z^A-Z^0-9]", tempSubInner)){
        teamStInf = paste0(teamStInf,",",tempSubInner  )
      }
    }
  } 
  teamStInf =  paste0(teamStInf," \n")
}
stFileName<- paste0("standingPL_", format(Sys.Date(), format="%B_%d_%Y"),".csv")
write.table(teamStInf,eol = "\n", file = stFileName , quote = FALSE, col.names = FALSE,row.names = FALSE )
file.remove("stTable.html")
