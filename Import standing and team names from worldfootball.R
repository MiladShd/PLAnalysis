#Premier League Analysis 
#By: Milad Shaddelan
#===================================================
#Standing
#===================================================
Clean <- function(DirtyStr){ #Remove redundant characters
  DirtyStr <- gsub("\\n","",DirtyStr)
  DirtyStr <- gsub("\\t","",DirtyStr)
  DirtyStr <- gsub("\\r","",DirtyStr)
  # ... 
  return(DirtyStr)
}
#=========================================
url <- "http://www.worldfootball.net/competition/eng-premier-league/"
standingTable <- url %>% read_html(url) %>%
  html_nodes(xpath='//*[@id="tabelle_0"]/div[2]')
standingTable = as.character(standingTable)

teamSt="Rank,Team,Matches,Diff,Pt"
trBPos <- (as.double(gregexpr("<tr", standingTable)[[1]]))
trEPos <- (as.double(gregexpr("</tr>", standingTable)[[1]]))

for (iRows in (1:length(trBPos))){
  tempRow <- substr(standingTable, trBPos[iRows], trEPos[iRows])
  tdBPos <- (as.double(gregexpr("<td", tempRow)[[1]]))
  tdEPos <- (as.double(gregexpr("</td>", tempRow)[[1]]))
  teamSt = paste0(teamSt,"\n")
  for (itd in 1:(length(tdBPos))){
    tempSubL <- substr(tempRow,tdBPos[itd],tdEPos[itd])
    fPos <- as.double(gregexpr("<", tempSubL)[[1]]) #Condition NEEDED
    sPos <- as.double(gregexpr(">", tempSubL)[[1]])
    for (iInform in 1:(length(sPos))){
      tempSubInner <- substr(tempSubL,sPos[iInform]+1,fPos[iInform+1]-1)
      tempSubInner <- Clean(tempSubInner)
      if (grepl("[a-z^A-Z^0-9]", tempSubInner)){
        teamSt = paste0(teamSt,",",tempSubInner)
      }
    }
  }
}

teamSt= gsub("\n,","\n",teamSt)
teamSt= gsub("\n\n","\n",teamSt)
con <- textConnection(teamSt)
standing <- read.csv(con)
close(con)
standing=standing[1:20,]
stFileName<- paste0("standing_", format(Sys.Date(), format="%B_%d_%Y"))
write.table(standing,eol = "\n", file = stFileName, quote = FALSE, col.names = FALSE,row.names = FALSE )

#one time write name of the teams in a different file if needed
#write.table(standing$Team,eol = "\n", file = "team_names.csv", quote = FALSE, col.names = FALSE,row.names = FALSE )
