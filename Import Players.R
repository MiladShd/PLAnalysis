teamNames = standing$Team
teamNames = gsub(' ', '-', teamNames) # you can get these two by running the Import standing .... file
for(ti in 1:length(teamNames)){
    player =""
    url <- paste0("http://www.worldfootball.net/teams/",teamNames[ti],"/2017/2/")
    Players_table <- url %>% read_html(url) %>%
      html_nodes(xpath='//*[@id="site"]/div[3]/div[1]/div/div[3]') 
    
    Players_table_text <- as.character (Players_table[[1]])
    #write.csv(Players_table_text,"Players_table_text2.csv" )
    
    trBPos <- (as.double(gregexpr("<tr", Players_table_text)[[1]]))
    trEPos <- (as.double(gregexpr("</tr>", Players_table_text)[[1]]))
    tempRow <- substr(Players_table_text, trBPos[1], trEPos[1])
    
    
    for (iRows in (1:length(trBPos))){
      tempRow <- substr(Players_table_text, trBPos[iRows], trEPos[iRows])
      #Header------------------------------------------------------------------
      if (length(grep("<th", tempRow))>0){ #Header starts with <th
        fPos <- as.double(gregexpr("<", tempRow)[[1]])  #Position of end of the potential header word
        sPos <- as.double(gregexpr(">", tempRow)[[1]])  #Position of start of the potential header word
        iLength <- length(sPos)	#number of possible iterations
        for (i in (1:(iLength-1))){ #iteration on the header
          tempSub <- substr(tempRow,sPos[i]+1,fPos[i+1]-1) #extracting the ith header
          tempSub <- Clean(tempSub) #let's clean it 
          if (grepl("[a-z^A-Z^0-9]", tempSub)){ #Check if it is word or a blank space! the website is written in this way that there is one header word and others are blank
            tempHead = gsub("\\s","",tempSub)	#We found the header   
          }
        }
      }
      #Players------------------------------------------------------------------
      else{
        tdBPos <- (as.double(gregexpr("<td", tempRow)[[1]]))
        tdEPos <- (as.double(gregexpr("</td>", tempRow)[[1]]))
        player = paste0(player,"\n", tempHead)
        for (itd in 1:(length(tdBPos)))
        {
          tempSubL <- substr(tempRow,tdBPos[itd],tdEPos[itd])
          fPos <- as.double(gregexpr("<", tempSubL)[[1]]) 
          sPos <- as.double(gregexpr(">", tempSubL)[[1]])
          for (i in 1:(length(sPos))){
            tempSubInner <- substr(tempSubL,sPos[i]+1,fPos[i+1]-1)
            tempSubInner <- Clean(tempSubInner)
            if (grepl("[a-z^A-Z^0-9]", tempSubInner)){
              player = paste0(player,",",tempSubInner  )
            }
          }
        } 
      }
    }
    
    #write Data
    fileName = gsub("-","_",paste0("Players_of_",teamNames[ti],".csv"))
    
    write.table(substring(as.character(player), 2),eol = "\n", file = fileName, quote = FALSE, col.names = FALSE,row.names = FALSE )
}
