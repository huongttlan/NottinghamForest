# Calling all libraries
library(DT)
library(dplyr)
library(GGally)
library(plotly)
library(sf)
library(leaflet)
library(engsoccerdata)
library(tidyr)
library(maps)
library(mapdata)
library(varhandle)
library(crosstalk)
library(gridExtra)
library(dplyr)
load("england.rda")
load("deductions.rda")
england<-as_tibble(england)
##################################################################
# Function all score teams
all_score_team_new<-function(df=NULL,score=NULL,teamname=NULL){
  if (is.null(score)){
    tmp <- df[(df$home==teamname | df$visitor==teamname), ]
    tmp <- tmp[order(tmp$Season),]
  }
  else{
    temp<-strsplit(score,split="-")
    score1<-paste(temp[[1]][2],temp[[1]][1],sep="-")
    tmp <- df[(df$FT==score | df$FT==score1) & (df$home==teamname | df$visitor==teamname), ]
    tmp <- tmp[order(tmp$Season),]
    
  }
  return(tmp)
}
##################################################################
# Calculate number of wins etc..
alltimerecord_new<-function (df=NULL, teamname=NULL) {
  
  .<-home<-visitor<-hgoal<-vgoal<-goaldif<-FT<-GF<-GA<-GD<-P<-W<-D<-L<-Season<-division<-result<-maxgoal<-mingoal<-absgoaldif<-NULL
  
  hrec<-df %>%
    dplyr::filter(home==teamname)%>%
    group_by(Season)%>%
    dplyr::summarise(P = sum(result %in% c("H", "D", "A")), W=sum(result=="H"), 
                     D=sum(result=="D"), L=sum(result=="A"),
                     GF = sum(hgoal), GA = sum(vgoal), GD=GF-GA)%>%
    ungroup()%>%
    mutate(GameType="Home")
  
  vrec<-df %>%
    dplyr::filter(visitor==teamname)%>%
    group_by(Season)%>%
    dplyr::summarise(P = sum(result %in% c("H", "D", "A")),  W=sum(result=="A"), 
                     D=sum(result=="D"), L=sum(result=="H"),
                     GF = sum(vgoal), GA = sum(hgoal), GD=GF-GA)%>%
    ungroup()%>%
    mutate(GameType="Away")
  
  arec<-df %>%
    dplyr::filter(visitor==teamname | home==teamname)%>%
    group_by(Season)%>%
    dplyr::mutate(result=case_when(home==teamname & result=="H" ~ "W",
                                   home==teamname & result=="A" ~ "L",
                                   home==teamname & result=="D" ~ "D",
                                   visitor==teamname & result=="H" ~ "L",
                                   visitor==teamname & result=="A" ~ "W",
                                   visitor==teamname & result=="D" ~ "D",
                                   TRUE ~ "NA"
    ))%>%
    dplyr::mutate(goals_scored=case_when(home==teamname~ hgoal, visitor==teamname~ vgoal))%>%
    dplyr::mutate(goals_against=case_when(home==teamname~ vgoal, visitor==teamname~hgoal))%>%
    dplyr::summarise(P = sum(result %in% c("W", "L", "D")),  W=sum(result=="W"), 
                     D=sum(result=="D"), L=sum(result=="L"),
                     GF = sum(goals_scored), GA = sum(goals_against), GD=GF-GA)%>%
    ungroup()%>%
    mutate(GameType="Total")
  
  temp<-rbind(hrec, vrec, arec)
  #rownames(temp)<-c("home", "away", "total")
  return(temp)
}
##################################################################
england_current_new <- function(){
  
  
  home<-visitor<-hgoal<-vgoal<-goaldif<-FT<-Season<-division<-result<-NULL
  
  url1 <- "https://www.11v11.com/competitions/premier-league/2021/matches/"
  url2 <- "https://www.11v11.com/competitions/league-championship/2021/matches/"
  url3 <- "https://www.11v11.com/competitions/league-one/2021/matches/"
  url4 <- "https://www.11v11.com/competitions/league-two/2021/matches/"
  
  x1 <- xml2::read_html(url1) %>% rvest::html_table(fill = TRUE)
  x2 <- xml2::read_html(url2) %>% rvest::html_table(fill = TRUE)
  x3 <- xml2::read_html(url3) %>% rvest::html_table(fill = TRUE)
  x4 <- xml2::read_html(url4) %>% rvest::html_table(fill = TRUE)
  
  make_data <- function(x){
    x <- x[[1]][,1:4]
    x <-x[grepl("([0-9]+).*$", x[,1]),]#get rid of months text
    colnames(x)<-c("Date","home","FT","visitor")
    x$Date <- as.character(as.Date(x$Date, format="%d %b %Y"))
    x$Season <- 2020
    x$FT <- gsub(":", "-", x$FT)
    x <- x[nchar(x$FT)>1,]
    hgvg <- matrix(unlist(strsplit(x$FT, "-")), ncol=2, byrow = T)
    x$hgoal <- as.numeric(hgvg[,1])
    x$vgoal <- as.numeric(hgvg[,2])
    x$totgoal <- x$hgoal+x$vgoal
    x$goaldif <- x$hgoal-x$vgoal
    x$result <- ifelse(x$hgoal>x$vgoal, "H", ifelse(x$hgoal<x$vgoal, "A", "D"))
    return(x)
  }
  
  x1d <- make_data(x1)
  x2d <- make_data(x2)
  x3d <- make_data(x3)
  x4d <- make_data(x4)
  
  x1d$division <- 1
  x1d$tier <- 1
  x2d$division <- 2
  x2d$tier <- 2
  x3d$division <- 3
  x3d$tier <- 3
  x4d$division <- 4
  x4d$tier <- 4
  
  xd <- rbind(x1d,x2d,x3d,x4d)
  xd <- xd[colnames(engsoccerdata::england)]
  
  xd %>%
    dplyr::mutate(home = dplyr::case_when(
      grepl("Brighton and Hove", home) ~ "Brighton & Hove Albion",
      grepl("Cheltenham Town", home) ~ "Cheltenham",
      grepl("Stevenage", home) ~ "Stevenage Borough",
      grepl("Harrogate Town", home) ~ "Harrogate Town A.F.C.",
      grepl("Macclesfield Town", home) ~ "Macclesfield",
      grepl("Yeovil", home) ~ "Yeovil",
      TRUE ~ home
    )) %>%
    dplyr::mutate(visitor = dplyr::case_when(
      grepl("Brighton and Hove", visitor) ~ "Brighton & Hove Albion",
      grepl("Cheltenham Town", visitor) ~ "Cheltenham",
      grepl("Stevenage", visitor) ~ "Stevenage Borough",
      grepl("Macclesfield Town", visitor) ~ "Macclesfield",
      grepl("Harrogate Town", visitor) ~ "Harrogate Town A.F.C.",
      grepl("Yeovil", visitor) ~ "Yeovil",
      TRUE ~ visitor
    )) -> xd
  
  return(xd)
  
}
##########################################################
maketable_eng_new <- function(df=NULL, Season=NULL, tier=NULL, division=NULL, penalties=FALSE){
  
  newPts<-penalty<-GA<-GF<-ga<-gf<-gd<-GD<-W<-Pts<-.<-Date<-home<-team<-visitor<-hgoal<-opp<-vgoal<-goaldif <-FT<-result<-maxgoal<-mingoal<-absgoaldif<-NULL
  
  #deductions <- deductions
  df<-df%>%filter(Season==Season)%>%
    filter(tier==tier)
  
  if (nrow(df)==0) {
    df
  }
  else {
    if(!is.null(division)){ df <-  df[df$division==division,] }
    
    #1981/82 - three points for a win introduced.
    if(Season>=1981){
      
      xx <- maketable(df,Season,tier,pts=3)
      
      if(any(xx$team %in% deductions$team & Season %in% deductions$Season)==T && penalties==T){
        
        penalty <- deductions[deductions$team %in% xx$team & deductions$Season %in% Season,]
        # need if penalties has no rows ... just return xx
        
        if(nrow(penalty)>0){
          penalty$newPts <- xx$Pts[match(penalty$team,xx$team)]-penalty$deduction
          newPts <- penalty$newPts[match(xx$team,penalty$team)]
          xx$Pts <- ifelse(!is.na(newPts), newPts, xx$Pts)
          
          # rearrange by same rules as before...
          xx %>%
            dplyr::arrange(-Pts, -gd, -gf) %>%
            dplyr::mutate(Pos = rownames(.)) %>%
            as.data.frame() -> xx
        }
        
      }
      
      
    } else
      
      
      #1976/77 - 1980/81 goal difference used in all tiers
      if(Season>=1976 & Season<1981){
        xx <- maketable(df,Season,tier,pts=2)
      } else
        
        
        #1974/75 and before goal average.- the number of goals scored divided by the number of goals conceded
        if(Season<=1974){
          xx <- maketable(df,Season,tier,pts=2)
          xx <- xx %>% dplyr::mutate(gd=gf/ga) %>% dplyr::arrange(-Pts,-gd,-gf) %>% dplyr::mutate(Pos=1:nrow(xx))
          
          
          if(any(xx$team %in% deductions$team & Season %in% deductions$Season)==T && penalties==T){
            
            
            penalty <- deductions[deductions$team %in% xx$team & deductions$Season %in% Season,]
            # need if penalties has no rows ... just return xx
            
            if(nrow(penalty)>0){
              penalty$newPts <- xx$Pts[match(penalty$team,xx$team)]-penalty$deduction
              newPts <- penalty$newPts[match(xx$team,penalty$team)]
              xx$Pts <- ifelse(!is.na(newPts), newPts, xx$Pts)
              
              # rearrange by same rules as before...
              xx %>%
                dplyr::arrange(-Pts,-gd,-gf) %>%
                dplyr::mutate(Pos=1:nrow(xx)) %>%
                as.data.frame() -> xx
            }
          }
        }  else
          
          
          if(Season==1975 & tier>1){
            xx <- maketable(df,Season,tier,pts=2)
            xx <- xx %>% dplyr::mutate(gd=gf/ga) %>% dplyr::arrange(-Pts,-gd,-gf) %>% dplyr::mutate(Pos=1:nrow(xx))
          }  else
            
            
            if(Season==1975 & tier==1){
              xx <- maketable(df,Season,tier,pts=2)
              
            }
    
    
    
    return(xx)
  }
}
#######################################################
mevstheworld<-function(df=england, team_compare=theworld){
  
  temp<- df %>%
    dplyr::filter(home==team_care & visitor==team_compare | home==team_compare & visitor==team_care)%>%
    mutate(ResultType=case_when(home==team_care & result=="H" ~ "Win",
                                home==team_care & result=="A" ~ "Loss",
                                home==team_care & result=="D" ~ "Draw",
                                !(home==team_care) & result=="H" ~ "Loss",
                                !(home==team_care) & result=="A" ~ "Win",
                                !(home==team_care) & result=="D" ~ "Draw",
                                TRUE ~ "Check the data"
    ))%>%
    mutate(me_vs_theworld=ifelse(home==team_care, paste0(home, " vs. ", visitor),
                                 paste0(visitor, " vs. ", home)))%>%
    mutate(goal_me=ifelse(home==team_care, hgoal,vgoal))%>%
    mutate(goal_theworld=ifelse(home==team_care, vgoal,hgoal))%>%
    mutate(team_compare=team_compare)%>%
    select(Date, Season, me_vs_theworld, team_compare, goal_me, goal_theworld, ResultType)
  
  if (nrow(temp)==0) {
    temp1<-tibble(me_vs_theworld=paste0(team_care, " vs. ", team_compare),
                  team_compare=team_compare,
                  goal_me_sum="N/A", goal_theworld_sum="N/A", 
                  ResultType="Win", no_Result= "N/A")
    temp2<-tibble(me_vs_theworld=paste0(team_care, " vs. ", team_compare),
                  team_compare=team_compare,
                  goal_me_sum="N/A", goal_theworld_sum="N/A", 
                  ResultType="Draw", no_Result= "N/A")
    temp3<-tibble(me_vs_theworld=paste0(team_care, " vs. ", team_compare),
                  team_compare=team_compare,
                  goal_me_sum="N/A", goal_theworld_sum="N/A", 
                  ResultType="Loss", no_Result= "N/A")
    temp_sum_spread <-rbind(temp1, temp2, temp3)
  }
  else
  {
    temp_sum<-temp%>%
      mutate(goal_me_sum=sum(goal_me))%>%
      mutate(goal_theworld_sum=sum(goal_theworld))%>%
      group_by(me_vs_theworld, team_compare, goal_me_sum, goal_theworld_sum, ResultType)%>%
      mutate(no_Result=n_distinct(Date))%>%
      ungroup()%>%
      group_by(me_vs_theworld, goal_me_sum, goal_theworld_sum, ResultType, no_Result)%>%
      filter(row_number()==1)%>%
      ungroup()%>%
      select(me_vs_theworld, team_compare, goal_me_sum, goal_theworld_sum, ResultType, no_Result)%>%
      mutate(goal_me_sum=as.character(goal_me_sum),
             goal_theworld_sum=as.character(goal_theworld_sum),
             no_Result=as.character(no_Result)
      )
  }
}
