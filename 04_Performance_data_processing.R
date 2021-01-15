dfhome<-england%>%mutate(team = home, opp = visitor,
                         GF=as.numeric(as.character(hgoal)), GA=as.numeric(as.character(vgoal)),
                         GD = GF-GA, result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")), venue="home") 
dfaway<-england%>%mutate(team = visitor, opp = home, GF=as.numeric(as.character(vgoal)),
                         GA=as.numeric(as.character(hgoal)), GD = GF-GA, 
                         result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")), venue="away")
dfboth <- rbind(dfhome,dfaway) %>% select(Date, Season, team, opp, GF, GA, GD)

#to calculate the cumulative goals for, goals against, goal difference and points for each game within a season for each team. 
# To enable comparisons across seasons, assume 3 points for a win throughout and I am not factoring in points penalties incurred.
mydf <- dfboth %>%
  group_by(Season, team) %>%
  mutate(result = ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
         pts = ifelse(GD>0, 3, ifelse(GD<0, 0, 1)),
         gameno = dense_rank(Date)) %>%
  arrange(Season,team, gameno) %>%
  mutate(Cumpts = cumsum(pts),
         CumGF = cumsum(GF),
         CumGA = cumsum(GA),
         CumGD = cumsum(GD),
         pts.pg = Cumpts/gameno,
         GF.pg = CumGF/gameno,
         GA.pg = CumGA/gameno,
         GD.pg = CumGD/gameno
  ) %>%
  select(Season, team, gameno, Cumpts, CumGF, CumGA, CumGD, pts.pg, GF.pg, GA.pg, GD.pg)
#to compare each team’s performance from one season to the next, keep only the cumulative data for the last match of each team in each season. 

mydf.final <- 
  mydf %>% filter(gameno == max(gameno))%>%
  ungroup()%>%
  arrange(team,Season)
#Calculating differences from one season to the next.
mydf.final1 <- mydf.final %>% 
  arrange(team, Season) %>%
  group_by(team)%>%
  mutate(pts.pglag = lag(pts.pg), pts.pgDIF = pts.pg - pts.pglag,
         GF.pglag = lag(GF.pg), GF.pgDIF = GF.pg - GF.pglag,
         GA.pglag = lag(GA.pg), GA.pgDIF = GA.pg - GA.pglag,
         GD.pglag = lag(GD.pg), GD.pgDIF = GD.pg - GF.pglag
  )
mydf.final2<-mydf.final1%>%filter(team==team_care)
saveRDS(mydf.final2, file = "mydf.final2.rds")


