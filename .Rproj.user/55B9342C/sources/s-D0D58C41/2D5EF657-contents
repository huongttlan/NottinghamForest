detailed_goals<-
  rbind(england%>%mutate(team = home, opp = visitor,GF=as.numeric(as.character(hgoal)),
                         GA=as.numeric(as.character(vgoal)),venue="Home")%>%
          select(Date, Season, team, opp, GF, GA, venue),
        england%>%mutate(team = visitor, opp = home, GF=as.numeric(as.character(vgoal)),
                         GA=as.numeric(as.character(hgoal)),venue="Away")%>%
          select(Date, Season, team, opp, GF, GA, venue))

detailed_goals_scored<-detailed_goals%>%
  mutate(goals=GF)%>%
  mutate(goaltype="Goals Scored")%>%
  select(Date, Season, team, opp, venue, goals, goaltype)

detailed_goals_against<-detailed_goals%>%
  mutate(goals=GA)%>%
  mutate(goaltype="Goals Against")%>%
  select(Date, Season, team, opp, venue, goals, goaltype)

detailed_goals_final<-rbind(detailed_goals_scored, detailed_goals_against)
detailed_goals_final<-rbind(detailed_goals_final%>%mutate(venue="All"),
                            detailed_goals_final)%>%
  filter(team==team_care)
saveRDS(detailed_goals_final, file = "detailed_goals_final.rds")

