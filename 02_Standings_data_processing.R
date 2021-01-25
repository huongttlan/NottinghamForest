#  We need to know all the combination of Season and tier
all_tier<- sort(unique(england$tier))

all_Season<-list()
all_positions<-list()
for (val in all_tier) {
  temp<-sort(unique((england%>%filter(tier==val))$Season))
  temp_data<-lapply(temp, function(x) {
    data_use <-maketable_eng_new(england, Season = x, tier=val)%>%
      mutate(Season=x)%>%
      mutate(tier=val)%>%
      mutate(Pos=as.character(Pos))
  })
  all_positions[[length(all_positions) + 1]]<-bind_rows(temp_data)
}

all_positions<-as_tibble(bind_rows(all_positions))

# Let's make a dummy variable for Y axis to show Division 4 is lower than Division 3 etc

all_positions_yaxis_tier<-sort(unique(all_positions$tier))
all_positions_yaxis_Pos<-sort(unique(all_positions$Pos))
all_positions_yaxis<-all_positions%>%select(tier, Pos)%>%
  mutate(tier_n=as.numeric(tier))%>%
  mutate(Pos_n=as.numeric(Pos))%>%
  group_by(tier, Pos)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  arrange(desc(tier_n), desc(Pos_n))%>%
  mutate(ord_number_Position=row_number())%>%
  mutate(Position=paste0("Tier ", tier, " - Standing ", Pos))

all_positions_data<-all_positions%>%
  left_join(all_positions_yaxis, by=c("tier", "Pos"))%>%
  mutate(Position_label=paste0("Season ", Season, " - ", Position, " (Points: ", Pts, 
                               "/W: ", W,  "/D: ", D, "/L: ", L, "/GF: ", gf, "/GA: ", 
                               ga, "/GAvg: ", gd, ")"))%>%
  mutate(Tier_label=case_when(tier==1 ~ "Premier League",
                              tier==2 ~ "Championship",
                              tier==3 ~ "League One",
                              tier==4 ~ "League Two"))
saveRDS(all_positions_data, file = "all_positions_data.rds")
