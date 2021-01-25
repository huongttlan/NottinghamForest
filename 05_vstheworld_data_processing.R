all_otherteam<- sort(unique(((rbind(england%>%mutate(team=home)%>%select(team),
                                    england%>%mutate(team=visitor)%>%select(team))
)%>%filter(!(team==team_care)))$team))

mevstheworld_result<-lapply(all_otherteam, function(x) { mevstheworld(team_compare=x)})

mevstheworld_result_spread<-as_tibble(bind_rows(mevstheworld_result))%>%
  spread(ResultType, no_Result)%>%
  mutate(Win=as.integer(Win), Draw=as.integer(Draw), Loss= as.integer(Loss),
         goal_me_sum=as.integer(goal_me_sum), goal_theworld_sum=as.integer(goal_theworld_sum))%>%
  mutate(Win=ifelse(is.na(Win), 0, Win))%>%
  mutate(Draw=ifelse(is.na(Draw), 0, Draw))%>%
  mutate(Loss=ifelse(is.na(Loss), 0, Loss))%>%
  mutate(goal_me_sum=ifelse(is.na(goal_me_sum), 0, goal_me_sum))%>%
  mutate(goal_theworld_sum=ifelse(is.na(goal_theworld_sum), 0, goal_theworld_sum))%>%
  select(team_compare, Win, Draw, Loss, goal_me_sum, goal_theworld_sum)

saveRDS(mevstheworld_result_spread, file = "mevstheworld_result_spread.rds")
