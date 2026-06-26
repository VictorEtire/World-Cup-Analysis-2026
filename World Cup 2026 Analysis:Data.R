#1. Number of Goals Scored by Home Teams (World Cup 2026)

library(tidyverse)
matches %>%
  filter(!is.na(home_team_id)) %>%
  group_by(home_team_id) %>%
  summarise(goals = sum(home_score, na.rm = TRUE))



#2. Line Graph Showing the Possession vs. Shots on Target (World Cup 2026)

library(tidyverse)
match_team_stats %>%
  ggplot(aes(x = possession_pct, y = shots_on_target)) +
  geom_point() +
  geom_smooth(method = "lm")



#3. Line Graph Showing the Statistical Measure of Expected Goals vs. Actual Scoreline by Home Team (World Cup 2026)

library(tidyverse)
matches %>%
  ggplot(aes(x = home_xg, y = home_score)) +
  geom_point() +
  geom_smooth(method = "lm")



#4. Bar Chats Showing the Distribution of Goals by Minute (World Cup 2026)

goals_timeline <- match_events %>%
  filter(event_type == "Goal" | str_detect(event_type, "Goal")) 

ggplot(goals_timeline, aes(x = minute)) +
  geom_histogram(binwidth = 5, fill = "darkred", color = "white") +
  scale_x_continuous(breaks = seq(0, 120, by = 15)) +
  labs(
    title = "When are Goals Scored?",
    subtitle = "Distribution of goals by match minute",
    x = "Match Minute",
    y = "Number of Goals"
  ) +
  theme_minimal()



#5. Data frame for Goals Against and Goals For (World Cup 2026)

home_view <- matches %>%
  filter(stage_id == 1, status == "Completed",
         !is.na(home_score), !is.na(away_score))
  transmute(
    team_id  = home_team_id,
    opp_id   = away_team_id,
    gf       = home_score,
    ga       = away_score,
    result   = case_when(home_score > away_score ~ "W",
                         home_score < away_score ~ "L",
                         TRUE                   ~ "D")
  )

away_view <- matches %>%
  transmute(
    team_id  = away_team_id,
    opp_id   = home_team_id,
    gf       = away_score,
    ga       = home_score,
    result   = case_when(away_score > home_score ~ "W",
                         away_score < home_score ~ "L",
                         TRUE                   ~ "D")
  )

all_results <- bind_rows(home_view, away_view) %>%
  filter(!is.na(gf), !is.na(ga))



#6. Data Frame Showing a "Goal" Event Type (World Cup 2026)

goals <- match_events %>%
  filter(event_type == "Goal")




#7. Data Frame Showing a "Yellow and Red Card" Event Type (World Cup 2026)

cards <- match_events %>% 
  filter(event_type %in%c("Yellow Card", "Red Card"))



#8. Data Frame and Data Set Showing the Distribution of Goals by 15-Minute Interval (World Cup 2026)

goal_timing <- goals %>%
  mutate(
    period = cut(
      minute,
      breaks = c(0, 15, 30, 45, 60, 75, 90, Inf),
      labels = c("1-15", "16-30", "31-45",
                 "46-60", "61-75", "76-90", "90+")
    )
  ) %>%
  count(period) %>%
  ggplot(aes(x = period, y = n, fill = period)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, fontface = "bold") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "When Do Goals Happen?",
    subtitle = "Goals by 15-minute interval",
    x = "Match Minute (interval)",
    y = "Number of Goals",
    caption = "Source: WC2026 dataset"
  )

print(goal_timing)



#9. Longitudinal and Latitudinal Mapping of Venues, Stadium and Capacity (World Cup 2026)

library(leaflet)
leaflet(venues) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    popup = ~paste0(stadium_name, city, capacity))




#10. Analysis and Graph by FIFA Ranking, Performance and Elo Rating (World Cup 2026)

library(tidyverse)
team_tiers <- teams %>%
  mutate(elo_tier = case_when(
    elo_rating >= 1900 ~ "Elite (1900+)",
    elo_rating >= 1700 ~ "Competitive (1700-1899)",
    TRUE               ~ "Underdog (<1700)"
  ))

tier_performance <- match_team_stats %>%
  right_join(team_tiers, by = "team_id") %>%
  filter(!is.na(possession_pct) & !is.na(total_shots))


ggplot(tier_performance, aes(x = elo_tier, y = total_shots, fill = elo_tier)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red") +
  labs(
    title = "Shot Volume Based on Team ELO Tier",
    subtitle = "Analyzing if higher-rated teams create more offensive opportunities",
    x = "Team Quality Tier",
    y = "Total Shots per Match"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


my_predictions <- teams %>%
  mutate(Predictions = case_when(
    fifa_ranking_pre_tournament <= 10 ~ "Favorites",
    between (fifa_ranking_pre_tournament, 11, 21) ~ "Moderate",
    fifa_ranking_pre_tournament >= 21 ~ "Low chances"
    )
  ) %>%
  select(
    team_id, team_name, fifa_ranking_pre_tournament, manager_name, Predictions)
  

