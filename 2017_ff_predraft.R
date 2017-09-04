library(dplyr)
library(devtools)
#devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)
library(lme4)

# League Parameters--will be used to determine value
# Note that this was designed for rather shallow leagues, 
# Those seeking to use this tool for deeper leagues will likely benefit by adding to 
# the usage spreadsheet referenced below

teams = 10
starting_qb = 1
starting_rb = 2
starting_wr = 2
starting_te = 1
flex_spots = 1
bench_spots = 7
pass_yd_pt = .025
rush_yd_pt = .1
rec_yd_pt = .1
pass_td_pt = 4
rush_td_pt = 6
rec_td_pt = 6
int_pt = -2
ppr = 0
decay_factor = 0.985

budget = 200
remove_players = TRUE

# Pull the play by play data for the prior years (will take 5-10 minutes, so I usually
# comment this line out after I load it into my environment)

# pbp_2016 <- season_play_by_play(2016)
# pbp_2015 <- season_play_by_play(2015)
# pbp_2016 = rbind(pbp_2016, pbp_2015)

# Correct error that refers to the Jaguars as both JAX AND JAC (and relocation)

pbp_2016 = cbind(pbp_2016 %>% select_if(function(col) !is.character(col)),
  data.frame(lapply(data.frame(pbp_2016 %>% select_if(is.character), 
                               stringsAsFactors = FALSE), 
         function(x){
  gsub("STL", "LA", gsub("JAX", "JAC", x))
}), stringsAsFactors = FALSE))

# Create the decay factor (more recent data gets heavier weighting)

pbp_2016 = data.frame(pbp_2016 %>%
                        mutate(weight = decay_factor ^ (as.numeric(
                          max(pbp_2016$Date) - Date) %/% 7)
                        ))

# Split data into pass and rush

pass_data_16 = data.frame(pbp_2016 %>%
                         filter(PassAttempt == 1 & PlayType != 'No Play' &
                                  !is.na(Passer)) %>%
                         group_by(Offense = posteam, GameID, DefensiveTeam,
                                  home = ifelse(posteam == HomeTeam, 1, 0),
                                  weight) %>%
                         summarise(att = n(),
                                   yds = sum(Yards.Gained),
                                   td = sum(Touchdown),
                                   int = sum(InterceptionThrown)))

rush_data_16 = data.frame(pbp_2016 %>%
                            filter(RushAttempt == 1 & PlayType != 'No Play' &
                                     !is.na(Rusher)) %>%
                            group_by(Offense = posteam, GameID, DefensiveTeam,
                                     home = ifelse(posteam == HomeTeam, 1, 0),
                                     weight) %>%
                            summarise(att = n(),
                                      yds = sum(Yards.Gained),
                                      td = sum(Touchdown)))

# Below is a series of mixed effects models predicting various pass and rush team
# statistics, as well as dataframes with the random effects in a more digestible format.

# The reason I chose mixed effects was so that I could best estimate the impact of 
# both the defense and the offense on each statistic (attempts, yards, TDs, etc)

pass_att_model_16 = lmer(att ~ home + (1 | Offense) + (1 | DefensiveTeam),
                 data = pass_data_16, weights = weight,
                 control=lmerControl("nloptwrap"))

most_att_offense_16 = data.frame(ranef(pass_att_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(pass_att_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_att_defense_16 = data.frame(ranef(pass_att_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(pass_att_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

rec_yd_model_16 = lmer(yds ~ home + (1 | Offense) + (1 | DefensiveTeam),
                    data = pass_data_16, weights = weight,
                    control=lmerControl("nloptwrap"))

most_rec_yd_offense_16 = data.frame(ranef(rec_yd_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(rec_yd_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_rec_yd_defense_16 = data.frame(ranef(rec_yd_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(rec_yd_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

rec_td_model_16 = lmer(td ~ home + (1 | Offense) + (1 | DefensiveTeam),
                       data = pass_data_16, weights = weight,
                       control=lmerControl("nloptwrap"))

most_rec_td_offense_16 = data.frame(ranef(rec_td_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(rec_td_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_rec_td_defense_16 = data.frame(ranef(rec_td_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(rec_td_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

int_model_16 = lmer(int ~ home + (1 | Offense) + (1 | DefensiveTeam),
                       data = pass_data_16, weights = weight,
                       control=lmerControl("nloptwrap"))

most_int_offense_16 = data.frame(ranef(int_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(int_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_int_defense_16 = data.frame(ranef(int_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(int_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))


rush_yd_model_16 = lmer(yds ~ home + (1 | Offense) + (1 | DefensiveTeam),
                       data = rush_data_16, weights = weight,
                       control=lmerControl("nloptwrap"))

most_rush_yd_offense_16 = data.frame(ranef(rush_yd_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(rush_yd_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_rush_yd_defense_16 = data.frame(ranef(rush_yd_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(rush_yd_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

rush_td_model_16 = lmer(td ~ home + (1 | Offense) + (1 | DefensiveTeam),
                       data = rush_data_16, weights = weight,
                       control=lmerControl("nloptwrap"))

most_rush_td_offense_16 = data.frame(ranef(rush_td_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(rush_td_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

most_rush_td_defense_16 = data.frame(ranef(rush_td_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(rush_td_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

### Individual Player work
# Below I have defined dataframes for relevant fantasy volume and efficiency statistics
# from last year.  For V0, these won't be used in the models, but I hope to add them as
# predictors in future years. These are particularly useful for contextualizing usage
# and influencing predictions for the usage priors sheet.

rush_player_16 = rush_data_16 = data.frame(pbp_2016 %>%
                          filter(RushAttempt == 1 & PlayType != 'No Play' &
                                                      !is.na(Rusher)) %>%
                          mutate(home = ifelse(posteam == HomeTeam, 1, 0)) %>%
                          select(
                            Offense = posteam,
                            Rusher,
                            home,
                            Yards.Gained,
                            DefensiveTeam,
                            yrdline100,
                            Touchdown,
                            weight
                          ))

goal_line_rush_df = data.frame(rush_player_16 %>%
                                 filter(yrdline100 <= 10))

rec_player_16 = rush_data_16 = data.frame(pbp_2016 %>%
                          filter(PassAttempt == 1 & PlayType != 'No Play' &
                                                      !is.na(Receiver)) %>%
                          mutate(home = ifelse(posteam == HomeTeam, 1, 0)) %>%
                          select(
                            Offense = posteam,
                            Receiver,
                            home,
                            Yards.Gained,
                            DefensiveTeam,
                            yrdline100,
                            Touchdown,
                            weight
                          ))

goal_line_rec_df = data.frame(rec_player_16 %>%
                                 filter(yrdline100 <= 20))

ypc_model_16 = lmer(Yards.Gained ~ home + yrdline100 + (1 | Offense) + 
                      (1 | Rusher) + (1 | DefensiveTeam), data = rush_player_16,
                    weights = weight,
                    control = lmerControl("nloptwrap"))

best_ypc_rb_16 = data.frame(ranef(ypc_model_16)$Rusher) %>% 
  mutate(rb = rownames(data.frame(ranef(ypc_model_16)$Rusher))) %>%
  arrange(desc(X.Intercept.))

best_ypc_offense_16 = data.frame(ranef(ypc_model_16)$Offense) %>% 
  mutate(team = rownames(data.frame(ranef(ypc_model_16)$Offense))) %>%
  arrange(desc(X.Intercept.))

best_ypc_defense_16 = data.frame(ranef(ypc_model_16)$DefensiveTeam) %>% 
  mutate(team = rownames(data.frame(ranef(ypc_model_16)$DefensiveTeam))) %>%
  arrange(desc(X.Intercept.))

goal_line_shares_rb = data.frame(goal_line_rush_df %>% 
                                group_by(Offense, Rusher) %>%
                                summarise(count = n()) %>%
                                mutate(freq = count / sum(count)) %>%
                                filter(count >= 5) %>%
                                arrange(desc(freq)))

goal_line_shares_wr = data.frame(goal_line_rec_df %>% 
                                group_by(Offense, Receiver) %>%
                                summarise(count = n()) %>%
                                mutate(freq = count / sum(count)) %>%
                                filter(count >= 5) %>%
                                arrange(desc(freq)))

rush_shares = data.frame(rush_player_16 %>% 
                                group_by(Offense, Rusher) %>%
                                summarise(count = n()) %>%
                                mutate(freq = count / sum(count)) %>%
                                filter(count >= 30) %>%
                                arrange(desc(freq)))

target_shares_wr = data.frame(rec_player_16 %>% 
                                group_by(Offense, Receiver) %>%
                                summarise(count = n()) %>%
                                mutate(freq = count / sum(count)) %>%
                                filter(count >= 20) %>%
                                arrange(desc(freq))) 

yard_shares_rb = data.frame(rush_player_16 %>%
                              group_by(Offense, Rusher) %>%
                              summarise(yards = sum(Yards.Gained)) %>%
                              mutate(share = yards / sum(yards)) %>%
                              filter(yards >= 100) %>%
                              arrange(desc(share)))

yard_shares_wr = data.frame(rec_player_16 %>%
                              group_by(Offense, Receiver) %>%
                              summarise(yards = sum(Yards.Gained)) %>%
                              mutate(share = yards / sum(yards)) %>%
                              filter(yards >= 100) %>%
                              arrange(desc(share)))

### Predictions
# Start by loading in the present season's schedule, I copied a link off of CBSSports
# and used some pretty low-tech Excel functions to create it.

schedule_2017 = read.csv("nfl_schedule_2017.csv", strip.white = TRUE)
schedule_2017 = data.frame(schedule_2017 %>%
                             filter(away != ""))

# Map full team names to their 2/3 letter abbreviations
# Would love to hear if you know of a better source for a .csv / JSON of the NFL Schedule
# Excludes week 17. Week 17 is terrible

team_name_key = read.csv("nfl_team_names.csv", strip.white = TRUE)

# Shape the schedule into something the models can predict off of (offense defense, etc.)

predict_df_2017 = merge(
  merge(
    schedule_2017, 
    team_name_key, 
    by.x = "away", 
    by.y = "full"), 
  team_name_key, 
  by.x = "home", 
  by.y = "full") %>% 
      select(Offense = team.x, DefensiveTeam = team.y)

predict_df_2017 = rbind(data.frame(predict_df_2017 %>%
                                     mutate(home = 0)), 
                        data.frame(predict_df_2017 %>% select(
                          "Offense" = DefensiveTeam, "DefensiveTeam" = Offense) %>%
                            mutate(home = 1)))

# Predict on the dataset with each model:

projections_2017 = cbind(predict_df_2017, 
                            rush_yd = predict(rush_yd_model_16, 
                                                  newdata = predict_df_2017))

projections_2017 = cbind(projections_2017,
                            rush_td = predict(rush_td_model_16,
                                              newdata = predict_df_2017))

projections_2017 = cbind(projections_2017,
                            catches = predict(pass_att_model_16,
                                              newdata = predict_df_2017))

projections_2017 = cbind(projections_2017,
                            rec_yd = predict(rec_yd_model_16,
                                             newdata = predict_df_2017))

projections_2017 = cbind(projections_2017,
                            rec_td = predict(rec_td_model_16,
                                             newdata = predict_df_2017))

projections_2017 = cbind(projections_2017,
                            ints = predict(int_model_16,
                                            newdata = predict_df_2017))

# For V0 I just sum it up at this point, eventually I hope tu do more with the
# week by week predictions instead of just season-long projections

# At this point, we have season-long projections for each team's rushing and passing 
# units, taking schedule into accoutn.

projections_season = data.frame(projections_2017 %>%
                                  group_by(Offense) %>%
                                  summarise(rush_yd = sum(rush_yd),
                                            rush_td = sum(rush_td),
                                            catches = sum(catches),
                                            rec_yd = sum(rec_yd),
                                            rec_td = sum(rec_td),
                                            ints = sum(ints)))

# Load in usage csv, where playing time / touches are allocated

usage = read.csv("usage_priors.csv")

# Calculate Fantasy Point output given above scoring parameters

projections_season = merge(projections_season, usage, by.x = "Offense", by.y = "Team")
projections_season = data.frame(projections_season %>% 
                                  mutate(qb_fp = pass_yd_pt*rec_yd + pass_td_pt*rec_td + 
                                           ints * int_pt + 
                                           qb_att_rate*rush_yd*rush_yd_pt + 
                                           qb_gl_rate*rush_td*rush_td_pt,
                                        rb1_fp = rush_yd_pt*rush_yd*rb1_att_rate + 
                                                  rush_td_pt*rush_td*rb1_gl_run_rate +
                                                  ppr*catches*rb1_rec_rate + 
                                                  rec_yd_pt*rec_yd*rb1_rec_rate +
                                                  rec_td_pt*rec_td*rb1_gl_rec_rate,
                                        rb2_fp = rush_yd_pt*rush_yd*rb2_att_rate + 
                                                  rush_td_pt*rush_td*rb2_gl_run_rate +
                                                  ppr*catches*rb2_rec_rate +
                                                  rec_yd_pt*rec_yd*rb2_rec_rate +
                                                  rec_td_pt*rec_td*rb2_gl_rec_rate,
                                        wr1_fp = rec_yd_pt*rec_yd*wr1_rec_rate +
                                                 ppr*catches*wr1_rec_rate +
                                                  rec_td_pt*rec_td*wr1_gl_rate,
                                        wr2_fp = rec_yd_pt*rec_yd*wr2_rec_rate +
                                                 ppr*catches*wr2_rec_rate +
                                                  rec_td_pt*rec_td*wr2_gl_rate,
                                        te_fp = rec_yd_pt*rec_yd*te_rec_rate +
                                                ppr*catches*te_rec_rate +
                                                  rec_td_pt*rec_td*te_gl_rate))

# Separate by position to determine replacement levels

qb_rankings = data.frame(projections_season %>% 
                           group_by(Offense, player = qb) %>%
                           summarise(points = sum(qb_fp)) %>%
                           mutate(position = "QB") %>%
                           arrange(desc(points)))

rostered_qbs = round((starting_qb + .1*bench_spots)*teams)

qb_rankings$start_pct = NA

for(i in as.numeric(rownames(qb_rankings))) {
  qb_rankings$start_pct[i] = 
    (phyper(starting_qb - 1, i - 1, rostered_qbs - i, (rostered_qbs %/% teams) - 1) * 
       (rostered_qbs - ((rostered_qbs %% teams) * 
                       ((rostered_qbs %/% teams) + 1)))/rostered_qbs) + 
  (phyper(starting_qb - 1, i - 1, rostered_qbs - i, (rostered_qbs %/% teams)) * 
     ((rostered_qbs %% teams) * ((rostered_qbs %/% teams) + 1))/rostered_qbs)
}

qb_rankings$start_pct = ifelse(is.na(qb_rankings$start_pct), 0, qb_rankings$start_pct)

# Start percentage was leading to some wonkiness in valuations here
# Question to be resolved in V1, but I just wasn't okay with Matt Ryan starting in 
# 80% of leagues. If you draft him, he's starting. This is a potential downfall of the 
# start percentage calculation--it goes based on random roster construction,
# but the presence of a higher ranked QB clearly affects the probability that another
# QB will be on the roster. 

# I would recommend turning back on for 2 or more QB leagues

qb_rankings$start_pct = 1

qb_rankings$starting_points = qb_rankings$points * qb_rankings$start_pct

qb_replacement_level = qb_rankings$starting_points[rostered_qbs]

if(is.na(qb_replacement_level)){
  print("Not enough QBs projected to create true replacement level")
  qb_replacement = min(qb_rankings$starting_points)
}

qb_rankings = data.frame(qb_rankings %>%
                        mutate(value = pmax(0, starting_points - qb_replacement_level)))

rb_rankings = data.frame(rbind(
  data.frame(projections_season %>% 
                           group_by(Offense, player = rb1) %>%
                           summarise(points = sum(rb1_fp))),
  data.frame(projections_season %>% 
                           group_by(Offense, player = rb2) %>%
                           summarise(points = sum(rb2_fp)))) %>%
    filter(is.na(points) == FALSE) %>%
    mutate(position = "RB") %>%
    arrange(desc(points)))

rostered_rbs = round((starting_rb + 0.5*flex_spots + .4*bench_spots)*teams)

rb_rankings$start_pct = NA

for(i in as.numeric(rownames(rb_rankings))) {
  rb_rankings$start_pct[i] = 
    (phyper(starting_rb - 1, i - 1, rostered_rbs - i, (rostered_rbs %/% teams) - 1) * 
       (rostered_rbs - ((rostered_rbs %% teams) * 
                       ((rostered_rbs %/% teams) + 1)))/rostered_rbs) + 
  (phyper(starting_rb - 1, i - 1, rostered_rbs - i, (rostered_rbs %/% teams)) * 
     ((rostered_rbs %% teams) * ((rostered_rbs %/% teams) + 1))/rostered_rbs)
}

rb_rankings$start_pct = ifelse(is.na(rb_rankings$start_pct), 0, rb_rankings$start_pct)

wr_rankings = data.frame(rbind(
  data.frame(projections_season %>% 
                           group_by(Offense, player = wr1) %>%
                           summarise(points = sum(wr1_fp))),
  data.frame(projections_season %>% 
                           group_by(Offense, player = wr2) %>%
                           summarise(points = sum(wr2_fp)))) %>%
    filter(is.na(points) == FALSE) %>%
    mutate(position = "WR") %>%
    arrange(desc(points)))

rostered_wrs = round((starting_wr + 0.5*flex_spots + .4*bench_spots)*teams)

wr_rankings$start_pct = NA

for(i in as.numeric(rownames(wr_rankings))) {
  wr_rankings$start_pct[i] = 
    (phyper(starting_wr - 1, i - 1, rostered_wrs - i, (rostered_wrs %/% teams) - 1) * 
       (rostered_wrs - ((rostered_wrs %% teams) * 
                       ((rostered_wrs %/% teams) + 1)))/rostered_wrs) + 
  (phyper(starting_wr - 1, i - 1, rostered_wrs - i, (rostered_wrs %/% teams)) * 
     ((rostered_wrs %% teams) * ((rostered_wrs %/% teams) + 1))/rostered_wrs)
}

wr_rankings$start_pct = ifelse(is.na(wr_rankings$start_pct), 0, wr_rankings$start_pct)

te_rankings = data.frame(projections_season %>% 
                           group_by(Offense, player = te) %>%
                           summarise(points = sum(te_fp)) %>%
                           mutate(position = "TE") %>%
                           arrange(desc(points)))

rostered_tes = round((starting_te + .1*bench_spots)*teams)

te_rankings$start_pct = NA

for(i in as.numeric(rownames(te_rankings))) {
  te_rankings$start_pct[i] = 
    (phyper(starting_te - 1, i - 1, rostered_tes - i, (rostered_tes %/% teams) - 1) * 
       (rostered_tes - ((rostered_tes %% teams) * 
                       ((rostered_tes %/% teams) + 1)))/rostered_tes) + 
  (phyper(starting_te - 1, i - 1, rostered_tes - i, (rostered_tes %/% teams)) * 
     ((rostered_tes %% teams) * ((rostered_tes %/% teams) + 1))/rostered_tes)
}

te_rankings$start_pct = ifelse(is.na(te_rankings$start_pct), 0, te_rankings$start_pct)

# Same thing as QBs. The one-starter positions made this kind of weird, because
# in all likelihood you're going to draft the player knowing they'll be started

te_rankings$start_pct = 1

te_rankings = data.frame(te_rankings %>% 
                           mutate(starting_points = start_pct * points))

# RB Flex start percentage (ignoring the TE flex case for now)
# Also only allowing for 1 Flex at the momoent. It's a bit more involved to allow for 2+
# and I haven't figured out a great way to solve for the general case.

rb_rankings$flex_pct = NA

for(i in as.numeric(rownames(rb_rankings))) {
  wrs_above = nrow(data.frame(wr_rankings %>%
                                filter(points > rb_rankings$points[i])))
  wrs_below = rostered_wrs - wrs_above
  rb_rankings$flex_pct[i] = 
    # Must consider all RB and WR roster setup types
    # Low End for both RBs and WRs
    ((dhyper(starting_rb, i - 1, rostered_rbs - i, (rostered_rbs %/% teams) - 1) * 
       (rostered_rbs - ((rostered_rbs %% teams) * 
                       ((rostered_rbs %/% teams) + 1)))/rostered_rbs) *
    phyper(starting_wr, wrs_above, wrs_below, (rostered_wrs %/% teams)) * 
       (rostered_wrs - ((rostered_wrs %% teams) * 
                       ((rostered_wrs %/% teams) + 1)))/rostered_wrs) +
    # Low End for RBs, High End for WRs
    ((dhyper(starting_rb, i - 1, rostered_rbs - i, (rostered_rbs %/% teams) - 1) * 
       (rostered_rbs - ((rostered_rbs %% teams) * 
                       ((rostered_rbs %/% teams) + 1)))/rostered_rbs) *
    (phyper(starting_wr, wrs_above, wrs_below, (rostered_wrs %/% teams) + 1) * 
     ((rostered_wrs %% teams) * ((rostered_wrs %/% teams) + 1))/rostered_wrs)) +
    # High End for RBs, Low End for WRs
    ((dhyper(starting_rb, i - 1, rostered_rbs - i, (rostered_rbs %/% teams)) * 
     ((rostered_rbs %% teams) * ((rostered_rbs %/% teams) + 1))/rostered_rbs) *
    phyper(starting_wr, wrs_above, wrs_below, (rostered_wrs %/% teams)) * 
       (rostered_wrs - ((rostered_wrs %% teams) * 
                       ((rostered_wrs %/% teams) + 1)))/rostered_wrs) +
    # High End for RBs and WRs
    ((dhyper(starting_rb, i - 1, rostered_rbs - i, (rostered_rbs %/% teams)) * 
     ((rostered_rbs %% teams) * ((rostered_rbs %/% teams) + 1))/rostered_rbs) *
    (phyper(starting_wr, wrs_above, wrs_below, (rostered_wrs %/% teams) + 1) * 
     ((rostered_wrs %% teams) * ((rostered_wrs %/% teams) + 1))/rostered_wrs))
}

if(flex_spots == 0){
  rb_rankings$flex_pct = 0
}

rb_rankings = data.frame(rb_rankings %>%
                           mutate(flex_pct = ifelse(is.na(flex_pct), 0, flex_pct)) %>%
                           mutate(start_pct = start_pct + flex_pct) %>%
                           mutate(starting_points = start_pct * points) %>%
                           select(-flex_pct))

# WR Flex start percent

wr_rankings$flex_pct = NA

for(i in as.numeric(rownames(wr_rankings))) {
  rbs_above = nrow(data.frame(rb_rankings %>%
                                filter(points > wr_rankings$points[i])))
  rbs_below = rostered_rbs - rbs_above
  wr_rankings$flex_pct[i] = 
    # Must consider all RB and WR roster setup types
    # Low End for both WRs and RBs
    ((dhyper(starting_wr, i - 1, rostered_wrs - i, (rostered_wrs %/% teams) - 1) * 
       (rostered_wrs - ((rostered_wrs %% teams) * 
                       ((rostered_wrs %/% teams) + 1)))/rostered_wrs) *
    phyper(starting_rb, rbs_above, rbs_below, (rostered_rbs %/% teams)) * 
       (rostered_rbs - ((rostered_rbs %% teams) * 
                       ((rostered_rbs %/% teams) + 1)))/rostered_rbs) +
    # Low End for WRs, High End for RBs
    ((dhyper(starting_wr, i - 1, rostered_wrs - i, (rostered_wrs %/% teams) - 1) * 
       (rostered_wrs - ((rostered_wrs %% teams) * 
                       ((rostered_wrs %/% teams) + 1)))/rostered_wrs) *
    (phyper(starting_rb, rbs_above, rbs_below, (rostered_rbs %/% teams) + 1) * 
     ((rostered_rbs %% teams) * ((rostered_rbs %/% teams) + 1))/rostered_rbs)) +
    # High End for WRs, Low End for RBs
    ((dhyper(starting_wr, i - 1, rostered_wrs - i, (rostered_wrs %/% teams)) * 
     ((rostered_wrs %% teams) * ((rostered_wrs %/% teams) + 1))/rostered_wrs) *
    phyper(starting_rb, rbs_above, rbs_below, (rostered_rbs %/% teams)) * 
       (rostered_rbs - ((rostered_rbs %% teams) * 
                       ((rostered_rbs %/% teams) + 1)))/rostered_rbs) +
    # High End for RBs and WRs
    ((dhyper(starting_wr, i - 1, rostered_wrs - i, (rostered_wrs %/% teams)) * 
     ((rostered_wrs %% teams) * ((rostered_wrs %/% teams) + 1))/rostered_wrs) *
    (phyper(starting_rb, rbs_above, rbs_below, (rostered_rbs %/% teams) + 1) * 
     ((rostered_rbs %% teams) * ((rostered_rbs %/% teams) + 1))/rostered_rbs))
}

if(flex_spots == 0){
  wr_rankings$flex_pct = 0
}

wr_rankings = data.frame(wr_rankings %>%
                           mutate(flex_pct = ifelse(is.na(flex_pct), 0, flex_pct)) %>%
                           mutate(start_pct = start_pct + flex_pct) %>%
                           mutate(starting_points = points * start_pct) %>%
                           select(-flex_pct))

rb_replacement_level = rb_rankings$starting_points[rostered_rbs]

if(is.na(rb_replacement_level)){
  print("Not enough RBs projected to create true replacement level")
  rb_replacement_level = min(rb_rankings$starting_points)
}

rb_rankings = data.frame(rb_rankings %>%
                          mutate(value = pmax(0, starting_points - rb_replacement_level)))

wr_replacement_level = wr_rankings$starting_points[rostered_wrs]

if(is.na(wr_replacement_level)){
  print("Not enough WRs projected to create true replacement level")
  wr_replacement_level = min(wr_rankings$starting_points)
}

wr_rankings = data.frame(wr_rankings %>%
                          mutate(value = pmax(0, starting_points - wr_replacement_level)))

te_replacement_level = te_rankings$starting_points[rostered_tes]

if(is.na(te_replacement_level)){
  print("Not enough TEs projected to create true replacement level")
  te_replacement = min(te_rankings$starting_points)
}

te_rankings = data.frame(te_rankings %>%
                          mutate(value = pmax(0, starting_points - te_replacement_level)))

# Compiled Rankings

compiled_rankings = data.frame(
  rbind(qb_rankings, rb_rankings, wr_rankings, te_rankings) %>%
  arrange(desc(value)))

roster_size = starting_qb + starting_rb + starting_wr + starting_te + flex_spots +
  bench_spots

auction_pool = (budget - 2)*teams

removal_list = read.csv("remove_from_pool.csv", strip.white = TRUE)

if(remove_players == TRUE){
  compiled_rankings = data.frame(
    compiled_rankings %>%
      filter(!player %in% removal_list$player))
  auction_pool = auction_pool - sum(removal_list$drafted_value)
}

total_value = sum(compiled_rankings$value)

compiled_rankings = data.frame(compiled_rankings %>%
  mutate(auction_values = round((value * auction_pool / total_value), 1)) %>%
    arrange(desc(auction_values), position, desc(points)))



