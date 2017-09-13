#EPL Standings Function

#Function Setup
EPL_Standings <- function(date, season) {

#load libraries
library(tidyverse)
library(lubridate)
library(stringr)

options(digits=2)
        
#temp variables for testing
#season <- "201617"
#date <- "030617"

#Manipulate input date and season to generate correct url

#Expected input date Format mm/dd/yyyy
input_date <- mdy(date)

#Expected input season format yyyy/yy
#Replace / if it is present in the submitted season field
input_yr <- str_replace(season, "/", "")

#Grab last four of the season for use in the URL
input_yr <- str_sub(input_yr,str_length(input_yr) - 3, str_length(input_yr))

#Check for a valid season entered
if(str_length(season) < 4 || as.numeric(str_sub(input_yr,3,4)) != as.numeric(str_sub(input_yr,1,2)) + 1) {
        stop("Season entered (", season, ") is invalid. Please enter a valid season like '2016/17'.")
}

#Check that date entered is in current season
season_start <- str_c("08/01/", str_sub(input_yr,1,2))
season_end <- str_c("06/01/", str_sub(input_yr,3,4))

if(input_date < mdy(season_start) || input_date > mdy(season_end)) {
        stop("Date entered ",date, " does not fall within the ", season, " season.")
}

#Concatenate strings to generate URL
url_name <- str_c("http://www.football-data.co.uk/mmz4281/", input_yr, "/E0.csv")

#Load data from website and select only necessary fields and rows on or before the input_date
epl_games <- read.csv(url(url_name)) %>%
        select(Date:FTR) %>%
        filter(dmy(Date) <= input_date)

#Convert Date format for easier use moving forward
epl_games$Date <- dmy(epl_games$Date)

#Split Home and Away Games
epl_away <- select(epl_games, AwayTeam, FTAG, FTHG) %>%
        mutate(win = str_count(epl_games$FTR, "A"), 
               draw = str_count(epl_games$FTR, "D"), 
               loss = str_count(epl_games$FTR, "H")) %>%
        group_by(AwayTeam) %>%
        summarize(away_wins = sum(win), 
                  away_draws = sum(draw), 
                  away_losses = sum(loss), 
                  away_gf = sum(FTAG), 
                  away_ga = sum(FTHG))

epl_home <- select(epl_games, HomeTeam, FTHG, FTAG) %>%
        mutate(win = str_count(epl_games$FTR, "H"), 
               draw = str_count(epl_games$FTR, "D"), 
               loss = str_count(epl_games$FTR, "A")) %>%
        group_by(HomeTeam) %>%
        summarize(home_wins = sum(win), 
                  home_draws = sum(draw), 
                  home_losses = sum(loss), 
                  home_gf = sum(FTHG), 
                  home_ga = sum(FTAG))

#Rename HomeTeam/AwayTeam to TeamName
colnames(epl_home)[1] <- "TeamName"
colnames(epl_away)[1] <- "TeamName"

#Combine home and away stats
epl_totals <- merge(epl_home, epl_away, by="TeamName") %>%
        group_by(TeamName) %>%
        mutate(ttl_wins = home_wins + away_wins,
                  ttl_losses = home_losses + away_losses,
                  ttl_draws = home_draws + away_draws, 
                  ttl_gf = home_gf + away_gf, 
                  ttl_ga = home_ga + away_ga)

#Streak and Last10
#Select necessary data from base data set
epl_streak_home <- select(epl_games, HomeTeam, Date, FTR)
epl_streak_away <- select(epl_games, AwayTeam, Date, FTR)

#Create new levels for W/L and convert H and A to W and L
levels(epl_streak_home$FTR) <- c(levels(epl_streak_home$FTR), "W", "L")
levels(epl_streak_away$FTR) <- c(levels(epl_streak_away$FTR), "W", "L")
epl_streak_home$FTR[epl_streak_home$FTR == "H"] <- "W"
epl_streak_home$FTR[epl_streak_home$FTR == "A"] <- "L"
epl_streak_away$FTR[epl_streak_away$FTR == "A"] <- "W"
epl_streak_away$FTR[epl_streak_away$FTR == "H"] <- "L"

#Update column titles to match on both data frames
headers <- c("Team", "Date", "Result")
colnames(epl_streak_away) <- headers
colnames(epl_streak_home) <- headers

#Bind Home and Away Stats and order by team and most recent date
epl_streak_ttl <- rbind(epl_streak_away, epl_streak_home) 
epl_streak_ttl <- arrange(epl_streak_ttl, Team, desc(Date))

#Generate Last10 and Streak
#Create vector of unique team names
teams <- unique(epl_streak_ttl$Team)

#Create empty data frame to contain Last10 and Streak results
results <- data.frame(TeamName = character(), Last10 = character(), Streak = character())

#Run loop for each of the 20 teams
for(i in teams) {
        
        #Create temporary data frame based on the current team
        temp <- epl_streak_ttl %>% 
                filter(Team == i)
        
        #Set/Reset W/L/D to 0 for next team        
        wins <- 0
        draws <- 0
        losses <- 0
        
        #Variable to contain the results of the teams most recent game
        current_streak <- temp[1,3]
        count <- 1
        
        for(j in 1:(nrow(temp) - 1)) {
                if(temp$Result[j] != temp$Result[j+1]) {
                        break
                } else {
                        count <- count + 1
                }
        }
        
        #Combine the results of the most recent game (W/L/D) with the streak count
        streak_results = str_c(current_streak, count)
        
        #Keep only a maximum of 10 rows in the temp data frame
        temp <- head(temp, 10)
        
        #Count W/L/D for each team
        for (j in 1:nrow(temp)) {
                if (temp$Result[j] == "W") {
                        wins <- wins + 1
                } else if (temp$Result[j] == "D") {
                        draws <- draws + 1
                } else {
                        losses <- losses + 1
                }
        }
        
        #Combine wins, losses and draws in the last 10 games
        last10_rec <- str_c(wins, losses, draws, sep = '-')
        
        #Add Last10 and Streak to the data frame
        results <- rbind(results, data.frame(TeamName = i, Last10 = last10_rec, Streak = streak_results))
}

#Generate the season records and stats by team
epl_stats <- select(epl_totals, everything()) %>%
        group_by(TeamName) %>%
        summarize(Record = str_c(ttl_wins, ttl_losses, ttl_draws, sep = "-"), 
                  HomeRec = str_c(home_wins, home_losses, home_draws, sep = "-"), 
                  AwayRec = str_c(away_wins, away_losses, away_draws, sep = "-"), 
                  MatchesPlayed = sum(ttl_wins, ttl_losses, ttl_draws), 
                  Points = ttl_wins * 3 + ttl_draws, 
                  PPM = Points / MatchesPlayed, 
                  PtPct = Points / (3 * MatchesPlayed), 
                  GS = ttl_gf, 
                  GSM = ttl_gf / MatchesPlayed, 
                  GA = ttl_ga, 
                  GAM = ttl_ga / MatchesPlayed)

#Merge season stats and Last10-Streak data frames to get current season results
epl_season <- merge(epl_stats, results, by = "TeamName") %>% 
        arrange(desc(PPM), desc(epl_totals$ttl_wins), desc(GSM), GAM)
}
        