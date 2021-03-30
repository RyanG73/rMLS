# rMLS
### Provides numerous functions for acquiring and analyzing major league soccer data. Data is acquired from various online resources and is molded into a tidy format to analyze in R.  

### Current Status: beta testing and development  


# Installation  
```
library(devtools)  
install_github("ryang73/rMLS")
```


# Overview  
There are many great soccer packages available for both R and Python. What I discovered as I dug in though was a major gap in what was available specifically for MLS. Most packages only support the top European leagues. I initially wanted to mirror the availability of statistics, I quickly realized that Major League Soccer has so many more nuances to offer in a data package. Below is an outline of just a few, while full documentation can be found in each function.

## Player Stats  
- The player_stats() function provides up to 181 attributes, ranging from traditional statistics to modern/advanced ones such as expected contributions. 
- The function takes a playerID, as designated by the fbref.com system. 
- Example: the following line will return a tibble containing 12 observations and 181 variables (as of spring 2021) of MLS legend, Justin Meram. 
```
player_stats("ae34c71d")
```

## Team Stats  
- The team_stats() function provides up to 217 variables aggregated at the team level for each individual season from 1996 to the present. 
- Users are able to choose specific "start" or "end" seasons, and enter a team's unique id (found in the team_info table) to gather as much or little data as needed.
- Example: the following line will return a tibble containing 217 variables aggregated for the 2020 MLS Cup Champion Columbus Crew. 
```
team_stats(start_season = 2020, end_season = 2020,team_name="Columbus Crew SC")
```

## Fixtures  
- The fixtures() function provides 16 variables for every game played in MLS in the designated season window. 
- Example: the following line will return a tibble containing the results of every MLS game from 2017-2020. 
```
fixtures(start_season = 2017,end_season = 2020)
```

## Box Scores 
- The box_scores() function provides player level data for an individual game. 
- This function is builds on top of the fxtures() function by taking the resulting tibble from fixtures() as the argument. It will then pull the necessary team and game data to find the full box score for your game. 
- If you pass a fixtures dataset with more than one game, it will only return the box score for the first game in the dataset. However, you are able to easily scrape and concatenate the box scores of many fixtures via the following code. 
```
# one fixture
fix <- rMLS::fixtures()
box_score <- rMLS::box_scores(fix)

# many fixtures 
fix <- rMLS::fixtures(start_season=2015,end_season=2015)
box <- tibble::tibble()
for(i in 1:nrow(fix)){
  box_one_row <- rMLS::box_scores(fix,row=i)
  box <- dplyr::bind_rows(box,box_one_row)
}
```

## Rosters
- The rosters() function provides the roster for every team in Major League Soccer for the given years designated in the function.  
- Included is the player name and player id for each individual who played any amount to appear in the box score. 
- Example: the following line will return all MLS rosters for the 2020 season. 
```
rosters(start_season = 2020,end_season = 2020)
```

## Fivethirtyeight predictions 
  - Fivethirtyeight provides a basic elo model which defines playoff odds, matchup odds, and global club rankings. These function import data specifically for MLS.  
```
fte_matches(start_season=2017,end_season=2020)  
fte_rankings()
```






# Stale Datasets  
### The following are datasets which are not updated in real time, but contains relevant information to Major League Soccer currently and historically. 

## Team Infomation  
### rMLS::team_info
- Basic team information meant for mapping & charting:  
  - Team: ID, Name, Abbreviation, 
  - Location: City, State, Stadium  
  - Aesthetics: Logos, Colors, Wordmark  
  - Social: Twitter

## Odds and Probabilities  
### rMLS::historical_odds
- Historical Odds (Static)
  - 2012-2020
  - All matchup results and market odds going back
- Current Odds  
  - *In DEVELOPMENT*   


## MLS Specific Rules  
### rMLS::designated_players
  - The Designated Player Rule, nicknamed the Beckham Rule, allows Major League Soccer franchises to sign up to three players that would be considered outside their salary cap either by offering the player higher wages or by paying a transfer fee for the player.  
  - https://en.wikipedia.org/wiki/Designated_Player_Rule  
  - *action item*  
    - add player IDs  
    
### rMLS::homegrown_players  
  - The Homegrown Player Rule is a Major League Soccer program that allows MLS teams to sign local players from their own development academies directly to MLS first team rosters while avoiding the full amount of the contract against the salary cap.   
  - https://en.wikipedia.org/wiki/Homegrown_Player_Rule_(Major_League_Soccer)  
  - *action item*  
    - add player IDs  
    
### rMLS::salaries
  - The Major League Soccer Players Association has released player salary data annually from 2007 to 2019.  
  - In 2020, the union did not release salary data due to the COVID-19 pandemic.  
  - *Action Items*  
    - add player IDs 
  
### rMLS::draft 
  - years: 2000 - 2020
  - Player, Pick Position. 
  - *Action Items*  
    - get results back to 1996  
    - fill in all player IDs 


# Release Notes  
- Version: 0.0.0.9000 
- Author: Ryan Gerda (RyanGerda@gmail.com)  




# Further Documentation  
