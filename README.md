# rMLS
### Provides numerous functions for acquiring and analyzing major league soccer data. Data is acquired from various online resources and is mold into a tidy format to analyze in R.  
Current Status: beta testing and development  


# Installation  
```
library(devtools)  
install_github("ryang73/rMLS")
```


# Overview  
There are many great soccer packages available for both R and Python, but what I discovered as I dug in was a major gap in what was available specifically for MLS. Most packages only support the top European leagues. While I initially wanted to mirror the availability of statistics, I quickly realized that Major League Soccer has so many more nuances to offer in a data package. Below is an outline of just a few, while full documentation can be found in the ReadMe's of each function.  

## Player Stats  
- player 

## Team Stats  
- Team Information  
- Historical Seasons

## Fixtures  



## Team Infomation  
- Basic team information meant for mapping & charting:  
  - Team: ID, Name, Abbreviation, 
  - Location: City, State, Stadium  
  - Aesthetics: Logos, Colors, Wordmark  
  - Social: Twitter

## Odds and Probabilities  
- Historical Odds (Static)
  - 2012-2020
  - All matchup results and market odds going back
- Current Odds  
  - *In DEVELOPMENT* 
- Fivethirtyeight predictions 
  - Fivethirtyeight provides a basic elo model which defines playoff odds, matchup odds, and global club rankings. This function imports this data specifically for MLS.  
```
fte_matches(start_season=2017,end_season=2020)  
fte_rankings()
```

## MLS Specific Rules  
### Designated Players  
  - The Designated Player Rule, nicknamed the Beckham Rule, allows Major League Soccer franchises to sign up to three players that would be considered outside their salary cap either by offering the player higher wages or by paying a transfer fee for the player.  
  - https://en.wikipedia.org/wiki/Designated_Player_Rule  
  - *action item*  
    - add player IDs  
    
### Homegrown Players  
  - The Homegrown Player Rule is a Major League Soccer program that allows MLS teams to sign local players from their own development academies directly to MLS first team rosters.   
  - https://en.wikipedia.org/wiki/Homegrown_Player_Rule_(Major_League_Soccer)  
  - *action item*  
    - add player IDs  
    
### Salaries (Static)  
  - The Major League Soccer Players Association has released player salary data annually from 2007 to 2019.  
  - In 2020, the union did not release salary data due to the COVID-19 pandemic.  
  - *Action Items*  
    - add player IDs 
  
### MLS Superdraft (Static)  
  - years: 2000 - 2020
  - Player, Pick Position. 
  - *Action Items*  
    - get results back to 1996  
    - fill in all player IDs 


# Release Notes  
- Version: 0.0.0.9000 
- Author: Ryan Gerda (RyanGerda@gmail.com)  




# Further Documentation  


#
