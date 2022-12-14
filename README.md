# comp456DeclanJoe

Our project, with contributors Joe Margolis and Declan Elias, explores if it is possible to be profitable betting on sports using the concepts of Positive Expected Value Betting and Closing Line value. To find Positive Expected Value Bets, we analyze the performance of different sportsbooks and attempt to create our own model for win probability based on the odds set by the sportsbooks. 

## Expected Value Analysis

### Data Dependencies

All data used can be found in [expected_value/betting_data](https://github.com/declanelias/comp456DeclanJoe/tree/main/expected_value/betting_data). This file contains both the raw data and the clean data. Instructions on how the raw data was collected and cleaned is contained in [expected value final writeup](https://github.com/declanelias/comp456DeclanJoe/blob/main/expected_value/final_work_ev.Rmd).

### Code Dependencies

R packages required:
dplyr
tidymodels
tidyverse
glmnet
probably
ggplot2
sf
maps
plotly


## Closing Line Value Analysis

### Data Dependencies

All data files used for this project can be found within the the folder [CLV_Analysis](https://github.com/declanelias/comp456DeclanJoe/tree/main/CLV_Analysis). The file [.betting_data.rdata](https://github.com/declanelias/comp456DeclanJoe/blob/main/CLV_Analysis/.betting_data.rdata.icloud) is where you can find the general lines data set that was used to create game ids for line history scraping. The folder [CLV_Analysis/basic_betting_data](https://github.com/declanelias/comp456DeclanJoe/tree/main/CLV_Analysis/basic_betting_data) contains the data used for general preliminary visualizations in the write up. Finally for any of the final cleaned data sets for each sport, you can download those withing the folder inside CLV_Analysis that corresponds with the respective sport. Instructions on how to scrape the data yourself along with more detail on what each data set contains can be found in the [Closing Line Value Final Narrative](https://github.com/declanelias/comp456DeclanJoe/blob/main/CLV_Analysis/CLV_Final_Narrative.html)

### Code Dependencies

R Packages Required:
dplyr
tidymodels
tidyverse
glmnet
probably
ggplot2
sf
maps
plotly
ranger
remotes
kableExtra
vip
rpart.plot
