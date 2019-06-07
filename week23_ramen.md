Week23 Ramen Ratings
================
Victor
07/06/2019

# Setup

``` r
library(tidyverse)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(viridis)
library(egg)
library(tidytext)
library(wordcloud)
library(LaCroixColoR)

# Read in data
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
```

# Clean data

``` r
cleaned_data <- ramen_ratings %>% 
  add_count(country) %>%
  arrange(desc(n)) %>%
  filter(n %in% head(unique(n),6)) %>%
  unnest_tokens(word, variety) %>% # separate variety column by word
  anti_join(get_stopwords()) %>% # remove stopwords
  group_by(country) %>%
  count(word, sort = TRUE)  %>%
  filter(n > 5)
```

    ## Joining, by = "word"

``` r
cleaned_data_wide <- cleaned_data %>% ungroup() %>% spread(word, n) %>% as.data.frame() # make wide
rownames(cleaned_data_wide) <- cleaned_data_wide$country # add rownames
cleaned_data_wide <- cleaned_data_wide[,2:ncol(cleaned_data_wide)] # remove countries variable
cleaned_data_wide[is.na(cleaned_data_wide)] <- 0 # replace NAs with 0
cleaned_data_wide <- t(cleaned_data_wide) # transpose

comparison.cloud(cleaned_data_wide,
                 colors = lacroix_palette("PassionFruit", 6),
                 title.size = 2,
                 scale = c(4, 0.5),
                 rot.per = 0.25,
                 title.bg.colors = c('white')) 
```

![](week23_ramen_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Save plot

``` r
png('plots/week23_ramen.png', units = 'in', height = 5.9, width = 10.1, res = 300)
comparison.cloud(cleaned_data_wide,
                 colors = lacroix_palette("PassionFruit", 6),
                 title.size = 1.7,
                 scale = c(4, 0.5),
                 rot.per = 0.3,
                 title.bg.colors = c('white')) 
dev.off()
```
