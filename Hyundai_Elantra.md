# Hyundai: ElantraSales regression
bdanalytics  

**  **    
**Date: (Sun) Mar 22, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/elantra.csv  
    New:        <prdct_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(plyr))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- FALSE    # or TRUE
glb_predct_var <- "ElantraSales"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Year", "Month")                # or NULL

glb_exclude_vars_as_features <- glb_id_vars     # or NULL                      
# List chrs converted into factors; num/int transformed  
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_is_regression <- TRUE; glb_is_classification <- FALSE

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/elantra.csv", 
    comment="glb_entity_df", force_header=TRUE, print_diagn=TRUE)
```

```
## [1] "Reading file ./data/elantra.csv..."
## [1] "dimensions of data in ./data/elantra.csv: 50 rows x 7 cols"
##   Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 1     1 2010         7690          9.7     153    213.377 217.466
## 2     1 2011         9659          9.1     259    229.353 221.082
## 3     1 2012        10900          8.2     354    244.178 227.666
## 4     1 2013        12174          7.9     230    242.560 231.321
## 5     1 2014        15326          6.6     232    247.575 234.933
## 6     2 2010         7966          9.8     130    209.924 217.251
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 8      2 2012        13820          8.3     296    247.615 228.138
## 21     5 2012        18877          8.2     275    242.208 228.884
## 37     9 2012        18305          7.8     374    254.333 231.086
## 42    10 2013        14876          7.2     223    243.374 233.782
## 43    11 2010         8631          9.8     161    219.303 219.544
## 50    12 2013        21692          6.7     279    246.189 234.594
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 45    11 2012        15923          7.8     246    248.136 231.190
## 46    11 2013        16751          7.0     231    242.301 234.033
## 47    12 2010        13096          9.4     170    227.190 220.437
## 48    12 2011        13025          8.5     253    243.015 227.093
## 49    12 2012        19024          7.9     275    244.698 231.099
## 50    12 2013        21692          6.7     279    246.189 234.594
## 'data.frame':	50 obs. of  7 variables:
##  $ Month       : int  1 1 1 1 1 2 2 2 2 2 ...
##  $ Year        : int  2010 2011 2012 2013 2014 2010 2011 2012 2013 2014 ...
##  $ ElantraSales: int  7690 9659 10900 12174 15326 7966 12289 13820 16219 16393 ...
##  $ Unemployment: num  9.7 9.1 8.2 7.9 6.6 9.8 9 8.3 7.7 6.7 ...
##  $ Queries     : int  153 259 354 230 232 130 266 296 239 240 ...
##  $ CPI_energy  : num  213 229 244 243 248 ...
##  $ CPI_all     : num  217 221 228 231 235 ...
##  - attr(*, "comment")= chr "glb_entity_df"
## NULL
```

```r
if (glb_is_separate_predict_dataset) {
    glb_predct_df <- myimport_data(
        url="<prdct_url>", 
        comment="glb_predct_df", force_header=TRUE, print_diagn=TRUE)
} else {
    glb_predct_df <- subset(glb_entity_df, Year > "2012")
#     glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
#                                           max(2, nrow(glb_entity_df) / 1000)),]    
    comment(glb_predct_df) <- "glb_predct_df"
    myprint_df(glb_predct_df)
    str(glb_predct_df)
}         
```

```
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 4      1 2013        12174          7.9     230    242.560 231.321
## 5      1 2014        15326          6.6     232    247.575 234.933
## 9      2 2013        16219          7.7     239    252.639 232.599
## 10     2 2014        16393          6.7     240    246.389 235.169
## 14     3 2013        26153          7.5     313    244.598 232.075
## 18     4 2013        24445          7.5     248    238.860 231.707
## 22     5 2013        25090          7.5     252    240.972 232.124
## 26     6 2013        22163          7.5     320    245.412 232.860
## 30     7 2013        23958          7.3     274    245.926 233.252
## 34     8 2013        24700          7.2     271    244.917 233.433
## 38     9 2013        19691          7.2     298    245.566 233.743
## 42    10 2013        14876          7.2     223    243.374 233.782
## 46    11 2013        16751          7.0     231    242.301 234.033
## 50    12 2013        21692          6.7     279    246.189 234.594
## 'data.frame':	14 obs. of  7 variables:
##  $ Month       : int  1 1 2 2 3 4 5 6 7 8 ...
##  $ Year        : int  2013 2014 2013 2014 2013 2013 2013 2013 2013 2013 ...
##  $ ElantraSales: int  12174 15326 16219 16393 26153 24445 25090 22163 23958 24700 ...
##  $ Unemployment: num  7.9 6.6 7.7 6.7 7.5 7.5 7.5 7.5 7.3 7.2 ...
##  $ Queries     : int  230 232 239 240 313 248 252 320 274 271 ...
##  $ CPI_energy  : num  243 248 253 246 245 ...
##  $ CPI_all     : num  231 235 233 235 232 ...
##  - attr(*, "comment")= chr "glb_predct_df"
```

```r
glb_entity_df <- subset(glb_entity_df, Year <= "2012")
print(dim(glb_entity_df))
```

```
## [1] 36  7
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# Month = the month of the year for the observation (1 = January, 2 = February, 3 = March, ...).
# Year = the year of the observation.
# ElantraSales = the number of units of the Hyundai Elantra sold in the United States in the given month.
# Unemployment = the estimated unemployment percentage in the United States in the given month.
# Queries = a (normalized) approximation of the number of Google searches for "hyundai elantra" in the given month.
# CPI_energy = the monthly consumer price index (CPI) for energy for the given month.
# CPI_all = the consumer price index (CPI) for all products for the given month; this is a measure of the magnitude of the prices paid by consumer households for goods and services (e.g., food, clothing, electricity, etc.).

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)

        Month.fctr=factor(Month, 
                    as.factor(union(obs_df$Month, obs_twin_df$Month))) 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>)        
                        )

    # If levels of a factor are different across obs_df & glb_predct_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_predct_df)
```

```
##      Month            Year       ElantraSales    Unemployment  
##  Min.   : 1.00   Min.   :2010   Min.   : 7690   Min.   :7.800  
##  1st Qu.: 3.75   1st Qu.:2010   1st Qu.:10690   1st Qu.:8.200  
##  Median : 6.50   Median :2011   Median :14449   Median :9.000  
##  Mean   : 6.50   Mean   :2011   Mean   :14462   Mean   :8.878  
##  3rd Qu.: 9.25   3rd Qu.:2012   3rd Qu.:18238   3rd Qu.:9.500  
##  Max.   :12.00   Max.   :2012   Max.   :22100   Max.   :9.900  
##                                                                
##     Queries        CPI_energy       CPI_all        Month.fctr
##  Min.   :130.0   Min.   :204.2   Min.   :217.3   1      : 3  
##  1st Qu.:175.2   1st Qu.:215.8   1st Qu.:218.8   2      : 3  
##  Median :270.5   Median :242.6   Median :225.3   3      : 3  
##  Mean   :264.6   Mean   :233.9   Mean   :224.2   4      : 3  
##  3rd Qu.:344.2   3rd Qu.:247.1   3rd Qu.:228.7   5      : 3  
##  Max.   :427.0   Max.   :256.4   Max.   :231.7   6      : 3  
##                                                  (Other):18  
##        Month         Year ElantraSales Unemployment      Queries 
##            0            0            0            0            0 
##   CPI_energy      CPI_all   Month.fctr 
##            0            0            0
```

```r
glb_predct_df <- add_new_diag_feats(glb_predct_df, glb_entity_df)
```

```
##      Month             Year       ElantraSales    Unemployment 
##  Min.   : 1.000   Min.   :2013   Min.   :12174   Min.   :6.60  
##  1st Qu.: 2.250   1st Qu.:2013   1st Qu.:16262   1st Qu.:7.05  
##  Median : 5.500   Median :2013   Median :20692   Median :7.25  
##  Mean   : 5.786   Mean   :2013   Mean   :19974   Mean   :7.25  
##  3rd Qu.: 8.750   3rd Qu.:2013   3rd Qu.:24323   3rd Qu.:7.50  
##  Max.   :12.000   Max.   :2014   Max.   :26153   Max.   :7.90  
##                                                                
##     Queries        CPI_energy       CPI_all        Month.fctr
##  Min.   :223.0   Min.   :238.9   Min.   :231.3   1      :2   
##  1st Qu.:233.8   1st Qu.:242.8   1st Qu.:232.2   2      :2   
##  Median :250.0   Median :245.2   Median :233.3   3      :1   
##  Mean   :260.7   Mean   :244.8   Mean   :233.3   4      :1   
##  3rd Qu.:277.8   3rd Qu.:246.1   3rd Qu.:234.0   5      :1   
##  Max.   :320.0   Max.   :252.6   Max.   :235.2   6      :1   
##                                                  (Other):6   
##        Month         Year ElantraSales Unemployment      Queries 
##            0            0            0            0            0 
##   CPI_energy      CPI_all   Month.fctr 
##            0            0            0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](Hyundai_Elantra_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_predct_df <- na.omit(glb_predct_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_predct_df <- mymap_codes(glb_predct_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))
# glb_predct_df$<col_name>.fctr <- factor(glb_predct_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_predct_df$<col_name>), -2, na.pad=TRUE)
# glb_predct_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_predct_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_predct_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_predct_df <- mutate(glb_predct_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_predct_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features())
```

```
##                        id      cor.y cor.y.abs
## Queries           Queries  0.6100645 0.6100645
## CPI_all           CPI_all  0.5936217 0.5936217
## CPI_energy     CPI_energy  0.5916491 0.5916491
## Unemployment Unemployment -0.5671458 0.5671458
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
                    merge(glb_feats_df, mydelete_cor_features(), all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                 Queries    CPI_all CPI_energy Unemployment
## Queries       1.0000000  0.7536732  0.8328381   -0.6411093
## CPI_all       0.7536732  1.0000000  0.9132259   -0.9562123
## CPI_energy    0.8328381  0.9132259  1.0000000   -0.8007188
## Unemployment -0.6411093 -0.9562123 -0.8007188    1.0000000
##                Queries   CPI_all CPI_energy Unemployment
## Queries      0.0000000 0.7536732  0.8328381    0.6411093
## CPI_all      0.7536732 0.0000000  0.9132259    0.9562123
## CPI_energy   0.8328381 0.9132259  0.0000000    0.8007188
## Unemployment 0.6411093 0.9562123  0.8007188    0.0000000
## [1] "cor(CPI_all, Unemployment)=-0.9562"
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(ElantraSales, CPI_all)=0.5936"
## [1] "cor(ElantraSales, Unemployment)=-0.5671"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping Unemployment as a feature
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-2.png) 

```
##                    id     cor.y cor.y.abs
## Queries       Queries 0.6100645 0.6100645
## CPI_all       CPI_all 0.5936217 0.5936217
## CPI_energy CPI_energy 0.5916491 0.5916491
##              Queries   CPI_all CPI_energy
## Queries    1.0000000 0.7536732  0.8328381
## CPI_all    0.7536732 1.0000000  0.9132259
## CPI_energy 0.8328381 0.9132259  1.0000000
##              Queries   CPI_all CPI_energy
## Queries    0.0000000 0.7536732  0.8328381
## CPI_all    0.7536732 0.0000000  0.9132259
## CPI_energy 0.8328381 0.9132259  0.0000000
## [1] "cor(CPI_all, CPI_energy)=0.9132"
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(ElantraSales, CPI_all)=0.5936"
## [1] "cor(ElantraSales, CPI_energy)=0.5916"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping CPI_energy as a feature
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-4.png) 

```
##              id     cor.y cor.y.abs
## Queries Queries 0.6100645 0.6100645
## CPI_all CPI_all 0.5936217 0.5936217
##           Queries   CPI_all
## Queries 1.0000000 0.7536732
## CPI_all 0.7536732 1.0000000
##           Queries   CPI_all
## Queries 0.0000000 0.7536732
## CPI_all 0.7536732 0.0000000
## [1] "cor(Queries, CPI_all)=0.7537"
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(ElantraSales, Queries)=0.6101"
## [1] "cor(ElantraSales, CPI_all)=0.5936"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping CPI_all as a feature
```

![](Hyundai_Elantra_files/figure-html/remove_correlated_features-6.png) 

```
##              id     cor.y cor.y.abs
## Queries Queries 0.6100645 0.6100645
##             id      cor.y cor.y.abs cor.low
## 3      Queries  0.6100645 0.6100645       1
## 1      CPI_all  0.5936217 0.5936217      NA
## 2   CPI_energy  0.5916491 0.5916491      NA
## 4 Unemployment -0.5671458 0.5671458      NA
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 656724357
## [1] 0.06366177
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6044.4 -2582.9  -731.9  2485.0  6765.2 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7120.649   1725.097   4.128 0.000224 ***
## Queries       27.751      6.181   4.489  7.8e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3296 on 34 degrees of freedom
## Multiple R-squared:  0.3722,	Adjusted R-squared:  0.3537 
## F-statistic: 20.16 on 1 and 34 DF,  p-value: 7.805e-05
## 
##     feats n.fit  R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB
## 1 Queries    36 0.3721787 0.06366177    0.3721787 369407024 656724357
##   f.score.OOB
## 1          NA
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)    
```

```
## [1] 425871091
## [1] 0.3928056
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6652.1 -2263.3  -892.9  3027.4  7063.6 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)   
## (Intercept)          9115.2905  2959.5117   3.080  0.00431 **
## Queries               176.5581   633.5956   0.279  0.78236   
## Queries:CPI_all        -0.6319     2.5576  -0.247  0.80649   
## Queries:CPI_energy      0.2063     0.3949   0.522  0.60515   
## Queries:Unemployment   -7.2229    13.1651  -0.549  0.58718   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3359 on 31 degrees of freedom
## Multiple R-squared:  0.4055,	Adjusted R-squared:  0.3288 
## F-statistic: 5.286 on 4 and 31 DF,  p-value: 0.002302
## 
##                                                                feats n.fit
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 656724357
## [1] 0.06366177
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6044.4 -2582.9  -731.9  2485.0  6765.2 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7120.649   1725.097   4.128 0.000224 ***
## Queries       27.751      6.181   4.489  7.8e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3296 on 34 degrees of freedom
## Multiple R-squared:  0.3722,	Adjusted R-squared:  0.3537 
## F-statistic: 20.16 on 1 and 34 DF,  p-value: 7.805e-05
## 
##                                                                feats n.fit
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
glb_sel_mdl <- glb_mdl                        

# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                        glb_predct_var),
                                                glb_exclude_vars_as_features),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 180470729
## [1] 0.7426902
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3865.1 -1211.7   -77.1  1207.5  3562.2 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  312509.280 144061.867   2.169 0.042288 *  
## Unemployment  -7739.381   2968.747  -2.607 0.016871 *  
## Queries          -4.764     12.938  -0.368 0.716598    
## CPI_energy      288.631     97.974   2.946 0.007988 ** 
## CPI_all       -1343.307    592.919  -2.266 0.034732 *  
## Month.fctr2    2254.998   1943.249   1.160 0.259540    
## Month.fctr3    6696.557   1991.635   3.362 0.003099 ** 
## Month.fctr4    7556.607   2038.022   3.708 0.001392 ** 
## Month.fctr5    7420.249   1950.139   3.805 0.001110 ** 
## Month.fctr6    9215.833   1995.230   4.619 0.000166 ***
## Month.fctr7    9929.464   2238.800   4.435 0.000254 ***
## Month.fctr8    7939.447   2064.629   3.845 0.001010 ** 
## Month.fctr9    5013.287   2010.745   2.493 0.021542 *  
## Month.fctr10   2500.184   2084.057   1.200 0.244286    
## Month.fctr11   3238.932   2397.231   1.351 0.191747    
## Month.fctr12   5293.911   2228.310   2.376 0.027621 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2306 on 20 degrees of freedom
## Multiple R-squared:  0.8193,	Adjusted R-squared:  0.6837 
## F-statistic: 6.044 on 15 and 20 DF,  p-value: 0.0001469
## 
##                                                                feats n.fit
## 4             Unemployment, Queries, CPI_energy, CPI_all, Month.fctr    36
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 4 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
Prb2.1_mdl <- glb_mdl

# User specified
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("Month.fctr", 
                            "Unemployment", "Queries", "CPI_energy", "CPI_all"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 180470729
## [1] 0.7426902
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3865.1 -1211.7   -77.1  1207.5  3562.2 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  312509.280 144061.867   2.169 0.042288 *  
## Month.fctr2    2254.998   1943.249   1.160 0.259540    
## Month.fctr3    6696.557   1991.635   3.362 0.003099 ** 
## Month.fctr4    7556.607   2038.022   3.708 0.001392 ** 
## Month.fctr5    7420.249   1950.139   3.805 0.001110 ** 
## Month.fctr6    9215.833   1995.230   4.619 0.000166 ***
## Month.fctr7    9929.464   2238.800   4.435 0.000254 ***
## Month.fctr8    7939.447   2064.629   3.845 0.001010 ** 
## Month.fctr9    5013.287   2010.745   2.493 0.021542 *  
## Month.fctr10   2500.184   2084.057   1.200 0.244286    
## Month.fctr11   3238.932   2397.231   1.351 0.191747    
## Month.fctr12   5293.911   2228.310   2.376 0.027621 *  
## Unemployment  -7739.381   2968.747  -2.607 0.016871 *  
## Queries          -4.764     12.938  -0.368 0.716598    
## CPI_energy      288.631     97.974   2.946 0.007988 ** 
## CPI_all       -1343.307    592.919  -2.266 0.034732 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2306 on 20 degrees of freedom
## Multiple R-squared:  0.8193,	Adjusted R-squared:  0.6837 
## F-statistic: 6.044 on 15 and 20 DF,  p-value: 0.0001469
## 
##                                                                feats n.fit
## 5             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 4             Unemployment, Queries, CPI_energy, CPI_all, Month.fctr    36
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 5 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 4 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
Prb3.1_mdl <- glb_mdl

# User specified
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("Month.fctr", 
                            "Unemployment", "CPI_energy", "CPI_all"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 190757747
## [1] 0.7280232
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3866.0 -1283.3  -107.2  1098.3  3650.1 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  325709.15  136627.85   2.384 0.026644 *  
## Month.fctr2    2410.91    1857.10   1.298 0.208292    
## Month.fctr3    6880.09    1888.15   3.644 0.001517 ** 
## Month.fctr4    7697.36    1960.21   3.927 0.000774 ***
## Month.fctr5    7444.64    1908.48   3.901 0.000823 ***
## Month.fctr6    9223.13    1953.64   4.721 0.000116 ***
## Month.fctr7    9602.72    2012.66   4.771 0.000103 ***
## Month.fctr8    7919.50    2020.99   3.919 0.000789 ***
## Month.fctr9    5074.29    1962.23   2.586 0.017237 *  
## Month.fctr10   2724.24    1951.78   1.396 0.177366    
## Month.fctr11   3665.08    2055.66   1.783 0.089062 .  
## Month.fctr12   5643.19    1974.36   2.858 0.009413 ** 
## Unemployment  -7971.34    2840.79  -2.806 0.010586 *  
## CPI_energy      268.03      78.75   3.403 0.002676 ** 
## CPI_all       -1377.58     573.39  -2.403 0.025610 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2258 on 21 degrees of freedom
## Multiple R-squared:  0.818,	Adjusted R-squared:  0.6967 
## F-statistic: 6.744 on 14 and 21 DF,  p-value: 5.73e-05
## 
##                                                                feats n.fit
## 5             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 4             Unemployment, Queries, CPI_energy, CPI_all, Month.fctr    36
## 6                      Month.fctr, Unemployment, CPI_energy, CPI_all    36
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 5 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 4 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 6 0.8180391 0.72802323    0.8180391 107064906 190757747          NA
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
Prb6.1_mdl <- glb_mdl
glb_sel_mdl <- glb_mdl                        

ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("Month.fctr", 
                            "Unemployment", "Queries", "CPI_energy", "CPI_all"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 180470729
## [1] 0.7426902
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3865.1 -1211.7   -77.1  1207.5  3562.2 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  312509.280 144061.867   2.169 0.042288 *  
## Month.fctr2    2254.998   1943.249   1.160 0.259540    
## Month.fctr3    6696.557   1991.635   3.362 0.003099 ** 
## Month.fctr4    7556.607   2038.022   3.708 0.001392 ** 
## Month.fctr5    7420.249   1950.139   3.805 0.001110 ** 
## Month.fctr6    9215.833   1995.230   4.619 0.000166 ***
## Month.fctr7    9929.464   2238.800   4.435 0.000254 ***
## Month.fctr8    7939.447   2064.629   3.845 0.001010 ** 
## Month.fctr9    5013.287   2010.745   2.493 0.021542 *  
## Month.fctr10   2500.184   2084.057   1.200 0.244286    
## Month.fctr11   3238.932   2397.231   1.351 0.191747    
## Month.fctr12   5293.911   2228.310   2.376 0.027621 *  
## Unemployment  -7739.381   2968.747  -2.607 0.016871 *  
## Queries          -4.764     12.938  -0.368 0.716598    
## CPI_energy      288.631     97.974   2.946 0.007988 ** 
## CPI_all       -1343.307    592.919  -2.266 0.034732 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2306 on 20 degrees of freedom
## Multiple R-squared:  0.8193,	Adjusted R-squared:  0.6837 
## F-statistic: 6.044 on 15 and 20 DF,  p-value: 0.0001469
## 
##                                                                feats n.fit
## 5             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 7             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 4             Unemployment, Queries, CPI_energy, CPI_all, Month.fctr    36
## 6                      Month.fctr, Unemployment, CPI_energy, CPI_all    36
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 5 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 7 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 4 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 6 0.8180391 0.72802323    0.8180391 107064906 190757747          NA
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
```

```r
Prb4.1_mdl <- glb_mdl
glb_sel_mdl <- glb_mdl                        

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

if (glb_is_regression)
    print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", 
                    size=3.5))
```

![](Hyundai_Elantra_files/figure-html/run_models-1.png) 

```r
if (glb_is_classification) {
    plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
    print(myplot_hbar(df=plot_models_df, xcol_name="feats.label", 
                      ycol_names="f.score.OOB"))
}

script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats())
```

```
##                        id         Pr.z
## Month.fctr     Month.fctr 0.0001658816
## CPI_energy     CPI_energy 0.0079881486
## Unemployment Unemployment 0.0168712350
## CPI_all           CPI_all 0.0347321946
## Queries           Queries 0.7165981623
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3865.1 -1211.7   -77.1  1207.5  3562.2 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  312509.280 144061.867   2.169 0.042288 *  
## Month.fctr2    2254.998   1943.249   1.160 0.259540    
## Month.fctr3    6696.557   1991.635   3.362 0.003099 ** 
## Month.fctr4    7556.607   2038.022   3.708 0.001392 ** 
## Month.fctr5    7420.249   1950.139   3.805 0.001110 ** 
## Month.fctr6    9215.833   1995.230   4.619 0.000166 ***
## Month.fctr7    9929.464   2238.800   4.435 0.000254 ***
## Month.fctr8    7939.447   2064.629   3.845 0.001010 ** 
## Month.fctr9    5013.287   2010.745   2.493 0.021542 *  
## Month.fctr10   2500.184   2084.057   1.200 0.244286    
## Month.fctr11   3238.932   2397.231   1.351 0.191747    
## Month.fctr12   5293.911   2228.310   2.376 0.027621 *  
## CPI_energy      288.631     97.974   2.946 0.007988 ** 
## Unemployment  -7739.381   2968.747  -2.607 0.016871 *  
## CPI_all       -1343.307    592.919  -2.266 0.034732 *  
## Queries          -4.764     12.938  -0.368 0.716598    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2306 on 20 degrees of freedom
## Multiple R-squared:  0.8193,	Adjusted R-squared:  0.6837 
## F-statistic: 6.044 on 15 and 20 DF,  p-value: 0.0001469
## 
##                                                                feats n.fit
## 5             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 7             Month.fctr, Unemployment, Queries, CPI_energy, CPI_all    36
## 4             Unemployment, Queries, CPI_energy, CPI_all, Month.fctr    36
## 6                      Month.fctr, Unemployment, CPI_energy, CPI_all    36
## 2 Queries, Queries:CPI_all, Queries:CPI_energy, Queries:Unemployment    36
## 1                                                            Queries    36
## 3                                                            Queries    36
## 8             Month.fctr, CPI_energy, Unemployment, CPI_all, Queries    36
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 5 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 7 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 4 0.8192642 0.74269016    0.8192642 106344077 180470729          NA
## 6 0.8180391 0.72802323    0.8180391 107064906 190757747          NA
## 2 0.4054717 0.39280555    0.4054717 349817625 425871091          NA
## 1 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 3 0.3721787 0.06366177    0.3721787 369407024 656724357          NA
## 8 0.8192642         NA    0.8192642 106344077        NA          NA
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Hyundai_Elantra_files/figure-html/fit_training.all-1.png) 

```
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 28     7 2011        15181          9.0     427    246.968 225.553
## 16     4 2011        22100          9.1     305    247.129 224.056
## 49    12 2012        19024          7.9     275    244.698 231.099
## 48    12 2011        13025          8.5     253    243.015 227.093
## 27     7 2010        18215          9.5     156    206.834 217.677
## 19     5 2010         9781          9.6     177    206.172 217.299
##    Month.fctr ElantraSales.predict ElantraSales.predict.err
## 28          7             19046.07                 3865.072
## 16          4             18537.84                 3562.158
## 49         12             15542.74                 3481.260
## 48         12             15899.43                 2874.431
## 27          7             15463.28                 2751.719
## 19          5             12396.79                 2615.787
```

```r
if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
    glb_entity_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_entity_df, type="response") >= 0.5) * 1.0
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_entity_df))                        
}    

print(glb_feats_df <- mymerge_feats_Pr.z())
```

```
##             id      cor.y cor.y.abs cor.low         Pr.z
## 3   Month.fctr         NA        NA      NA 0.0001658816
## 2   CPI_energy  0.5916491 0.5916491      NA 0.0079881486
## 5 Unemployment -0.5671458 0.5671458      NA 0.0168712350
## 1      CPI_all  0.5936217 0.5936217      NA 0.0347321946
## 4      Queries  0.6100645 0.6100645       1 0.7165981623
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1])
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1])
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(glb_entity_df)
```

![](Hyundai_Elantra_files/figure-html/fit_training.all-2.png) ![](Hyundai_Elantra_files/figure-html/fit_training.all-3.png) ![](Hyundai_Elantra_files/figure-html/fit_training.all-4.png) ![](Hyundai_Elantra_files/figure-html/fit_training.all-5.png) 

```
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 28     7 2011        15181          9.0     427    246.968 225.553
## 16     4 2011        22100          9.1     305    247.129 224.056
## 49    12 2012        19024          7.9     275    244.698 231.099
## 48    12 2011        13025          8.5     253    243.015 227.093
## 27     7 2010        18215          9.5     156    206.834 217.677
##    Month.fctr ElantraSales.predict ElantraSales.predict.err  .label
## 28          7             19046.07                 3865.072  2011:7
## 16          4             18537.84                 3562.158  2011:4
## 49         12             15542.74                 3481.260 2012:12
## 48         12             15899.43                 2874.431 2011:12
## 27          7             15463.28                 2751.719  2010:7
```

![](Hyundai_Elantra_files/figure-html/fit_training.all-6.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_predct_df, type="response")

if (glb_is_classification)
    glb_predct_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_predct_df, type="response") >= 0.5) * 1.0
    
myprint_df(glb_predct_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    Year Month ElantraSales ElantraSales.predict
## 4  2013     1        12174             9547.885
## 5  2014     1        15326            16195.016
## 9  2013     2        16219            14500.256
## 10 2014     2        16393            16978.629
## 14 2013     3        26153            18520.189
## 18 2013     4        24445            18528.047
## 22 2013     5        25090            18422.064
## 26 2013     6        22163            20186.569
## 30 2013     7        23958            22288.986
## 34 2013     8        24700            20552.830
## 38 2013     9        19691            17268.947
## 42 2013    10        14876            14428.049
## 46 2013    11        16751            16029.692
## 50 2013    12        21692            20546.435
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2) / nrow(glb_predct_df)) ^ 0.5))                        
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    
    glb_predct_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_predct_df[, glb_predct_var_name] - glb_predct_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_predct_df)))                                                      
#     glb_predct_df[, "<Output Pred variable>"] <- func(glb_predct_df[, glb_pred_var_name])                         
}                         
```

```
## [1] "Total SSE: 180470729.0430"
## [1] "RMSE: 3590.3713"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](Hyundai_Elantra_files/figure-html/predict_newdata-1.png) 

```
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 14     3 2013        26153          7.5     313    244.598 232.075
## 22     5 2013        25090          7.5     252    240.972 232.124
## 18     4 2013        24445          7.5     248    238.860 231.707
## 34     8 2013        24700          7.2     271    244.917 233.433
## 4      1 2013        12174          7.9     230    242.560 231.321
## 38     9 2013        19691          7.2     298    245.566 233.743
##    Month.fctr ElantraSales.predict ElantraSales.predict.err
## 14          3            18520.189                 7632.811
## 22          5            18422.064                 6667.936
## 18          4            18528.047                 5916.953
## 34          8            20552.830                 4147.170
## 4           1             9547.885                 2626.115
## 38          9            17268.947                 2422.053
```

```r
if (glb_is_classification)
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
    
glb_analytics_diag_plots(glb_predct_df)
```

![](Hyundai_Elantra_files/figure-html/predict_newdata-2.png) ![](Hyundai_Elantra_files/figure-html/predict_newdata-3.png) ![](Hyundai_Elantra_files/figure-html/predict_newdata-4.png) ![](Hyundai_Elantra_files/figure-html/predict_newdata-5.png) 

```
##    Month Year ElantraSales Unemployment Queries CPI_energy CPI_all
## 14     3 2013        26153          7.5     313    244.598 232.075
## 22     5 2013        25090          7.5     252    240.972 232.124
## 18     4 2013        24445          7.5     248    238.860 231.707
## 34     8 2013        24700          7.2     271    244.917 233.433
## 4      1 2013        12174          7.9     230    242.560 231.321
##    Month.fctr ElantraSales.predict ElantraSales.predict.err .label
## 14          3            18520.189                 7632.811 2013:3
## 22          5            18422.064                 6667.936 2013:5
## 18          4            18528.047                 5916.953 2013:4
## 34          8            20552.830                 4147.170 2013:8
## 4           1             9547.885                 2626.115 2013:1
```

![](Hyundai_Elantra_files/figure-html/predict_newdata-6.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13     survival_2.38-1
## [5] ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5   formatR_1.0     
##  [5] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  knitr_1.9       
##  [9] labeling_0.3     lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1 
## [17] scales_0.2.4     splines_3.1.2    stringr_0.6.2    tcltk_3.1.2     
## [21] tools_3.1.2      yaml_2.1.13
```
