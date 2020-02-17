Statistical assignment 3
================
Emilia COok 670005042
15/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

``` r
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.5.3

``` r
# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths


files <- dir(
             # Select the folder where the files are stored.
             "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "/Users/emili/Documents/Data SOC Term 2 Year 2/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6,names_to = "variable",values_to = "value") %>% separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>% pivot_wider(names_from = variable, values_from = value)
  
Long
```

    ## # A tibble: 584,234 x 6
    ##        pidp wave  memorig sex_dv age_dv vote6
    ##       <int> <chr>   <int>  <int>  <int> <int>
    ##  1 68001367 a           1      1     39     3
    ##  2 68001367 b          NA     NA     NA    NA
    ##  3 68001367 c          NA     NA     NA    NA
    ##  4 68001367 d          NA     NA     NA    NA
    ##  5 68001367 e          NA     NA     NA    NA
    ##  6 68001367 f          NA     NA     NA    NA
    ##  7 68001367 g          NA     NA     NA    NA
    ##  8 68004087 a           1      1     59     2
    ##  9 68004087 b           1      1     60     2
    ## 10 68004087 c           1      1     61     2
    ## # ... with 584,224 more rows

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig==1) %>%
        mutate(sex_dv = ifelse(sex_dv==2, "female", ifelse(sex_dv == 1, "male", NA))) %>%
        mutate(vote6= ifelse(vote6<0, NA, vote6))

Long %>%
        count(sex_dv)
```

    ## # A tibble: 3 x 2
    ##   sex_dv      n
    ##   <chr>   <int>
    ## 1 female 117665
    ## 2 male   100342
    ## 3 <NA>        8

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
        filter(!is.na(sex_dv)) %>%
  group_by(sex_dv, wave) %>%
  summarise( meanVote6 = mean(vote6,na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex_dv [2]
    ##    sex_dv wave  meanVote6
    ##    <chr>  <chr>     <dbl>
    ##  1 female a          2.84
    ##  2 female b          2.82
    ##  3 female c          2.87
    ##  4 female d          2.89
    ##  5 female e          2.87
    ##  6 female f          2.81
    ##  7 female g          2.73
    ##  8 male   a          2.53
    ##  9 male   b          2.51
    ## 10 male   c          2.54
    ## 11 male   d          2.55
    ## 12 male   e          2.51
    ## 13 male   f          2.47
    ## 14 male   g          2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanVote6 %>%
  pivot_wider(names_from = wave, values_from = meanVote6)
```

    ## # A tibble: 2 x 8
    ## # Groups:   sex_dv [2]
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

Interpretation: The survey shows the range of political interest between
men and women in waves 1 to 7. The value of 1 indicates being interested
in politics and 4 indicates not being very interested. From the findings
its shows womens higest political interest was 2.73 in wave 7 and the
lowest being in wave 4 at 2.89. Men on the otherhand their highest is in
wave 7 at 2.42 and their lowest being wave 4 at 2.55. There is an
indication of variation between male and female political participation,
however wave 7 could suggest teh Brexit debate and therefore would
result in a higher political interest due to being a controversial
issue. \#\# Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings. Interpretation: There
    is an indication that the higher the delta, the more unstable
    political interest there is. The table shows that those who are
    younger have a higher political instability, than those who are
    younger as it steadily decreases as the age increases. The political
    interest of those who are older is much more stable, and this
    suggests that younger people are more politically active.

<!-- end list -->

``` r
Respondents.na <- all7%>%
  filter(complete.cases(a_vote6,b_vote6,c_vote6,d_vote6,e_vote6,f_vote6, g_vote6))

Eachpers.delta <- Respondents.na %>%
  mutate(Eachpers.delta=abs(b_vote6-a_vote6)+abs(c_vote6-b_vote6)+abs(d_vote6-c_vote6)+abs(e_vote6-d_vote6)+abs(f_vote6-e_vote6)+abs(g_vote6-f_vote6))

Eachpers.delta %>%
  count(Eachpers.delta)
```

    ## # A tibble: 50 x 2
    ##    Eachpers.delta     n
    ##             <int> <int>
    ##  1              0  3376
    ##  2              1  2058
    ##  3              2  3563
    ##  4              3  2770
    ##  5              4  2502
    ##  6              5  1264
    ##  7              6   843
    ##  8              7   373
    ##  9              8   215
    ## 10              9   179
    ## # ... with 40 more rows

``` r
gender.delta <- Eachpers.delta %>%
  group_by(a_sex_dv) %>%
  summarise( Deltagender = mean(Eachpers.delta, na.rm = TRUE))
gender.delta
```

    ## # A tibble: 2 x 2
    ##   a_sex_dv Deltagender
    ##      <int>       <dbl>
    ## 1        1        4.42
    ## 2        2        3.62

``` r
age.delta <- Eachpers.delta %>%
  group_by(a_age_dv) %>%
  summarise(Deltaage = mean(Eachpers.delta, na.rm = TRUE))

age.delta
```

    ## # A tibble: 80 x 2
    ##    a_age_dv Deltaage
    ##       <int>    <dbl>
    ##  1       15    19.4 
    ##  2       16     8.36
    ##  3       17     6.31
    ##  4       18     9.73
    ##  5       19     7.35
    ##  6       20     5.32
    ##  7       21     5.65
    ##  8       22     5.26
    ##  9       23     4.36
    ## 10       24     4.02
    ## # ... with 70 more rows

``` r
age.delta %>%
  ggplot(aes(x=a_age_dv, y=Deltaage, group=1)) +geom_point(color="blue", alpha= 0.3, size= 3)
```

![](assignment3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
