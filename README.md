README

## Purposes
To analyse the household power consumption data set to gain some information from it.

1. Predict the power consumption of days in the next years, which should be able to use regression
  - Find the trend of power consumption

2. Find the pattern how the household consume the power and give some adivce, which will need association rules

## Tasks
1. Load data
  - load in dictionary
  - convert them to proper data type
2. Deal with missing data
  - Find missing data
    a. missing data 2017.04.28-30
  - visual the missing data as a Graph
  - drop it
  - mark the data, data provenance
  
3. Explore the data
  - tasks when exploring
    - summary the data
    - find outliers, and whether it make sense or not
    - find data range
    - with 2 ways
      a. box plot
      b. scatter graph
4. Preprocess
  - seperate the time
  - calculate the rate and total Power
  - a list of group by time
  - deal with data
    a. visualize rate and intensity and voltage
    b. portion between four usage of electric
    c. totoal power against time
    d. voltage against intensity
5. Build the model
    a. regression
      i. power consumption vs time, one variable regression
      ii. efficent rate = active/reactive+active with submetering and the relationship with intensity multiple regression
    b. association rules
      i. the user's pattern
        - translate it into different pattern
        - work with it
6. validate it
7. Report


## Packages

1. mice
2. VIM

## Record

1. Deal with missing
  a. only time and sub_metering_3 has the missing data.
  b. Time missing data is only 120, should be randomly
  c. There are 25979 missing data for Sub_Metering_3, the rate it low
  d. but the box plot show the differences with the origin data, so we need to exanime it.
