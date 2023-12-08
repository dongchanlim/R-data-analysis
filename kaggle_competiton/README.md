Group Project
================
Varun Selvam, Ahsan Ahmad, Richard Lim
2023-11-26

## Introduction & Project goal

### Introduction

In this group project, we are expecting to build the maximum predictive
accuracy by using all available predictors & interactions.there is no
restriction on variables to utilize, or on combinations of
variables—interactions and polynomial terms are encouraged, as
appropriate.

### Project goal

This group project is aimed to develop a linear model predicting housing
prices in Ames, IA using dataset provided in **Kaggle.com**. Model
performance benchmark for minimum estimated out-of-sample $R^2$ is 0.85.

## Description of the data

### Description

There are two datasets for the group project. One is `train.csv` and the
other is `test.csv` which are with 1460 and 1459 observations,
respectively. Data includes the categorical & numeric features of
properties (Predictors) and their corresponding `SalePrice`. (Target)

### Data Profile

- SalePrice - the property’s sale price in dollars. This is the target
  variable that you’re trying to predict.
- MSSubClass: The building class
- MSZoning: The general zoning classification
- LotFrontage: Linear feet of street connected to property
- LotArea: Lot size in square feet
- Street: Type of road access
- Alley: Type of alley access
- LotShape: General shape of property
- LandContour: Flatness of the property
- Utilities: Type of utilities available
- LotConfig: Lot configuration
- LandSlope: Slope of property
- Neighborhood: Physical locations within Ames city limits
- Condition1: Proximity to main road or railroad
- Condition2: Proximity to main road or railroad (if a second is
  present)
- BldgType: Type of dwelling
- HouseStyle: Style of dwelling
- OverallQual: Overall material and finish quality
- OverallCond: Overall condition rating
- YearBuilt: Original construction date
- YearRemodAdd: Remodel date
- RoofStyle: Type of roof
- RoofMatl: Roof material
- Exterior1st: Exterior covering on house
- Exterior2nd: Exterior covering on house (if more than one material)
- MasVnrType: Masonry veneer type
- MasVnrArea: Masonry veneer area in square feet
- ExterQual: Exterior material quality
- ExterCond: Present condition of the material on the exterior
- Foundation: Type of foundation
- BsmtQual: Height of the basement
- BsmtCond: General condition of the basement
- BsmtExposure: Walkout or garden level basement walls
- BsmtFinType1: Quality of basement finished area
- BsmtFinSF1: Type 1 finished square feet
- BsmtFinType2: Quality of second finished area (if present)
- BsmtFinSF2: Type 2 finished square feet
- BsmtUnfSF: Unfinished square feet of basement area
- TotalBsmtSF: Total square feet of basement area
- Heating: Type of heating
- HeatingQC: Heating quality and condition
- CentralAir: Central air conditioning
- Electrical: Electrical system
- 1stFlrSF: First Floor square feet
- 2ndFlrSF: Second floor square feet
- LowQualFinSF: Low quality finished square feet (all floors)
- GrLivArea: Above grade (ground) living area square feet
- BsmtFullBath: Basement full bathrooms
- BsmtHalfBath: Basement half bathrooms
- FullBath: Full bathrooms above grade
- HalfBath: Half baths above grade
- Bedroom: Number of bedrooms above basement level
- Kitchen: Number of kitchens
- KitchenQual: Kitchen quality
- TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
- Functional: Home functionality rating
- Fireplaces: Number of fireplaces
- FireplaceQu: Fireplace quality
- GarageType: Garage location
- GarageYrBlt: Year garage was built
- GarageFinish: Interior finish of the garage
- GarageCars: Size of garage in car capacity
- GarageArea: Size of garage in square feet
- GarageQual: Garage quality
- GarageCond: Garage condition
- PavedDrive: Paved driveway
- WoodDeckSF: Wood deck area in square feet
- OpenPorchSF: Open porch area in square feet
- EnclosedPorch: Enclosed porch area in square feet
- 3SsnPorch: Three season porch area in square feet
- ScreenPorch: Screen porch area in square feet
- PoolArea: Pool area in square feet
- PoolQC: Pool quality
- Fence: Fence quality
- MiscFeature: Miscellaneous feature not covered in other categories
- MiscVal: \$Value of miscellaneous feature
- MoSold: Month Sold
- YrSold: Year Sold
- SaleType: Type of sale
- SaleCondition: Condition of sale

## Load Libraries

``` r
# load libraries for project
library(dplyr) 
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
```

## Load dataset

``` r
# load Data sets
train <- read.csv("train.csv")
test <- read.csv("test.csv")
```

``` r
# 5 sameples of train.csv
head(train)
```

    ##   Id MSSubClass MSZoning LotFrontage LotArea Street Alley LotShape LandContour
    ## 1  1         60       RL          65    8450   Pave  <NA>      Reg         Lvl
    ## 2  2         20       RL          80    9600   Pave  <NA>      Reg         Lvl
    ## 3  3         60       RL          68   11250   Pave  <NA>      IR1         Lvl
    ## 4  4         70       RL          60    9550   Pave  <NA>      IR1         Lvl
    ## 5  5         60       RL          84   14260   Pave  <NA>      IR1         Lvl
    ## 6  6         50       RL          85   14115   Pave  <NA>      IR1         Lvl
    ##   Utilities LotConfig LandSlope Neighborhood Condition1 Condition2 BldgType
    ## 1    AllPub    Inside       Gtl      CollgCr       Norm       Norm     1Fam
    ## 2    AllPub       FR2       Gtl      Veenker      Feedr       Norm     1Fam
    ## 3    AllPub    Inside       Gtl      CollgCr       Norm       Norm     1Fam
    ## 4    AllPub    Corner       Gtl      Crawfor       Norm       Norm     1Fam
    ## 5    AllPub       FR2       Gtl      NoRidge       Norm       Norm     1Fam
    ## 6    AllPub    Inside       Gtl      Mitchel       Norm       Norm     1Fam
    ##   HouseStyle OverallQual OverallCond YearBuilt YearRemodAdd RoofStyle RoofMatl
    ## 1     2Story           7           5      2003         2003     Gable  CompShg
    ## 2     1Story           6           8      1976         1976     Gable  CompShg
    ## 3     2Story           7           5      2001         2002     Gable  CompShg
    ## 4     2Story           7           5      1915         1970     Gable  CompShg
    ## 5     2Story           8           5      2000         2000     Gable  CompShg
    ## 6     1.5Fin           5           5      1993         1995     Gable  CompShg
    ##   Exterior1st Exterior2nd MasVnrType MasVnrArea ExterQual ExterCond Foundation
    ## 1     VinylSd     VinylSd    BrkFace        196        Gd        TA      PConc
    ## 2     MetalSd     MetalSd       None          0        TA        TA     CBlock
    ## 3     VinylSd     VinylSd    BrkFace        162        Gd        TA      PConc
    ## 4     Wd Sdng     Wd Shng       None          0        TA        TA     BrkTil
    ## 5     VinylSd     VinylSd    BrkFace        350        Gd        TA      PConc
    ## 6     VinylSd     VinylSd       None          0        TA        TA       Wood
    ##   BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 1       Gd       TA           No          GLQ        706          Unf
    ## 2       Gd       TA           Gd          ALQ        978          Unf
    ## 3       Gd       TA           Mn          GLQ        486          Unf
    ## 4       TA       Gd           No          ALQ        216          Unf
    ## 5       Gd       TA           Av          GLQ        655          Unf
    ## 6       Gd       TA           No          GLQ        732          Unf
    ##   BsmtFinSF2 BsmtUnfSF TotalBsmtSF Heating HeatingQC CentralAir Electrical
    ## 1          0       150         856    GasA        Ex          Y      SBrkr
    ## 2          0       284        1262    GasA        Ex          Y      SBrkr
    ## 3          0       434         920    GasA        Ex          Y      SBrkr
    ## 4          0       540         756    GasA        Gd          Y      SBrkr
    ## 5          0       490        1145    GasA        Ex          Y      SBrkr
    ## 6          0        64         796    GasA        Ex          Y      SBrkr
    ##   X1stFlrSF X2ndFlrSF LowQualFinSF GrLivArea BsmtFullBath BsmtHalfBath FullBath
    ## 1       856       854            0      1710            1            0        2
    ## 2      1262         0            0      1262            0            1        2
    ## 3       920       866            0      1786            1            0        2
    ## 4       961       756            0      1717            1            0        1
    ## 5      1145      1053            0      2198            1            0        2
    ## 6       796       566            0      1362            1            0        1
    ##   HalfBath BedroomAbvGr KitchenAbvGr KitchenQual TotRmsAbvGrd Functional
    ## 1        1            3            1          Gd            8        Typ
    ## 2        0            3            1          TA            6        Typ
    ## 3        1            3            1          Gd            6        Typ
    ## 4        0            3            1          Gd            7        Typ
    ## 5        1            4            1          Gd            9        Typ
    ## 6        1            1            1          TA            5        Typ
    ##   Fireplaces FireplaceQu GarageType GarageYrBlt GarageFinish GarageCars
    ## 1          0        <NA>     Attchd        2003          RFn          2
    ## 2          1          TA     Attchd        1976          RFn          2
    ## 3          1          TA     Attchd        2001          RFn          2
    ## 4          1          Gd     Detchd        1998          Unf          3
    ## 5          1          TA     Attchd        2000          RFn          3
    ## 6          0        <NA>     Attchd        1993          Unf          2
    ##   GarageArea GarageQual GarageCond PavedDrive WoodDeckSF OpenPorchSF
    ## 1        548         TA         TA          Y          0          61
    ## 2        460         TA         TA          Y        298           0
    ## 3        608         TA         TA          Y          0          42
    ## 4        642         TA         TA          Y          0          35
    ## 5        836         TA         TA          Y        192          84
    ## 6        480         TA         TA          Y         40          30
    ##   EnclosedPorch X3SsnPorch ScreenPorch PoolArea PoolQC Fence MiscFeature
    ## 1             0          0           0        0   <NA>  <NA>        <NA>
    ## 2             0          0           0        0   <NA>  <NA>        <NA>
    ## 3             0          0           0        0   <NA>  <NA>        <NA>
    ## 4           272          0           0        0   <NA>  <NA>        <NA>
    ## 5             0          0           0        0   <NA>  <NA>        <NA>
    ## 6             0        320           0        0   <NA> MnPrv        Shed
    ##   MiscVal MoSold YrSold SaleType SaleCondition SalePrice
    ## 1       0      2   2008       WD        Normal    208500
    ## 2       0      5   2007       WD        Normal    181500
    ## 3       0      9   2008       WD        Normal    223500
    ## 4       0      2   2006       WD       Abnorml    140000
    ## 5       0     12   2008       WD        Normal    250000
    ## 6     700     10   2009       WD        Normal    143000

``` r
dim(train)
```

    ## [1] 1460   81

``` r
# 5 sameples of test.csv
head(test)
```

    ##     Id MSSubClass MSZoning LotFrontage LotArea Street Alley LotShape
    ## 1 1461         20       RH          80   11622   Pave  <NA>      Reg
    ## 2 1462         20       RL          81   14267   Pave  <NA>      IR1
    ## 3 1463         60       RL          74   13830   Pave  <NA>      IR1
    ## 4 1464         60       RL          78    9978   Pave  <NA>      IR1
    ## 5 1465        120       RL          43    5005   Pave  <NA>      IR1
    ## 6 1466         60       RL          75   10000   Pave  <NA>      IR1
    ##   LandContour Utilities LotConfig LandSlope Neighborhood Condition1 Condition2
    ## 1         Lvl    AllPub    Inside       Gtl        NAmes      Feedr       Norm
    ## 2         Lvl    AllPub    Corner       Gtl        NAmes       Norm       Norm
    ## 3         Lvl    AllPub    Inside       Gtl      Gilbert       Norm       Norm
    ## 4         Lvl    AllPub    Inside       Gtl      Gilbert       Norm       Norm
    ## 5         HLS    AllPub    Inside       Gtl      StoneBr       Norm       Norm
    ## 6         Lvl    AllPub    Corner       Gtl      Gilbert       Norm       Norm
    ##   BldgType HouseStyle OverallQual OverallCond YearBuilt YearRemodAdd RoofStyle
    ## 1     1Fam     1Story           5           6      1961         1961     Gable
    ## 2     1Fam     1Story           6           6      1958         1958       Hip
    ## 3     1Fam     2Story           5           5      1997         1998     Gable
    ## 4     1Fam     2Story           6           6      1998         1998     Gable
    ## 5   TwnhsE     1Story           8           5      1992         1992     Gable
    ## 6     1Fam     2Story           6           5      1993         1994     Gable
    ##   RoofMatl Exterior1st Exterior2nd MasVnrType MasVnrArea ExterQual ExterCond
    ## 1  CompShg     VinylSd     VinylSd       None          0        TA        TA
    ## 2  CompShg     Wd Sdng     Wd Sdng    BrkFace        108        TA        TA
    ## 3  CompShg     VinylSd     VinylSd       None          0        TA        TA
    ## 4  CompShg     VinylSd     VinylSd    BrkFace         20        TA        TA
    ## 5  CompShg     HdBoard     HdBoard       None          0        Gd        TA
    ## 6  CompShg     HdBoard     HdBoard       None          0        TA        TA
    ##   Foundation BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1
    ## 1     CBlock       TA       TA           No          Rec        468
    ## 2     CBlock       TA       TA           No          ALQ        923
    ## 3      PConc       Gd       TA           No          GLQ        791
    ## 4      PConc       TA       TA           No          GLQ        602
    ## 5      PConc       Gd       TA           No          ALQ        263
    ## 6      PConc       Gd       TA           No          Unf          0
    ##   BsmtFinType2 BsmtFinSF2 BsmtUnfSF TotalBsmtSF Heating HeatingQC CentralAir
    ## 1          LwQ        144       270         882    GasA        TA          Y
    ## 2          Unf          0       406        1329    GasA        TA          Y
    ## 3          Unf          0       137         928    GasA        Gd          Y
    ## 4          Unf          0       324         926    GasA        Ex          Y
    ## 5          Unf          0      1017        1280    GasA        Ex          Y
    ## 6          Unf          0       763         763    GasA        Gd          Y
    ##   Electrical X1stFlrSF X2ndFlrSF LowQualFinSF GrLivArea BsmtFullBath
    ## 1      SBrkr       896         0            0       896            0
    ## 2      SBrkr      1329         0            0      1329            0
    ## 3      SBrkr       928       701            0      1629            0
    ## 4      SBrkr       926       678            0      1604            0
    ## 5      SBrkr      1280         0            0      1280            0
    ## 6      SBrkr       763       892            0      1655            0
    ##   BsmtHalfBath FullBath HalfBath BedroomAbvGr KitchenAbvGr KitchenQual
    ## 1            0        1        0            2            1          TA
    ## 2            0        1        1            3            1          Gd
    ## 3            0        2        1            3            1          TA
    ## 4            0        2        1            3            1          Gd
    ## 5            0        2        0            2            1          Gd
    ## 6            0        2        1            3            1          TA
    ##   TotRmsAbvGrd Functional Fireplaces FireplaceQu GarageType GarageYrBlt
    ## 1            5        Typ          0        <NA>     Attchd        1961
    ## 2            6        Typ          0        <NA>     Attchd        1958
    ## 3            6        Typ          1          TA     Attchd        1997
    ## 4            7        Typ          1          Gd     Attchd        1998
    ## 5            5        Typ          0        <NA>     Attchd        1992
    ## 6            7        Typ          1          TA     Attchd        1993
    ##   GarageFinish GarageCars GarageArea GarageQual GarageCond PavedDrive
    ## 1          Unf          1        730         TA         TA          Y
    ## 2          Unf          1        312         TA         TA          Y
    ## 3          Fin          2        482         TA         TA          Y
    ## 4          Fin          2        470         TA         TA          Y
    ## 5          RFn          2        506         TA         TA          Y
    ## 6          Fin          2        440         TA         TA          Y
    ##   WoodDeckSF OpenPorchSF EnclosedPorch X3SsnPorch ScreenPorch PoolArea PoolQC
    ## 1        140           0             0          0         120        0   <NA>
    ## 2        393          36             0          0           0        0   <NA>
    ## 3        212          34             0          0           0        0   <NA>
    ## 4        360          36             0          0           0        0   <NA>
    ## 5          0          82             0          0         144        0   <NA>
    ## 6        157          84             0          0           0        0   <NA>
    ##   Fence MiscFeature MiscVal MoSold YrSold SaleType SaleCondition
    ## 1 MnPrv        <NA>       0      6   2010       WD        Normal
    ## 2  <NA>        Gar2   12500      6   2010       WD        Normal
    ## 3 MnPrv        <NA>       0      3   2010       WD        Normal
    ## 4  <NA>        <NA>       0      6   2010       WD        Normal
    ## 5  <NA>        <NA>       0      1   2010       WD        Normal
    ## 6  <NA>        <NA>       0      4   2010       WD        Normal

``` r
dim(test)
```

    ## [1] 1459   80

## Data Summarization

### Numeric data summary

``` r
mean <- train %>%
  select_if(is.numeric) %>%
  summarise_all(~mean(.x, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "columns", values_to = "mean")

min <- train %>%
  select_if(is.numeric) %>%
  summarise_all(~min(.x, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "columns", values_to = "min")

max <- train %>%
  select_if(is.numeric) %>%
  summarise_all(~max(.x, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "columns", values_to = "max")

variance <- train %>%
  select_if(is.numeric) %>%
  summarise_all(~var(.x, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "columns", values_to = "variance")

sd <- train %>%
  select_if(is.numeric) %>%
  summarise_all(~sd(.x, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "columns", values_to = "standard deviation")


mean %>%
  left_join(min, by = "columns") %>%
  left_join(max, by = "columns") %>%
  left_join(variance, by = "columns") %>%
  left_join(sd, by = "columns")
```

    ## # A tibble: 38 × 6
    ##    columns          mean   min    max    variance `standard deviation`
    ##    <chr>           <dbl> <int>  <int>       <dbl>                <dbl>
    ##  1 Id             730.       1   1460   177755                  422.  
    ##  2 MSSubClass      56.9     20    190     1789.                  42.3 
    ##  3 LotFrontage     70.0     21    313      590.                  24.3 
    ##  4 LotArea      10517.    1300 215245 99625650.                9981.  
    ##  5 OverallQual      6.10     1     10        1.91                 1.38
    ##  6 OverallCond      5.58     1      9        1.24                 1.11
    ##  7 YearBuilt     1971.    1872   2010      912.                  30.2 
    ##  8 YearRemodAdd  1985.    1950   2010      426.                  20.6 
    ##  9 MasVnrArea     104.       0   1600    32785.                 181.  
    ## 10 BsmtFinSF1     444.       0   5644   208025.                 456.  
    ## # ℹ 28 more rows

This is a summary table of the mean, median, max, variance, and standard
deviation for all of the numeric variables. Some of the variables may
seem extreme, however it’s plausible that these values are genuine which
is why they were not removed from the model.

### Categorical Data Summary

``` r
lapply(train %>% select_if(is.character), unique)
```

    ## $MSZoning
    ## [1] "RL"      "RM"      "C (all)" "FV"      "RH"     
    ## 
    ## $Street
    ## [1] "Pave" "Grvl"
    ## 
    ## $Alley
    ## [1] NA     "Grvl" "Pave"
    ## 
    ## $LotShape
    ## [1] "Reg" "IR1" "IR2" "IR3"
    ## 
    ## $LandContour
    ## [1] "Lvl" "Bnk" "Low" "HLS"
    ## 
    ## $Utilities
    ## [1] "AllPub" "NoSeWa"
    ## 
    ## $LotConfig
    ## [1] "Inside"  "FR2"     "Corner"  "CulDSac" "FR3"    
    ## 
    ## $LandSlope
    ## [1] "Gtl" "Mod" "Sev"
    ## 
    ## $Neighborhood
    ##  [1] "CollgCr" "Veenker" "Crawfor" "NoRidge" "Mitchel" "Somerst" "NWAmes" 
    ##  [8] "OldTown" "BrkSide" "Sawyer"  "NridgHt" "NAmes"   "SawyerW" "IDOTRR" 
    ## [15] "MeadowV" "Edwards" "Timber"  "Gilbert" "StoneBr" "ClearCr" "NPkVill"
    ## [22] "Blmngtn" "BrDale"  "SWISU"   "Blueste"
    ## 
    ## $Condition1
    ## [1] "Norm"   "Feedr"  "PosN"   "Artery" "RRAe"   "RRNn"   "RRAn"   "PosA"  
    ## [9] "RRNe"  
    ## 
    ## $Condition2
    ## [1] "Norm"   "Artery" "RRNn"   "Feedr"  "PosN"   "PosA"   "RRAn"   "RRAe"  
    ## 
    ## $BldgType
    ## [1] "1Fam"   "2fmCon" "Duplex" "TwnhsE" "Twnhs" 
    ## 
    ## $HouseStyle
    ## [1] "2Story" "1Story" "1.5Fin" "1.5Unf" "SFoyer" "SLvl"   "2.5Unf" "2.5Fin"
    ## 
    ## $RoofStyle
    ## [1] "Gable"   "Hip"     "Gambrel" "Mansard" "Flat"    "Shed"   
    ## 
    ## $RoofMatl
    ## [1] "CompShg" "WdShngl" "Metal"   "WdShake" "Membran" "Tar&Grv" "Roll"   
    ## [8] "ClyTile"
    ## 
    ## $Exterior1st
    ##  [1] "VinylSd" "MetalSd" "Wd Sdng" "HdBoard" "BrkFace" "WdShing" "CemntBd"
    ##  [8] "Plywood" "AsbShng" "Stucco"  "BrkComm" "AsphShn" "Stone"   "ImStucc"
    ## [15] "CBlock" 
    ## 
    ## $Exterior2nd
    ##  [1] "VinylSd" "MetalSd" "Wd Shng" "HdBoard" "Plywood" "Wd Sdng" "CmentBd"
    ##  [8] "BrkFace" "Stucco"  "AsbShng" "Brk Cmn" "ImStucc" "AsphShn" "Stone"  
    ## [15] "Other"   "CBlock" 
    ## 
    ## $MasVnrType
    ## [1] "BrkFace" "None"    "Stone"   "BrkCmn"  NA       
    ## 
    ## $ExterQual
    ## [1] "Gd" "TA" "Ex" "Fa"
    ## 
    ## $ExterCond
    ## [1] "TA" "Gd" "Fa" "Po" "Ex"
    ## 
    ## $Foundation
    ## [1] "PConc"  "CBlock" "BrkTil" "Wood"   "Slab"   "Stone" 
    ## 
    ## $BsmtQual
    ## [1] "Gd" "TA" "Ex" NA   "Fa"
    ## 
    ## $BsmtCond
    ## [1] "TA" "Gd" NA   "Fa" "Po"
    ## 
    ## $BsmtExposure
    ## [1] "No" "Gd" "Mn" "Av" NA  
    ## 
    ## $BsmtFinType1
    ## [1] "GLQ" "ALQ" "Unf" "Rec" "BLQ" NA    "LwQ"
    ## 
    ## $BsmtFinType2
    ## [1] "Unf" "BLQ" NA    "ALQ" "Rec" "LwQ" "GLQ"
    ## 
    ## $Heating
    ## [1] "GasA"  "GasW"  "Grav"  "Wall"  "OthW"  "Floor"
    ## 
    ## $HeatingQC
    ## [1] "Ex" "Gd" "TA" "Fa" "Po"
    ## 
    ## $CentralAir
    ## [1] "Y" "N"
    ## 
    ## $Electrical
    ## [1] "SBrkr" "FuseF" "FuseA" "FuseP" "Mix"   NA     
    ## 
    ## $KitchenQual
    ## [1] "Gd" "TA" "Ex" "Fa"
    ## 
    ## $Functional
    ## [1] "Typ"  "Min1" "Maj1" "Min2" "Mod"  "Maj2" "Sev" 
    ## 
    ## $FireplaceQu
    ## [1] NA   "TA" "Gd" "Fa" "Ex" "Po"
    ## 
    ## $GarageType
    ## [1] "Attchd"  "Detchd"  "BuiltIn" "CarPort" NA        "Basment" "2Types" 
    ## 
    ## $GarageFinish
    ## [1] "RFn" "Unf" "Fin" NA   
    ## 
    ## $GarageQual
    ## [1] "TA" "Fa" "Gd" NA   "Ex" "Po"
    ## 
    ## $GarageCond
    ## [1] "TA" "Fa" NA   "Gd" "Po" "Ex"
    ## 
    ## $PavedDrive
    ## [1] "Y" "N" "P"
    ## 
    ## $PoolQC
    ## [1] NA   "Ex" "Fa" "Gd"
    ## 
    ## $Fence
    ## [1] NA      "MnPrv" "GdWo"  "GdPrv" "MnWw" 
    ## 
    ## $MiscFeature
    ## [1] NA     "Shed" "Gar2" "Othr" "TenC"
    ## 
    ## $SaleType
    ## [1] "WD"    "New"   "COD"   "ConLD" "ConLI" "CWD"   "ConLw" "Con"   "Oth"  
    ## 
    ## $SaleCondition
    ## [1] "Normal"  "Abnorml" "Partial" "AdjLand" "Alloca"  "Family"

## Data Missing Handling

``` r
count_missings <- function(x) sum(is.na(x)) # Sum provides a total of how many values are missing for each variable. 

# Summarizes all the missing values for each variable.
train %>% # Data set name
  summarize_all(count_missings) %>%
  t()
```

    ##               [,1]
    ## Id               0
    ## MSSubClass       0
    ## MSZoning         0
    ## LotFrontage    259
    ## LotArea          0
    ## Street           0
    ## Alley         1369
    ## LotShape         0
    ## LandContour      0
    ## Utilities        0
    ## LotConfig        0
    ## LandSlope        0
    ## Neighborhood     0
    ## Condition1       0
    ## Condition2       0
    ## BldgType         0
    ## HouseStyle       0
    ## OverallQual      0
    ## OverallCond      0
    ## YearBuilt        0
    ## YearRemodAdd     0
    ## RoofStyle        0
    ## RoofMatl         0
    ## Exterior1st      0
    ## Exterior2nd      0
    ## MasVnrType       8
    ## MasVnrArea       8
    ## ExterQual        0
    ## ExterCond        0
    ## Foundation       0
    ## BsmtQual        37
    ## BsmtCond        37
    ## BsmtExposure    38
    ## BsmtFinType1    37
    ## BsmtFinSF1       0
    ## BsmtFinType2    38
    ## BsmtFinSF2       0
    ## BsmtUnfSF        0
    ## TotalBsmtSF      0
    ## Heating          0
    ## HeatingQC        0
    ## CentralAir       0
    ## Electrical       1
    ## X1stFlrSF        0
    ## X2ndFlrSF        0
    ## LowQualFinSF     0
    ## GrLivArea        0
    ## BsmtFullBath     0
    ## BsmtHalfBath     0
    ## FullBath         0
    ## HalfBath         0
    ## BedroomAbvGr     0
    ## KitchenAbvGr     0
    ## KitchenQual      0
    ## TotRmsAbvGrd     0
    ## Functional       0
    ## Fireplaces       0
    ## FireplaceQu    690
    ## GarageType      81
    ## GarageYrBlt     81
    ## GarageFinish    81
    ## GarageCars       0
    ## GarageArea       0
    ## GarageQual      81
    ## GarageCond      81
    ## PavedDrive       0
    ## WoodDeckSF       0
    ## OpenPorchSF      0
    ## EnclosedPorch    0
    ## X3SsnPorch       0
    ## ScreenPorch      0
    ## PoolArea         0
    ## PoolQC        1453
    ## Fence         1179
    ## MiscFeature   1406
    ## MiscVal          0
    ## MoSold           0
    ## YrSold           0
    ## SaleType         0
    ## SaleCondition    0
    ## SalePrice        0

## Variable Selection

``` r
# Methodology 1: Classification Tree (rpart function)
train_tree <- train %>% #Assign changes to train data set to a new variable
  select(-Id) #Remove ID as a predictor, it's not needed.

house_prices_tree <- rpart(formula = SalePrice ~ ., data = train_tree) 
# Assign tree to a variable titled "house_prices_tree".
house_prices_tree #Display Tree
```

    ## n= 1460 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ##  1) root 1460 9207911000000 180921.2  
    ##    2) OverallQual< 7.5 1231 2987549000000 157832.4  
    ##      4) Neighborhood=Blueste,BrDale,BrkSide,Edwards,IDOTRR,MeadowV,Mitchel,NAmes,NPkVill,OldTown,Sawyer,SWISU 713  874553500000 132242.5  
    ##        8) X1stFlrSF< 1050.5 410  325435600000 118198.6 *
    ##        9) X1stFlrSF>=1050.5 303  358833900000 151245.7 *
    ##      5) Neighborhood=Blmngtn,ClearCr,CollgCr,Crawfor,Gilbert,NoRidge,NridgHt,NWAmes,SawyerW,Somerst,StoneBr,Timber,Veenker 518 1003420000000 193055.7  
    ##       10) GrLivArea< 1719 350  377069800000 176242.8  
    ##         20) GrLivArea< 1120 63   23302790000 135391.3 *
    ##         21) GrLivArea>=1120 287  225550400000 185210.3 *
    ##       11) GrLivArea>=1719 168  321301500000 228082.4  
    ##         22) BsmtFinSF1< 860.5 138  168333200000 217077.2 *
    ##         23) BsmtFinSF1>=860.5 30   59371410000 278706.2 *
    ##    3) OverallQual>=7.5 229 2036506000000 305035.9  
    ##      6) OverallQual< 8.5 168  681872600000 274735.5  
    ##       12) GrLivArea< 1971.5 103  240207200000 249392.5 *
    ##       13) GrLivArea>=1971.5 65  270683000000 314894.6 *
    ##      7) OverallQual>=8.5 61  775590500000 388486.1  
    ##       14) GrLivArea< 2229 35   77633010000 341248.2 *
    ##       15) GrLivArea>=2229 26  514723800000 452075.5 *

Based on this classification tree, the top predictors are:

- Overall Quality
- Neighborhood
- First Floor Sqft
- Ground Living Area
- Basement Finished Sqft 1

``` r
# Methodology 2: Removing all predictors with even one NA value and saving it to a dataset.

without_na_train <-
  train %>% 
  select(names(which(colSums(is.na(.)) == 0)))
 

# Creating a Multiple Linear Regression Model with all the remaining predictors
model_a <- lm(SalePrice ~ ., data = without_na_train)
model_a_summary <- summary(model_a)

 

# Creating a Dataframe with Predictor/Variable name and it's corresponding P-Value and arranging it in ascending order to get the top 5 predictors
p_values <- model_a_summary$coefficients[, "Pr(>|t|)"]
coefficients_p_value <- data.frame(
  Variable_name = rownames(model_a_summary$coefficients),
  P_Value = p_values
)


coefficients_p_value %>% 
  arrange(P_Value) %>% 
  head(n = 20)
```

    ##                           Variable_name      P_Value
    ## RoofMatlWdShngl         RoofMatlWdShngl 3.507660e-86
    ## RoofMatlCompShg         RoofMatlCompShg 8.958658e-76
    ## RoofMatlTar&Grv         RoofMatlTar&Grv 2.284001e-60
    ## RoofMatlWdShake         RoofMatlWdShake 1.231063e-59
    ## RoofMatlRoll               RoofMatlRoll 1.027869e-50
    ## RoofMatlMembran         RoofMatlMembran 1.668892e-49
    ## RoofMatlMetal             RoofMatlMetal 8.195861e-46
    ## X2ndFlrSF                     X2ndFlrSF 2.837790e-38
    ## X1stFlrSF                     X1stFlrSF 3.125304e-24
    ## BsmtFinSF1                   BsmtFinSF1 1.032993e-16
    ## Condition2PosN           Condition2PosN 1.729676e-16
    ## KitchenQualGd             KitchenQualGd 3.454613e-15
    ## OverallQual                 OverallQual 7.285789e-15
    ## LotArea                         LotArea 1.004991e-10
    ## ExterQualGd                 ExterQualGd 1.497614e-10
    ## KitchenQualTA             KitchenQualTA 3.241245e-10
    ## OverallCond                 OverallCond 8.254391e-10
    ## ExterQualTA                 ExterQualTA 1.117271e-08
    ## NeighborhoodStoneBr NeighborhoodStoneBr 3.610777e-06
    ## YearBuilt                     YearBuilt 9.336735e-06

We have determined the predictors based on the p-value of linear model.

- RoofMatlWdShngl  
- RoofMatlCompShg  
- RoofMatlTar&Grv  
- RoofMatlWdShake  
- RoofMatlRoll  
- RoofMatlMembran  
- RoofMatlMeta  
- X2ndFlrSF
- X1stFlrSF
- BsmtFinSF1

``` r
#Summarize any missing variables in the train dataset
train %>% #Dat Set name
  select(OverallQual, Neighborhood, X1stFlrSF, GrLivArea, BsmtFinSF1, RoofMatl, SaleType, Utilities, Condition1, YearBuilt, X2ndFlrSF, SaleType, KitchenQual, LotArea, ExterQual, OverallCond, BsmtFinSF2 ) %>% # Predictors that this model will utilize.
  summarize_all(count_missings)
```

    ##   OverallQual Neighborhood X1stFlrSF GrLivArea BsmtFinSF1 RoofMatl SaleType
    ## 1           0            0         0         0          0        0        0
    ##   Utilities Condition1 YearBuilt X2ndFlrSF KitchenQual LotArea ExterQual
    ## 1         0          0         0         0           0       0         0
    ##   OverallCond BsmtFinSF2
    ## 1           0          0

Based on the two methodologies (Rpart tree & non-na linear model)
combined, we are selecting our predictors as

- OverallQual
- Neighborhood
- X1stFlrSF
- GrLivArea
- BsmtFinSF1
- RoofMatl
- SaleType
- Utilities
- Condition1
- YearBuilt
- X2ndFlrSF
- SaleType
- KitchenQual
- LotArea
- ExterQual
- OverallCond
- BsmtFinSF2

## Data Cleansing

``` r
ggplot(data = train, mapping = aes(x = SalePrice)) +
  geom_histogram(fill = 'orange') +
  labs(title = "Histogram of Sale Price") +
  theme_classic()
```

![](Kaggle-Group-Project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The Histogram for Sale Price is right skewed, however much of the data
still appears to near the center.

## Change variables as factor

``` r
# overwrite original dataset with the new changes
train <- train %>% 
  mutate(OverallQual = factor(OverallQual)) %>% # Change overall quality to a factor.
  mutate(Neighborhood = factor(Neighborhood)) %>% 
  mutate(RoofMatl = factor(RoofMatl)) %>% 
  mutate(SaleType = factor(SaleType)) %>% 
  mutate(Utilities = factor(Utilities)) %>% 
  mutate(Condition1 = factor(Condition1)) %>% 
  mutate(KitchenQual = factor(KitchenQual)) %>% 
  mutate(ExterQual = factor(ExterQual)) %>% 
  mutate(OverallCond = factor(OverallCond))
```

## Data split for validation

``` r
#Set seed for reproducibility
set.seed(100) 
index <- sample(x = 1:nrow(train), size = nrow(train)*.7, replace = F) # Randomly sample 70% of the rows

head(index) # Display row numbers.
```

    ## [1]  503 1382  985 1004  919  470

``` r
# Subset train using the index and assign it to train_fold.
train_fold <- train[index, ]

# Subset all remaining rows into the validation fold.
validation_fold <- train[-index, ]
```

## Interaction model

### Interaction 1

``` r
train %>%
  ggplot(aes(x = X1stFlrSF, y = SalePrice, color = Neighborhood)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE) +
  labs(title = "Interaction between sqft of first floor and salesprice by neighborhood",
       subtitle= "There is an interaction between two variables with diffrent slopes per each subgroup") +
  theme_classic()
```

![](Kaggle-Group-Project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Interaction 2

``` r
train %>%
  ggplot(aes(x = X2ndFlrSF, y = SalePrice, color = Neighborhood)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, show.legend = FALSE) +
  labs(title = "Interaction between SalesPrice and sqft of second floor by neighborhood",
       subtitle= "There is an interaction between two variables with diffrent slopes per each subgroup") +
  theme_classic()
```

![](Kaggle-Group-Project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

## Data Modeling

``` r
SalePrice_Model <- lm(SalePrice ~ OverallQual + YearBuilt + Neighborhood * X1stFlrSF + GrLivArea + BsmtFinSF1 + RoofMatl + Neighborhood * X2ndFlrSF + SaleType + Utilities + Condition1 + KitchenQual + LotArea + ExterQual + OverallCond + BsmtFinSF2, data = train_fold) #Data must be train-fold becasue we are training the model, so train_fold data should be used.

summary(SalePrice_Model) #Provide summary of the Sale Price Model.
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ OverallQual + YearBuilt + Neighborhood * 
    ##     X1stFlrSF + GrLivArea + BsmtFinSF1 + RoofMatl + Neighborhood * 
    ##     X2ndFlrSF + SaleType + Utilities + Condition1 + KitchenQual + 
    ##     LotArea + ExterQual + OverallCond + BsmtFinSF2, data = train_fold)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -140541   -9510       0   10070  138439 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   -1.424e+06  1.648e+05  -8.638  < 2e-16 ***
    ## OverallQual2                  -2.906e+04  4.232e+04  -0.687 0.492501    
    ## OverallQual3                  -2.623e+04  2.827e+04  -0.928 0.353669    
    ## OverallQual4                  -2.313e+04  2.778e+04  -0.833 0.405266    
    ## OverallQual5                  -2.252e+04  2.796e+04  -0.806 0.420706    
    ## OverallQual6                  -1.247e+04  2.800e+04  -0.445 0.656117    
    ## OverallQual7                   1.988e+03  2.805e+04   0.071 0.943508    
    ## OverallQual8                   2.842e+04  2.825e+04   1.006 0.314623    
    ## OverallQual9                   5.982e+04  2.876e+04   2.080 0.037834 *  
    ## OverallQual10                  1.018e+05  2.993e+04   3.400 0.000703 ***
    ## YearBuilt                      4.050e+02  6.831e+01   5.929 4.36e-09 ***
    ## NeighborhoodBlueste           -1.276e+04  6.187e+04  -0.206 0.836694    
    ## NeighborhoodBrDale             9.865e+03  1.123e+05   0.088 0.930047    
    ## NeighborhoodBrkSide            6.645e+04  9.181e+04   0.724 0.469376    
    ## NeighborhoodClearCr            1.300e+05  9.561e+04   1.359 0.174354    
    ## NeighborhoodCollgCr            6.334e+04  8.999e+04   0.704 0.481661    
    ## NeighborhoodCrawfor            9.862e+04  9.046e+04   1.090 0.275934    
    ## NeighborhoodEdwards            8.435e+04  9.031e+04   0.934 0.350547    
    ## NeighborhoodGilbert            5.738e+04  9.153e+04   0.627 0.530898    
    ## NeighborhoodIDOTRR             5.027e+04  9.268e+04   0.542 0.587703    
    ## NeighborhoodMeadowV            8.115e+04  9.170e+04   0.885 0.376462    
    ## NeighborhoodMitchel            8.926e+04  9.072e+04   0.984 0.325436    
    ## NeighborhoodNAmes              1.008e+05  8.980e+04   1.122 0.261975    
    ## NeighborhoodNoRidge           -4.640e+04  9.203e+04  -0.504 0.614202    
    ## NeighborhoodNPkVill            1.758e+05  6.066e+05   0.290 0.771978    
    ## NeighborhoodNridgHt           -5.037e+04  9.142e+04  -0.551 0.581796    
    ## NeighborhoodNWAmes             1.111e+05  9.094e+04   1.222 0.221970    
    ## NeighborhoodOldTown            9.001e+04  9.018e+04   0.998 0.318503    
    ## NeighborhoodSawyer             1.011e+05  9.029e+04   1.120 0.263003    
    ## NeighborhoodSawyerW            7.190e+04  9.084e+04   0.791 0.428868    
    ## NeighborhoodSomerst            1.264e+04  9.081e+04   0.139 0.889330    
    ## NeighborhoodStoneBr           -1.042e+05  9.424e+04  -1.106 0.269046    
    ## NeighborhoodSWISU              1.134e+05  9.489e+04   1.195 0.232315    
    ## NeighborhoodTimber             8.748e+04  9.191e+04   0.952 0.341482    
    ## NeighborhoodVeenker            6.365e+04  1.121e+05   0.568 0.570390    
    ## X1stFlrSF                      8.558e+01  6.430e+01   1.331 0.183580    
    ## GrLivArea                      3.647e+01  2.049e+01   1.780 0.075449 .  
    ## BsmtFinSF1                     2.901e+01  2.046e+00  14.180  < 2e-16 ***
    ## RoofMatlCompShg                5.639e+05  4.942e+04  11.411  < 2e-16 ***
    ## RoofMatlMembran                5.866e+05  5.509e+04  10.647  < 2e-16 ***
    ## RoofMatlMetal                  5.728e+05  5.563e+04  10.297  < 2e-16 ***
    ## RoofMatlRoll                   5.672e+05  5.431e+04  10.445  < 2e-16 ***
    ## RoofMatlTar&Grv                5.587e+05  5.042e+04  11.082  < 2e-16 ***
    ## RoofMatlWdShake                5.587e+05  5.108e+04  10.938  < 2e-16 ***
    ## RoofMatlWdShngl                6.056e+05  5.075e+04  11.933  < 2e-16 ***
    ## X2ndFlrSF                      7.011e+01  4.666e+01   1.503 0.133291    
    ## SaleTypeCon                    4.007e+04  2.336e+04   1.715 0.086668 .  
    ## SaleTypeConLD                  7.536e+03  1.024e+04   0.736 0.462043    
    ## SaleTypeConLI                 -4.737e+03  1.166e+04  -0.406 0.684566    
    ## SaleTypeConLw                  6.309e+03  1.274e+04   0.495 0.620461    
    ## SaleTypeCWD                   -1.043e+04  1.464e+04  -0.713 0.476214    
    ## SaleTypeNew                    3.095e+04  5.878e+03   5.265 1.75e-07 ***
    ## SaleTypeOth                   -7.829e+03  1.704e+04  -0.459 0.646050    
    ## SaleTypeWD                     6.912e+03  5.069e+03   1.364 0.173007    
    ## UtilitiesNoSeWa               -1.443e+04  2.402e+04  -0.600 0.548358    
    ## Condition1Feedr               -3.205e+03  5.708e+03  -0.561 0.574604    
    ## Condition1Norm                 6.477e+03  4.787e+03   1.353 0.176350    
    ## Condition1PosA                 1.101e+04  1.117e+04   0.986 0.324558    
    ## Condition1PosN                 1.780e+04  8.145e+03   2.185 0.029125 *  
    ## Condition1RRAe                -6.884e+03  9.574e+03  -0.719 0.472360    
    ## Condition1RRAn                 2.594e+03  6.982e+03   0.371 0.710369    
    ## Condition1RRNe                -6.593e+03  1.744e+04  -0.378 0.705552    
    ## Condition1RRNn                -1.442e+03  1.765e+04  -0.082 0.934937    
    ## KitchenQualFa                 -1.791e+04  6.791e+03  -2.638 0.008489 ** 
    ## KitchenQualGd                 -8.916e+03  4.272e+03  -2.087 0.037150 *  
    ## KitchenQualTA                 -1.178e+04  4.631e+03  -2.544 0.011130 *  
    ## LotArea                        6.065e-01  7.865e-02   7.712 3.30e-14 ***
    ## ExterQualFa                   -6.208e+03  1.103e+04  -0.563 0.573716    
    ## ExterQualGd                   -1.221e+03  5.974e+03  -0.204 0.838059    
    ## ExterQualTA                   -3.816e+03  6.343e+03  -0.602 0.547631    
    ## OverallCond2                   2.024e+04  3.691e+04   0.548 0.583628    
    ## OverallCond3                   2.523e+04  3.438e+04   0.734 0.463243    
    ## OverallCond4                   3.899e+04  3.516e+04   1.109 0.267825    
    ## OverallCond5                   4.318e+04  3.513e+04   1.229 0.219299    
    ## OverallCond6                   5.188e+04  3.512e+04   1.477 0.139950    
    ## OverallCond7                   6.038e+04  3.517e+04   1.716 0.086417 .  
    ## OverallCond8                   6.181e+04  3.526e+04   1.753 0.079965 .  
    ## OverallCond9                   8.139e+04  3.573e+04   2.278 0.022965 *  
    ## BsmtFinSF2                     1.773e+01  4.973e+00   3.565 0.000384 ***
    ## NeighborhoodBlueste:X1stFlrSF         NA         NA      NA       NA    
    ## NeighborhoodBrDale:X1stFlrSF  -7.481e+01  1.008e+02  -0.742 0.458202    
    ## NeighborhoodBrkSide:X1stFlrSF -4.146e+01  6.531e+01  -0.635 0.525719    
    ## NeighborhoodClearCr:X1stFlrSF -8.686e+01  6.534e+01  -1.329 0.184080    
    ## NeighborhoodCollgCr:X1stFlrSF -4.001e+01  6.155e+01  -0.650 0.515809    
    ## NeighborhoodCrawfor:X1stFlrSF -5.684e+01  6.163e+01  -0.922 0.356676    
    ## NeighborhoodEdwards:X1stFlrSF -6.400e+01  6.193e+01  -1.034 0.301647    
    ## NeighborhoodGilbert:X1stFlrSF -3.248e+01  6.298e+01  -0.516 0.606212    
    ## NeighborhoodIDOTRR:X1stFlrSF  -3.068e+01  6.620e+01  -0.463 0.643148    
    ## NeighborhoodMeadowV:X1stFlrSF -7.961e+01  6.626e+01  -1.202 0.229871    
    ## NeighborhoodMitchel:X1stFlrSF -6.993e+01  6.220e+01  -1.124 0.261190    
    ## NeighborhoodNAmes:X1stFlrSF   -7.643e+01  6.140e+01  -1.245 0.213517    
    ## NeighborhoodNoRidge:X1stFlrSF  8.427e+00  6.184e+01   0.136 0.891630    
    ## NeighborhoodNPkVill:X1stFlrSF -1.485e+02  5.663e+02  -0.262 0.793288    
    ## NeighborhoodNridgHt:X1stFlrSF  3.682e+01  6.225e+01   0.591 0.554377    
    ## NeighborhoodNWAmes:X1stFlrSF  -8.026e+01  6.199e+01  -1.295 0.195779    
    ## NeighborhoodOldTown:X1stFlrSF -7.027e+01  6.235e+01  -1.127 0.260007    
    ## NeighborhoodSawyer:X1stFlrSF  -7.885e+01  6.197e+01  -1.272 0.203620    
    ## NeighborhoodSawyerW:X1stFlrSF -5.392e+01  6.227e+01  -0.866 0.386722    
    ## NeighborhoodSomerst:X1stFlrSF -1.017e+00  6.205e+01  -0.016 0.986925    
    ## NeighborhoodStoneBr:X1stFlrSF  7.659e+01  6.402e+01   1.196 0.231866    
    ## NeighborhoodSWISU:X1stFlrSF   -1.006e+02  6.607e+01  -1.522 0.128371    
    ## NeighborhoodTimber:X1stFlrSF  -6.266e+01  6.247e+01  -1.003 0.316087    
    ## NeighborhoodVeenker:X1stFlrSF -4.087e+01  7.890e+01  -0.518 0.604601    
    ## NeighborhoodBlueste:X2ndFlrSF         NA         NA      NA       NA    
    ## NeighborhoodBrDale:X2ndFlrSF   5.469e+01  1.474e+02   0.371 0.710769    
    ## NeighborhoodBrkSide:X2ndFlrSF -3.462e+01  4.383e+01  -0.790 0.429858    
    ## NeighborhoodClearCr:X2ndFlrSF -6.702e+01  4.283e+01  -1.565 0.117969    
    ## NeighborhoodCollgCr:X2ndFlrSF -4.732e+01  4.147e+01  -1.141 0.254184    
    ## NeighborhoodCrawfor:X2ndFlrSF -4.941e+01  4.196e+01  -1.178 0.239270    
    ## NeighborhoodEdwards:X2ndFlrSF -6.598e+01  4.219e+01  -1.564 0.118205    
    ## NeighborhoodGilbert:X2ndFlrSF -4.109e+01  4.240e+01  -0.969 0.332777    
    ## NeighborhoodIDOTRR:X2ndFlrSF  -4.933e+01  4.450e+01  -1.109 0.267886    
    ## NeighborhoodMeadowV:X2ndFlrSF -7.601e+01  4.454e+01  -1.707 0.088221 .  
    ## NeighborhoodMitchel:X2ndFlrSF -6.200e+01  4.280e+01  -1.448 0.147836    
    ## NeighborhoodNAmes:X2ndFlrSF   -6.694e+01  4.148e+01  -1.614 0.106939    
    ## NeighborhoodNoRidge:X2ndFlrSF  1.587e+01  4.238e+01   0.375 0.708079    
    ## NeighborhoodNPkVill:X2ndFlrSF -9.050e+01  2.197e+02  -0.412 0.680499    
    ## NeighborhoodNridgHt:X2ndFlrSF  9.836e-01  4.198e+01   0.023 0.981311    
    ## NeighborhoodNWAmes:X2ndFlrSF  -6.365e+01  4.204e+01  -1.514 0.130413    
    ## NeighborhoodOldTown:X2ndFlrSF -6.266e+01  4.190e+01  -1.495 0.135156    
    ## NeighborhoodSawyer:X2ndFlrSF  -6.459e+01  4.234e+01  -1.526 0.127467    
    ## NeighborhoodSawyerW:X2ndFlrSF -3.926e+01  4.163e+01  -0.943 0.345856    
    ## NeighborhoodSomerst:X2ndFlrSF -1.558e+01  4.266e+01  -0.365 0.715062    
    ## NeighborhoodStoneBr:X2ndFlrSF  7.137e-01  4.215e+01   0.017 0.986494    
    ## NeighborhoodSWISU:X2ndFlrSF   -3.611e+01  4.763e+01  -0.758 0.448653    
    ## NeighborhoodTimber:X2ndFlrSF  -4.320e+01  4.261e+01  -1.014 0.310967    
    ## NeighborhoodVeenker:X2ndFlrSF         NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 22240 on 897 degrees of freedom
    ## Multiple R-squared:  0.9297, Adjusted R-squared:   0.92 
    ## F-statistic: 96.42 on 123 and 897 DF,  p-value: < 2.2e-16

``` r
rmse <- function(observed, predicted) sqrt(mean((observed - predicted)^2)) #Create RMSE function for test data

R2 <- function(observed, predicted){ #Create R-Squared function for test data
  TSS <- sum((observed - mean(observed))^2)
  RSS <- sum((observed - predicted)^2)
  1- RSS/TSS
}

# Calculate Estimated RMSE
rmse(observed = train_fold$SalePrice, #observed is the SalePrice values that are already in the dataset. 
     predicted = fitted(SalePrice_Model)) #fitted represents the values that the model predicted.
```

    ## [1] 20845.86

``` r
R2(train_fold$SalePrice, fitted(SalePrice_Model))
```

    ## [1] 0.9296826

The “In Sample” R-squared is .92 which means that this model explains
92% of the variation for the In Sample Data set. In the context of this
model, 92% of the model’s predictions for Sale Price fit very closely
with the actual values for Sale Price.

The estimated RMSE indicates that this model’s predictions are off on
average by \$20845.86 when estimating the Sale Price of a house.

## Data model performance for validation

``` r
predictions <- predict(SalePrice_Model, newdata = validation_fold)

# Create functions for calculating RMSE and R-squared (necessary for estimating Out of sample performance)

rmse(validation_fold$SalePrice, predictions)
```

    ## [1] 29074.05

``` r
R2(validation_fold$SalePrice, predictions)
```

    ## [1] 0.8719141

This model has an out of sample R-Squared of 87% which means that this
model explains 87% of the variation in the data. In the context of this
model, the 87% of the model’s predictions for Sale Price fit very
closely with the actual values for Sale Price. The “In Sample” model had
an R-Squared of 92% while the validation model had an R-Square of 87%.

Most models will face a similar decline in R-Squared when they are ran
with the validation fold data. However if a model experiences a massive
decline in R-Squared, this is indicative of over-fitting. In this case
the model, since there was a modest decline, this indicates that the
Sale Price Model is not overfitting.

The RMSE indicates that this model’s predictions are off on average by
\$29,074.05 dollars when estimating the Sale Price of a house.

## Data Engineering for test dataset

``` r
test <- test %>%  #Override test dataset with changes to make test dataset match the train dataset.
  mutate(OverallQual = factor(OverallQual)) %>% # Change overall quality to a factor.
  mutate(Neighborhood = factor(Neighborhood)) %>% 
  mutate(RoofMatl = factor(RoofMatl)) %>% 
  mutate(SaleType = factor(SaleType)) %>% 
  mutate(Utilities = factor(Utilities)) %>%
  mutate(Condition1 = factor(Condition1)) %>% 
  mutate(KitchenQual = factor(KitchenQual)) %>% 
  mutate(ExterQual = factor(ExterQual)) %>%
  mutate(OverallCond = factor(OverallCond))

# 3. Checking for NA Values for our five selected predictors in the test set.
count_missings <- function(x) sum(is.na(x))

test %>% 
  select(RoofMatl, X1stFlrSF, X2ndFlrSF, BsmtFinSF1 , OverallQual, Utilities, SaleType, GrLivArea, Neighborhood, Condition1, KitchenQual, LotArea, ExterQual, OverallCond, BsmtFinSF2) %>% 
  summarize_all(count_missings) 
```

    ##   RoofMatl X1stFlrSF X2ndFlrSF BsmtFinSF1 OverallQual Utilities SaleType
    ## 1        0         0         0          1           0         2        1
    ##   GrLivArea Neighborhood Condition1 KitchenQual LotArea ExterQual OverallCond
    ## 1         0            0          0           1       0         0           0
    ##   BsmtFinSF2
    ## 1          1

``` r
# Replacing the missing value for BsmtFinSF1 with the median

test$BsmtFinSF1 <- as.numeric((test$BsmtFinSF1))
test$BsmtFinSF2 <- as.numeric((test$BsmtFinSF2))

test <- test %>% 
  mutate(BsmtFinSF1 = replace_na(data = BsmtFinSF1, replace = median(BsmtFinSF1, na.rm = TRUE))) %>% 
  mutate(Utilities = replace_na(data = Utilities, replace = "AllPub")) %>% 
  mutate(SaleType = replace_na(data = SaleType, replace = "New")) %>%
  mutate(KitchenQual = replace_na(data = KitchenQual, replace = "Gd")) %>% 
  mutate(BsmtFinSF2 = replace_na(data = BsmtFinSF2, replace = median(BsmtFinSF1, na.rm = TRUE)))
```

``` r
test %>% 
  select(RoofMatl, X1stFlrSF, X2ndFlrSF, BsmtFinSF1 , OverallQual, Utilities, SaleType, GrLivArea, Neighborhood, Condition1, KitchenQual, LotArea, ExterQual, OverallCond, BsmtFinSF2) %>% 
  summarize_all(count_missings) 
```

    ##   RoofMatl X1stFlrSF X2ndFlrSF BsmtFinSF1 OverallQual Utilities SaleType
    ## 1        0         0         0          0           0         0        0
    ##   GrLivArea Neighborhood Condition1 KitchenQual LotArea ExterQual OverallCond
    ## 1         0            0          0           0       0         0           0
    ##   BsmtFinSF2
    ## 1          0

There are some missing values in the test dataset. We will address this
by imputing the missing values in the variable with the median and mode.

After replacing the missing values with the median, we will check one
more time. Based of the table, there are no missing values, which
indicates that the data cleaning was successful.

## Kaggle Submission

``` r
# Assign a new linear regression model with the same predictors to a new variable. Use the full data set this time.
kaggle_model <- lm(SalePrice ~ OverallQual + YearBuilt + Neighborhood * X1stFlrSF + GrLivArea + BsmtFinSF1 + RoofMatl + Neighborhood * X2ndFlrSF + SaleType + Utilities + Condition1 + KitchenQual + LotArea + ExterQual + OverallCond + BsmtFinSF2, data = train) 

# Utilize newdata argument.
kaggle_predictions <- predict(kaggle_model, newdata = test) 

#Object for predict is the kaggle_model which is the regression model that is using the full train set.

# see first few predictions
head(kaggle_predictions)
```

    ##        1        2        3        4        5        6 
    ## 117825.7 170170.4 180890.7 195797.1 179727.6 163163.9

``` r
kaggle_submission <- test %>% #Assign changes from test to new variable called kaggle_submission
  select(Id) %>% #Match Kaggle Competition format rules by selecting ID
  mutate(SalePrice = kaggle_predictions) #Create new column in data set that contains the predictions for the Sale Price values.

# Check
head(kaggle_submission) #Check that formatting is correct.
```

    ##     Id SalePrice
    ## 1 1461  117825.7
    ## 2 1462  170170.4
    ## 3 1463  180890.7
    ## 4 1464  195797.1
    ## 5 1465  179727.6
    ## 6 1466  163163.9

``` r
# write to csv
write.csv(kaggle_submission, "kaggle_submission_14.csv", row.names = F)
```

## Kaggle Rank & Score

- Rank: 2767
- Score: 0.14910
