---
title: 'Reproducible Research: Peer Assessment 2'
author: "AWP"
date: "Monday, July 20, 2015"
output: html_document
---

##Synopsis


##Data Processing

```r
library(dplyr)
#download data if not already downloaded
if(!file.exists("FStormData.csv.bz2")){
  url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, dest="FStormData.csv.bz2", mode="wb") 
  #read in the data from the compresed file
  
}
#We save the raw data to disk as rds, check if it exists, if not create it.
if (!file.exists("raw_data.rds")){
   rawData <- read.csv("FStormData.csv.bz2")
  
  #Convert EVTTYPT to all upper-case and add a new column called EVENTTYPE
  rawData$EVENTTYPE<-toupper(rawData$EVTYPE)
  
  #Narrow our focus
  rawNarrowed<-subset(rawData,select=c(EVENTTYPE,BGN_DATE,MAG,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,WFO))
  
  #save off the data to a rds format
  saveRDS(rawNarrowed,"raw_data.rds")
  rawData<-rawNarrowed
  rm(rawNarrowed)
}else{
 
  rawData<-readRDS("raw_data.rds")
}
```

##Analysis

```r
#Get counts of complete and incomplete data sets
incompleteRows<-sum(!complete.cases(rawData))
completeRows<-sum(complete.cases(rawData))

names(rawData)
```

```
##  [1] "EVENTTYPE"  "BGN_DATE"   "MAG"        "FATALITIES" "INJURIES"  
##  [6] "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"
```

```r
#EVTYPE BGN_DATE
#FATALITIES
#INJURIES
#PROPDMG          PROPDMGEXP        CROPDMG          CROPDMGEXP  

#max_stepsByDay<-aggregate(steps ~ interval+date, data = omittedNA, FUN = mean)
require(plyr); 
require(lubridate)
library(dplyr)
```

The raw data has 0 incomplete rows and 902297 complete rows. Elminiation of incomplete rows is not possilbe at this point and time.



```r
#get all of the Eventtypes
allEventTypes<-subset(rawData,select=c(EVENTTYPE))
#find all eventtypes that are duplicates
duplicateEventTypes<-allEventTypes[duplicated(allEventTypes[,c("EVENTTYPE")]),]
#find unique eventtypes from the duplicate list
uniqueDuplicateEventTypes<-unique(duplicateEventTypes)

outcome<-rawData %>% group_by(EVENTTYPE) %>% summarise_each(funs(sum))
nrow(outcome)
```

```
## [1] 898
```

```r
outcome[outcome$EVENTTYPE=="TORNADO",]
```

```
## Source: local data frame [1 x 10]
## 
##   EVENTTYPE  BGN_DATE  MAG FATALITIES INJURIES PROPDMG PROPDMGEXP  CROPDMG
## 1   TORNADO 519386830 4331       5633    91346 3212258     898934 100018.5
## Variables not shown: CROPDMGEXP (int), WFO (int)
```

word


```r
xx<-rawData[rawData$EVENTTYPE=="TORNADO",] 

propertyDamage<-xx %>% group_by(EVENTTYPE,PROPDMGEXP) %>% summarise_each(funs(sum),vars=c(-BGN_DATE,-FATALITIES,-INJURIES,-MAG,-WFO,-CROPDMGEXP,-CROPDMG))
cropDamage<-xx %>% group_by(EVENTTYPE,CROPDMGEXP) %>% summarise_each(funs(sum),vars=c(-BGN_DATE,-FATALITIES,-INJURIES,-MAG,-WFO,-PROPDMGEXP,-PROPDMG))

FATALITIES<-xx %>% group_by(EVENTTYPE) %>% summarise_each(funs(sum),vars=c(-BGN_DATE,-INJURIES,-MAG,-WFO,-PROPDMGEXP,-PROPDMG,-CROPDMG,-CROPDMGEXP))

INJURIES<-xx %>% group_by(EVENTTYPE) %>% summarise_each(funs(sum),vars=c(-BGN_DATE,-FATALITIES,-MAG,-WFO,-PROPDMGEXP,-PROPDMG,-CROPDMG,-CROPDMGEXP))

propertyDamage
```

```
## Source: local data frame [11 x 3]
## Groups: EVENTTYPE
## 
##    EVENTTYPE PROPDMGEXP    PROPDMG
## 1    TORNADO                  3.00
## 2    TORNADO          +      60.00
## 3    TORNADO          0     133.50
## 4    TORNADO          1       0.00
## 5    TORNADO          5     102.20
## 6    TORNADO          6       0.00
## 7    TORNADO          7       0.00
## 8    TORNADO          B       5.30
## 9    TORNADO          K 3163480.48
## 10   TORNADO          m      11.50
## 11   TORNADO          M   48462.18
```

```r
cropDamage
```

```
## Source: local data frame [4 x 3]
## Groups: EVENTTYPE
## 
##   EVENTTYPE CROPDMGEXP  CROPDMG
## 1   TORNADO                0.00
## 2   TORNADO          0   160.00
## 3   TORNADO          K 99543.11
## 4   TORNADO          M   315.41
```

```r
FATALITIES
```

```
## Source: local data frame [1 x 2]
## 
##   EVENTTYPE FATALITIES
## 1   TORNADO       5633
```

```r
INJURIES
```

```
## Source: local data frame [1 x 2]
## 
##   EVENTTYPE INJURIES
## 1   TORNADO    91346
```



```r
names(rawData)
```

```
##  [1] "EVENTTYPE"  "BGN_DATE"   "MAG"        "FATALITIES" "INJURIES"  
##  [6] "PROPDMG"    "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"
```

```r
xx %>% group_by(EVENTTYPE,CROPDMGEXP,PROPDMGEXP) %>% summarise_each(funs(sum),vars=c(-PROPDMGEXP,-CROPDMGEXP))
```

```
## Source: local data frame [20 x 10]
## Groups: EVENTTYPE, CROPDMGEXP
## 
##    EVENTTYPE CROPDMGEXP PROPDMGEXP  BGN_DATE  MAG FATALITIES INJURIES
## 1    TORNADO                        79884947 1167         42      892
## 2    TORNADO                     +     10979    0          0        0
## 3    TORNADO                     0     92632    0          3       24
## 4    TORNADO                     1     40781    0          0        0
## 5    TORNADO                     5     25147    0          0        0
## 6    TORNADO                     6      3803    0          0        0
## 7    TORNADO                     7      8203    0          0        1
## 8    TORNADO                     K 333422577 2690       1647    25456
## 9    TORNADO                     m      7697    0          0       36
## 10   TORNADO                     M  27612657  231       2877    52977
## 11   TORNADO          0          K     28017    0          0        0
## 12   TORNADO          K              2303102    0          0        0
## 13   TORNADO          K          0      1888    0          0        0
## 14   TORNADO          K          B     22751    0        206     1995
## 15   TORNADO          K          K  69898051  243        177     1875
## 16   TORNADO          K          M   5313281    0        561     6950
## 17   TORNADO          M                20237    0          0        0
## 18   TORNADO          M          0      6933    0          0        0
## 19   TORNADO          M          K    214747    0         22      153
## 20   TORNADO          M          M    468400    0         98      987
## Variables not shown: PROPDMG (dbl), CROPDMG (dbl), WFO (int)
```

```r
xx %>% group_by(EVENTTYPE) %>% summarise(theEVENTTYPE==EVENTTYPE,MagSum=sum(MAG),FatalititySum=sum(FATALITIES),InjueriesSum=sum(INJURIES))
```

```
## Error in eval(expr, envir, enclos): object 'theEVENTTYPE' not found
```

```r
eventType_dates<-subset(rawData,select=c(EVENTTYPE,BGN_DATE)) 
names(eventType_dates)
```

```
## [1] "EVENTTYPE" "BGN_DATE"
```

```r
cleanEventTypeDate<-eventType_dates[!duplicated(eventType_dates[,c("EVENTTYPE","BGN_DATE")]),]
rm(eventType_dates)

cleanEventTypeDate<-mutate(cleanEventTypeDate, date = parse_date_time(BGN_DATE,"%m/%d/%y %H%M%S"),year = year(date))

#head(cleanEventTypeDate)

head(arrange(cleanEventTypeDate,year,EVENTTYPE))
```

```
##   EVENTTYPE          BGN_DATE       date year
## 1   TORNADO 4/18/1950 0:00:00 1950-04-18 1950
## 2   TORNADO 1/13/1950 0:00:00 1950-01-13 1950
## 3   TORNADO 2/12/1950 0:00:00 1950-02-12 1950
## 4   TORNADO 3/26/1950 0:00:00 1950-03-26 1950
## 5   TORNADO  4/2/1950 0:00:00 1950-04-02 1950
## 6   TORNADO 8/24/1950 0:00:00 1950-08-24 1950
```

```r
#head(aggregate(cleanEventTypeDate,by=list(year,EVTYPE),FUN=sum))
library(sqldf)
#sqldf('SELECT A, SUM(B) AS B FROM df GROUP BY A')
#numEventsByYear<-sqldf('SELECT EVTYPE, year,count(*) as cnt FROM cleanEventTypeDate where year>=1950 and year<=1954 GROUP BY year,EVTYPE')
```



```r
numEventsByYear<-cleanEventTypeDate %>% group_by(EVENTTYPE,year) %>% tally()
Eliminate50_54<-sqldf('SELECT EVENTTYPE, year,n FROM numEventsByYear where year>=1950 and year<=1954 ')
Eliminate50_54
```

```
##   EVENTTYPE year   n
## 1   TORNADO 1950  91
## 2   TORNADO 1951 108
## 3   TORNADO 1952  98
## 4   TORNADO 1953 136
## 5   TORNADO 1954 161
```

```r
aggEventType<-aggregate( numEventsByYear$n ~numEventsByYear$EVENTTYPE ,FUN = sum, na.rm=TRUE)
names(aggEventType)<-c("EVENTTYPE","Num")

#Which event has the most occurences 
top_10<-aggEventType %>%  top_n(n=10) 
```

```
## Selecting by Num
```

```r
OrderedTop10<-top_10[order(-top_10$Num),]




library(ggplot2)

ggplot(data=OrderedTop10, aes(x=EVENTTYPE, y=Num)) +geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
```

![plot of chunk ploting](figure/ploting-1.png) 

```r
OrderedTop10
```

```
##       EVENTTYPE   Num
## 4          HAIL 10918
## 9       TORNADO 10507
## 10    TSTM WIND 10348
## 1   FLASH FLOOD  4109
## 2         FLOOD  3736
## 7     HIGH WIND  3530
## 8     LIGHTNING  3476
## 6    HEAVY SNOW  2962
## 5    HEAVY RAIN  2727
## 3  FUNNEL CLOUD  2510
```




```r
numEventsByEventType<-cleanEventTypeDate %>% group_by(EVENTTYPE) %>% tally()

aggEventTypeOnly<-aggregate( numEventsByYear$n ~numEventsByYear$EVENTTYPE ,FUN = sum, na.rm=TRUE)

head(aggEventTypeOnly)
```

```
##   numEventsByYear$EVENTTYPE numEventsByYear$n
## 1        HIGH SURF ADVISORY                 1
## 2             COASTAL FLOOD                 1
## 3               FLASH FLOOD                 1
## 4                 LIGHTNING                 1
## 5                 TSTM WIND                 4
## 6           TSTM WIND (G45)                 1
```

```r
top_10Events<-aggEventTypeOnly %>%  top_n(n=10) 
```

```
## Selecting by numEventsByYear$n
```

```r
OrderedTop10Events<-top_10Events[order(-top_10$Num),]

OrderedTop10Events
```

```
##    numEventsByYear$EVENTTYPE numEventsByYear$n
## 4                       HAIL             10918
## 9                    TORNADO             10507
## 10                 TSTM WIND             10348
## 1                FLASH FLOOD              4109
## 2                      FLOOD              3736
## 7                  HIGH WIND              3530
## 8                  LIGHTNING              3476
## 6                 HEAVY SNOW              2962
## 5                 HEAVY RAIN              2727
## 3               FUNNEL CLOUD              2510
```




##Results


    1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

    2. Across the United States, which types of events have the greatest economic consequences?

Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.
