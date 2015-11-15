
#  Assignment Email Ham or Spam
##  Reproducible notes for Email spam or not

#            Anil Kumar  IIT Madras



## [[source files available on GitHub](https://github.com/anilcs13m)]]
## [[connect on linkedin]](https://in.linkedin.com/in/anilcs13m)]]

## PRELIMINARIES

Load the library that are required in the assignment:

```r
library("tm")
library("SnowballC")

library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")
```


## INTRODUCTION

Nearly every email user has at some point encountered a "spam" email, which is an unsolicited message often advertising a
product, containing links to malware, or attempting to scam the recipient. Roughly 80-90% of more than 100 billion emails
sent each day are spam emails, most being sent from botnets of malware-infected computers. The remainder of emails are
called "ham" emails.

As a result of the huge number of spam emails being sent across the Internet each day, most email providers offer a spam
filter that automatically flags likely spam messages and separates them from the ham. Though these filters use a number
of techniques, most rely heavily on the analysis of the contents of an email via text analytics.

We will be looking into how to use the text of emails is spam or ham.

We will be extracting word frequencies from the text of the documents, and then integrating those
frequencies into predictive models.  

We are going to talk about __predictive coding__ -- an emerging use of text analytics in the area
of criminal justice.


### The _eDiscovery_ Problem 

we will build and evaluate a spam filter using a publicly available dataset first described in the 2006 conference paper 
"Spam Filtering with Naive Bayes -- Which Naive Bayes?" by V. Metsis, I. Androutsopoulos, and G. Paliouras. The "ham" messages 
in this dataset come from the inbox of former Enron Managing Director for Research Vincent Kaminski, one of the inboxes 
in the Enron Corpus. One source of spam messages in this dataset is the SpamAssassin corpus, which contains hand-labeled 
spam messages contributed by Internet users. The remaining spam was collected by Project Honey Pot, a project that collects spam messages and identifies spammers by publishing email address that humans would know not to contact but that bots might target with spam.

### Predictive Coding

__Predictive coding__ is a new technique in which attorneys manually label some documents and then
use text analytics models trained on the manually labeled documents to predict which of the
remaining documents are responsive.


### The Data

The data set contains just two fields:

* __text__: the text of the email in question, 
* __spam__: a binary (0/1) variable telling whether the email was spam.

## LOADING THE DATA


```r
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
```
## Let's do the some analytics of the data

```r
str(emails)
```

```
## 'data.frame':	5728 obs. of  2 variables:
##  $ text: chr  "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market is full of suqg"| __truncated__ "Subject: the stock trading gunslinger  fanny is merrill but muzo not colza attainder and penultimate like esmark perspicuous ra"| __truncated__ "Subject: unbelievable new homes made easy  im wanting to show you this  homeowner  you have been pre - approved for a $ 454 , 1"| __truncated__ "Subject: 4 color printing special  request additional information now ! click here  click here for a printable version of our o"| __truncated__ ...
##  $ spam: int  1 1 1 1 1 1 1 1 1 1 ...
```
Let's look at a few examples (using the `strwrap()` function for easier-to-read formatting):

```r
strwrap(emails$text[1])
```

```
##  [1] "Subject: naturally irresistible your corporate identity lt is"     
##  [2] "really hard to recollect a company : the market is full of"        
##  [3] "suqgestions and the information isoverwhelminq ; but a good catchy"
##  [4] "logo , stylish statlonery and outstanding website will make the"   
##  [5] "task much easier .  we do not promise that havinq ordered a iogo"  
##  [6] "your company will automaticaily become a world ieader : it isguite"
##  [7] "ciear that without good products , effective business organization"
##  [8] "and practicable aim it will be hotat nowadays market ; but we do"  
##  [9] "promise that your marketing efforts will become much more"         
## [10] "effective . here is the list of clear benefits : creativeness :"   
## [11] "hand - made , original logos , specially done to reflect your"     
## [12] "distinctive company image . convenience : logo and stationery are" 
## [13] "provided in all formats ; easy - to - use content management"      
## [14] "system letsyou change your website content and even its structure" 
## [15] ". promptness : you will see logo drafts within three business days"
## [16] ". affordability : your marketing break - through shouldn ' t make" 
## [17] "gaps in your budget . 100 % satisfaction guaranteed : we provide"  
## [18] "unlimited amount of changes with no extra fees for you to be"      
## [19] "surethat you will love the result of this collaboration . have a"  
## [20] "look at our portfolio _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" 
## [21] "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ not"   
## [22] "interested . . . _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
## [23] "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
```

Let's check the second email: 

```r
strwrap(emails$text[2])
```

```
##  [1] "Subject: the stock trading gunslinger fanny is merrill but muzo"   
##  [2] "not colza attainder and penultimate like esmark perspicuous ramble"
##  [3] "is segovia not group try slung kansas tanzania yes chameleon or"   
##  [4] "continuant clothesman no libretto is chesapeake but tight not"     
##  [5] "waterway herald and hawthorn like chisel morristown superior is"   
##  [6] "deoxyribonucleic not clockwork try hall incredible mcdougall yes"  
##  [7] "hepburn or einsteinian earmark no sapling is boar but duane not"   
##  [8] "plain palfrey and inflexible like huzzah pepperoni bedtime is"     
##  [9] "nameable not attire try edt chronography optima yes pirogue or"    
## [10] "diffusion albeit no"
```
We can check this in the value of the `responsive` variable.

```r
emails$spam[2]
```

```
## [1] 1
```

Which word appears at the beginning of every email in the dataset

```r
emails$text[1]
```

```
## [1] "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market is full of suqgestions and the information isoverwhelminq ; but a good  catchy logo , stylish statlonery and outstanding website  will make the task much easier .  we do not promise that havinq ordered a iogo your  company will automaticaily become a world ieader : it isguite ciear that  without good products , effective business organization and practicable aim it  will be hotat nowadays market ; but we do promise that your marketing efforts  will become much more effective . here is the list of clear  benefits : creativeness : hand - made , original logos , specially done  to reflect your distinctive company image . convenience : logo and stationery  are provided in all formats ; easy - to - use content management system letsyou  change your website content and even its structure . promptness : you  will see logo drafts within three business days . affordability : your  marketing break - through shouldn ' t make gaps in your budget . 100 % satisfaction  guaranteed : we provide unlimited amount of changes with no extra fees for you to  be surethat you will love the result of this collaboration . have a look at our  portfolio _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ not interested . . . _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"
```
How many characters are in the longest email in the dataset
where longest is measured in terms of the maximum number of characters?

```r
max(nchar(emails$text))
```

```
## [1] 43952
```
print the longest email present in the dataset 

```r
min(nchar(emails$text))
```

```
## [1] 13
```

```r
which(nchar(emails$text) == 13)
```

```
## [1] 1992
```
Which row contains the shortest email in the dataset?

```r
which.min(nchar(emails$text))
```

```
## [1] 1992
```

Let's look at the breakdown of the number of emails are spam and not.

```r
table(emails$spam)
```

```
## 
##    0    1 
## 4360 1368
```
We see that the data set is unbalanced, with a relatively small proportion of emails responsive to
the query.  This is typical in predictive coding problems.


## CREATING A CORPUS

Follow the standard steps to build and pre-process the corpus:

1) Build a new corpus variable called corpus.

2) Using tm_map, convert the text to lowercase.

3) Using tm_map, remove all punctuation from the corpus.

4) Using tm_map, remove all English stopwords from the corpus.

5) Using tm_map, stem the words in the corpus.

6) Build a document term matrix from the corpus, called dtm.

we are calling our corpus as the dtm

# first step 

```r
corpus <- Corpus(VectorSource(emails$text))
```

Let's take a look at corpus:

```r
corpus
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 5728
```
# preprocessing the corpus
We use the `tm_map()` function which takes as

* its first argument the name of a __corpus__ and 
* as second argument a __function performing the transformation__ that we want to apply to the text.


```r
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
```

### Removing _stop words_ 

Removing words can be done with the `removeWords` argument to the `tm_map()` function, with an
extra argument, _i.e._ what the stop words are that we want to remove, for which we simply 
use the list for english that is provided by the `tm` package.

We will remove all of these English stop words, but we will also remove the word "_apple_"
since all of these tweets have the word "_apple_" and it probably won't be very useful in our
prediction problem.


```r
corpus <- tm_map(corpus, removeWords, stopwords("english"))
```

### Stemming

Lastly, we want to stem our document with the `stemDocument` argument.

```r
corpus <- tm_map(corpus, stemDocument)
```
It is a lot harder to read now that we removed all the stop words and punctuation and word stems,
but now the emails in this corpus are ready for our machine learning algorithms.

## BAG OF WORDS

### Create a _Document Term Matrix_

We are now ready to extract the __word frequencies__ to be used in our prediction problem.
The `tm` package provides a function called `DocumentTermMatrix()` that generates a __matrix__ where:

* the __rows__ correspond to __documents__, and 
* the __columns__ correspond to __words__ .

The values in the matrix are the number of times that word appears in each document.


```r
dtm = DocumentTermMatrix(corpus)
```
#  view the document term matrix

```r
dtm
```

```
## <<DocumentTermMatrix (documents: 5728, terms: 28687)>>
## Non-/sparse entries: 481719/163837417
## Sparsity           : 100%
## Maximal term length: 24
## Weighting          : term frequency (tf)
```
To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store
this result as spdtm, and view the number of terms present in the spdtm?

So we want to remove the terms that don't appear too often in our data set.

### Remove sparse terms


```r
spdtm = removeSparseTerms(dtm, 0.95)
```
Now we can take a look at the summary statistics for the document-term matrix:

```r
spdtm
```

```
## <<DocumentTermMatrix (documents: 5728, terms: 330)>>
## Non-/sparse entries: 213551/1676689
## Sparsity           : 89%
## Maximal term length: 10
## Weighting          : term frequency (tf)
```
We can see that we have decreased the number of terms to __330__, 
which is a much more reasonable number.

### Creating a data Frame from the _DTM_
Build a data frame called __emailsSparse__ from _spdtm_, and use the __make.names__ function to make the variable names of
emailsSparse valid.


```r
emailsSparse = as.data.frame(as.matrix(spdtm))
```
### use the _make.names_ function to make the variable names of emailsSparse valid.

```r
colnames(emailsSparse) = make.names(colnames(emailsSparse))
```
# sort the words

__colSums()__ is an R function that returns the sum of values for each variable in our data frame. Our data frame contains the
number of times each word stem (columns) appeared in each email (rows). Therefore, __colSums(emailsSparse)__ returns the
number of times a word stem appeared across all the emails in the dataset. What is the word stem that shows up most
frequently across all the emails in the dataset? Hint: think about how you can use __sort()__ or __which.max()__ to pick out the
maximum frequency.

What is the word stem that shows up most frequently across all the emails in the dataset

```r
sort(colSums(emailsSparse))
```

```
##     vkamin      begin     either       done      sorri        lot 
##        301        317        318        337        343        348 
##    mention    thought      bring       idea     better     immedi 
##        355        367        374        378        383        385 
##    without       mean      write      happi      repli       life 
##        389        390        390        396        397        400 
##     experi     involv     specif     arrang      creat       read 
##        405        405        407        410        413        413 
##       wish       open     realli       link        say    respond 
##        414        416        417        421        423        430 
##      sever       keep        etc      anoth        run       info 
##        430        431        434        435        437        438 
##     togeth      short     sincer        buy        due    alreadi 
##        438        439        441        442        445        446 
##       line      allow     recent    special      given     believ 
##        448        450        451        451        453        456 
##     design        put      remov       X853  wednesday       type 
##        457        458        460        462        464        466 
##     public       full       hear       join     effect     effort 
##        468        469        469        469        471        473 
##    tuesday     robert      locat      check       area      final 
##        474        482        485        488        489        490 
##    increas       soon    analysi       sure       deal     return 
##        491        492        495        495        498        509 
##      place      onlin    success       sinc understand      still 
##        516        518        519        521        521        523 
##     import    comment    confirm      hello       long      thing 
##        530        531        532        534        534        535 
##      point    appreci       feel      howev     member       hour 
##        536        541        543        545        545        548 
##        net    continu      event     expect    suggest       unit 
##        548        552        552        554        554        554 
##    resourc       case    version     corpor     applic      engin 
##        556        561        564        565        567        571 
##       part     attend   thursday      might       morn        abl 
##        571        573        575        577        586        590 
##     assist     differ     intern      updat       move       mark 
##        598        598        606        606        612        613 
##     depart       even       made   internet       high      cours 
##        621        622        622        623        624        626 
##   contract     gibner        end      right        per      invit 
##        629        633        635        639        642        647 
##     approv       real     monday     result     school      kevin 
##        648        648        649        655        655        656 
##     direct       home     detail        tri       form    problem 
##        657        660        661        661        664        666 
##        web        doc      deriv        don      april       note 
##        668        675        676        676        682        688 
##      relat     websit       juli   director    complet       rate 
##        694        700        701        705        707        717 
##       valu      futur    student        set     within     requir 
##        721        722        726        727        732        736 
##    softwar       book       mani     person      click       file 
##        739        756        758        767        769        770 
##      addit      money     associ   particip       term     access 
##        774        776        777        782        786        789 
##     custom    possibl       copi       oper       cost    respons 
##        796        796        797        820        821        824 
##      today    account       base      great       dear     london 
##        828        829        837        837        838        843 
##     friday    support      secur       hope       much       back 
##        854        854        857        858        861        864 
##        way       find     invest        ask      start      shall 
##        864        867        867        871        880        884 
##     origin       come       plan    financi        two       site 
##        892        903        904        909        911        913 
##   opportun       team      first      resum       issu       data 
##        918        926        929        933        944        955 
##      month      peopl     credit   industri    process     review 
##        958        958        960        970        975        976 
##       talk       last      phone       X000      chang        fax 
##        981        998       1001       1007       1035       1038 
##       john    current    stinson       give    univers      offic 
##       1042       1044       1051       1055       1059       1068 
##        gas    schedul     financ      state       name       X713 
##       1070       1071       1073       1086       1089       1097 
##       good      posit   crenshaw     system       well       sent 
##       1097       1104       1115       1118       1125       1126 
##      visit       free      next.      avail   question    address 
##       1126       1141       1145       1152       1152       1154 
##      offer     attach     number       date    product      order 
##       1171       1176       1182       1187       1197       1210 
##      think     includ     report       best     confer        now 
##       1216       1238       1279       1291       1297       1300 
##        www    discuss  interview     servic   communic    request 
##       1323       1326       1333       1337       1343       1344 
##       just       take      trade       send     provid       list 
##       1354       1361       1366       1379       1405       1410 
##       help    program     option       want    project    contact 
##       1430       1438       1488       1488       1522       1543 
##    present     follow     receiv        see    houston       http 
##       1543       1552       1557       1567       1582       1609 
##        edu       call    shirley       corp       week   interest 
##       1627       1687       1689       1692       1758       1814 
##        day       also    develop       make       year        let 
##       1860       1864       1882       1884       1890       1963 
##     messag       look     regard      email        one      power 
##       1983       2003       2045       2066       2108       2117 
##     energi      model       risk       mail        new    compani 
##       2179       2199       2267       2269       2281       2290 
##       busi       need        use       like        get        may 
##       2313       2328       2330       2352       2462       2465 
##      manag      group       know       meet      price     inform 
##       2600       2604       2614       2623       2694       2701 
##       work     market   research      X2001       time    forward 
##       2708       2750       2820       3089       3145       3161 
##      thank        can   kaminski      X2000      pleas        com 
##       3730       4257       4801       4967       5113       5443 
##        hou       will       vinc    subject        ect      enron 
##       5577       8252       8532      10202      11427      13388
```

```r
which.max(colSums(emailsSparse))
```

```
## enron 
##    92
```
# Adding the variable
Add a variable called "spam" to emailsSparse containing the email spam labels, this can be done by copying over the "spam"
variable from the original data frame.

```r
emailsSparse$spam = emails$spam
```
Now let's see how many time word stems appear at least 5000 times in the ham emails in the dataset
We can read the most frequent terms in the ham dataset

```r
sort(colSums(subset(emailsSparse, spam == 0)))
```

```
##       spam       life      remov      money      onlin    without 
##          0         80        103        114        173        191 
##     websit      click    special       wish      repli        buy 
##        194        217        226        229        239        243 
##        net       link     immedi       done       mean     design 
##        243        247        249        254        259        261 
##        lot     effect       info     either       read      write 
##        268        270        273        279        279        286 
##       line      begin      sorri    success     involv      creat 
##        289        291        293        293        294        299 
##    softwar     better     vkamin        say       keep      bring 
##        299        301        301        305        306        311 
##     believ       full    increas     realli    mention    thought 
##        313        317        320        324        325        325 
##       idea     invest      secur     specif      sever     experi 
##        327        327        337        338        340        346 
##      thing      allow      check        due       type      happi 
##        347        348        351        351        352        354 
##     return     expect      short     effort       open   internet 
##        355        356        357        358        360        361 
##     sincer     public     recent      anoth    alreadi       home 
##        361        364        368        369        372        375 
##       made    respond      given        etc        put     within 
##        380        382        383        385        385        386 
##      place      right    version      hello       sure       area 
##        388        390        390        395        396        397 
##        run     arrang    account       join       hour      locat 
##        398        399        401        403        404        406 
##     togeth      engin     import        per     corpor       high 
##        406        411        411        412        414        416 
##     result       hear      final       deal     applic       even 
##        418        420        422        423        428        429 
##        web     custom       soon       long       sinc      futur 
##        430        433        435        436        439        440 
##     member       X000      event        don       part       feel 
##        446        447        447        450        450        453 
##    tuesday  wednesday      still       unit       site       X853 
##        454        456        457        457        458        461 
##    continu understand    resourc     robert    analysi       form 
##        464        464        466        466        468        468 
##      point     assist    confirm     differ     intern      might 
##        474        475        485        489        489        490 
##       real       case      howev    comment        abl    complet 
##        490        492        496        505        515        515 
##       rate    appreci        tri       move      updat     approv 
##        516        518        521        526        527        533 
##    suggest       free   contract     detail       morn        end 
##        533        535        544        546        546        550 
##       mani     attend   thursday     direct     requir      cours 
##        550        558        558        561        562        567 
##     person      relat     depart      today      start        way 
##        569        573        575        577        580        586 
##       mark       valu    problem      peopl       note     school 
##        588        590        593        599        600        607 
##      invit     access       term       juli     monday     gibner 
##        614        617        625        630        630        633 
##       base   director      offer       cost      addit      kevin 
##        635        640        643        646        648        654 
##      great        set       file       find       much       oper 
##        655        658        659        665        669        669 
##      order      deriv        doc      april       book    address 
##        669        673        673        677        680        693 
##       copi    financi      month    student    respons    possibl 
##        700        702        709        710        711        712 
##     associ   particip        now      first   industri       dear 
##        715        717        725        726        731        734 
##    support       plan       back       name       come   opportun 
##        734        738        739        745        748        760 
##     report    product        two     origin        ask     credit 
##        772        776        787        796        797        798 
##      state     system    process       hope     london       just 
##        806        816        826        828        828        830 
##     receiv      chang     review    current      shall     friday 
##        830        831        834        841        844        847 
##       team      phone       issu       data      avail       last 
##        850        858        865        868        872        874 
##       good       give        www        gas       list      posit 
##        876        883        897        905        907        917 
##      visit     includ      resum       best      offic     servic 
##        920        924        928        933        935        942 
##       talk     number       well        fax     provid       sent 
##        943        951        961        963        970        971 
##      next.       send       http       john    univers     financ 
##        975        986       1009       1022       1025       1038 
##    stinson    schedul       take       date       want   question 
##       1051       1054       1057       1060       1068       1069 
##    program      think       X713   crenshaw     attach      trade 
##       1080       1084       1097       1115       1155       1167 
##       help      email    compani    request        see   communic 
##       1168       1201       1225       1227       1238       1251 
##     confer    discuss       make    contact     follow  interview 
##       1264       1270       1281       1301       1308       1320 
##    project       mail    present       busi   interest     option 
##       1328       1352       1397       1416       1429       1432 
##        day       call        one       year       week     messag 
##       1440       1497       1516       1523       1527       1538 
##    houston       also       look        edu       corp    shirley 
##       1577       1604       1607       1620       1643       1687 
##    develop        get        new        use        let     regard 
##       1691       1768       1777       1784       1856       1859 
##     inform       need      power        may       like       risk 
##       1883       1890       1972       1976       1980       2097 
##     energi     market      model      price       work      manag 
##       2124       2150       2170       2191       2293       2334 
##       know      group       meet       time   research    forward 
##       2345       2474       2544       2552       2752       2952 
##      X2001        can      thank        com      pleas   kaminski 
##       3060       3426       3558       4444       4494       4801 
##      X2000        hou       will       vinc    subject        ect 
##       4935       5569       6802       8531       8625      11417 
##      enron 
##      13388
```
# dataset to the spam emails

```r
# subset(emailsSparse, spam == 1)
```
# we can read the most frequent terms 

```r
sort(colSums(subset(emailsSparse, spam == 1)))
```

```
##       X713   crenshaw      enron     gibner   kaminski    stinson 
##          0          0          0          0          0          0 
##     vkamin       X853       vinc        doc      kevin    shirley 
##          0          1          1          2          2          2 
##      deriv      april    houston      resum        edu     friday 
##          3          5          5          5          7          7 
##        hou  wednesday        ect     arrang  interview     attend 
##          8          8         10         11         13         15 
##     london     robert    student    schedul   thursday     monday 
##         15         16         16         17         17         19 
##       john    tuesday     attach    suggest    appreci       mark 
##         20         20         21         21         23         25 
##      begin    comment    analysi      X2001      model       hope 
##         26         26         27         29         29         30 
##    mention      X2000     togeth     confer      invit    univers 
##         30         32         32         33         33         34 
##     financ       talk     either        run       morn      shall 
##         35         38         39         39         40         40 
##      happi    thought     depart    confirm    respond     school 
##         42         42         46         47         48         48 
##       corp        etc       hear      howev      sorri       idea 
##         49         49         49         49         50         51 
##     energi    discuss       open     option       soon understand 
##         55         56         56         56         57         57 
##      cours     experi     associ      point      bring   director 
##         59         59         62         62         63         65 
##   particip      anoth       join      still      final   research 
##         65         66         66         66         68         68 
##       case        set     specif      given       juli    problem 
##         69         69         69         70         71         73 
##        put    alreadi        ask        abl       deal        fax 
##         73         74         74         75         75         75 
##       book       team       issu      locat       meet      updat 
##         76         76         79         79         79         79 
##        lot     sincer     better      short       sinc       done 
##         80         80         82         82         82         83 
##   question     recent    possibl   contract        end       move 
##         83         83         84         85         85         86 
##       data      might    continu       note       feel    resourc 
##         87         87         88         88         90         90 
##      sever       area   communic     realli        due     direct 
##         90         92         92         93         94         96 
##     origin       copi       unit       long     member       sure 
##         96         97         97         98         99         99 
##      allow       dear     public      write      event        let 
##        102        104        104        104        105        107 
##     differ       file     involv    respons      creat       type 
##        109        111        111        113        114        114 
##     approv     detail     effort     intern    request        say 
##        115        115        115        117        117        118 
##     import    support       part      relat     assist       last 
##        119        120        121        121        123        124 
##        two       back       keep      addit       date      place 
##        124        125        125        126        127        128 
##      group       mean       valu      think      offic       read 
##        130        131        131        132        133        134 
##     immedi      check     applic      hello        tri     review 
##        136        137        139        139        140        142 
##     believ      phone       hour      power    present    process 
##        143        143        144        145        146        149 
##     corpor       oper       full     return       come       sent 
##        151        151        152        154        155        155 
##   opportun       real      repli       line      engin       term 
##        158        158        158        159        160        161 
##     credit       well        gas       info       plan      next. 
##        162        164        165        165        166        170 
##       risk    increas     access       give      thank       link 
##        170        171        172        172        172        174 
##     requir    version       cost      great       wish     regard 
##        174        174        175        182        185        186 
##      posit      thing       call    develop    complet       much 
##        187        188        190        191        192        192 
##       even    project     design       form     expect     person 
##        193        194        196        196        198        198 
##    without        buy      trade     effect       rate       base 
##        198        199        199        201        201        202 
##       find    current      first      chang      visit    financi 
##        202        203        203        204        206        207 
##       high       mani    forward       good    special        don 
##        208        208        209        221        225        226 
##    success        per     number       week     result        web 
##        226        230        231        231        237        238 
##   industri    contact       made     follow      month      right 
##        239        242        242        244        249        249 
##      today       also       help   internet      manag       know 
##        251        260        262        262        266        269 
##        way      avail      state      futur       home      start 
##        278        280        280        282        285        300 
##     system       take        net     includ       life        see 
##        302        304        305        314        320        329 
##       name      onlin     within      remov       best    program 
##        344        345        346        357        358        358 
##      peopl     custom       year       like   interest       send 
##        359        363        367        372        385        393 
##     servic       look       work        day       want    product 
##        395        396        415        420        420        421 
##        www    account     provid       need    softwar     messag 
##        426        428        435        438        440        445 
##       site    address        may       list      price        new 
##        455        461        489        503        503        504 
##     websit     report      secur       just      offer     invest 
##        506        507        520        524        528        540 
##      order        use      click       X000        now        one 
##        541        546        552        560        575        592 
##       time       http     market       make       free      pleas 
##        593        600        600        603        606        619 
##      money        get     receiv     inform        can      email 
##        662        694        727        818        831        865 
##       busi       mail        com    compani       spam       will 
##        897        917        999       1065       1368       1450 
##    subject 
##       1577
```
## Build Machine learnig model

First, convert the dependent variable to a factor with __emailsSparse$spam = as.factor(emailsSparse$spam)__

```r
emailsSparse$spam = as.factor(emailsSparse$spam)
```
we are setting the random seed some value so that every time same result will come.

```r
set.seed(123)
```
# Building the model
before building the model we need to split our data into training and testing by using __sample.split__ function with 70 data in training and rest in test

```r
spl = sample.split(emailsSparse$spam, 0.7)
```
# Train and Test Subset 
use the __subset__ function __TRUE__ for the train and __FALSE__ for the test

```r
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
```
# Logistic Regression Model

we are creating a logistic regression model called as spamLog, for logistic regression we use function __glm__ with family binomial.
we are using the all the variable as independent variable for training our model, for detection.


```r
spamLog = glm(spam~., data=train, family="binomial")
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```
# summary
summary of logistic model spamLog

```r
# summary(spamLog)
```
# BUILD A CART MODEL

create one model __CART__ and called this model as __spamCART__, we are using default parameters to train this model, so that no need to add __minbucket__ or __cp__ parameters. Remember to add the argument method="class" since this is a binary classification problem.

```r
spamCART = rpart(spam~., data=train, method="class")
```
# PLOT CART


```r
prp(spamCART)
```

![plot of chunk CART_plot_tree](figure/CART_plot_tree-1.png) 
# BUILD A RANDOM FOREST
We are creating one more model random forest model and called this model as __spamRF__, we are using default parameters to train this mode also, no need to worry about specifying ntree or nodsize. Directly before training the random forest model, set the random seed to 123 (even though we've already done this, it's important to set the seed right before training the model so we all obtain the
same results. Keep in mind though that on certain operating systems, your results might still be slightly different).

```r
set.seed(123)
```
### Create Random Forest Model using all the parameters.

```r
spamRF = randomForest(spam~., data=train)
```

### Out-of-Sample Performance of all the above model we created

Now that we have trained a models, we need to evaluate it on the test set.  
For each model, obtain the predicted spam probabilities for the training set. Be careful to obtain probabilities instead of
predicted classes, because we will be using these values to compute training set __AUC__ values. Recall that you can obtain
probabilities for CART models by not passing any type parameter to the predict() function, and you can obtain probabilities
from a random forest by adding the argument __type="prob"__. For CART and random forest, you need to select the second
column of the output of the __predict()__ function, corresponding to the probability of a message being spam

* The __first__ column is the predicted probability of the document being __non-responsive__.
* The __second__ column is the predicted probability of the document being __responsive__.
* They sum to 1.
In our case we are interested in the predicted probability of the document being responsive, so for that we are using __second__.


```r
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]
```
This new object gives us the predicted probabilities on the test set.
# predicted probabilities

How many of the training set predicted probabilities from spamLog are less than 0.00001?

```r
table(predTrainLog<0.00001)
```

```
## 
## FALSE  TRUE 
##   964  3046
```
How many of the training set predicted probabilities from spamLog are more than 0.99999?

```r
table(predTrainLog > 0.99999)
```

```
## 
## FALSE  TRUE 
##  3056   954
```
How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?

```r
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)
```

```
## 
## FALSE  TRUE 
##  4000    10
```
# summary

```r
# summary(spamLog)
```
# Accuracy

What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions.

```r
table(train$spam, predTrainLog > 0.5)
```

```
##    
##     FALSE TRUE
##   0  3052    0
##   1     4  954
```

```r
(3052+954)/nrow(train)
```

```
## [1] 0.9990025
```
What is the training set AUC of spamLog?

```r
predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)
```

```
## [1] 0.9999959
```
What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?

```r
table(train$spam, predTrainCART > 0.5)
```

```
##    
##     FALSE TRUE
##   0  2885  167
##   1    64  894
```
# accuracy

```r
(2885+894)/nrow(train)
```

```
## [1] 0.942394
```
What is the training set AUC of spamCART?

```r
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)
```

```
## [1] 0.9696044
```
What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?

```r
table(train$spam, predTrainRF > 0.5)
```

```
##    
##     FALSE TRUE
##   0  3013   39
##   1    44  914
```
# accuracy 

```r
(3013+914)/nrow(train)
```

```
## [1] 0.9793017
```

# EVALUATING ON THE TEST SET
We are interested in the __accuracy__ of our models on the test set, _i.e._ out-of-sample.
First we compute the _confusion matrix_:

```r
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
```
# Test Accuracy of all the above model

testing set accuracy of spamLog, using a threshold of 0.5 for predictions?

```r
table(test$spam, predTestLog > 0.5)
```

```
##    
##     FALSE TRUE
##   0  1257   51
##   1    34  376
```
# accuracy

```r
(1257+376)/nrow(test)
```

```
## [1] 0.9505239
```
What is the testing set AUC of spamLog

```r
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)
```

```
## [1] 0.9627517
```
What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?

```r
table(test$spam, predTestCART > 0.5)
```

```
##    
##     FALSE TRUE
##   0  1228   80
##   1    24  386
```
# accuracy 

```r
(1228+386)/nrow(test)
```

```
## [1] 0.9394645
```
# What is the testing set AUC of spamCART?

```r
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)
```

```
## [1] 0.963176
```

What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?

```r
table(test$spam, predTestRF > 0.5)
```

```
##    
##     FALSE TRUE
##   0  1290   18
##   1    25  385
```
# accuracy 

```r
(1290+385)/nrow(test)
```

```
## [1] 0.9749709
```
# What is the testing set AUC of spamRF?

```r
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)
```

```
## [1] 0.9975656
```
