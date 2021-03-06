# COVID measure compliance
 
Data and analyses regarding the study on the compliance with COVID measures


```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE, include = TRUE)
library(dplyr)
```

In this README, the steps of data preparations, cleaning and analyses for each experiment in the study are described step by step. This study holds three experiments where similar steps and data analyses are used. Nevertheless, we describe each experiment separately. All data preparations and cleanings are performed in R statistical software. The code below works directly from the `Rproject` directory. But the R-files can also used manually. The data are stored in excel files. 

The current file is executive, whereas the `memo.pdf` file describes the separate files more descriptively.

## Experiment 1

### 1. Seed

First a seed is set to enable exact replication of the analyses. 

```{r}
seed <- 432973; set.seed(seed)

options(
  tibble.print_max = 1000
)

```


### 2. Install packages

```{r}
if (!isTRUE("here" %in% rownames(installed.packages()))) install.packages("here")
if (!isTRUE("here" %in% loadedNamespaces())) library(here)

source("experiment1/R/exp1_install_packages.R")
``` 


### 3. Load packages

```{r}
source("experiment1/R/exp1_load_packages.R")
```


### 4. Load public data

With the code below, the data can be loaded for experiment 1. 

```{r}
load("experiment1/data/experiment1.rdata")
```

### 5. Load variable labels

In the spreadsheet below the variable labels and value labels are stored. 

```{r}
codebook <- openxlsx::read.xlsx("experiment1/data/codebook_experiment1.xlsx")
```

The summary of the data with the relevant means, frequency and number of missings can be found at:

```{r}
"experiment1/data/data_summary_experiment1.txt"
```




### 6. Statistical analyses

In this syntax, all statistical analyses for experiment 1 are performed.

```{r}
source("experiment1/R/exp1_statistical_analyses.R")
```


### 7. Save output

The output from the statistical analyses is stored in to locations: in the `rdata` folder as an `.rdata` file and in the `txt` folder as a `.txt` file. 

```{r}
source("experiment1/R/exp1_save_output.R")
```




## Experiment 2


# 1. seed
```{r}
seed <- 648731; set.seed(seed)

options(
  tibble.print_max = 1000
)
```

### 2. Install packages

```{r}
if (!isTRUE("here" %in% rownames(installed.packages()))) install.packages("here")
if (!isTRUE("here" %in% loadedNamespaces())) library(here)

source("experiment2/R/exp2_install_packages.R")
``` 


### 3. Load packages

```{r}
source("experiment2/R/exp2_load_packages.R")
```


### 4. Load public data

With the code below, the data can be loaded for experiment 2. 

```{r}
load("experiment2/data/experiment2.rdata")
```

### 5. Load variable labels

In the spreadsheet below the variable labels and value labels are stored. 

```{r}
codebook <- openxlsx::read.xlsx("experiment2/data/codebook_experiment2.xlsx")
```

The summary of the data with the relevant means, frequency and number of missings can be found at:

```{r}
"experiment2/data/data_summary_experiment2.txt"
```



### 6. Statistical analyses

In this syntax, all statistical analyses for experiment 2 are performed.

```{r}
source("experiment2/R/exp2_statistical_analyses.R")
```


### 7. Save output

The output from the statistical analyses is stored in to locations: in the `rdata` folder as an `.rdata` file and in the `txt` folder as a `.txt` file. 

```{r}
source("experiment2/R/exp2_save_output.R")
```



## Experiment 3


# 1. seed
```{r}
seed <- 942165; set.seed(seed)

options(
  tibble.print_max = 1000
)
```


### 2. Install packages

```{r}



source("experiment3/R/exp3_install_packages.R")
``` 


### 3. Load packages

```{r}
source("experiment3/R/exp3_load_packages.R")
```


### 4. Load public data

With the code below, the data can be loaded for experiment 3. 

```{r}
load("experiment3/data/experiment3.rdata")
```

### 5. Load variable labels

In the spreadsheet below the variable labels and value labels are stored. 

```{r}
codebook <- openxlsx::read.xlsx("experiment3/data/codebook_experiment3.xlsx")
```


The summary of the data with the relevant means, frequency and number of missings can be found at:

```{r}
"experiment3/data/data_summary_experiment3.txt"
```


### 6. Statistical analyses

In this syntax, all statistical analyses for experiment 3 are performed.

```{r}
source("experiment3/R/exp3_statistical_analyses.R")
```


### 7. Save output

The output from the statistical analyses is stored in to locations: in the `rdata` folder as an `.rdata` file and in the `txt` folder as a `.txt` file. 

```{r}
source("experiment3/R/exp3_save_output.R")
```

