# install packages -------------------------------------------------------------

packages <- rownames(installed.packages()); packages

if (!isTRUE("here" %in% packages)) install.packages("here")
if (!isTRUE("broom" %in% packages)) install.packages("broom")
if (!isTRUE("coin" %in% packages)) install.packages("coin")
if (!isTRUE("dplyr" %in% packages)) install.packages("dplyr")
if (!isTRUE("ggplot2" %in% packages)) install.packages("ggplot2")
if (!isTRUE("haven" %in% packages)) install.packages("haven")
if (!isTRUE("purrr" %in% packages)) install.packages("purrr")
if (!isTRUE("readxl" %in% packages)) install.packages("readxl")
if (!isTRUE("tidyr" %in% packages)) install.packages("tidyr")
if (!isTRUE("tidyverse" %in% packages)) install.packages("tidyverse")
