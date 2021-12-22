# load packages ---------------------------------------------------------------

loaded_packages <- loadedNamespaces(); loaded_packages

if (!isTRUE("here" %in% loaded_packages)) library(here)
if (!isTRUE("broom" %in% loaded_packages)) library(broom)
if (!isTRUE("coin" %in% loaded_packages)) library(coin)
if (!isTRUE("dplyr" %in% loaded_packages)) library(dplyr)
if (!isTRUE("ggplot2" %in% loaded_packages)) library(ggplot2)
if (!isTRUE("haven" %in% loaded_packages)) library(haven)
if (!isTRUE("purrr" %in% loaded_packages)) library(purrr)
if (!isTRUE("readxl" %in% loaded_packages)) library(readxl)
if (!isTRUE("tidyr" %in% loaded_packages)) library(tidyr)
if (!isTRUE("tidyverse" %in% loaded_packages)) library(tidyverse)
