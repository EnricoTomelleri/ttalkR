ttalkR <img src="logo_ttalkr.png" width="200" align="right"/>
======================================================================================================
![license](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 


A unified processing workflow for standardising treetalkers data across large networks. Conversion of digital numbers to physical units is done according to the TT+ User Manual (Version 3.2_SA_020920). 'ttalkR' provides tools for downloading, cleaning, quality assurance and diagnostic plotting. The architecture and the procedures are described in "Toward a Unified TreeTalker Data Curation Process" Tomelleri et al. 2022 https://doi.org/10.3390/f13060855.

'ttalkR' has been developed within the context of the project ItTREEnet: "The Italian TREETALKER NETWORK: continuous large scale monitoring of tree functional traits and vulnerabilities to climate change". The project is funded by the Italian ministery for instruction, university and research - Progetti di ricerca di Rilevante Interesse Nazionale (PRIN 2017).

To install the ttalkR package (Windows users will need to install "RTools"):
```{r, eval = F}
install.packages("devtools")
devtools::install_github("EnricoTomelleri/ttalkR")
help(package=ttalkR)
citation("ttalkR")

Enrico Tomelleri (2021)
ttalkR: unified pre-processing of TreeTalker data 
R package version 1.0.0

A BibTeX entry for LaTeX users is

@Article{
AUTHOR = {Tomelleri, Enrico and Belelli Marchesini, Luca and Yaroslavtsev, Alexey and Asgharinia, Shahla and Valentini, Riccardo},
TITLE = {Toward a Unified TreeTalker Data Curation Process},
JOURNAL = {Forests},
VOLUME = {13},
YEAR = {2022},
NUMBER = {6},
ARTICLE-NUMBER = {855},
URL = {https://www.mdpi.com/1999-4907/13/6/855},
ISSN = {1999-4907},
DOI = {10.3390/f13060855}
}

```
