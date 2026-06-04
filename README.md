# *Westside Paper:*

Analysis for Westside Paper can be found here: https://github.com/uchicago-justice-project/pre_1965/tree/main/code/analysis/05_westside_paper

These files are a self-contained step by step process to produce the analyses in the paper. 

Data for Westside Paper: 

Homicides 1940-1965:
https://uchicago.app.box.com/folder/318526334129


Chicago Fishnet: 
https://uchicago.app.box.com/folder/318525882006


Homicides Post 1965:
https://uchicago.app.box.com/file/1845519499098

Chicago Census Tract Crosswalk: See data folder and for more information ask David Hackett

Westside: 
See data folder

Housing: 
See data folder

Streets: 
Major Streets https://uchicago.app.box.com/folder/304858434772


# *Data for Other Analyses:*
Other Analyses may at this point have outdated file paths or incomplete data. The analyses were investigatory and frequently rough drafts.

Black Migration based on UIC Maps: https://uchicago.app.box.com/folder/308084262823

Redlining: https://uchicago.app.box.com/file/1800024558667

For homcide_hexagons, landsales, racial_violence please see Robert Vargas or David Hackett


# Folder Structure

The WestSide Paper Analysis has a 00 folder to set up the folders required for the analysis


├── README.md          <- The top-level README for developers using this project.
├── header             <- Loads packages

├── data

│   ├── intermediate   <- Intermediate data that has been transformed.

│   ├── mst            <- final datasets ready for analysis

│   └── raw            <- Original data 

├── code

│   ├── import         <- First import of raw data (only clean up, no transformations)

│   ├── build          <- Any structural transformations of data to be used directly for analysis

│   └── analysis       <- Any analysis of data

├── output             <- Any output from analysis
