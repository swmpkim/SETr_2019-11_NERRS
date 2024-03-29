Reserve-level workflow repository
================

-   [Current file structure](#current-file-structure)
-   [Proposed workflow](#proposed-workflow)

This page and repository are for the scripts as they're modified to work at the Reserve-level. The file structure necessarily needs to be different from what I've been doing to wrangle data from multiple reserves. It needs to work on other people's computers, on their individual datasets.

Here, my efforts are to construct a standardized file directory that Reserves can use to manage and perform basic analyses on SET data into the future. The only folder being tracked here on GitHub is the `R_scripts` folder.


Proposed workflow
-----------------

Each of the folders referenced below is a subfolder of the `Reserve_Template` directory. I'll use Grand Bay as an example in this workflow; anywhere you see *gnd*, substitute in your own Reserve code.

1.  **Enter new data** into raw data spreadsheet, tentative name *gndset.xlsx*, in the `data/raw` folder. This spreadsheet will be formatted in a consistent way for all reserves. It is currently a single excel file, with a separate worksheet for each individual SET.
2.  **Run a processing script** from the `R_scripts/processing_and_qc` folder to read in the raw Excel file and generate a single csv file, tentative name *gndset\_QC.csv*, that contains data for all SETs. This file will be saved into the `data/processed` folder.
3.  **Update your metadata**, if necessary. Found in the `metadata` folder.
4.  **Run interactive QC app**. This is a Shiny application that will be run by a script in the `R_scripts` folder, and will pull data from the `data/processed` folder.
5.  **Fix/flag any problems** *in the raw data spreadsheet* ( *gndset.xlsx*, in `data/raw` folder).
6.  **Update metadata** if necessary to address any data quality issues.
7.  **Re-run processing script** from step 2, to generate an updated *gndset\_QC.csv* file in the `data/processed` folder. This will overwrite the previous file.
8.  **Submit processed data** (*gndset\_QC.csv*) and metadata to the CDMO annually for flat file hosting.
9.  **Run analysis scripts**, in `R_scripts` folder. Output will be written into the `R_output` folder.
