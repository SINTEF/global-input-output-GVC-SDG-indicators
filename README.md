# #####################################################################################
# Copyright (c) 2025, SINTEF (http://www.sintef.no) 
# All rights reserved.
# This Source Code Form is subject to the terms of the 
# GNU General Public License v3.0
# #####################################################################################

# Referencing this Code
When using this code, please always cite

Wiebe et al., Linking the Sustainable Development Goals to input-output analysis: sensitivity of indicators to GMRIO coefficients for renewable electricity production. (provisionally accepted) Economic Systems Research


# Calculating the SDG indicators at country level from environmental and socio-economic extensions in global MRIO databases

The current indicator code is based on the industry classification of the OECD ICIO. Inputs into the SDG indicator calculation function in R are 

- The matrices F and S, containing the extensions (in columns) by countries and industries

- A reorganized 3-dimensional version of F, called Country_Industry_F, of dimensions countries by industries by extensions

- And two matrices with country by extension indicators, one with production-based numbers (Country_F) and one with consumption-based numbers (Country_CBA_F)

The function returns a list containing

- A 2-dimensional matrix with meta information for each SDG indicator (SDGcode = SDG indicator number; Indicatortype = Base, Proxy, Indicator; SDGname = SDG indicator name, Sourcedata = details on calculation; eaSi-system_QA = Quality Assessment; negativeVar = whether or not an increase is positive or negative; ShortDescr = Short Description, Description = Long description)

- A 2-dimensional matrix: countries by SDG indicators

# Meta data in GVCextensions_SDGindicators.xlsx

This excel contains some additional meta data information


| Sheet | Content |
| :------ | :------ |
| SDGindicator_info_table | "Table of SDG indicators as explained in Wiebe et al., Linking the Sustainable Development Goals to input-output analysis: sensitivity of indicators to GMRIO coefficients for renewable electricity production. (provisionally accepted) Economic Systems Research" |
| Extension_indicator_names_units | List of global IO extensions/GVC relevant indicators, including names used in the code, and unit of the indicators |
| SDGmetadata_output | Example of the meta data output of the code |
| ICIO_ReadME_Area_Activities | Industry and country lists taken from sheet Area_Activities of ReadMe_ICO_small.xlsx at http://oe.cd/icio


The code for converting GLORIA059 extensions to OECD ICIO Ed. 2023 country and industry classification is published on 

https://github.com/SINTEF/Gloria059ExtensionsToOECDICIOed2023