# #####################################################################################
# Copyright (c) 2025, SINTEF (http://www.sintef.no) 
# All rights reserved.
# This Source Code Form is subject to the terms of the 
# GNU General Public License v3.0
# Contact kirsten.wiebe@sintef.no
# #####################################################################################

#' Calculating all the SDGs, SDG proxies and SDG base indicators
#' based on the OECD ICIO Ed 2023 for 2020, extensions  
#' made by SINTEF from the WB's entreprise survey
#' and extensions from Gloria database version 059

#################################################
# SDG indicators
#################################################
# Proxy if we use the numbers as a proxy indicator
#        for the SDG
# Base  if this is the numbers we need to further
#        correlate quantitatively or qualitatively
#        to the actual SDG indicator
#
# Metadata available from https://unstats.un.org/sdgs/metadata/
#
# TODO   improve once we have more extension data

#' @param Country_F 2-dim array: countries x GVC indicators
#'                                        countries can also be regions
#' @param Country_Industry_F 3-dim array: countries x industries x GVC indicators
#' @param A Coefficient matrix
#' @param S Stressor matrix
#' @param F Extension/impact matrix
#' @param Country_CBA_F 2-dim array: countries x GVC indicators (consumption-based accounts CBA)
#'                                        countries can also be regions
#' @param regaggr region aggregation matrix
#' @param regional =TRUE if SDG calculation for regions, else = FALSE
#' @param coucodes coucodes (also in case for regions)
#' @return list(SDGnames,SDGmatrix) 2-dim array: SDG indicators x description, 2-dim array: countries x SDG indicators: scenario relative to baseline
#'
#' Use of "negativeVar" column:
#' convert negative Variables (i.e. those where an increase is negative) to positive variables
#' datatemp1 = t(t(SDGrelative2baselineByregion)*as.numeric(SDGmetabaseline[,"negativeVar"]))


CalculateSDGs_fromICIOed2023extensions2020 <-
  function(Country_F,Country_Industry_F,A,S,F,Country_CBA_F,regaggr,regional,coucodes){

    # List of all SDG indicators, with eaSi-system comments
    # https://sintef.sharepoint.com/:x:/r/teams/SIPSustainabilityimpactassessment/Delte%20dokumenter/General/Workspace/WP3%20SDG%20impacts/SDGindicatorList.xlsx?d=wac902b2fe33f46689c8ea2c38e347795&csf=1&web=1&e=8qa2rF

    # matrix of codes and names
    SDGnames <- array("NA",dim = c(1,7))
    dimnames(SDGnames)[[2]] = c("SDGcode","Indicatortype","SDGname","Sourcedata","eaSi-system_QA","negativeVar","ShortDescr")
    # "eaSi-system_QA" = eaSi-system data Quality Assessment
    # options: "OK" & "be careful with interpretation"
    # negativeVar: a decrease is positive, therefore put -1. otherwise put 1. For figures, multiply with this
    tempnames = SDGnames

    # matrix with data SDGs x countries
    SDGdata <- array(0,dim = c(1,dim(Country_F)[1]))
    dimnames(SDGdata)[[2]] = dimnames(Country_F)[[1]]


    #*******************************************************
    # 1.1.1 Proportion of the population living below the international
    # poverty line by sex, age, employment status and geographic location
    # (urban/rural)
    #*******************************************************
    # Base indicator:
    # we need value added / wages for low-skilled by male/female -> household data
    # TODO

    # Gloria059:
    # 372	Employment skill	Employment_skill_low	ppl

    SDG111 <- (Country_F[,"Employment_skill_low"])

    SDGnames[1,"SDGcode"] = "1.1.1"
    SDGnames[1,"Indicatortype"] = "Base"
    SDGnames[1,"SDGname"] = "Proportion of the population living below the international poverty line"
    SDGnames[1,"Sourcedata"] = "Gloria059 Employment_skill_low"
    SDGnames[1,"eaSi-system_QA"] = "OK"
    SDGnames[1,"negativeVar"] = -1
    SDGnames[1,"ShortDescr"] = "Low skilled employment & poverty"

    SDGdata[1,] = SDG111


    #*******************************************************
    # 2.3.1 Volume of production per labour unit by classes of
    # farming/pastoral/forestry enterprise size
    #*******************************************************
    # Base indicator:
    # output per employee (different types of employment) for agriculture

    # ICIO2023:
    # X
    # EMPN
    SDG231 <- (Country_Industry_F[,"A01_02","X"]/Country_Industry_F[,"A01_02","EMPN"])


    tempnames[1,"SDGcode"] = "2.3.1_o"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Volume of production per labour unit by classes of farming/pastoral/forestry"
    tempnames[1,"Sourcedata"] = "ICIO2023 X[A01_02], EMPN[A01_02], note: EMPN not available for all countries"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Agricultural output & employment"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG231)

    # Gloria059:
    # 368	Employment	Female	ppl
    # 369	Employment	Male	ppl

    SDG231_g <- (Country_Industry_F[,"A01_02","X"]/rowSums(Country_Industry_F[,"A01_02",c("Female","Male")]))
    tempnames[1,"SDGcode"] = "2.3.1_g"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Volume of production per labour unit by classes of farming/pastoral/forestry"
    tempnames[1,"Sourcedata"] = "ICIO2023 X[A01_02], Gloria059 sum(Female, Male)[A01_02]"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Agricultural output & employment"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG231_g)

    #*******************************************************
    # 2.3.2 Average income of small-scale food producers, by
    # sex and indigenous status
    #*******************************************************
    # need household survey data
    # TODO

    #*******************************************************
    # 2.4.1 Proportion of agricultural area under productive
    # and sustainable agriculture
    #*******************************************************
    # need additional data, not only on land-use, which we could get, but also if that's protected
    # can be Base: interpretation if landuse increases, the proportion of area
    # under protection reduces if no other action is taken

    # Gloria059
    # 373	Land use	Annual_crops	1000 ha
    # 374	Land use	Permanent_crops	1000 ha
    # 375	Land use	Pastures	1000 ha


    SDG241 <- (Country_F[,"LU_Annual_crops"] +
               Country_F[,"LU_Permanent_crops"] +
               Country_F[,"LU_Pastures"])

    tempnames[1,"SDGcode"] = "2.4.1"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Proportion of agricultural area under productive and sustainable agriculture"
    tempnames[1,"Sourcedata"] = "Gloria059 sum(Annual_crops,Permanent_crops,Pastures)"
    tempnames[1,"eaSi-system_QA"] = "we advise not to use"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Land used for crops and pastures"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG241)


    #*******************************************************
    # 3.4.1 Mortality rate attributed to cardiovascular disease,
    # cancer, diabetes or chronic respiratory disease
    #*******************************************************
    # Proxy indicator for chronic respiratory disease
    # local emissions

    # Gloria059:
    # 2877	Emissions (EDGAR)	'pm25_bio_total_EDGAR_consistent'	kilotonnes
    # 2950	Emissions (EDGAR)	'pm25_fossil_total_EDGAR_consistent'	kilotonnes
    selind = c("pm25_bio_total_EDGAR_consistent","pm25_fossil_total_EDGAR_consistent")
    SDG341_ge <- rowSums(Country_F[,selind])

    tempnames[1,"SDGcode"] = "3.4.1_ge"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Mortality rate attributed to [...] or chronic respiratory disease"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 sum(",selind[1],", ",selind[2],")")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Local emissions & chronic \nrespiratory disease"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG341_ge)

    # Gloria059:
    # 5652	Emissions (OECD)	'pm25_bio_total_OECD_consistent'	kilotonnes
    # 5725	Emissions (OECD)	'pm25_fossil_total_OECD_consistent'	kilotonnes
    selind = c("pm25_bio_total_OECD_consistent","pm25_fossil_total_OECD_consistent")
    SDG341_go <- rowSums(Country_F[,selind])

    tempnames[1,"SDGcode"] = "3.4.1_go"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Mortality rate attributed to [...] or chronic respiratory disease"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 sum(",selind[1],", ",selind[2],")")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Local emissions & chronic \nrespiratory disease"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG341_go)


    #*******************************************************
    # 4.3.1 Participation rate of youth and adults in formal
    # and non-formal education and training in the previous
    # 12 months, by sex
    #*******************************************************
    # Proxy indicator: (medium and) high skilled need training

    # Gloria059:
    # 373	Employment skill	Employment_skill_medium	ppl
    # 374	Employment skill	Employment_skill_high	ppl

    SDG431 <- (Country_F[,"Employment_skill_high"])

    tempnames[1,"SDGcode"] = "4.3.1"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Participation rate of youth and adults in [...] training"
    tempnames[1,"Sourcedata"] = "Gloria059 Employment_skill_high"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Demand for high skilled & \neducation/training"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG431)

    #*******************************************************
    # 5.4.1 Proportion of time spent on unpaid domestic and care work, by sex, age and location
    #*******************************************************
    # household survey or time use survey data
    # TODO

    #*******************************************************
    # 5.5.2 Proportion of women in managerial positions
    #*******************************************************
    # need data on employment by gender and skill
    # Base indicator: female employment, maybe interpret together with 4.3.1 high skilled

    # WB enterprisesurvey, more info see 16.5.2

    SDG552 <- (Country_F[,"WomanManager"])

    tempnames[1,"SDGcode"] = "5.5.2"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Proportion of women in managerial positions"
    tempnames[1,"Sourcedata"] = "WB enterprisesurvey Percent of firms with a woman top manager"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Women in managerial positions"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG552)


    #*******************************************************
    # 6.4.1 Change in water-use efficiency over time
    #*******************************************************
    # could be Proxy X["Water provision industry"] / X[total]
    # but E not only water, but "Water supply; sewerage, waste management and remediation activities"
    # TODO

    #*******************************************************
    # 6.4.2 Level of water stress: freshwater withdrawal as
    # a proportion of available freshwater resources
    #*******************************************************
    # can be Proxy using Gloria extensions, but questionable data quality
    #
    # Gloria059
    # 391	Water_stress	Agriculture water stress	million m3 H2Oeq
    # 392	Water_stress	Non-agriculture water stress	million m3 H2Oeq

    SDG642 <- (Country_F[,"Agriculture water stress"] +
                 Country_F[,"Non-agriculture water stress"])

    tempnames[1,"SDGcode"] = "6.4.2"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Level of water stress: freshwater withdrawal as a proportion of available freshwater resources"
    tempnames[1,"Sourcedata"] = "Gloria059 sum(Agriculture water stress, Non-agriculture water stress)"
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Water stress"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG642)

    #*******************************************************
    # 7.1.1 Proportion of population with access to electricity
    # 7.1.2 Proportion of population with primary reliance on clean fuels and technology
    # 7.2.1 Renewable energy share in the total final energy consumption
    # 7.b.1 Installed renewable energy-generating capacity in developing countries (in watts per capita)
    #*******************************************************
    # story lines in scenarios


    #*******************************************************
    # 7.3.1 Energy intensity measured in terms of primary energy and GDP
    #*******************************************************
    # Proxy indicator using OECD VA for energy extracting industry
    # B05_06	Mining and quarrying, energy producing products


    SDG731_o <- (Country_Industry_F[,"B05_06","VA"]/Country_F[,"VA"])

    tempnames[1,"SDGcode"] = "7.3.1_o"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Energy intensity measured in terms of primary energy and GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for B05_06, total VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Value added in energy \nmining as share of total"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG731_o)


    # Proxy indicator using gloria
    # 379	Energy	Coal and peat	TJ
    # 380	Energy	Oil and natural gas	TJ
    # 381	Energy	Nuclear	TJ
    # 382	Energy	Solid biofuels	TJ


    SDG731_g <- (Country_F[,"Coal and peat"] +
                   Country_F[,"Oil and natural gas"] +
                   Country_F[,"Nuclear"]+
                   Country_F[,"Solid biofuels"]) /Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "7.3.1_g"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Energy intensity measured in terms of primary energy and GDP"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 sum(Coal and peat,Oil and natural gas,Nuclear,Solid biofuels), ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Fossil energy extraction \nper value added"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG731_g)



    #*******************************************************
    # 8.1.1 Annual growth rate of real GDP per capita
    #*******************************************************
    # Indicator:
    # total GDP, but not per capita. But if we assume population is the same in
    # Baseline and scenario it is taken out when taking the ratio between scenarios

    # ICIO2023:
    # VA
    SDG811 <- Country_F[,"VA"]


    tempnames[1,"SDGcode"] = "8.1.1"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Annual growth rate of real GDP per capita"
    tempnames[1,"Sourcedata"] = "ICIO2023 VA"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "GDP growth per capita"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG811)



    #*******************************************************
    # 8.2.1 Annual growth rate of real GDP per employed person
    #*******************************************************
    # Indicator:
    # output per employee (different types of employment) for agriculture

    # ICIO2023:
    # VA
    # EMPN
    SDG821_o <- Country_F[,"VA"]/Country_F[,"EMPN"]


    tempnames[1,"SDGcode"] = "8.2.1_o"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Annual growth rate of real GDP per employed person"
    tempnames[1,"Sourcedata"] = "ICIO2023 VA, EMPN, note: EMPN not available for all countries"
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "GDP growth per \nemployed person"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG821_o)


    # ICIO2023:
    # VA
    # EMPN
    SDG821_og <- Country_F[,"VA"]/(Country_F[,"Female"]+Country_F[,"Male"])


    tempnames[1,"SDGcode"] = "8.2.1_og"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Annual growth rate of real GDP per employed person"
    tempnames[1,"Sourcedata"] = "ICIO2023 VA, Gloria059 Employment sum(Female,Male)"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "GDP growth per \nemployed person"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG821_og)


    #*******************************************************
    # 8.3.1 Proportion of informal employment in total
    # employment, by sector and sex
    #*******************************************************
    # possible once data on informal employment by gender is available
    # TODO



    #*******************************************************
    # 8.4.1 Material footprint, material footprint per
    # capita, and material footprint per GDP
    #*******************************************************
    # consumption-based indicators need to be calculated

    GloriaMaterialIndicators <- GloriaMaterialIndicators_function()
    selectS = as.matrix(rowSums(S[,GloriaMaterialIndicators]))
    dimnames(selectS)[[2]] = "Material"
    #materialCBA = GVCtools_consumptionbasedindicators(L,Y,selectS,coucodes)
    materialCBA = rowSums(Country_CBA_F[,GloriaMaterialIndicators])
    
    if(regional==FALSE){
      SDG841a <- t(materialCBA)
    }else{
      SDG841a <- t(materialCBA) %*% regaggr
    }

    tempnames[1,"SDGcode"] = "8.4.1a"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Material footprint"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Material Indicators, CBA calculation with ICIO2023")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Material footprint"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG841a)


    if(regional==FALSE){
      SDG841b <- t(materialCBA)/Country_F[,"VA"]
    }else{
      SDG841b <- (t(materialCBA) %*% regaggr)/Country_F[,"VA"]
    }

    tempnames[1,"SDGcode"] = "8.4.1b"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Material footprint per GDP"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Material Indicators, CBA calculation with ICIO2023, ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Material footprint \nper GDP"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG841b)

    #*******************************************************
    # 8.4.2 Domestic material consumption, domestic material
    # consumption per capita, and domestic material
    # consumption per GDP
    #*******************************************************
    # Proxy indicator using OECD VA for mining industries
    # B05_06	Mining and quarrying, energy producing products
    # B07_08	Mining and quarrying, non-energy producing products


    SDG842_ao <- (Country_Industry_F[,"B05_06","VA"]+
                   Country_Industry_F[,"B05_06","VA"])

    tempnames[1,"SDGcode"] = "8.4.2a_o"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Domestic material consumption (and per capita), [...]"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for B05_06 & B07_08")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Domestic mining activity \n& material consumption"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG842_ao)



    SDG842_bo <- (Country_Industry_F[,"B05_06","VA"]+
                   Country_Industry_F[,"B05_06","VA"])/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "8.4.2b_o"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "[...], domestic material consumption per GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for B05_06 & B07_08, total VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Domestic mining activity \nas share of GDP"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG842_bo)


    # Proxy indicator using Gloria Materials indicators
    GloriaMaterialIndicators <- GloriaMaterialIndicators_function()


    SDG842a_g <- rowSums(Country_F[,GloriaMaterialIndicators])

    tempnames[1,"SDGcode"] = "8.4.2a_g"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Domestic material consumption (and per capita), [...]"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Materials 329-367 (abiotic materials)")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Domestic abiotic material \nextraction & material \nconsumption"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG842a_g)

    SDG842b_g <- rowSums(Country_F[,GloriaMaterialIndicators])/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "8.4.2b_g"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "[...], domestic material consumption per GDP"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Materials 329-367 (abiotic materials),ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Domestic abiotic material \nextraction per GDP"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG842b_g)




    #*******************************************************
    # 8.5.1 Average hourly earnings of employees, by sex,
    # age, occupation and persons with disabilities
    #*******************************************************
    # Proxy possible once more employment data is available
    # TODO



    #*******************************************************
    # 9.2.1 Manufacturing value added as a proportion of GDP and per capita
    #*******************************************************
    # Indicator
    # OECD VA data Manufacturing industries C
    selind = c("C10T12","C13T15","C16","C17_18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31T33")

    SDG921 <- rowSums(Country_Industry_F[,selind,"VA"])/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "9.2.1"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Manufacturing value added as a proportion of GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for all C, total VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Manufacturing value added \nas a proportion of GDP"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG921)



    #*******************************************************
    # 9.2.2 Manufacturing employment as a proportion of total employment
    #*******************************************************
    # Indicator
    # OECD VA data Manufacturing industries C
    selind = c("C10T12","C13T15","C16","C17_18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","C31T33")

    SDG922_o <- rowSums(Country_Industry_F[,selind,"EMPN"])/Country_F[,"EMPN"]

    tempnames[1,"SDGcode"] = "9.2.2_o"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Manufacturing employment as a proportion of total employment"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 EMPN for all C, total EMPN")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Manufacturing employment \nas a proportion of \ntotal employment"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG922_o)

    # Gloria059 female + male data Manufacturing industries C
    SDG922_g <- (rowSums(Country_Industry_F[,selind,"Female"])+rowSums(Country_Industry_F[,selind,"Male"]))/
      (Country_F[,"Female"]+Country_F[,"Male"])

    tempnames[1,"SDGcode"] = "9.2.2_g"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Manufacturing employment as a proportion of total employment"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 female+male for all C, total female+male")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Manufacturing employment \nas a proportion of \ntotal employment"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG922_g)


    #*******************************************************
    # 9.4.1 CO2 emission per unit of value added per country
    #*******************************************************
    # Proxy indicator using OECD PROD_GHG

    SDG941_o <- (Country_F[,"PROD_GHG"]/Country_F[,"VA"])

    tempnames[1,"SDGcode"] = "9.4.1_o"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "CO2 emission per unit of value added per country"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 PROD_GHG, VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "GHG emissions \nper unit of value added"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG941_o)


    # Indicator using Gloria059

    SDG941_go <- (Country_F[,"co2_excl_short_cycle_org_c_total_OECD_consistent"]/Country_F[,"VA"])

    tempnames[1,"SDGcode"] = "9.4.1_go"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "CO2 emission per unit of value added per country"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 'co2_excl_short_cycle_org_c_total_OECD_consistent', ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "CO2 emissions \nper unit of value added"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG941_go)

    SDG941_ge <- (Country_F[,"co2_excl_short_cycle_org_c_total_EDGAR_consistent"]/Country_F[,"VA"])

    tempnames[1,"SDGcode"] = "9.4.1_ge"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "CO2 emission per unit of value added per country"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 'co2_excl_short_cycle_org_c_total_EDGAR_consistent', ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "CO2 emissions \nper unit of value added"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG941_ge)

    #*******************************************************
    # 9.5.1 Research and development expenditure as a proportion of GDP
    #*******************************************************
    # Proxy OECD VA M Professional, scientific and technical activities

    SDG951 <- (Country_Industry_F[,"M","VA"])/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "9.5.1"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Research and development expenditure as a proportion of GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for M Professional, scientific and technical activities, total VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Professional, scientific \nand technical activities \n& R&D expenditures"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG951)


    #*******************************************************
    # 9.5.2 Researchers (in full-time equivalent) per million inhabitants
    #*******************************************************
    # Base indicator:
    # Gloria059 Employment_skill_high

    SDG952 <- Country_Industry_F[,"M","Employment_skill_high"]#(Country_F[,"Employment_skill_high"])

    tempnames[1,"SDGcode"] = "9.5.2"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Researchers (in full-time equivalent) per million inhabitants"
    tempnames[1,"Sourcedata"] = "Gloria059 industry M Employment_skill_high"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "High-skilled employment \nin Professional, scientific \nand technical activities"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG952)


    #*******************************************************
    # 9.b.1 Proportion of medium and high-tech industry value added in total value added
    #*******************************************************
    # Indicator
    # OECD VA data on medium and high-tech industries
    # https://unstats.un.org/sdgs/metadata/files/Metadata-09-0B-01.pdf
    # leaving out 252 weapons and ammunition, and 325 medical and dental instruments and supplies
    # including ships and boats in C30
    selind = c("C20","C21","C26","C27","C28","C29","C30")

    SDG9b1 <- rowSums(Country_Industry_F[,selind,"VA"])/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "9.b.1"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Proportion of medium and high-tech industry value added in total value added"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 VA for medium and high-tech, total VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Value added in medium- \nand high-tech industries \nas share of total"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG9b1)



    #*******************************************************
    # 10.1.1 Growth rates of household expenditure or income
    # per capita among the bottom 40 per cent of the population
    # and the total population
    #*******************************************************
    # household survey data
    # TODO


    #*******************************************************
    # 10.4.1 Labour share of GDP
    #*******************************************************
    # Indictor: OECD LABR and VA

    SDG1041 <- Country_F[,"LABR"]/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "10.4.1"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Labour share of GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 LABR and VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Labour compensation \nin total value added"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1041)


    #*******************************************************
    # 11.6.2 Annual mean levels of fine particulate matter
    # (e.g. PM2.5 and PM10) in cities (population weighted)
    #*******************************************************
    # Proxy as we only have country, but not city data
    # Gloria059
    # 'pm25_bio_total_EDGAR_consistent'
    # 'pm25_fossil_total_EDGAR_consistent'
    # 'pm10_total_EDGAR_consistent'

    SDG1162_ge <- (Country_F[,"pm25_bio_total_EDGAR_consistent"] +
                   Country_F[,"pm25_fossil_total_EDGAR_consistent"] +
                   Country_F[,"pm10_total_EDGAR_consistent"])

    tempnames[1,"SDGcode"] = "11.6.2_ge"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Annual mean levels of fine particulate matter (e.g. PM2.5 and PM10) in cities (population weighted)"
    tempnames[1,"Sourcedata"] = "Gloria059 sum('pm25_bio_total_EDGAR_consistent','pm25_fossil_total_EDGAR_consistent','pm10_total_EDGAR_consistent')"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "PM2.5 and PM10 \nemissions at country level"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1162_ge)


    # Gloria059
    # 'pm25_bio_total_OECD_consistent'
    # 'pm25_fossil_total_OECD_consistent'
    # 'pm10_total_OECD_consistent'


    SDG1162_go <- (Country_F[,"pm25_bio_total_OECD_consistent"] +
                     Country_F[,"pm25_fossil_total_OECD_consistent"] +
                     Country_F[,"pm10_total_OECD_consistent"])

    tempnames[1,"SDGcode"] = "11.6.2_go"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Annual mean levels of fine particulate matter (e.g. PM2.5 and PM10) in cities (population weighted)"
    tempnames[1,"Sourcedata"] = "Gloria059 sum('pm25_bio_total_OECD_consistent','pm25_fossil_total_OECD_consistent','pm10_total_OECD_consistent')"
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "PM2.5 and PM10 \nemissions at country level"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1162_go)



    #*******************************************************
    #* 12.2.1 Material footprint, material footprint per capita,
    #* and material footprint per GDP
    #*******************************************************
    # SDG1221 = SDG841

    rowindex = which(SDGnames[,"SDGcode"]=="8.4.1a")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.1a"
    SDG1221a = SDG841a
    SDGdata = rbind(SDGdata,SDG1221a)
    #
    rowindex = which(SDGnames[,"SDGcode"]=="8.4.1b")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.1b"
    SDG1221b = SDG841b
    SDGdata = rbind(SDGdata,SDG1221b)

    #*******************************************************
    #* 12.2.2 Domestic material consumption, domestic material
    #* consumption per capita, and domestic material consumption per GDP
    #*******************************************************

    rowindex = which(SDGnames[,"SDGcode"]=="8.4.2a_o")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.2a_o"
    SDG1222a_o = SDG842_ao
    SDGdata = rbind(SDGdata,SDG1222a_o)
    #
    rowindex = which(SDGnames[,"SDGcode"]=="8.4.2b_o")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.2b_o"
    SDG1222b_o = SDG842_bo
    SDGdata = rbind(SDGdata,SDG1222b_o)
    #
    rowindex = which(SDGnames[,"SDGcode"]=="8.4.2a_g")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.2a_g"
    SDG1222a_g = SDG842a_g
    SDGdata = rbind(SDGdata,SDG1222a_g)
    #
    rowindex = which(SDGnames[,"SDGcode"]=="8.4.2b_g")
    SDGnames = rbind(SDGnames,SDGnames[rowindex,])
    SDGnames[dim(SDGnames)[1],"SDGcode"]="12.2.2b_g"
    SDG1222b_g = SDG842b_g
    SDGdata = rbind(SDGdata,SDG1222b_g)

    #*******************************************************
    # 13.2.2 Total greenhouse gas emissions per year
    #*******************************************************
    # Indicator using

    # OECD PROD_GHG

    SDG1322_o <- Country_F[,"PROD_GHG"]

    tempnames[1,"SDGcode"] = "13.2.2_o"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Total greenhouse gas emissions per year"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 PROD_GHG")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Total GHG emissions"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1322_o)


    # Indicator using Gloria059

    SDG1322_go <- (Country_F[,"GHG_total_OECD_consistent"])

    tempnames[1,"SDGcode"] = "13.2.2_go"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Total greenhouse gas emissions per year"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 'GHG_total_OECD_consistent'")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Total GHG emissions"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1322_go)

    SDG1322_ge <- (Country_F[,"GHG_total_EDGAR_consistent"])

    tempnames[1,"SDGcode"] = "13.2.2_ge"
    tempnames[1,"Indicatortype"] = "Indicator"
    tempnames[1,"SDGname"] = "Total greenhouse gas emissions per year"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 'GHG_total_EDGAR_consistent', ICIO2023 VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Total GHG emissions"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1322_ge)


    #*******************************************************
    # 14.3.1 Average marine acidity (pH) measured at agreed
    # suite of representative sampling stations
    #*******************************************************
    # Base total GHG

    #*******************************************************
    # 14.4.1 Proportion of fish stocks within biologically sustainable levels
    #*******************************************************
    #* Base
    #* Gloria059 Wild fish catch

    SDG11441 <- (Country_F[,"Wild fish catch"])

    tempnames[1,"SDGcode"] = "14.4.1"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Proportion of fish stocks within biologically sustainable levels"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Materials Wild fish catch")
    tempnames[1,"eaSi-system_QA"] = "we advise not to use"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Wild fish catch & \nsustainable fish stocks"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG11441)

    #*******************************************************
    # 15.5.1 Red List Index
    #*******************************************************
    #*Proxy using biodiversity Potentially Disappeared Fraction

    selindic = c("BD_Annual_crops","BD_Permanent_crops","BD_Pastures",
                 "BD_Intensive_forestry","BD_Extensive_forestry","BD_Urban")
    SDG1551 <- rowSums(Country_F[,selindic])

    tempnames[1,"SDGcode"] = "15.5.1"
    tempnames[1,"Indicatortype"] = "Base"
    tempnames[1,"SDGname"] = "Red List Index"
    tempnames[1,"Sourcedata"] = paste0("Gloria059 Biodiversity_loss Potentially Disappeared Fraction")
    tempnames[1,"eaSi-system_QA"] = "we advise not to use"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Biodiversity Potentially \nDisappeared Fraction \n& Red List Index"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1551)

    #*******************************************************
    #* 16.5.2 Proportion of businesses that had at least one
    #* contact with a public official and that paid a bribe
    #* to a public official, or were asked for a bribe by
    #* those public officials during the previous 12 months
    #*******************************************************
    #print("16.5.2 WB enterprisesurveys Bribery incidence")
    # https://www.enterprisesurveys.org/en/enterprisesurveys
    # https://www.enterprisesurveys.org/en/custom-query
    # 1. Choose Economies: All
    # 2. Choose Topics: Corruption (All)
    # 3. Choose Subgroups: Sector
    # Indicator: Bribery incidence (percent of firms experiencing at least one bribe payment request)

    SDG1652 <- Country_F[,"Bribery"]/Country_F[,"X"]

    tempnames[1,"SDGcode"] = "16.5.2"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Proportion of businesses receiving at least on bribe request"
    tempnames[1,"Sourcedata"] = paste0("WB enterprisesurvey Bribery incidence (percent of firms experiencing at least one bribe payment request), ICIO2023 X")
    tempnames[1,"eaSi-system_QA"] = "be careful with interpretation"
    tempnames[1,"negativeVar"] = -1
    tempnames[1,"ShortDescr"] = "Percent of firms experiencing \nat least one bribe payment request"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1652)



    #*******************************************************
    # 17.1.1 Total government revenue as a proportion of GDP, by source
    #*******************************************************
    # Proxy: OECD TLS and VA

    SDG1711 <- Country_F[,"TLS"]/Country_F[,"VA"]

    tempnames[1,"SDGcode"] = "17.1.1"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Total government revenue as a proportion of GDP"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 TLS (on products) and VA")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Taxes less subsidies on \nproducts as share of GDP"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG1711)

    #*******************************************************
    # 17.11.1 Developing countries and least developed
    # countries share of global exports
    #*******************************************************

    # TODO
    SDG17111 = array(0,dim=c(1,length(coucodes)))
    dimnames(SDG17111)[[2]]=coucodes
    Z = A %*% diag(F[,"X"])
    dimnames(Z)[[2]] = dimnames(Z)[[1]]
    for(c in coucodes){
      for(impc in coucodes){
        if(impc != c){
          SDG17111[,c] = SDG17111[,c] + sum(Z[paste0(c,"_",indcodes),paste0(impc,"_",indcodes)])
        }
      }
    }
    if(regional){
      print("SDG17.11.1 is not export by region, but sum over all countries' export in the region")
      SDG17111 = SDG17111 %*% regaggr
    }

    tempnames[1,"SDGcode"] = "17.11.1"
    tempnames[1,"Indicatortype"] = "Proxy"
    tempnames[1,"SDGname"] = "Developing countries and least developed countries share of global exports"
    tempnames[1,"Sourcedata"] = paste0("ICIO2023 Intermediate exports calculated")
    tempnames[1,"eaSi-system_QA"] = "OK"
    tempnames[1,"negativeVar"] = 1
    tempnames[1,"ShortDescr"] = "Intermediate exports \nby country"
    SDGnames = rbind(SDGnames,tempnames)
    SDGdata = rbind(SDGdata, SDG17111)

    Description <- SDGnames[,"ShortDescr"]
    Description <- sub('\n','',Description)
    Description <- sub('\n','',Description) # needs to be done twice, as some have it twice

    SDGnames <- cbind(SDGnames,Description)

    #############################################
    # return result data
    dimnames(SDGdata)[[1]] = SDGnames[,"SDGcode"]
    SDGmatrix = t(SDGdata)
    return(list(SDGnames,SDGmatrix))
    #############################################

  }# end GVCanalysis_calculateSDGs_fromICIOed2023extensions2020
