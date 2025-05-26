#Author: Dylan Saez

rm(list = ls())
library(geofacet)
library(scales)
library(tidyverse)
library(data.table)
library(magrittr)
library(yaml)
library(rlang)
library(ggtext)
library(tidycensus)
library(Hmisc)
#Pull from FRED
library(fredr)
library(pkgconfig)

options(scipen=999)
source("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R")

#==============================================================================
#                                                   Interest rates & Fed Policy

#Interest Rates
fedfundsrate <- fredr_series_observations(series_id = "FEDFUNDS",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), FEDFUNDSRATE = value) %>%
  select(c("date", "month", "Year", "FEDFUNDSRATE")) %>%
  mutate(
    #Quarterly Change
    FEDFUNDSRATE_quarterly_change = (FEDFUNDSRATE - lag(FEDFUNDSRATE, 3))
    / lag(FEDFUNDSRATE, 3) * 100,
    #Annual Change
    FEDFUNDSRATE_annual_change = (FEDFUNDSRATE - lag(FEDFUNDSRATE, 12))
    / lag(FEDFUNDSRATE, 12) * 100
  )

#Summary of Economic Projections
sum_ep <- fredr_series_observations(series_id = "FEDTARMD",
                                    observation_start = as.Date('1950-01-01'),
                                    observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), sum_ep = value) %>%
  select(c("date", "month", "Year", "sum_ep")) %>%
  mutate(
    #Annual
    sum_ep_annual_change = (sum_ep - lag(sum_ep, 1)) / lag(sum_ep, 1) * 100)



#                                                                     INFLATION



#PPI
inflation <- fredr_series_observations(series_id = "PPIACO",
                                       observation_start = as.Date('1950-01-01'),
                                       observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), PPI = value) %>%
  select(c("date", "month", "Year", "PPI")) %>%
  mutate(
    #Quarterly Change
    PPI_quarterly_change = (PPI - lag(PPI, 3)) / lag(PPI, 3) * 100,
    #Annual Change
    PPI_annual_change = (PPI - lag(PPI, 12)) / lag(PPI, 12) * 100
  )

#CPI all items, for all urban consumers in U.S. City Average
cpi <- fredr_series_observations(series_id = "CPIAUCSL",
                                 observation_start = as.Date('1950-01-01'),
                                 observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), CPI = value) %>%
  select(c("date", "month", "Year", "CPI")) %>%
  mutate(
    #Quarterly Change
    CPI_quarterly_change = (CPI - lag(CPI, 3)) / lag(CPI, 3) * 100,
    #Annual Change
    CPI_annual_change = (CPI - lag(CPI, 12)) / lag(CPI, 12) * 100
  )


#CPI: Core Inflation Less Food and Energy
inflation <- fredr_series_observations(series_id = "CPILFESL",
                                       observation_start = as.Date('1950-01-01'),
                                       observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), INFLATION = value) %>%
  select(c("date", "month", "Year", "INFLATION")) %>%
  mutate(
    #Quarterly Change
    infl_quarterly_change = (INFLATION - lag(INFLATION, 3)) / lag(INFLATION, 3) * 100,
    #Annual Change
    infl_annual_change = (INFLATION - lag(INFLATION, 12)) / lag(INFLATION, 12) * 100
  )









#                                                                     GDP & GNP
#GDP
nom_gdp <- fredr_series_observations(series_id = "GDP",
                                     observation_start = as.Date('1950-01-01'),
                                     observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), `GDP (bil)` = value) %>%
  select(c("date", "month", "Year", "GDP (bil)")) %>%
  mutate(
    #Quarterly Change
    gdp_quarterly_change = (`GDP (bil)` - lag(`GDP (bil)`, 1)) / lag(`GDP (bil)`, 1) * 100,
    #Annual Change
    gdp_annual_change = (`GDP (bil)` - lag(`GDP (bil)`, 4)) / lag(`GDP (bil)`, 4) * 100
  )

#Real GDP, Chained 2017

real_gdp <- fredr_series_observations(series_id = "GDPC1",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), `GDP (bil) R` = value) %>%
  select(c("date", "month", "Year", "GDP (bil) R")) %>%
  mutate(
    #Quarterly Change
    gdp_quarterly_change_R = (`GDP (bil) R` - lag(`GDP (bil) R`, 1)) / lag(`GDP (bil) R`, 1) * 100,
    #Annual Change
    gdp_annual_change_R = (`GDP (bil) R` - lag(`GDP (bil) R`, 4)) / lag(`GDP (bil) R`, 4) * 100
  )

#GNP

GNP <- fredr_series_observations(series_id = "GNP",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), `GNP` = value) %>%
  select(c("date", "month", "Year", "GNP")) %>%
  mutate(
    #Quarterly Change
    GNP_quarterly_change = (GNP - lag(GNP, 1)) / lag(GNP, 1) * 100,
    #Annual Change
    GNP_annual_change = (GNP - lag(GNP, 4)) / lag(GNP, 4) * 100
  )


#                                                                     EMPLOYMENT




#Unemployment
unemp <- fredr_series_observations(series_id = "UNRATE",
                                     observation_start = as.Date('1950-01-01'),
                                     observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), UNRATE = value) %>%
  select(c("date", "month", "Year", "UNRATE")) %>%
  mutate(
    #Quarterly Change
    UNRATE_quarterly_change = (UNRATE - lag(UNRATE, 3)) / lag(UNRATE, 3) * 100,
    #Annual Change
    UNRATE_annual_change = (UNRATE - lag(UNRATE, 12)) / lag(UNRATE, 12) * 100
  )


#Weekly Continued Claims (Insured Unemployment)
ccsa <- fredr_series_observations(series_id = "CCSA",
                                   observation_start = as.Date('1950-01-01'),
                                   observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), week = week(date), CCSA = value) %>%
  select(c("date","week", "month", "Year", "CCSA")) %>%
  mutate(
    #Quarterly Change
    CCSA_weekly_change = (CCSA - lag(CCSA, 3)) / lag(CCSA, 3) * 100,
    #Annual Change
    CCSA_weekly_change = (CCSA - lag(CCSA, 12)) / lag(CCSA, 12) * 100
  )


#Nonfarm payrolls (monthly jobs report)

nonfarm_p <- fredr_series_observations(series_id = "PAYEMS",
                                       observation_start = as.Date('1950-01-01'),
                                       observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), nonfarm_p = value) %>%
  select(c("date", "month", "Year", "nonfarm_p")) %>%
  mutate(
    #Quarterly Change
    nonfarm_p_quarterly_change = (nonfarm_p - lag(nonfarm_p, 3)) / lag(nonfarm_p, 3) * 100,
    #Annual Change
    nonfarm_p_annual_change = (nonfarm_p - lag(nonfarm_p, 12)) / lag(nonfarm_p, 12) * 100
  )

#Average Hourly Earnings
hourly <- fredr_series_observations(series_id = "CES0500000003",
                                    observation_start = as.Date('1950-01-01'),
                                    observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), hourly = value) %>%
  select(c("date", "month", "Year", "hourly")) %>%
  mutate(
    #Quarterly Change
    hourly_quarterly_change = (hourly - lag(hourly, 3)) / lag(hourly, 3) * 100,
    #Annual Change
    hourly_annual_change = (hourly - lag(hourly, 12)) / lag(hourly, 12) * 100
  )

#Job openings and Labor Turnover Survey (JOLTS)
job_open <- fredr_series_observations(series_id = "JTSJOL",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), job_open = value) %>%
  select(c("date", "month", "Year", "job_open")) %>%
  mutate(
    #Quarterly Change
    job_open_quarterly_change = (job_open - lag(job_open, 3)) / lag(job_open, 3) * 100,
    #Annual Change
    job_open_annual_change = (job_open - lag(job_open, 12)) / lag(job_open, 12) * 100
  )



#                                                 CONSUMER CONFIDENCE & SPENDING

retail <- fredr_series_observations(series_id = "MRTSSM44000USS",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), retail = value) %>%
  select(c("date", "month", "Year", "retail")) %>%
  mutate(
    #Quarterly Change
    retail_quarterly_change = (retail - lag(retail, 3)) / lag(retail, 3) * 100,
    #Annual Change
    retail_annual_change = (retail - lag(retail, 12)) / lag(retail, 12) * 100
  )


#PCE
pce <- fredr_series_observations(series_id = "PCE",
                                   observation_start = as.Date('1950-01-01'),
                                   observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), PCE = value) %>%
  select(c("date", "month", "Year", "PCE")) %>%
  mutate(
    #Quarterly Change
    PCE_quarterly_change = (PCE - lag(PCE, 3)) / lag(PCE, 3) * 100,
    #Annual Change
    PCE_annual_change = (PCE - lag(PCE, 12)) / lag(PCE, 12) * 100
  )

pce_ex_f_e <- fredr_series_observations(series_id = "DPCCRV1Q225SBEA",
                                 observation_start = as.Date('1950-01-01'),
                                 observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), PCE_ex_FE = value) %>%
  select(c("date", "month", "Year", "PCE_ex_FE"))

#University of Michigan: Consumer Sentiment

con_sent <- fredr_series_observations(series_id = "UMCSENT",
                                    observation_start = as.Date('1950-01-01'),
                                    observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), UMSCENT = value) %>%
  select(c("date", "month", "Year", "UMSCENT")) %>%
  mutate(
    #Quarterly Change
    UMSCENT_quarterly_change = (UMSCENT - lag(UMSCENT, 3)) / lag(UMSCENT, 3) * 100,
    #Annual Change
    UMSCENT_annual_change = (UMSCENT - lag(UMSCENT, 12)) / lag(UMSCENT, 12) * 100
  )


#                                                 BENCHMARK FOR INTEREST RATES
#                                                 MANUFACTURING & SERVICES ACTIVITY
#                                                 HOUSING MARKET
#                                                 CORPORATE EARNINGS
#                                                 GLOBAL EVENTS
#                                                 TRADE
#                                                 CURRENCY & COMMODITY RATES








#------------------------------------------------------------------------------






