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
#                           Macro Variables

#Interest Rates
fedfundsrate <- fredr_series_observations(series_id = "FEDFUNDS",
                                      observation_start = as.Date('1950-01-01'),
                                      observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), FEDFUNDSRATE = value) %>%
  select(c("date", "month", "Year", "FEDFUNDSRATE")) %>%
  mutate(
    #Quarterly Change
    FEDFUNDSRATE_quarterly_change = (FEDFUNDSRATE - lag(FEDFUNDSRATE, 1)) / lag(FEDFUNDSRATE, 1) * 100,
    #Annual Change
    FEDFUNDSRATE_annual_change = (FEDFUNDSRATE - lag(FEDFUNDSRATE, 4)) / lag(FEDFUNDSRATE, 4) * 100
  )

#Inflation
inflation <- fredr_series_observations(series_id = "CPILFESL",
                                          observation_start = as.Date('1950-01-01'),
                                          observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), INFLATION = value) %>%
  select(c("date", "month", "Year", "INFLATION")) %>%
  mutate(
    #Quarterly Change
    infl_quarterly_change = (INFLATION - lag(INFLATION, 1)) / lag(INFLATION, 1) * 100,
    #Annual Change
    infl_annual_change = (INFLATION - lag(INFLATION, 4)) / lag(INFLATION, 4) * 100
  )

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

#Unemployment
unemp <- fredr_series_observations(series_id = "UNRATE",
                                     observation_start = as.Date('1950-01-01'),
                                     observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), UNRATE = value) %>%
  select(c("date", "month", "Year", "UNRATE")) %>%
  mutate(
    #Quarterly Change
    UNRATE_quarterly_change = (UNRATE - lag(UNRATE, 1)) / lag(UNRATE, 1) * 100,
    #Annual Change
    UNRATE_annual_change = (UNRATE - lag(UNRATE, 4)) / lag(UNRATE, 4) * 100
  )


#Weekly Continued Claims (Insured Unemployment)
ccsa <- fredr_series_observations(series_id = "CCSA",
                                   observation_start = as.Date('1950-01-01'),
                                   observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), week = week(date), CCSA = value) %>%
  select(c("date","week", "month", "Year", "CCSA")) %>%
  mutate(
    #Quarterly Change
    CCSA_weekly_change = (CCSA - lag(CCSA, 1)) / lag(CCSA, 1) * 100,
    #Annual Change
    CCSA_weekly_change = (CCSA - lag(CCSA, 4)) / lag(CCSA, 4) * 100
  )

#PCE
pce <- fredr_series_observations(series_id = "PCE",
                                   observation_start = as.Date('1950-01-01'),
                                   observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), PCE = value) %>%
  select(c("date", "month", "Year", "PCE")) %>%
  mutate(
    #Quarterly Change
    PCE_quarterly_change = (PCE - lag(PCE, 1)) / lag(PCE, 1) * 100,
    #Annual Change
    PCE_annual_change = (PCE - lag(PCE, 4)) / lag(PCE, 4) * 100
  )

pce_ex_f_e <- fredr_series_observations(series_id = "DPCCRV1Q225SBEA",
                                 observation_start = as.Date('1950-01-01'),
                                 observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), PCE_ex_FE = value) %>%
  select(c("date", "month", "Year", "PCE_ex_FE")) %>%
  mutate(
    #Quarterly Change
    PCE_ex_FE_quarterly_change = (PCE_ex_FE - lag(PCE_ex_FE, 1)) / lag(PCE_ex_FE, 1) * 100,
    #Annual Change
    PCE_ex_FE_annual_change = (PCE_ex_FE - lag(PCE_ex_FE, 4)) / lag(PCE_ex_FE, 4) * 100
  )


#CPI

#CPI all items
cpi <- fredr_series_observations(series_id = "CPIAUCSL",
                                 observation_start = as.Date('1950-01-01'),
                                 observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), CPI = value) %>%
  select(c("date", "month", "Year", "CPI")) %>%
  mutate(
    #Quarterly Change
    CPI_quarterly_change = (CPI - lag(CPI, 1)) / lag(CPI, 1) * 100,
    #Annual Change
    CPI_annual_change = (CPI - lag(CPI, 4)) / lag(CPI, 4) * 100
  )

#CPI ex Food and Energy

cpi_ex_f_e <- fredr_series_observations(series_id = "CORESTICKM159SFRBATL",
                                        observation_start = as.Date('1950-01-01'),
                                        observation_end = Sys.Date()) %>%
  mutate(month = month(date), Year = year(date), CPI_ex_FE = value) %>%
  select(c("date", "month", "Year", "CPI_ex_FE")) %>%
  mutate(
    #Quarterly Change
    CPI_ex_FE_quarterly_change = (CPI_ex_FE - lag(CPI_ex_FE, 1)) / lag(CPI_ex_FE, 1) * 100,
    #Annual Change
    CPI_ex_FE_annual_change = (CPI_ex_FE - lag(CPI_ex_FE, 4)) / lag(CPI_ex_FE, 4) * 100
  )






