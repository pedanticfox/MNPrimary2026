#Load packages
library(tidyverse)

#create pointer to directory where the files are
dir <- "./Downloads/"

#read in the precinct table
prnct <- read.csv(paste0(dir, "PrctTbl.txt") , 
                 sep = ";", 
                 header = FALSE, 
                 col.names = c("cntyid", "prcnt_id", "prcnt_nme", "cong_dist", "leg_dist", "cnty_cmsh_dist",
                               "jud_dist", "swc_dist", "mcd_fips", "sch_dist_num"))

#read in the county table
cnty <- read.csv(paste0(dir, "Cntytbl.txt") , 
                  sep = ";", 
                  header = FALSE, 
                  col.names = c("cntyid", "cnty_nme", "num_prcnts"))
                 
#read in the governor primary election results
guv <- read.csv(paste0(dir, "governorpct.txt") , 
                sep = ";", 
                header = FALSE, 
                col.names = c("state", "cntyid", "prcnt_nme", "offid", "offname", "dist", "candord", 
                              "candname", "suffix", "incmb_cd", "prtyabrv", "num_prcnt_rpt", 
                              "num_prcnt_vote", "votes", "pct_vts", "tot_vts_area")) |>
  #create fips in case I decide to map stuff
  mutate(cnty_fips = (cntyid *2)-1) |>
  #join in the county information
  left_join(cnty |>
              select(cntyid, cnty_nme)) |>
  #align the column names for precinct id to make linkage automatic
  rename(prcnt_id = prcnt_nme) |>
  #link the precinct information
  left_join(prnct |>
              select(cntyid, prcnt_id, prcnt_nme, leg_dist, cong_dist))

#create a dataframe that is only has Amy and any Republican
guv2 <- guv |>
  filter(str_detect(candname, "Amy") | prtyabrv == "R")

#create a tabulation for the state level
state_guv <- guv2 |>
  #rename DFL to Amy since it's just her
  mutate(prty_abrv = if_else(prtyabrv  == "R", prtyabrv, "Amy")) |>
  group_by(prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+R))

#Create county level tabulations
cnty_guv <- guv2 |>
  #rename DFL to Amy since it's just her
  mutate(prty_abrv = if_else(prtyabrv  == "R", prtyabrv, "Amy")) |>
  group_by(cnty_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+R))

#create a dataframe that is only Amy or Kobey
guv3 <- guv |>
  filter(str_detect(candname, "Amy|Kobey"))

#Examine state level differences between Amy and Kobey for votes cast
state_dfl_guv <- guv3 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  group_by(candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+Kobey))

#examine the county level differences for votes cast between Amy and Kobey
cnty_dfl_guv <- guv3 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  group_by(cnty_nme, candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+Kobey))

#Precinct level analysis of Saint Paul for Amy versus republicans
prcnt_guv <- guv2 |>
  #rename DFL to Amy since it's just her
  mutate(prty_abrv = if_else(prtyabrv  == "R", prtyabrv, "Amy")) |>
  #Keep only Saint Paul Wards and precincts
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+R))

#Precinct level analysis of Saint Paul for Amy versus Kobey
prcnt_dfl_guv <- guv3 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  #Keep only Saint Paul Wards and precincts
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop) = Amy/(Amy+Kobey))

#Still need to export analyses