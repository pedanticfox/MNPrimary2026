#Load packages
library(tidyverse)

#create pointer to directory where the files are
dir <- "./Downloads/"

#file layouts of results and linkage tables from MN SOS
#https://electionresults.sos.mn.gov/Results/MediaFileLayout/Index?erselectionId=200

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
  filter(str_detect(candname, "Amy") | prtyabrv == "R") |>
  #rename DFL to Amy since it's just her
  mutate(prtyabrv = if_else(prtyabrv =="R", prtyabrv, "Amy" ))

#create a tabulation for the state level
state_guv <- guv2 |>

  group_by(prtyabrv) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+R))

#Create county level tabulations
cnty_guv <- guv2 |>
  group_by(cnty_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
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
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+Kobey))

#examine the county level differences for votes cast between Amy and Kobey
cnty_dfl_guv <- guv3 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  group_by(cnty_nme, candname) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+Kobey))

#Precinct level analysis of Saint Paul for Amy versus republicans
prcnt_guv <- guv2 |>
  #Keep only Saint Paul Wards and precincts
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
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
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Amy/(Amy+Kobey))


#Angie Craig and Peggy Flanagan

#read in the senate primary election results
senate <- read.csv(paste0(dir, "ussenatepct.txt") , 
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


#create a dataframe that is only Amy or Kobey
senate2 <- senate |>
  filter(str_detect(candname, "Flanagan|Craig"))

#Examine state level differences between Amy and Kobey for votes cast
uscong_dfl <- senate2 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Flanagan"), "Flanagan", "Craig")) |>
  group_by(candname) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Flanagan/(Flanagan+Craig))

#examine the county level differences for votes cast between Amy and Kobey
cnty_uscong_dfl <- senate2 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Flanagan"), "Flanagan", "Craig")) |>
  group_by(cnty_nme, candname) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Flanagan/(Craig+Flanagan))

#Precinct level analysis of Saint Paul for Amy versus Kobey
prcnt_uscong_dfl <- senate2 |>
  #Simplify names for easy listing in calculations and better readability
  mutate(candname = if_else(str_detect(candname, "Flanagan"), "Flanagan", "Craig")) |>
  #Keep only Saint Paul Wards and precincts
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, candname) |>
  summarize(count = sum(votes)) |>
  ungroup() |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(prop = Flanagan/(Craig+Flanagan))


#Create notes tab

notes <- tibble(Note = c("All analyses are the proportion of votes to Amy Klobuchar or Peggy Flanagan respectively", rep(NA, 8)),
                Tab_Name = c("Gov-AmyVRep_St","Gov-AmyVRep_Cnty",
                               "Gov-AmyVRep_Prcnts", "Gov-AmyVKobey_St",
                               "Gov-AmyVKobey_Cnty","Gov-AmyVKobey_Prcnts",
                               "Sen-CraigVFlanagan_St","Sen-CraigVFlanagan_Cnty",
                               "Sen-CraigVFlanagan_Prcnts"),
                Description = c("Governor Primary 2026 - Amy votes versus all votes cast for Republications - State Level",
                                  "Governor Primary 2026 - Amy votes versus all votes cast for Republications - County Level",
                                  "Governor Primary 2026 - Amy votes versus all votes cast for Republications - Saint Paul Precinct Level",
                                  "Governor Primary 2026 - Amy votes versus votes cast for Kobey - State Level",
                                  "Governor Primary 2026 - Amy votes versus votes cast for Kobey - County Level",
                                  "Governor Primary 2026 - Amy votes versus votes cast for Kobey - Saint Paul Precinct Level",
                                  "Senate Primary 2026 - Peggy Flanagan votes versus votes cast for Angie Craig - State Level",
                                  "Senate Primary 2026 - Peggy Flanagan votes versus votes cast for Angie Craig - County Level",
                                  "Senate Primary 2026 - APeggy Flanagan votes versus votes cast for Angie Craig - Saint Paul Precinct Level"
                                  ), 
                Addtl = c(NA, "cnty_nme = County Name", "prcnt_nme = Precinct Name", NA,"cnty_nme = County Name", "prcnt_nme = Precinct Name", NA,"cnty_nme = County Name", "prcnt_nme = Precinct Name" ))

#create list of files to export
files <- lst(  "Notes" = notes,
               "Gov-AmyVRep_St" = state_guv,
               "Gov-AmyVRep_Cnty" = cnty_guv,
               "Gov-AmyVRep_Prcnts" = prcnt_guv,
               "Gov-AmyVKobey_St" = state_dfl_guv,
               "Gov-AmyVKobey_Cnty" = cnty_dfl_guv,
               "Gov-AmyVKobey_Prcnts" = prcnt_dfl_guv,
               "Sen-CraigVFlanagan_St" = uscong_dfl,
               "Sen-CraigVFlanagan_Cnty" = cnty_uscong_dfl,
               "Sen-CraigVFlanagan_Prcnts" = prcnt_uscong_dfl
               )

openxlsx::write.xlsx(files, file = paste0(dir, "Primary Analysis Output 2026.xlsx"))

imap(files, ~ data.table::fwrite(.x, file = paste0(dir, "delimited/", .y, ".csv), sep = ";", na = "" ) )