library(tidyverse)

dir <- "./Downloads/"

prnct <- read.csv(paste0(dir, "PrctTbl.txt") , 
                 sep = ";", 
                 header = FALSE, 
                 col.names = c("cntyid", "prcnt_id", "prcnt_nme", "cong_dist", "leg_dist", "cnty_cmsh_dist",
                               "jud_dist", "swc_dist", "mcd_fips", "sch_dist_num"))


cnty <- read.csv(paste0(dir, "Cntytbl.txt") , 
                  sep = ";", 
                  header = FALSE, 
                  col.names = c("cntyid", "cnty_nme", "num_prcnts"))
                 
                 
guv <- read.csv(paste0(dir, "governorpct.txt") , 
                sep = ";", 
                header = FALSE, 
                col.names = c("state", "cntyid", "prcnt_nme", "offid", "offname", "dist", "candord", 
                              "candname", "suffix", "incmb_cd", "prtyabrv", "num_prcnt_rpt", 
                              "num_prcnt_vote", "votes", "pct_vts", "tot_vts_area")) |>
  mutate(cnty_fips = (cntyid *2)-1) |>
  left_join(cnty |>
              select(cntyid, cnty_nme)) |>
  rename(prcnt_id = prcnt_nme) |>
  left_join(prnct |>
              select(cntyid, prcnt_id, prcnt_nme, leg_dist, cong_dist))

  
guv2 <- guv |>
  filter(str_detect(candname, "Amy") | prtyabrv == "R")

state_guv <- guv2 |>
  group_by(prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(ratio = DFL/(DFL+R))

cnty_guv <- guv2 |>
  group_by(cnty_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(ratio = DFL/(DFL+R))


guv3 <- guv |>
  filter(str_detect(candname, "Amy|Kobey"))

state_dfl_guv <- guv3 |>
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  group_by(candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(ratio = Amy/(Amy+Kobey))

cnty_dfl_guv <- guv3 |>
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  group_by(cnty_nme, candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(ratio = Amy/(Amy+Kobey))


prcnt_guv <- guv2 |>
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, prtyabrv) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = prtyabrv, 
              values_from = count) |>
  mutate(ratio = DFL/(DFL+R))


prcnt_dfl_guv <- guv3 |>
  mutate(candname = if_else(str_detect(candname, "Kobey"), "Kobey", "Amy")) |>
  filter(str_detect(prcnt_nme, regex("^St. Paul W", ignore_case = TRUE))) |>
  group_by(prcnt_nme, candname) |>
  summarize(count = sum(votes)) |>
  pivot_wider(names_from = candname, 
              values_from = count) |>
  mutate(ratio = Amy/(Amy+Kobey))