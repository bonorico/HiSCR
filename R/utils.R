# For extra info on HiSCR please see https://pubmed.ncbi.nlm.nih.gov/26201313/
# 

library(tidyverse)
library(shiny)
library(epitools)

data <- read.csv("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-04-13/HiSCR_dat.csv")

data %>% head()
data %>% str()

# Endpoint function: RR at week 16
RR <- function(.data, TRT, HiSCR, 
               sm = c("RR", "OR"), 
               method_RR = c("wald", "small", "boot"),
               method_OR = c("midp", "fisher", "wald", "small"))
{
  sm <- match.arg(sm)
  method_RR <- match.arg(method_RR)
  method_OR <- match.arg(method_OR)
  
  thatway <- list(RR = function(x) x,
                  OR = function(x) desc(x))[[sm]]
  
  tab <- .data %>% 
    group_by({{TRT}}, {{HiSCR}}) %>% 
    summarise(n = n()) %>% 
    arrange(thatway({{TRT}})) %>% 
    pull(n) %>% 
    matrix(nrow = 2) %>% 
    t()
  
  dimnames(tab) <- list("Treatment" = rev(unique(.data %>% select({{TRT}}) %>% pull() )),
                        "Outcome" = unique(.data %>% select({{HiSCR}}) %>% pull()))
  
  switch(sm,
         RR = riskratio(tab, method = method_RR),
         OR = oddsratio(tab, method = method_OR)
  )
  
  
}

#RR(data, TRT, HiSCR, sm = "OR")

#' Computes AN count
#' @param .data - data.frame as given by Github repo above
#' @param abscesses - character - label for variable number of abscesses
#' @param infl_nodes - character - label for variable number of inflammatory nodules
AN_count <- function(.data, abscesses, infl_nodes)
{
  .data[[abscesses]] + .data[[infl_nodes]]
}
#
#' Measure fraction change from baseline
#' @param .data - data.frame as given by Github repo above
#' @param baseline - numeric vector
#' @param week16 - numeric vector 
change <- function(.data, baseline, week16, method = c("fraction", "absolute"))
{
  method <- match.arg(method)
  
  switch(method,
         fraction = {
           if (any(.data %>% select({{baseline}}) %>% pull() <= 0) )
           {
             warning("Some baseline values are negative: setting them to NA")
             .data %>% 
               mutate("{{baseline}}":= if_else({{baseline}} <= 0, 
                                               NA_integer_, 
                                               {{baseline}} ))
           }
             
           .data %>% 
             mutate(change = ({{week16}} - {{baseline}})/{{baseline}}) %>% 
             pull(change)
           
         },
         absolute = {
           .data %>% 
             mutate(change = ({{week16}} - {{baseline}})) %>% 
             pull(change)
         }
  )
  
}

#' Recalculate HiSCR based on new definitions
#' @param data - data.frame as given by Github repo above
#' @param AN_incr - scalar numeric - Negative value for fraction change in AN counts relative to baseline
#' @param fist_incr - scalar numeric - Value for absolute change in number of draining fistula relative to baseline
#' @param abscesses_incr - scalar numeric - Value for absolute change in number of abscesses relative to baseline
#' @param checkupvs - character - label of original HiSCR variable to check up new results to
#' @returns newly computed HiSCR and TRT
HiSCR <- function(.data, AN_incr = -0.5, fist_incr = 0, abscesses_incr = 0, checkupvs = "HiSCR")
{
  if (AN_incr > 0)
    stop("AN_increase (AN count increase) must be negative")
  # if (fist_incr > 0 | abscesses_incr > 0)
  #   stop("fist_incr and abscesses_incr (increase in number of draining fistulae or abscesses) must be negative")
  # the other tresholds can be integer positive and negative
  
  .data$AN_count.base <- .data %>% AN_count("abscesses.base", "infl.nod.base")
  .data$AN_count.w16 <- .data %>% AN_count("abscesses.w16", "infl.nod.w16")
  .data$AN_change <- .data %>% change(AN_count.base, AN_count.w16, method = "fraction")
  .data$fist_change <- .data %>% change(drain.fist.base, drain.fist.w16, method = "absolute")
  .data$abscess_change <- .data %>% change(abscesses.base, abscesses.w16, method = "absolute")
  
  .data %>% mutate(newHiSCR = if_else(
    AN_change <= AN_incr & fist_change <= fist_incr & abscess_change <= abscesses_incr,
    "Yes", "No")  
    ) %>% 
    # { 
    #   browser()
    #   if (any(.data[[checkupvs]] != .data[["newHiSCR"]]))
    #     message("new HiSCR values have changed from reference ones")
    #   .
    # } %>% 
    select(TRT, newHiSCR, HiSCR)
}

newdat <- HiSCR(data)

RR(newdat, TRT, HiSCR, sm = "OR")
RR(newdat, TRT, newHiSCR, sm = "OR")

#'Returns data frame with treatment effect estimates for varying HiSCR definitions
#' @description builds data by internally calling function HiSCR above
#' @param .data
varying_def_data <- function(.data, 
                             AN_incr_range = -seq(0.25, 0.75, 0.05),
                             fist_incr_range = 0,
                             abscesses_incr = 0,
                             sm = c("RR", "OR"), 
                             method_RR = c("wald", "small", "boot"),
                             method_OR = c("midp", "fisher", "wald", "small")
                             
                             
                             
                             )
{
  
  sm <- match.arg(sm)
  method_RR <- match.arg(method_RR)
  method_OR <- match.arg(method_OR)
  
  
  do.call("rbind",
          lapply(AN_incr_range, function(i)
            do.call("rbind",
                    lapply(fist_incr_range, function(j)
                      do.call("rbind",
                              lapply(abscesses_incr, function(k)
                              {
                                TE <- RR(
                                  HiSCR(.data, i, j, k),
                                  TRT, newHiSCR,          # fixed 
                                  sm, method_RR, method_OR
                                  )
                                
                                out <- as.data.frame(as.list(TE$measure[2,]))
                                out$AN_incr <- i
                                out$fist_incr <- j
                                out$abscesses_incr <- k
                                return(out)
                              }
                                ))
                      ))
            ) )
  
}


#TODO: ggplot function to plot RR against range of values for AN count decrease geom_pointrange()

# TODO shiny app capture printout --- create time animation for varying HiSCR definitions

# Change in treatment effect at W 16 (RR or OR) for varying HiSCR definitions (animatet plot), Y-axis = sm, X-axis = AN_increase 