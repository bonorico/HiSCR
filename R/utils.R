# For extra info on HiSCR please see https://pubmed.ncbi.nlm.nih.gov/26201313/
# 

library(tidyverse)
library(shiny)
library(gganimate)
library(transformr)
library(gifski)
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
  
  # invert effect direction depending on chosen measure (for RR small is good - RR for not having HiSCR, whereas for OR big is good - OR for HiSCR)
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
HiSCR <- function(.data, AN_incr = -0.5, fist_incr = 0, abscesses_incr = 0)
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
    "Yes", "No")  )
  # ) %>% 
  #   select(TRT, newHiSCR, HiSCR)
}

#newdat <- HiSCR(data)

# Something does not agree with original data ... ???
# RR(newdat, TRT, HiSCR, sm = "OR")
# RR(newdat, TRT, newHiSCR, sm = "OR")

#'Returns data frame with treatment effect estimates for varying HiSCR definitions
#' @description builds data by internally calling function HiSCR above
#' @param .data
varying_def_data <- function(.data, 
                             AN_incr_range = -seq(0.25, 0.75, 0.05),
                             fist_incr_range = 0,
                             abscesses_incr_range = 0,
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
                              lapply(abscesses_incr_range, function(k)
                              {
                                TE <- RR(
                                  HiSCR(.data, i, j, k),
                                  TRT, newHiSCR,          # fixed 
                                  sm, method_RR, method_OR
                                )
                                
                                out <- as.data.frame(as.list(TE$measure[2,])) # extract data
                                out$AN_incr <- i*(-100) # percent positive for plotting
                                out$fist_incr <- j
                                out$abscesses_incr <- k
                                return(out)
                              }
                              ))
                    ))
          ) )
  
}


#newdat <- varying_def_data(data)
#RR(HiSCR(data), TRT, newHiSCR, sm = "RR")$measure

#' Plots treatment effect for vaying definitinos of HiSCR
#' @description  see above for arguments.
plot_effect <- function(.data, 
                        AN_incr_range = -seq(0.25, 0.75, 0.05),
                        transition_var = "fist_incr",
                        fist_incr_range = 0,
                        abscesses_incr_range = 0,
                        sm = c("RR", "OR"), 
                        method_RR = c("wald", "small", "boot"),
                        method_OR = c("midp", "fisher", "wald", "small")
                        )
{
  sm <- match.arg(sm)
  method_RR <- match.arg(method_RR)
  method_OR <- match.arg(method_OR)
  
  ref_dat <- varying_def_data(data, AN_incr_range = -0.5,
                              sm = sm, 
                              method_RR = method_RR,
                              method_OR = method_OR) %>% 
    select(estimate, lower, upper, AN_incr) %>% 
    rename(ref_AN_incr = AN_incr)
  
  newdat <- varying_def_data(data, 
                             AN_incr_range,
                             fist_incr_range,
                             abscesses_incr_range,
                             sm, 
                             method_RR,
                             method_OR)
  # transvar <- switch(newdat %>% select({{transition_var}}) %>% names,
  #                    fist_incr = "Change in number of fistulae from baseline",
  #                    abscesses_incr = "Change in number of abscesses from baseline"
  # )
  transvar <- switch(transition_var,
                     fist_incr = "Change in number of fistulae from baseline",
                     abscesses_incr = "Change in number of abscesses from baseline"
  )

  ggplot(newdat, aes(x = AN_incr, y = estimate) ) +
    geom_pointrange(data = ref_dat, aes(x = ref_AN_incr, y = estimate, 
                                        ymin = lower, ymax = upper ),
                    alpha = 0.8, colour = "red", position = position_dodge2(width=1.5, 
                                                                            preserve = "single")) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    ggtitle("Treatment effect on HiSCR (point estimate with 95% CIs)",
            subtitle = "{transvar} is {round(frame_time, 0)}")  +
    #theme_bw() + 
    xlab("Percent decrease in AN count") +
    ylab(switch(sm,
                RR = "treatment better \U2190 1/RR \U2192 treatment worst",
                OR = "treatment worst \U2190 OR \U2192 treatment better")) +
    # transition_time({{transition_var}}) +
    transition_time(eval(sym(transition_var), newdat)) +  # must evaluate symbol at current data !!!
    ease_aes('linear') +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    geom_hline(yintercept = 1, colour = "green", alpha = 0.5)
    
  
}


# plot_effect(data, transition_var = "fist_incr", fist_incr_range = seq(0, 10, 1))

# NOTE: you could implement bi-dimensional transition by creating a new transition variable as the result of combining two separate ones (e.g., va1.var2 as real number). But I think this is too much for the actual task, so I will go with selective transition (choose to transition either with one or other variable).


