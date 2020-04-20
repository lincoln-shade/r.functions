##--------------------------------------------------------------------------------------
## Create Table 1 data.table
##--------------------------------------------------------------------------------------
## Output table will have at least 4 columns
## Variable: variable from dt that is being summarized
## Labels: depends on variable type
##    Continuous variables: "Mean (SD)" and "Median [Min, Max]"
##    Factor variables:     Factor levels for variable
## Overall: summaries for the non-stratified input data frame
## N Missing: number and percent of missing values for each variable
## Additional columns for summaries of each stratum of stratifying variable, if present
##--------------------------------------------------------------------------------------

require(data.table)
require(magrittr)

TableOne <- function(dt,            # data frame 
                     outcome = NULL # string of column name to stratify table by
) {
  if (!is.data.frame(dt)) {
    stop("The input must be a data frame!")
    }
  if (!is.null(outcome) & sum(colnames(dt) == outcome) == 0) {
    stop("Outcome variable not variable in input table")
    }
  
  dt <- as.data.table(dt)

  ##---------------------------------------
  ## Variable, Label, and Overall columns
  ##---------------------------------------
  
  ## total subjects
  n <- dt[, .N]
  
  ## initialize output table
  var.names.t1 <- character()
  label.t1 <- character()
  summary.t1 <- character()
  n.missing.t1 <- character()
  
  NMiss <- function(dt, i, n) {    # fill in missing data
    paste0(sum(is.na(dt[[i]])), 
           " (", round(sum(is.na(dt[[i]])) / n * 100, 1), 
           "%)")
  }
  
  ## don't create output rows for stratified variable
  if (is.null(outcome)) {
    table.vars <- colnames(dt)
  } else {
      table.vars <- colnames(dt)[!colnames(dt) == outcome]
  }
  
  
  for (i in table.vars) {
    var.names.i <- character()
    label.i <- character()
    summary.i <- character()
    n.missing.i <- character()
    
    if (is.factor(dt[[i]])) { ## for factor variables
      lvls <- levels(dt[[i]])
      for (j in 1:length(lvls)) {
        var.names.i[j] <- i
        
        label.i[j] <- lvls[j]
        
        lvls.n <- dt[!is.na(dt[[i]]), .N, i] %>% setkeyv(col=i)
        
        summary.i[j] <- 
          paste0(lvls.n[["N"]][j], " (", 
                 round(lvls.n[["N"]][j] / n * 100, 1), "%)")
        
        n.missing.i[j] <- NMiss(dt, i, n)
      } 
      
    } else { ## for continuous/non-factor variables
      miss <- is.na(dt[[i]])
      var.names.i <- rep(i, 2)
      label.i <- c("Mean (SD)", "Median [Min, Max]")
      summary.i <- c( ## mean and SD
        paste0(round(mean(dt[[i]][!miss]), 1),
               " (", round(sd(dt[[i]][!miss]), 1), ")"
        ), ## median and range
        paste0(round(median(dt[[i]][!miss]), 1),
               " [", round(min(dt[[i]][!miss]), 1), ", ", 
               round(max(dt[[i]][!miss]), 1), "]"
        ))
      n.missing.i <- rep(NMiss(dt, i, n), 2)
    }
    
    ## concatenate the rows from the variable to the output vectors
    var.names.t1 <- c(var.names.t1, var.names.i)
    label.t1 <- c(label.t1, label.i)
    summary.t1 <- c(summary.t1, summary.i)
    n.missing.t1 <- c(n.missing.t1, n.missing.i)
    
  }
  
  ## bind vectors into data.table
  table.one <- data.table(
    "Variable" = var.names.t1,
    "Labels" = label.t1,
    "Overall" = summary.t1
    
  )
  
  ##-------------------------------
  ## Stratifying variable columns
  ##-------------------------------
  
  if (!is.null(outcome)) { ## if there is a variable to stratify by, make a vector for each level
    strat <- levels(dt[[outcome]])
    for (k in strat) { ## initialize vector for each level of outcome
      assign(paste(k), 
             character())
    }
    
    FactorSummary <- function(dt, var, j.var, lev) {
      vec <- character()
      lvls <- levels(dt[[var]])
      j.var.v <- dt[[j.var]]
      lev.n <- dt[j.var.v == lev, .N]
      jlev.var.n <- dt[j.var.v == lev, .N, var] %>% setkeyv(cols=var) %>% .[, N]
      for (j in 1:length(lvls)) {
        vec[j] <- paste0(jlev.var.n[j], " (", 
                         round(jlev.var.n[j] / lev.n * 100, 1), "%)")
      }
      return(vec)
    }
    
    ContinuousSummary <- function(dt, var, j.var, lev) {
      dt.j.var.v <- dt[get(j.var) == lev & !is.na(get(var)), 
                       c(round(mean(get(var)), 1),
                         round(sd(get(var)), 1), 
                         round(median(get(var)), 1), 
                         round(min(get(var)), 1),
                         round(max(get(var)), 1)
                       )
                       ]
      vec <- c( ## mean and SD
        paste0(dt.j.var.v[1],
               " (", dt.j.var.v[2], ")"
        ), ## median and range
        paste0(dt.j.var.v[3],
               " [", dt.j.var.v[4], ", ", 
               dt.j.var.v[5], "]"
        ))
      return(vec)
    }
    
    for (i in colnames(dt)[!colnames(dt) == outcome]) {
      if (is.factor(dt[[i]])) { ## factor variables
        for (k in strat) {
          assign(paste(k), c(get(paste(k)), 
                             FactorSummary(dt, i, outcome, k)
          ))
        }
      } else {
        for (k in strat) {
          assign(paste(k), c(get(paste(k)), 
                             ContinuousSummary(dt, i, outcome, lev=k)
          ))
        }
      }
    }
    for (k in strat) {
      table.one[, paste(k) := get(k)]
    }
  }
  ##----------------------
  ## N Missing column
  ##----------------------
  
  table.one[, "N Missing" := n.missing.t1]
  
  ##---------------
  ## Output table
  ##---------------
  
  return(table.one[])
}
