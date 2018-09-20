##### Function to export  weightedt test function to LaTeX  #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####

## Description
# This function takes the output of the wtd.t.test function, from the 
# weights package, and prints the LaTeX code for a well-formated 
# table, based on the stargazer package.
## Arguments
# list - a list in which each element is the output of the wtd.t.test 
# function.
# row.names - an optional character vector with the names of the rows 
# for the output table.
# col.names - an optional character vector with the column names for 
# the output table.
# filename - an optional file path for the printed output. If abcent, 
# the output is printed to the console.
# digits - number of decimal digits to be printed.
# ... - other arguments to be passed to stargazer
## Notes
# This function works only with most of stargazer default values. So, 
# for example, the only float environment supported is "table", the 
# only significance character supported is "*" and the only 
# significance cutoffs supported are .9, .95 and .99.

wtd.t.test.stargazer = function(list, row.names = NULL, 
  col.names = NULL, filename = NULL, digits = 2, ...){
  # Vectors with statistics
  Difference = sapply(list, function(i) i$additional[1])
  Mean.X = sapply(list, function(i) i$additional[2])
  Mean.Y = sapply(list, function(i) i$additional[3])
  SE = sapply(list, function(i) i$additional[4])
  pval = sapply(list, function(i) i$coefficients[3])
  # Data frame to be printed 
  df = data.frame(Mean.X, Mean.Y, Difference)
  # Renaming rows
  rownames(df) = row.names
  # Renaming columns
  if(!is.null(col.names)) colnames(df)[-3] = col.names
  ## Exporting
  library(stargazer)
  # Capturing stargazer output to hack it
  out = capture.output(
    stargazer(df, summary = F, digits = digits, 
      notes = c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
        "Standard errors shown in parentheses"), ...)
  )
  # Adding stars
  index = grep("\\\\\\hline", out)[3]
  out[index + which(pval <= .1 & pval > .05)] = 
    diag(as.matrix(sapply(out[index + which(pval <= .1 & pval > .05)], 
          function(x) sapply(Difference[pval <= .1 & pval > .05], 
            function(y) gsub(formatC(y, format = 'f', digits), 
              paste(round(y, 2), "{*}", sep = "^"), x)))))
  out[index + which(pval <= .05 & pval > .01)] = 
    diag(as.matrix(sapply(out[index + which(pval <= .05 & pval > 
            .01)], function(x) sapply(Difference[pval <= .05 & pval 
          > .01], function(y) gsub(formatC(y, format = 'f', digits = 
          digits), paste(round(y, 2), "{**}", sep = "^"), x)))))
  out[index + which(pval <= .01)] = diag(as.matrix(sapply(out[index + 
          which(pval <= .01)], function(x) sapply(Difference[pval <= 
          .01], function(y) gsub(formatC(y, format = 'f', digits = 
          digits), paste(round(y, 2), "{***}", sep = "^"), x)))))
  out = unlist(out)
  # Adding standard errors
  for(i in 1:length(list)){
    index = index + 1
    out = c(out[1:index], paste0(" & & & ($", round(SE[i], digits), 
        "$) \\\\ "), out[(index + 1):length(out)])
    index = index + 1
  }
  # Exporting
  if(!is.null(filename)){
    cat(out, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
      collapse = ""))
  }else{
    cat(out, sep = "\n")
  }
}

## Example\
# Generating random variables
library(MASS)
set.seed(62442)
x = mvrnorm(100, rep(0, 4), diag(nrow = 4))
y = cbind(x[, 1], x[, 2] - .3, x[, 3] - .35, x[, 4] - .4)

# Running t tests
library(weights)
res = list(wtd.t.test(x[, 1], y[, 1]), wtd.t.test(x[, 2], y[, 2]), 
  wtd.t.test(x[, 3], y[, 3]), wtd.t.test(x[, 4], y[, 4]))

# Exporting 
wtd.t.test.stargazer(list = res, row.names = c("T 1", "T 2", "T 3", 
    "T 4"), col.names = c("X", "Y"), 
  filename = "output/wtd.t.test.stargazer.example.table.tex", 
  title = "Output example", align = T)