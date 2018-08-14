test <- stargazer(dyErgm.result$result.vergm.l, title = "results of yearly ERGMs", dep.var.labels = c("spillover of listed bank"), 
          column.labels = c(2007:2016) %>% as.character, 
          covariate.labels = c("cns",sub(pattern = "coef.", replacement = "",  names(result))), 
          notes = "", 
          digits = 2, 
          label = paste0("tab:",filename), 
          out = paste0(filename,".tex"), table.placement = "H", out.header = FALSE, no.space = TRUE, nobs = TRUE, model.numbers=FALSE, type = "latex")#type

#Number & 7 & 14 & 14 & 14 & 16 & 16 & 16 & 16 & 16 & 16 \\


test <- c(test[1:29],
      "Number & 7 & 14 & 14 & 14 & 16 & 16 & 16 & 16 & 16 & 16 \\",
      test[30:33])
print(test, 
      file="test.tex", 
      sanitize.text.function = function(str) gsub("_", "\\_", str, fixed = TRUE),
      caption.placement = "top",
      comment = "TRUE"
)
