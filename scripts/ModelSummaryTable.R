source("LoadRpackages.R")
#source("Q1.R")
#source("Q2.R")
#source("Q3.R")

html_file <- "../output/supplementary_materials/Supplementary_Materials_Model_summary.html"


tab_model(DSI.r.tweedie, top3.kinship.REML, model.nonkin, duration.kinship, variability.kinship, model3, 
          transform = NULL, 
          show.re.var = TRUE,
          linebreak = TRUE,
          dv.labels = c("Model 1a (DSI)", "Model 1b (bond vs. not a bond)", "Model 1c (non-kin vs. kin)", "Model 2a (bond years)", "Model 2b (DSI variability)", "Model 3 (non-kin vs. kin)"),
          pred.labels = c("(Intercept)", "R (coefficient of relatedness)", "Maximum dyadic DSI", "Kinship [non-kin vs. kin]", "Number of available kin", "Age", "Rank", "Group size"),
          file = html_file)

vif_duration    <- vif(duration.kinship)
vif_m3          <- vif(model3)

make_vif_text <- function(name, vif_values) {
  paste0(
    "<b>", name, ":</b> ",
    paste(names(vif_values), round(vif_values, 2), sep=" = ", collapse=", "),
    "<br>"
  )
}

vif_html <- paste0(
  make_vif_text("Model 2a (bond years)", vif_duration),
  make_vif_text("Model 3 (non-kin vs. kin)", vif_m3)
)

vif_html <- paste(purrr::imap_chr(vif_list, make_block), collapse = "\n")
vif_html <- gsub("maximal.dyadic.DSI", "maximum dyadic DSI", vif_html)
vif_html <- gsub("kinship", "kinship [non-kin vs. kin]", vif_html)
vif_html <- gsub("focal.kin.available", "number of available kin", vif_html)
vif_html <- gsub("percofsex.dominated", "rank", vif_html)
vif_html <- gsub("group.size", "group size", vif_html)


cat(
  "<hr><h4>Variance Inflation Factors (VIF)</h4>\n",
  vif_html,
  file = html_file,
  append = TRUE
)

new_text <- "
<div style='text-align: center; margin: 0.5em 0;'>
  <strong>Table S1.</strong> A summary of parameters and results of all six statistical models performed in this study.
</div>
"


old_html <- readLines(html_file, warn = FALSE)

writeLines(
  c(new_text, old_html),
  html_file
)



