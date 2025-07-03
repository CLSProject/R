# konnte bisher nicht getestet werden, auch abhängig von Input und Output, muss noch final definiert werden
library(pheatmap)
library(gplots)
library(RColorBrewer)
library(dendextend)

create_heatmap <- function(
  daten_matrix,
  palette_colors,
  analysis_params = NULL
) {

  pheatmap(daten_matrix,
    main = paste("Analysis result for: ", analysis_params$file_name,
      "\nSelected disease: ", analysis_params$disease,
      "\nDistance measure: ", analysis_params$distance,
      "\nLinkage criterion: ", analysis_params$linkage,
      "\n", analysis_params$cluster_crit
    ),
    color = palette_colors,
    fontsize_number = 7,
    cellwidth = 15,
    cellheight = 12,
    border_color = "black"
  )
}
