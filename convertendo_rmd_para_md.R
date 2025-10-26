rmarkdown::render("./calcular_coordenadas/README.Rmd",
                  rmarkdown::md_document(variant = "gfm"),
                  output_options = list(fig_path = "man/figures/"))
