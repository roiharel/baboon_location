library(DT)
library(htmlwidgets)

# Create the interactive table with download options
interactive_table <- datatable(
  last_rows_per_tag_html,
  escape = FALSE,
  extensions = c('Buttons', 'Responsive'),
  options = list(
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = 20,
    lengthMenu = c(nrow(last_rows_per_tag_html)),
    autoWidth = TRUE,
    dom = 'Blfrtip',
    buttons = list(
      list(
        extend = 'copy',
        text = '???? Copy',
        className = 'btn-sm'
      ),
      list(
        extend = 'csv',
        text = '???? CSV',
        filename = paste0('baboon_metadata_', Sys.Date())
      ),
      list(
        extend = 'excel',
        text = '???? Excel',
        filename = paste0('baboon_metadata_', Sys.Date())
      ),
      list(
        extend = 'pdf',
        text = '???? PDF',
        filename = paste0('baboon_metadata_', Sys.Date())
      ),
      list(
        extend = 'print',
        text = '??????? Print'
      ),
      list(
        extend = 'colvis',
        text = '??????? Columns'
      )
    ),
    language = list(
      search = "Filter records:",
      lengthMenu = "_MENU_ rows per page",
      info = "Showing _START_ to _END_ of _TOTAL_ records"
    )
  ),
  filter = 'top',
  selection = 'multiple',
  class = 'cell-border stripe hover'
)

# Save as HTML file (standalone - works offline)
saveWidget(
  interactive_table,
  file = 'plots/metadata_table_interactive.html',
  selfcontained = TRUE
)

cat("??? Interactive table saved to: plots/metadata_table_interactive.html\n")
cat("??? File is standalone and works offline\n")
cat("??? Download options: CSV, Excel, PDF, Print, Copy\n")

# Alternative: Create R Markdown HTML with enhanced styling
rmd_content <- '
---
title: "Baboon GPS Metadata Table"
output:
  html_document:
    theme: bootstrap
    highlight: tango
    self_contained: yes
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(DT)
library(dplyr)
```

## ???? Baboon GPS Metadata - Interactive Table

**Last Updated:** `r format(Sys.time(), "%Y-%m-%d %H:%M:%S")`

**Total Records:** `r nrow(last_rows_per_tag_html)`

---

```{r}
interactive_table <- datatable(
  last_rows_per_tag_html,
  escape = FALSE,
  extensions = c("Buttons", "Responsive"),
  options = list(
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = 20,
    lengthMenu = c(10, 20, 50, nrow(last_rows_per_tag_html)),
    autoWidth = TRUE,
    dom = "Blfrtip",
    buttons = list(
      list(extend = "copy", text = "???? Copy"),
      list(extend = "csv", text = "???? CSV", filename = paste0("baboon_metadata_", Sys.Date())),
      list(extend = "excel", text = "???? Excel", filename = paste0("baboon_metadata_", Sys.Date())),
      list(extend = "pdf", text = "???? PDF", filename = paste0("baboon_metadata_", Sys.Date())),
      list(extend = "print", text = "??????? Print"),
      list(extend = "colvis", text = "??????? Columns")
    )
  ),
  filter = "top",
  selection = "multiple",
  class = "cell-border stripe hover"
)

interactive_table
```

**Features:**
- ???? Search/Filter records by any column
- ???? Download as CSV, Excel, or PDF
- ???? Copy to clipboard
- ??????? Print directly
- ??????? Show/hide columns
- ???? Responsive design (works on mobile)
- ???? Offline compatible
'

writeLines(rmd_content, 'plots/metadata_table_rmd.Rmd')

cat("\n??? Alternative R Markdown file created: plots/metadata_table_rmd.Rmd\n")
cat("  Render with: rmarkdown::render(\"plots/metadata_table_rmd.Rmd\")\n")
