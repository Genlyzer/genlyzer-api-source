#!/usr/bin/env Rscript
# ============================================================================
# GSEA ranking and result-ordering helpers
#
# api/helpers/gsea_helpers.R ranks DE genes for clusterProfiler::GSEA and
# orders the resulting gene-set table. These checks pin down:
#   - p-value ranking puts the most significant genes first (including p = 0),
#   - the other ranking methods keep their documented direction,
#   - result rows are ordered by abs(NES) desc, p.adjust asc, setSize asc,
#     then name.
#
# Usage: Rscript api/tests/test_gsea_helpers.R   (from the repo root)
# ============================================================================

if (!file.exists("api/helpers/gsea_helpers.R")) {
  cat("ERROR: run from the repo root (api/helpers/gsea_helpers.R not found)\n")
  quit(status = 1)
}

source("api/helpers/gsea_helpers.R")

n_pass <- 0L
failures <- character()
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else failures <<- c(failures, desc)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  invisible(ok)
}

# ---- ranking methods --------------------------------------------------------
de <- data.frame(
  pvalue = c(0.01, 0.001, NA, 0.5, 0),
  stat = c(1, 2, 3, 4, 5),
  log2FC = c(1, -1, 0, 2, -2),
  row.names = c("gene_a", "gene_b", "gene_c", "gene_d", "gene_e"),
  stringsAsFactors = FALSE
)

pv <- rankGenes(de, "pvalue")
check("p-value ranking puts an underflowed p = 0 first",
      identical(names(pv)[1], "gene_e"))
check("p-value ranking orders the remaining genes by increasing p-value",
      identical(names(pv)[2:4], c("gene_b", "gene_a", "gene_d")))
check("missing p-values rank last",
      identical(names(pv)[5], "gene_c"))
check("p-value rank is -log10(p)",
      isTRUE(all.equal(unname(pv["gene_b"]), -log10(0.001))))
check("p-value ranks descend from most to least significant",
      all(diff(unname(pv)) <= 0))

stat <- rankGenes(de, "stat")
check("stat ranking is descending",
      identical(names(stat), c("gene_e", "gene_d", "gene_c", "gene_b", "gene_a")))

lfc <- rankGenes(de, "log2FoldChange")
check("log2 fold change ranking is descending",
      identical(names(lfc), c("gene_d", "gene_a", "gene_c", "gene_b", "gene_e")))

signed <- rankGenes(de, "-log10(pvalue) * log2FC_sign")
check("signed -log10(p) ranking puts the strongest positive gene first",
      identical(names(signed), c("gene_a", "gene_d", "gene_c", "gene_b", "gene_e")))

check("invalid ranking method raises",
      inherits(try(rankGenes(de, "not_a_method"), silent = TRUE), "try-error"))

# ---- result ordering --------------------------------------------------------
results <- data.frame(
  name = c("beta", "alpha", "gamma", "delta", "epsilon"),
  absNES = c(1.0, 2.0, 1.0, 1.0, 2.0),
  p.adjust = c(0.01, 0.05, 0.01, 0.2, 0.05),
  setSize = c(100, 50, 10, 20, 50),
  stringsAsFactors = FALSE
)
ordered <- orderGSEAResults(results)
check("abs(NES) is the primary sort key",
      identical(ordered$name, c("alpha", "epsilon", "gamma", "beta", "delta")))

tie_name <- data.frame(
  name = c("zeta", "alpha"),
  absNES = c(1, 1),
  p.adjust = c(0.01, 0.01),
  setSize = c(10, 10),
  stringsAsFactors = FALSE
)
check("name breaks exact ties alphabetically",
      identical(orderGSEAResults(tie_name)$name, c("alpha", "zeta")))

tie_padj <- data.frame(
  name = c("larger_p", "smaller_p"),
  absNES = c(1, 1),
  p.adjust = c(0.2, 0.01),
  setSize = c(10, 10),
  stringsAsFactors = FALSE
)
check("p.adjust breaks abs(NES) ties ascending",
      identical(orderGSEAResults(tie_padj)$name, c("smaller_p", "larger_p")))

tie_size <- data.frame(
  name = c("large_set", "small_set"),
  absNES = c(1, 1),
  p.adjust = c(0.01, 0.01),
  setSize = c(100, 10),
  stringsAsFactors = FALSE
)
check("setSize breaks remaining ties ascending",
      identical(orderGSEAResults(tie_size)$name, c("small_set", "large_set")))

missing_absnes <- data.frame(
  name = c("missing", "present"),
  absNES = c(NA, 1),
  p.adjust = c(0.01, 0.5),
  setSize = c(10, 10),
  stringsAsFactors = FALSE
)
check("missing abs(NES) rows sort last",
      identical(orderGSEAResults(missing_absnes)$name, c("present", "missing")))

missing_padj <- data.frame(
  name = c("missing", "present"),
  absNES = c(1, 1),
  p.adjust = c(NA, 0.01),
  setSize = c(10, 10),
  stringsAsFactors = FALSE
)
check("missing p.adjust rows sort last",
      identical(orderGSEAResults(missing_padj)$name, c("present", "missing")))

total <- n_pass + length(failures)
cat(sprintf("\nGSEA helper tests: %d/%d checks passed\n", n_pass, total))
if (length(failures)) {
  cat("Failed checks:\n")
  for (f in failures) cat("  -", f, "\n")
  quit(status = 1)
}
cat("PASS: GSEA ranking direction and result ordering\n")
