# Compute Travel Times

Computes tract-to-tract transit travel times for baseline CTA service and the Red Line Extension scenario.

Also saves the baseline-extension change matrix used by gravity estimation and the counterfactual.

Prerequisites: `r5r` in R and Java 11+ available via `JAVA_HOME`.

Quick check:
```bash
Rscript -e 'cat("JAVA_HOME=", Sys.getenv("JAVA_HOME"), "\n"); cat("rJava=", requireNamespace("rJava", quietly=TRUE), "\n"); cat("r5r=", requireNamespace("r5r", quietly=TRUE), "\n")'
```

## Running

```bash
cd code && make
```

Main outputs: `output/travel_time_matrix_baseline.csv`, `output/travel_time_matrix_extension.csv`, and `output/travel_time_change.csv`.
