# Setup Environment

Checks and loads required R packages used by the task pipeline.

Writes a package log that downstream tasks can reference.

Note: `r5r` is treated as optional in `packages.R`, but it is required for `compute_travel_times`.
That step also needs Java 11+ with a valid `JAVA_HOME`.

## Running

```bash
cd code && make
```

Main output: `output/R_packages.txt`.
