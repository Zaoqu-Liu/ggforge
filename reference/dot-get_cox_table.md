# Compute Cox Table

Internal function to compute Cox proportional-hazards model summary
table with optional parallel computation

## Usage

``` r
.get_cox_table(
  data,
  vars,
  time = "time",
  event = "event",
  scale = FALSE,
  nonExpression_ratio = 1,
  parallel = FALSE,
  n_cores = 1
)
```
