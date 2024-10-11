# Benchmark dplyr alternatives

This repo compares alternative data manipulation packages to dplyr. The packages benchmarked packages include:

- dplyr (<https://dplyr.tidyverse.org/>)
- Arrow (<https://arrow.apache.org/docs/r/>)
- dtplyr (<https://dtplyr.tidyverse.org/>)
- Polars (<https://github.com/etiennebacher/tidypolars>)

Two common data manipulation tasks were compared: 1) grouping on some variable and taking the mean and 2) left joining a dataframe. Note: I have not much experience in using the other packages than dplyr and there might be approaches using the other packages which makes them faster.

From the analysis done here (figure below), it seemed like the dtplyr package was very fast in both data manipulation tasks as well as with large datasets. It was also noted that "preprocessing" a data frame into a format which works with a given package costs some computing time. Thus, it is probably good to stay with the same package and format to decrease unnecessary conversions. The script used in this analysis is found [here](https://github.com/kristjanpullerits/benchmark-dplyr-alternatives/blob/main/scripts/001.R).


<img src="./output/p_group-by-mean_and_left-join.png" width="3600" />
