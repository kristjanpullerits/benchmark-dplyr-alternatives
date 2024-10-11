
# Libraries ---------------------------------------------------------------

library("microbenchmark")
library("tidyverse")
library("polars")
library("tidypolars")
library("arrow")
library("dtplyr")
library("OlinkAnalyze")
library("patchwork")

# Helpers -----------------------------------------------------------------

olink_theme_with_grid <- function() {
  OlinkAnalyze::set_plot_theme() +
    theme(panel.grid.major.x = element_line(size = 0.5),
          panel.grid.major.y = element_line(size = 0.5),
          panel.grid.minor.x = element_line(size = 0.25),
          panel.grid.minor.y = element_line(size = 0.25))
}

# Generate test data ------------------------------------------------------

set.seed(1337)

n <- 100

generate_df <- function(n) {
  data.frame(
    x = sample(x = as.character(c(1:10)),
               size = n,
               replace = TRUE),
    y = sample(x = c(1:10),
               size = n,
               replace = TRUE),
    z = runif(n)
  ) %>%
    mutate(
      z = ifelse(x %in% c("1", "2", "3"), z + 2, z)
    )
}

# data.frame
df_small <- generate_df(1e3)
df_mid <- generate_df(1e5)
df_large <- generate_df(1e7)

# Polars
df_small_polars <- as_polars_df(df_small)
df_mid_polars <- as_polars_df(df_mid)
df_large_polars <- as_polars_df(df_large)

# Arrow
df_small_arrow <- arrow_table(df_small, as_data_frame = FALSE)
df_mid_arrow <- arrow_table(df_mid, as_data_frame = FALSE)
df_large_arrow <- arrow_table(df_large, as_data_frame = FALSE)

# dtplyr
df_small_dtplyr <- lazy_dt(df_small)
df_mid_dtplyr <- lazy_dt(df_mid)
df_large_dtplyr <- lazy_dt(df_large)



# left join df-s ----------------------------------------------------------

df_left_join = data.frame(
  x = as.character(1:10),
  q = LETTERS[1:10]
)

# Polars
df_left_join_polars <- as_polars_df(df_left_join)

# Arrow
df_left_join_arrow <- arrow_table(df_left_join, as_data_frame = FALSE)

# dtplyr
df_left_join_dtplyr <-lazy_dt(df_left_join)

# Calculate grouped mean --------------------------------------------------

# dplyr
## polars and dtplyr same as dplyr?
dplyr_calc_group_mean <- function(df) {
  df %>%
    summarize(mean_val = mean(z), .by = x)
}

# Arrow
arrow_calc_group_mean <- function(df) {
  dplyr_calc_group_mean(df) %>%
    collect()
}

test <- microbenchmark(
      df_small %>%
        dplyr_calc_group_mean(),
      df_small %>%
        as_polars_df() %>%
        dplyr_calc_group_mean(),
      df_small %>%
        arrow_table() %>%
        arrow_calc_group_mean(),
      df_small %>%
        lazy_dt() %>%
        dplyr_calc_group_mean(),
      df_small_polars %>%
        dplyr_calc_group_mean(),
      df_small_arrow %>%
        arrow_calc_group_mean(),
      df_small_dtplyr %>%
        dplyr_calc_group_mean(),
      df_mid %>%
        dplyr_calc_group_mean(),
      df_mid %>%
        as_polars_df() %>%
        dplyr_calc_group_mean(),
      df_mid %>%
        arrow_table() %>%
        arrow_calc_group_mean(),
      df_mid %>%
        lazy_dt() %>%
        dplyr_calc_group_mean(),
      df_mid_polars %>%
        dplyr_calc_group_mean(),
      df_mid_dtplyr %>%
        dplyr_calc_group_mean(),
      df_mid_arrow %>%
        arrow_calc_group_mean(),
      df_large %>%
        dplyr_calc_group_mean(),
      df_large %>%
        as_polars_df() %>%
        dplyr_calc_group_mean(),
      df_large %>%
        arrow_table() %>%
        arrow_calc_group_mean(),
      df_large %>%
        lazy_dt() %>%
        dplyr_calc_group_mean(),
      df_large_polars %>%
        dplyr_calc_group_mean(),
      df_large_dtplyr %>%
        dplyr_calc_group_mean(),
      df_large_arrow %>%
        arrow_calc_group_mean(),
  times = 50L,
  unit = "microseconds"
)

results <- tibble(func = test$expr, time = test$time) %>%
  mutate(
    package = case_when(
      str_detect(func, "polars") ~ "Polars",
      str_detect(func, "arrow") ~ "Arrow",
      str_detect(func, "dtplyr|lazy_dt") ~ "dtplyr",
      TRUE ~ "dplyr"
    ),
    dataset = case_when(
      str_detect(func, "small") ~ "small (n = 1e3)",
      str_detect(func, "mid") ~ "mid (n = 1e5)",
      str_detect(func, "large") ~ "large (n = 1e7)"
    ),
    preprocess = case_when(
      str_detect(func, "as_polars_df()") ~ "as_polars_df()",
      str_detect(func, "arrow_table()") ~ "arrow_table()",
      str_detect(func, "lazy_dt()") ~ "lazy_dt()",
      TRUE ~ ""
    ),
    package_and_preprocess = paste0(package, "\n", preprocess)
  ) %>%
  mutate(
    dataset = factor(dataset, levels = c("small (n = 1e3)", "mid (n = 1e5)", "large (n = 1e7)"))
  )


## Plot -------------------------------------------------------------------


p_group_by_mean <- results %>%
  ggplot(aes(x = package_and_preprocess, y = time,
             fill = package)) +
  facet_wrap(~dataset)+
  geom_boxplot()+
  olink_theme_with_grid() +
  olink_fill_discrete()+
  labs(
    title = "df %>%\n  group_by(x) %>%\n  summarize(mean(y)",
    x = "",
    y = "Microseconds",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1)
  )


# Calculate left_join -----------------------------------------------------

# dplyr
## polars same as dplyr?
dplyr_left_join <- function(df1, df2) {
  df1 %>%
    left_join(df2, by = join_by(x))
}

# dtplyr
dtplyr_left_join <- function(df1, df2) {
  df1 %>%
    left_join(df2, by = "x")
}
# Arrow
arrow_left_join <- function(df1, df2) {
  dplyr_left_join(df1, df2) %>%
    collect()
}

test2 <- microbenchmark(
  df_small %>%
    dplyr_left_join(., df_left_join),
  df_small %>%
    as_polars_df() %>%
    dplyr_left_join(., df_left_join_polars),
  df_small %>%
    arrow_table() %>%
    arrow_left_join(., df_left_join_arrow),
  df_small %>%
    lazy_dt() %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_small_polars %>%
    dplyr_left_join(., df_left_join_polars),
  df_small_arrow %>%
    arrow_left_join(., df_left_join_arrow),
  df_small_dtplyr %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_mid %>%
    dplyr_left_join(., df_left_join),
  df_mid %>%
    as_polars_df() %>%
    dplyr_left_join(., df_left_join_polars),
  df_mid %>%
    arrow_table() %>%
    arrow_left_join(., df_left_join_arrow),
  df_mid %>%
    lazy_dt() %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_mid_polars %>%
    dplyr_left_join(., df_left_join_polars),
  df_mid_dtplyr %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_mid_arrow %>%
    arrow_left_join(., df_left_join_arrow),
  df_large %>%
    dplyr_left_join(., df_left_join),
  df_large %>%
    as_polars_df() %>%
    dplyr_left_join(., df_left_join_polars),
  df_large %>%
    arrow_table() %>%
    arrow_left_join(., df_left_join_arrow),
  df_large %>%
    lazy_dt() %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_large_polars %>%
    dplyr_left_join(., df_left_join_polars),
  df_large_dtplyr %>%
    dtplyr_left_join(., df_left_join_dtplyr),
  df_large_arrow %>%
    arrow_left_join(., df_left_join_arrow),
  times = 50L,
  unit = "microseconds"
)

results2 <- tibble(func = test2$expr, time = test2$time) %>%
  mutate(
    package = case_when(
      str_detect(func, "polars") ~ "Polars",
      str_detect(func, "arrow") ~ "Arrow",
      str_detect(func, "dtplyr|lazy_dt") ~ "dtplyr",
      TRUE ~ "dplyr"
    ),
    dataset = case_when(
      str_detect(func, "small") ~ "small (n = 1e3)",
      str_detect(func, "mid") ~ "mid (n = 1e5)",
      str_detect(func, "large") ~ "large (n = 1e7)"
    ),
    preprocess = case_when(
      str_detect(func, "as_polars_df()") ~ "as_polars_df()",
      str_detect(func, "arrow_table()") ~ "arrow_table()",
      str_detect(func, "lazy_dt()") ~ "lazy_dt()",
      TRUE ~ ""
    ),
    package_and_preprocess = paste0(package, "\n", preprocess)
  ) %>%
  mutate(
    dataset = factor(dataset, levels = c("small (n = 1e3)", "mid (n = 1e5)", "large (n = 1e7)"))
  )


## Plot -------------------------------------------------------------------


p_left_join <- results2 %>%
  ggplot(aes(x = package_and_preprocess, y = time,
             fill = package)) +
  facet_wrap(~dataset)+
  geom_boxplot()+
  olink_theme_with_grid() +
  olink_fill_discrete()+
  labs(
    title = "df %>%\n  left_join(x)",
    subtitle = "The x dataframe is already preprocessed to the package specific format.",
    x = "",
    y = "Microseconds",
    fill = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1)
  )

# Merge plots -------------------------------------------------------------

p_all <- (p_group_by_mean / p_left_join) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Compute times of different sized datasets (small/mid/large)",
    subtitle = str_wrap(
      'Packages except for dplyr require some kind of "preprocessing" of the dataset beforehand. For example dtplyr uses the
                  function lazy_dt() to convert the dataframe to a "lazy" data table. This is noted on the x axis.',
      width = 120
    )
  )

ggsave("output/p_group-by-mean_and_left-join.png",
       p_all,
       width = 9,
       height = 9)
