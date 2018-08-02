library(tidyverse)
library(ggthemes)

source('~/Projects/retentiontimealignment/R/misc/black.R')
data.path  = 'data/micro_vs_nano'

data_paths = file.path(list.files(data.path, full.names=T),
                       'r_preprocessed.Rd')

nano = c("2016-141 HYE nano2018_20180717_nano_60min_paper",
         "2016-141 HYE nano2018_20180716_120min_nano_paper")
micro = c("2016-141 HYE Microflow_20180723_60min_MF_paper",
          "2016-141 HYE Microflow_20180716_MF_120min_paper")
min60 = c("2016-141 HYE nano2018_20180717_nano_60min_paper",
          "2016-141 HYE Microflow_20180723_60min_MF_paper")
min120 = c("2016-141 HYE nano2018_20180716_120min_nano_paper",
           "2016-141 HYE Microflow_20180716_MF_120min_paper")
# load all files into one DT
datasets = list()
for(i in 1:4){
  load(data_paths[i])
  D1 = D$data %>% group_by(id) %>%
    mutate(rt_median      = median(rt),
           rt_median_dist = rt - rt_median) %>%
    ungroup()
  D1$run = ordered(D1$run)
  D1_stats = D1 %>% group_by(id) %>% summarise(run_cnt = n())
  D1_stats = D1_stats %>% filter(run_cnt == 6)
  D1 = D1 %>% filter(id %in% D1_stats$id) %>% mutate(experiment = list.files(data.path)[i])
  datasets[[i]] = D1
}
D = bind_rows(datasets)
D$experiment = factor(D$experiment)
# getting common FWMH
FWHM_quantiles = quantile(D$FWHM,
                          seq(0, 1, length.out = 5),
                          include.lowest = T,
                          ordered_result = T)

D = D %>%
  group_by(experiment, run) %>%
  mutate(FWHM_quantile = cut(FWHM, 
                             FWHM_quantiles,
                             include.lowest = T, 
                             ordered_result = T)) %>%
  ungroup


# plots -------------------------------------------------------------------------------
#base_plot = D %>%

get_base_plot = function(E,
                         min_y = -.1,
                         max_y =  .4) E %>%  
  ggplot(aes(x     = rt_median,
             y     = rt_median_dist,
             size  = FWHM_quantile, 
             color = FWHM_quantile)) +
  geom_point(alpha = .1) +
  theme_tufte() +
  xlab('Median Retention Time') +
  ylab('Distance to Median Retention Time') +
  ylim(min_y, max_y) +
  facet_grid(run ~ experiment)

# 60 minutes
D %>% 
  filter(experiment %in% min60, run > 1) %>%
  get_base_plot(max_y = .1, min_y= -.1)

D %>% 
  filter(experiment %in% min60, run > 1) %>%
  ggplot(aes(x     = rt_median,
             y     = rt_median_dist,
             color = FWHM_quantile)) +
  geom_point(size = .5) +
  theme_tufte() +
  xlab('Median Retention Time') +
  ylab('Distance to Median Retention Time') +
  ylim(-.1, .1) +
  facet_grid(run ~ experiment)

D %>% 
  filter(experiment %in% min120) %>%
  get_base_plot(max_y = .2)

levels(D$experiment)
D$experiment = ordered(D$experiment, levels=c(min60, min120) )
D %>% 
  ggplot(aes(x = experiment, fill = run, y = FWHM)) +
  geom_violin() +
  theme_tufte()

# the binned FWHM:
D1 %>% group_by(run) %>% summarize(rt_quantile = quantile(rt, probs = seq(0,1,40)))

x = D1$FWHM
get_quantiles = function(x){
  percent = seq(0, 1, length.out = 40)
  tibble(percent  = percent,
         quantile = quantile(x, probs = percent) )
}

helpful_table = 
  D1 %>% 
  group_by(run) %>% 
  do( get_quantiles(.$FWHM) ) %>% 
  ungroup

# Common quantiles: otherwise per-run quantiles differn and we cannot easily order them
prob_cut_offs = seq(0, 1, length.out = 40)
common_quantiles = quantile(D1$rt,
                            prob_cut_offs, 
                            include.lowest = T,
                            ordered_result = T)
D1 = D1 %>% 
  group_by(run) %>%
  mutate(rt_quantile = cut(rt, common_quantiles,
                           include.lowest = T, 
                           ordered_result = T)) %>%
  ungroup

D1 %>% 
  ggplot(aes(x = rt_quantile, y = FWHM)) + 
  geom_boxplot() +
  facet_grid(run ~ .) + 
  theme_tufte() +
  geom_hline(yintercept = 0, color = 'red')

# looks like no difference between these distributions, really.
