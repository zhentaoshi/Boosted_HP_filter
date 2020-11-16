
### HP Filter application 1 of Mark Aguiar and Gita Gopinath


## source


* we apply the idea and data of the paper:
* Emerging Market Business Cycles: The Cycle Is the Trend
* Author(s): Mark Aguiar and Gita Gopinath: Journal of Political Economy, Vol. 115, No. 1 (February 2007), pp. 69-102
* the source of original data  is : http://www.markaguiar.com/papers/cycle_is_trend/datapage.html

## data

* Using "VBA_code" to separate the work sheet of original data file "data_cycle_is_trend.xls" and save them as "CSV" files which are stored in the document "separated_countries_data".

* there are 26 countries' time series data. 13 are of emerging markets, and the other 13 are of developed markets.

## key moments

* the main code is "master.R" which sourced file "function_hp_all.R".
* the results can be seen in files "moments_format_summary.xlsx", "results_moments.RData","moments_format_median.xls".
* for detail report of each countries' key moments, please see "statistics report for each country.pdf".

## graph

* the main graph is "cyc_all.jpg".
* the main code for graph is "ggplot_graph_final.R" using the data in "results_moments.RData".
* the graph historical data are saved in "graph_data_final.RData".
* more details are saved in "graph_data_alter.RData".
