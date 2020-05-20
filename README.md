# MA_COVID-19

This repository is for analyzing and visualizing COVID-19 data for the state of Massachusetts, with a particular focus on municipality (city or town) data.

Statewide, weekly COVID-19 case and rate data have been pulled from the MA COVID-19 dashboard (https://www.mass.gov/info-details/covid-19-response-reporting#covid-19-cases-by-city/town-). These files names are of the format covid-19-city-town-M-DD-YYYY.txt and data are tab-delimitted files.

Scripts for plotting weekly Massachusetts city/town data are provided in the following R scripts:
MA_city_COVID_plots.R

Higher resolution case data is also provided for Worcester and surrounding towns (Grafton, Holden, Lecesiter, Millbury, and Shrewsbury) by scraping data from daily announcement from the city of Worcester, avaiable here: http://www.worcesterma.gov/announcements
Text from these webpages are manually concatenated together in the file all_Worcester_COVID_announcements.txt and case numbers are parsed with the script parse_city_data.pl into individual, tab-delimited files for each city (file name format: NNNNNNNN_city.tsv). Data for West Boylston are compiled manually from announements posted here: https://www.westboylston-ma.gov/home/news/town-administrator-covid-19-updates

Scripts for plotting of Worcester area COVID-19 data are provided in the the following R scripts:
Worcester_COVID.R --> plots cumulative case numbers and daily new cases/day

![Cumulative case numbers and new daily cases/day over time for Worcester](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_COVID_v2.jpg)


Worcester_area_cities_COVID_plots.R --> plots cumulative case numbers for Worcester and surrounding cities

![Cumulative case numbers over time for Worcester and surrounding cities](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_cities_cases.jpg)

![Plot of population density vs. current COVID-19 cases per capita for Worcester and surrounding cities](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_cities_dens_cases.jpg)
