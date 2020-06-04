# MA_COVID-19

This repository is for analyzing and visualizing COVID-19 data for the state of Massachusetts, with a particular focus on municipality (city or town) data.

Statewide, weekly COVID-19 case and rate data have been pulled from the MA COVID-19 dashboard (https://www.mass.gov/info-details/covid-19-response-reporting#covid-19-cases-by-city/town-). These files names are of the format covid-19-city-town-M-DD-YYYY.txt and data are tab-delimitted files.

Scripts for plotting weekly Massachusetts city/town data are provided in the following R scripts:
MA_city_COVID_plots.R

Note, preliminary analysis. Plot of new cases/day per 100,000 people based on linear regressions of weekly data for the last 3 time points (i.e. weeks). Breaks in colors are according to 1%, 5%, 25%, 75%, 95%, and 99% quartiles. White cities/towns are locations where case counts are <5 and regressions are not calculated.

![New cases/day estimated from regressions of weekly data, last 3 weeks](https://github.com/nahlgren/MA_COVID-19/blob/master/MA_city-town_3weekregression_cases_per_day_percapita_05-20-2020.jpg)

Higher resolution case data is also provided for Worcester and surrounding towns (Grafton, Holden, Lecesiter, Millbury, and Shrewsbury) by scraping data from daily announcement from the city of Worcester, avaiable here: http://www.worcesterma.gov/announcements
Text from these webpages are manually concatenated together in the file all_Worcester_COVID_announcements.txt and case numbers are parsed with the script parse_city_data.pl into individual, tab-delimited files for each city (file name format: NNNNNNNN_city.tsv). Data for West Boylston are compiled manually from announements posted here: https://www.westboylston-ma.gov/home/news/town-administrator-covid-19-updates

Scripts for plotting of Worcester area COVID-19 data are provided in the the following R scripts:

Worcester_COVID.R --> plots cumulative case numbers and daily new cases/day


![Cumulative case numbers and new daily cases/day over time for Worcester](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_COVID_v2.jpg)

Yellow points represent Sundays.


![Worcester COVID-19 daily new cases and hospitalizations](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_COVID_7daymean_cases_hospitalizations.jpg)

Seven day means for COVID-19 daily new cases and COVID-19 positive hospitalizations (all combined medical facilities of Saint Vincent Hospital and UMass Memorial Health) in Worcester, MA. Yellow points represent Sundays.

  
![Worcester COVID-19 daily new cases and deaths](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_COVID_cases_deaths.jpg)

COVID-19 daily new cases and daily new COVID-19 related deaths in all combined medical facilities of Saint Vincent Hospital and UMass Memorial Health) in Worcester, MA. Yellow points represent Sundays.


![Worcester and MA COVID-19 daily new cases](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_MA_daily_new_cases.jpg)
Yet another plot showing daily new cases normalized to population.


Worcester_area_cities_COVID_plots.R --> plots cumulative case numbers for Worcester and surrounding cities


![Cumulative case numbers over time for Worcester and surrounding cities](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_cities_cases.jpg)

![Plot of population density vs. current COVID-19 cases per capita for Worcester and surrounding cities](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_cities_dens_cases.jpg)

Worcester county was lagging initially the timing and strength of decline in new daily COVID-19 cases and deaths compared to other counties and the state as a whole, but in the last few weeks has dropped down to levels similar to other counties.

![Worcester county daily new COVID-19 cases and deaths, 7 day average](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_county_vs_MA_popnorm.jpg)

![Suffolk county daily new COVID-19 cases, 7 day average](https://github.com/nahlgren/MA_COVID-19/blob/master/Suffolk_county_vs_MA_popnorm.jpg)

![Middlesex county daily new COVID-19 cases, 7 day average](https://github.com/nahlgren/MA_COVID-19/blob/master/Middlesex_county_vs_MA_popnorm.jpg)


The same initial lag in decline also occurs for COVID-19 related deaths for Worcester county (needs to be updated).

![Worcester county daily new COVID-19 cases and deaths, 7 day average](https://github.com/nahlgren/MA_COVID-19/blob/master/Worcester_county_vs_MA_deaths.jpg)

![Suffolk county daily new COVID-19 cases and deaths, 7 day average](https://github.com/nahlgren/MA_COVID-19/blob/master/Suffolk_county_vs_MA_deaths.jpg)



