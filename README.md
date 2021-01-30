###### Anthony J. Gertz
###### Nashville Software School
###### Midterm R Project

# Comparative Flood Resiliency Across Time for the South Eastern United States
<br>
<br>

## Application's Function
The primary objective of this platform is threefold: 

1.)	Standardize disaster data values making comparisons meaningful, <br>
2.)	Develop a user interface by which NOAA and NWS disaster data is easily accessible and displayed in a manner that makes county-by-county and state-by-state comparisons simple, <br>
3.)	Create a metric by which geographic resiliency can be measured,  <br>
4.) Utilize the metric to investigate locations which appear to have greater or lesser disaster resiliency. <br>
<br>

## Hypothesis
Comparing flood damage to population growth will identify locations of interest, either places where measures are successful at abating flood damage or places of unrestricted development. <br>
<br>
<small><i>Identification of these locations does not directly imply causation, but only points to places of interest where further investigation is warranted.</small></i>
<br>

## Methodology
#### PPI vs. CPI
Disaster damage is often recorded in two metrics, property damage and crop damage. Using crop damage as a metric for geographic comparison would leave any analyst wanting as it is so infrequently and sparsely recorded. However, property damage is (although inaccurate at times) is more comprehensively and diligently recorded. <br> 
<br>
NOAA and the NWS catalog the values that are reported to them by local and state authorities, but do not adjust for inflation. Therefore, comparing these values without adjusting the recorded values for inflation would be irresponsible. <br>
<br>
The available flood data (at a level that is considered ‘complete’) dates back to 1996. Ideally, one would use the Bureau of Labor Statistics Producer’s Price Index (PPI) valuation on residential and commercial construction costs to adjust for increases in prices. However, the PPI only dates back to 2009. <br>
<br>
A comparison of the Bureau of Labor Statistics Consumer Price Index (CPI) to the PPI from 2009 to 2020 was conducted. The variance between the two values never exceeded 2.5%. It was  therefore determined that this error in inflationary index was less important than was including an additional 13 years of disaster data. <br>

#### Geographic Scope
For the initial development of this project, the scope is limited to one geographic region of the United States. The Bureau of Labor Statistics publishes their PPI and CPI data by region, the ‘South’ was used for this version. <br> <br>
Data from Florida and Louisiana was excluded from the disaster database. Both states received an incredibly higher amount of grant funding from the federal government for hurricane mitigation projects. These projects also would help increase resiliency against other types of flooding and were excluded so as to not confound the data. In further iterations of this platform, their inclusion will be considered. <br>
<br>
To offset the loss of the datasets size, Kentucky and West Virginia were added to the dataset used. They are more economically and culturally similar to the rest of the south than are other neighboring states. <br>

#### Resiliency Factor
The attempted metric by which to compare counties and states is labeled “Resiliency Factor.” This number is was computed by comparing population change since the last recorded disaster event. That is: <br>
<br>
The change in population from the last event is multiplied by the last events recorded damage. The result is referred to as ‘Expected Damage.’<br>
<br>
A high resiliency factor would indicate that the selected county or state should have expected much higher levels of property damage than it actually incurred. A low value would indicate the opposite.<br>
<br>

## Limitations
There exists numerous obvious limitations to this approach. First and foremost, this analysis does not currently take into account the magnitude of the flooding, that is, it’s depth and breadth. This data does not exist in any meaningful or geographically broad way. <br>
<br>
Secondly, the dataset begins in 1996. We’re considering events that occur at 10, 50, 100, and 500 year intervals. This limitation in scope certainly confounds one’s ability to draw significantly meaningful conclusions from the determined resiliency factor. <br>
<br>


### Data Sources:
National Oceanic and Atmospheric Administration, National Center for Environmental Information <br>
&nbsp; -Storm Events (1996-2020, "Flash Flood") <br>
&nbsp; -Storm Events (1996-2020, "Flood") <br>
United States Bureau of Labor Statistics <br>
&nbsp; - Consumer Price Index (1996-2019 - Southern Region) <br>
&nbsp; - Producers Price Index (2009-2019 - Southern Region) <br>
United States Census Bureau - Demographics (1996-2019) <br>
<br>

### R Packages: 
shiny <br>
shinydashboard <br>
dashboardthemes <br>
tidyverse <br>
stringr <br>
data.table <br>
plotly <br>
ggplot2 <br>
scales <br>


