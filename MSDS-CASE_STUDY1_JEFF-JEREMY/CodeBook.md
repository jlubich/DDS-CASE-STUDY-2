CodeBook for MSDS 6306: CaseStudy 01
====================================


Original Data
---------------
The original dataset was obtained by GPIF from Openbeerdb.com 
- [Openbeer.com](https://openbeerdb.com/)
The databases were made available under the Open Database License agreement.
Note: The original databases were altered by GPIF prior to submission to CDS.


Conventions followed
--------------------
Processing code and dataset variable naming follows the conventions described in 
[Google R Styde Guide](http://google-styleguide.googlecode.com/svn/trunk/Rguide.xml).


Dataset(s)
------------
Beers dataset (Beers.csv)  
Breweries dataset (Breweries.csv)  
US State/Regions dataset (US_Regions.csv)


Raw Dataset(s)
------------
Beers dataset (Beers.csv):
The Beers dataset contains a list of 2410 US craft beers currently being produced in the US, along
with other pertinent data (Beer ID, Alcohol content (ABV), International Bitterness Units (IBU),
Brewery ID, Style, and Ounces).

Breweries dataset (Breweries.csv):
The Breweries dataset contains a list of 558 craft beer Breweries in the US, along with other pertinent
data (Brew_ID, City and State of headquarters)

US State/Region dataset (US_Regions.csv)
The US State/Region dataset contains a list of 50 US states and District of Columbia, along with other
pertinent data (region, divisions).  The dataset was created by CDS, following 
Census_Regions_and_Division_of_the_United_States map.
- [source](https://commons.wikimedia.org/wiki/File:Census_Regions_and_Division_of_the_United_States.svg)


Tidy Dataset
-----------------
Tidy data set contains merged datasets (Beers.csv | Breweries.csv), using Brewery_id as the unique
identifier.
Original variable names were modified in the following way:


#### Variable of Tidy data names compared to Raw data variable names
 
 |DataFile Name  |Raw data            | Tidy data       | Description                                |
 |---------------|--------------------|-----------------|--------------------------------------------|
 |Beers.csv      |`Name`              | `Name`          | Name of Beer                               |
 |Beers.csv      |`Beer_ID`           |`Beer_ID`        | Unique identifier of the beer              |
 |Beers.csv      |`ABV`               |`ABV`            | Alcohol by volume of the beer              |
 |Beers.csv      |`IBU`               |`IBU`            | International Bitterness Units of the beer |
 |Beers.csv      |`Brewery_id`        |`Brewery_id`     | Brewery ID associated with the beer        |
 |Beers.csv      |`Style`             |`Style`          | Style of the beer                          |
 |Beers.csv      |`Ounces`            |`Ounces`         | Ounces of the beer                         |
 |Breweries.csv  |`Brew_ID`           |`Brewery_id`     | Unique identifier of the brewery           |
 |Breweries.csv  |`Name`              |`Brewery Name`   | Name of the Brewery                        |
 |Breweries.csv  |`City`              |`City`           | City where the brewery is located          |
 |Breweries.csv  |`State`             |`State`          | U.S. State where the brewery is located    |

Final Deliverable
-----------------
The purpose of this consultancy agreement was to gather, clean and summarize data sets presented to CDS
by Lifestyle Investments, which contained Brewery and Beer data from US breweries, to assist GPIF in 
developing strategies and market plans focused on investing in craft breweries within the US.