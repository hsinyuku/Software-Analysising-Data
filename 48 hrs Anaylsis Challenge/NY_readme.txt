1. Title: NY Weather and Traffic

2. Sources:
	(a) Creator: Henning Feller, Editor: Lisa Gotzian
	(b) Retrieved from:
		The National Oceanic and Atmospheric Administration
		BetaNYC, Google BigQuery
	(c) Generated: January 2018

3. Past Usage:
	evaluating the impact of weather on traffic in the city of New York

4. Relevant Information:
	The dataset resembles data on both weather (20 attributes) and traffic
	(5 attributes) in New York from July 2013 till December 2016. The traffic
	data has been gathered from various sources.

5. Number of Instances: 1280 days

6. Number of Attributes: 32

7. Attribute Information:

Colname	Type Description 
DATE	Date	Date
WEEKDAY	Factor	Weekday
HOLIDAY	Logical	Indicating whether or not this date was a holiday
MONTHYEAR Factor Month and Year (Format YYYY-MM)
YEAR	Numeric	Year
Month	Factor	Month
DAYMONTH Factor	Day and Month without year
WEEKEND	Logical	Indicating whether or not this date was a weekend
AWND	Numeric	Average wind speed (Miles per Hour)
PRCP	Numeric	Precipitation (Inch)
PRCP_LVL Factor	Precipitation split into 4 groups, None: x = 0, Slight: 0<x<=10,
		Moderate: 10<x<=40, Heavy: 40<x
SNOW	Numeric	Snow fall (Inch)
SNWD 	Numeric	Snow depth (Inch)
TAVG	Numeric	Average temperature (Fahrenheit)
TMAX	Numeric	Maximum temperature (Fahrenheit)
TMIN	Numeric	Minimum temperature (Fahrenheit)
WDF2	Integer	Direction of fastest 2-minute-wind (degrees)
WDF5	Integer	Direction of fastest 5-second-wind (degrees)
WSF2	Numeric	Fastest 2-Minute wind Speed
WSF5	Numeric	Fastest 5-second wind speed
WT01	Logical	Weather Type: Fog, ice fog, or freezing fog (may include heavy fog)
WT02	Logical	Weather Type: Heavy fog or heaving freezing fog
WT03	Logical	Weather Type: Thunder
WT04	Logical	Weather Type: Ice pellets, sleet, snow pellets, or small hail
WT06	Logical	Weather Type: Glaze or rime
WT08	Logical	Weather Type: Smoke or haze
WT09	Logical	Weather Type: Blowing or drifting snow
BIKE	Numeric	Number of CitiBike Trips
TAXI	Numeric	Number of Taxi Trips
GREEN	Numeric	Number of Limousine Trips
TRAFFIC	Numeric	Average Traffic Speed
ACCIDENTS Numeric Number of (registered) accidents

8. Missing Attributes:
	Not all variables were available throughout the entire timeframe.
	WDF5: 30 days are missing
	WSF5: 30 days are missing
	BIKE: 96 days are missing
	TRAFFIC: 669 days are missing
	GREEN: 32 days are missing

	In total, there is 857 missing values.
	