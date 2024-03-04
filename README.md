# eGRID

This repository includes R scripts to create the Emissions & Generation Resource Integrated Database (eGRID).

## Background

eGRID is a comprehensive source of data from EPA's Clean Air Markets Division on the environmental characteristics of almost all electric power generated in the United States. The preeminent source of emissions data for the electric power sector, eGRID is based on available plant-specific data for all U.S. electricity generating plants that provide power to the electric grid and report data to the U.S. government. Data reported include, but are not limited to, net electric generation; resource mix (for renewable and nonrenewable generation); mass emissions of carbon dioxide (CO2), nitrogen oxides (NOx), sulfur dioxide (SO2), methane (CH4), nitrous oxide (N2O), and mercury (Hg); emission rates for CO2, NOx, SO2, CH4, and N2O; heat input; and nameplate capacity. eGRID reports this information on an annual basis (as well as by ozone season for NOx) at different levels of aggregation.

The data includes emissions, emission rates, generation, heat input, resource mix, and many other attributes. eGRID is typically used for greenhouse gas registries and inventories, carbon footprints, consumer information disclosure, emission inventories and standards, power market changes, and avoided emission estimates.

The final eGRID database includes eight levels of data aggregation:

-   Unit

-   Generator

-   Plant

-   State

-   Balancing authority

-   eGRID subregion

-   NERC region

-   US

## Data Sources

### CAMD's Power Sector Emissions Data (EPA/CAMD)

CAMD’s Power Sector Emissions Data (EPA/CAMD): this includes data reported to EPA by 
electric generating units to comply with the regulations in 40 CFR Part 75 and 40 CFR Part 
63. Data include annual emissions of CO2, NOx, SO2, and Hg; ozone season emissions of 
NOX; and annual and ozone season generation and heat input.

### EIA-860

This includes data reported to EIA on electric generators. Data include nameplate 
capacity, prime mover, primary fuel type, and indication of whether the generator is a 
combined-heat-and-power unit.

+----------------------------------------------+
| EIA-860 Tables                               |
+==============================================+
| EIA-860 Boiler Generator                     |
+----------------------------------------------+
| EIA-860 Boiler Info & Design Parameters      |
+----------------------------------------------+
| EIA-860 Boiler Mercury                       |
+----------------------------------------------+
| EIA-860 Boiler NOx                           |
+----------------------------------------------+
| EIA-860 Boiler PM                            |
+----------------------------------------------+
| EIA-860 Boiler SO2                           |
+----------------------------------------------+
| EIA-860 Emission Standards & Strategies      |
+----------------------------------------------+
| EIA-860 Emissions Control Equipment          |
+----------------------------------------------+
| EIA-860 EnviroEquip - FGD                    |
+----------------------------------------------+
| EIA-860 Operable                             |
+----------------------------------------------+
| EIA-860 Owner                                |
+----------------------------------------------+
| EIA-860 Plant                                |
+----------------------------------------------+
| EIA-860 Proposed                            |
+----------------------------------------------+
| EIA-860 Retired and Canceled                 |
+----------------------------------------------+
| Puerto Rico Data                             |
|                                              |
| -   EIA-860M Generator December Operating PR |
|                                              |
| -   EIA-860M Generator December Planned PR   |
|                                              |
| -   EIA-860M Generator December Retired PR   |
+----------------------------------------------+


### EIA-923 

This includes data reported to EIA on fuel consumption and generation. Data 
include monthly generation and heat input at the unit or generator level for a subset of units 
and generators, and at the prime mover level for all plants.


| EIA-923 Tables                          |
|-----------------------------------------|
| EIA-923 8C Air Emissions Control Info   |
| EIA-923 Boiler                          |
| EIA-923 Generation and Fuel             |
| EIA-923 Generation and Fuel Puerto Rico |
| EIA-923 Generator                       |

| EIA-861 Tables                    |
|-----------------------------------|
| EIA-861 Balancing Authority       |
| EIA-861 Sales Ult Cust            |
| EIA-861 Utility Data - with count |



## Output
