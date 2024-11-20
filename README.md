# eGRID
This repository includes all necessary scripts and documentation to create the Emissions & Generation Resource Integrated Database (eGRID). 

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

## Creating eGRID

This project is structured as an R Studio project. To ensure that all scripts run correctly, load the `eGRID_R.Rproj` within RStudio to enable the project environment. `eGRID_master.qmd` is a Quarto document that serves as a master script (i.e., it runs all necessary scripts in the correct order), while also providing documentation for the scripts and steps performed therein. To create eGRID, simply execute all code chunks in `eGRID_master.qmd`, which can be accomplished by rendering the document as an html document.

## Reporting Issues 


