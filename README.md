# Remunicipalization, Corporatization, and Outsourcing: The Performance of Public-Sector Firms after Reorganization

**author:** Caroline Stiel

Research article published in Stiel, C. (2023): [Remunicipalization, Corporatization, and Outsourcing: The Performance of Public-Sector Firms after Reorganization.](https://doi.org/10.1080/10967494.2022.2038317) International Public Management Journal. 26(4). pp. 463-488.

The empirical analysis was done in `R`. In this repository you find all statistical programs to conduct the analyses in the paper.

## Summary

_In advanced economies, public firms play an important role in sectors of general interest such as energy and water supply. The conditions under which they operate in the EU have changed fundamentally since 1998, with new strategies required for firms to preserve market shares in the face of liberalization and technological innovation. The paper investigates the impact of reorganization on productivity within public firms addressing the owners' composition, the board-management relationship, and the management's decision to outsource activities. Considering a large panel of 2,325 German municipally-owned firms between 2003 and 2014, this study estimates firm-level productivity based on a control function approach. Contrary to public choice theory, I do not find support for the hypothesis that an increase in government owners' control over the firm, either through shares or the board's control rights, decreases productivity. Looking at reorganization invoked by the management, I find that outsourcing increases productivity, where effects are strongest for service outsourcing._

## Methods and data

### Firm-level data

I use official microdata from the _Federal Statistical Office_ consisting of

- balance sheet and product data [_(AFiD)_](https://gitlab.com/modern-state-owned-firms/data/afid-data) from German public utilities covering the years 2003-14
- as well as data on public firms in Germany [_(JAB)_](https://gitlab.com/modern-state-owned-firms/data/afid-data) and data from the [German business register](https://doi.org/10.21242/52121.2014.00.00.1.1.0).

I merge the firm-level data with 

-  data on the [German economy](https://gitlab.com/modern-state-owned-firms/data/data-german-economy).

See [gitlab.com/modern-state-owned-firms/data](https://gitlab.com/modern-state-owned-firms/data) for more information on the data sources and the linkage strategy to merge all the data sources.

### Methods

I apply different statistical methods including 

- descriptive analyses
- panel data econometrics (GMM, structural estimation)
- bootstrap
- hypothesis testing.

## Further reading

The article is part of the research project _Modern state-owned firms' performance - An empirical analysis of productivity, market power and innovation_ financed by the German Science Foundation (DFG, 2019-2023). Visit https://modern-state-owned-firms.gitlab.io/landing-page/ for more information on the project and related research in this field.

