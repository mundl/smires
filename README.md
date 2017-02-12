smires
================

SMIRES is a COST Action addressing the Science and Management of Intermittent Rivers & Ephemeral Streams. SMIRES brings together more than 200 hydrologists, biogeochemists, ecologists, modellers, environmental economists, social researchers and stakeholders from 31 different countries to develop a research network for synthesising the fragmented, recent knowledge on IRES, improving our understanding of IRES and translating this into a science-based, sustainable management of river networks. More information about SMIRES can be found at <http://www.smires.eu/>.

This git repository hosts the R-package `smires`, one of several outcomes of Working Group 1 (WG1, Prevalence, distribution and trends of IRES). Given time series of daily (weekly, monthly) discharges, its purpose is:

-   to identify gauging stations that have an IRES flow regime;
-   to calculate relevant flow and intermittency metrics.

Installation of the R-package
=============================

In order to use the development version of the package `smires` you will need to install the package `devtools` once.

``` r
install.packages("devtools")
```

It provides the convenient commant `install_github()` which installs the most recent version of a package hosted on github. To do so, simply execute the following three lines:

``` r
library(devtools)
install_github("mundl/smires")
library(smires)
```

The current version is `0.3.0`. To load this package in an R session, `library(smires)` has to be called.

``` r
library(smires)
packageVersion("smires")
```

Examples
========

Each participating country was asked to suggest metrics and to submit a few time series with intermittent streamflow. As we plan to integrate the provided time series in the R package, we need permission of the copyright holder to make the data availabe. The column **include** of the following table indicates if the data can be published.

### Time Series

<table>
<colgroup>
<col width="3%" />
<col width="12%" />
<col width="55%" />
<col width="3%" />
<col width="25%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">country</th>
<th align="left">contact</th>
<th align="left">time series</th>
<th align="left">include</th>
<th align="left">comment</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">ch</td>
<td align="left"><a href="mailto:ilja.vanmeerveld@geo.uzh.ch">ilja.vanmeerveld@geo.uzh.ch</a></td>
<td align="left">Altlandenberg</td>
<td align="left">FALSE</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">es</td>
<td align="left"><a href="mailto:francesc.gallart@idaea.csic.es">francesc.gallart@idaea.csic.es</a></td>
<td align="left">Riu Manol</td>
<td align="left">FALSE</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">fr</td>
<td align="left"><a href="mailto:eric.sauquet@irstea.fr">eric.sauquet@irstea.fr</a></td>
<td align="left">H1333010, H1503910, H1513210, H1603010, H1713010, H1932020</td>
<td align="left">FALSE</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">it</td>
<td align="left"><a href="mailto:giuseppe.verdiani@adb.puglia.it">giuseppe.verdiani@adb.puglia.it</a></td>
<td align="left">Carapelle Torrent</td>
<td align="left">FALSE</td>
<td align="left">Region Puglia</td>
</tr>
<tr class="odd">
<td align="left">it</td>
<td align="left"><a href="mailto:annamaria.degirolamo@ba.irsa.cnr.it">annamaria.degirolamo@ba.irsa.cnr.it</a></td>
<td align="left">Celone, Salsola</td>
<td align="left">FALSE</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">lt</td>
<td align="left"><a href="mailto:hydro@mail.lei.lt">hydro@mail.lei.lt</a></td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">pl</td>
<td align="left"><a href="mailto:kazimierz_banasik@sggw.pl">kazimierz_banasik@sggw.pl</a></td>
<td align="left">Plachty Stare</td>
<td align="left">FALSE</td>
<td align="left">uses hydrological year, starting with November</td>
</tr>
<tr class="even">
<td align="left">pl</td>
<td align="left"><a href="mailto:rmrutkow@cyf-kr.edu.pl">rmrutkow@cyf-kr.edu.pl</a></td>
<td align="left">Goryczkowa</td>
<td align="left">FALSE</td>
<td align="left">only 3 years of observation</td>
</tr>
<tr class="odd">
<td align="left">pt</td>
<td align="left"><a href="mailto:teresal@ipcb.pt">teresal@ipcb.pt</a></td>
<td align="left">Coruche, Monforte, Pavia, Moinho</td>
<td align="left">FALSE</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">uk</td>
<td align="left"><a href="mailto:catsef@ceh.ac.uk">catsef@ceh.ac.uk</a></td>
<td align="left">Balder at <a href="http://nrfa.ceh.ac.uk/data/station/info/25022">Balderhead Reservoir</a>, Ampney Brook at <a href="http://nrfa.ceh.ac.uk/data/station/info/39099">Ampney St Peter</a></td>
<td align="left">TRUE</td>
<td align="left">Balder: human influence, Ampney Brook: chalk stream that dries naturally</td>
</tr>
</tbody>
</table>

### Metrics

| metric                        | type      | description                                                                                   | Δt    | comment | at  | ch  | gb  | es  |
|:------------------------------|:----------|:----------------------------------------------------------------------------------------------|:------|:--------|:----|:----|:----|:----|
| Number of days with zero flow | duration  | Total number of days with zero flow                                                           | daily | NA      | ✔   | ✔   | ✔   |     |
| Recession constant            | magnitude | Recession constant during the period with the lowest flow or slope of the flow duration curve | daily | NA      |     | ✔   |     | ✔   |

Getting in Contact
==================

In case you are interested or you want to contribute to the package `smires` (even though you are not part of the [SMIRES cost action](http://www.smires.eu/)) please contact <t.gauster@boku.ac.at>.
