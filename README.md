smires
================

SMIRES is a COST Action addressing the Science and Management of Intermittent Rivers & Ephemeral Streams. SMIRES brings together more than 200 hydrologists, biogeochemists, ecologists, modellers, environmental economists, social researchers and stakeholders from 31 different countries to develop a research network for synthesising the fragmented, recent knowledge on IRES, improving our understanding of IRES and translating this into a science-based, sustainable management of river networks. More information about SMIRES can be found at <http://www.smires.eu/>.

This git repository hosts the R-package `smires`, one of several outcomes of Working Group 1 (WG1, Prevalence, distribution and trends of IRES). Given time series of daily (weekly, monthly) discharges, its purpose is:

-   to estimate if it was observed at an intermittent river,
-   to calculate relevant low flow and drought metrics.

Installation of the R-package
=============================

In order to use the development version of the package `smires` you will need to install the package `devtools`.

``` r
install.packages("devtools")
install_github("mundl/smires")
```

Examples
========

Each participating country was asked to suggest metrics and to submit a few time series with intermittent streamflow.

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
<td align="left">pt</td>
<td align="left"><a href="mailto:teresal@ipcb.pt">teresal@ipcb.pt</a></td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">uk</td>
<td align="left"><a href="mailto:catsef@ceh.ac.uk">catsef@ceh.ac.uk</a></td>
<td align="left">Balder at <a href="http://nrfa.ceh.ac.uk/data/station/info/25022">Balderhead Reservoir</a>, Ampney Brook at <a href="http://nrfa.ceh.ac.uk/data/station/info/39099">Ampney St Peter</a></td>
<td align="left">TRUE</td>
<td align="left">Balder: human influence, Ampney Brook: chalk stream that dries naturally</td>
</tr>
</tbody>
</table>

### Metrics
