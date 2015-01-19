This repository contains code to build cosponsorship networks from bills passed in the [lower][ch] and [upper][se] chambers of the Belgian Parliament.

- [interactive demo](http://briatte.org/belparl)
- [static plots](http://briatte.org/belparl/plots.html)

[ch]: http://www.lachambre.be/
[se]: http://www.senate.be/

# HOWTO

Replicate by running `make.r` in R.

The `data-ch.r` script deals with the lower house; it will scrape all sponsors for legislatures 48-54 and all dossiers for legislatures 47-53. The first and last legislatures are then excluded from the network building routine in `build-ch.r`: there are no sponsor details for legislature 47, and not enough bills in legislature 54, which started in late 2014.

The `data-se.r` and `build-se.r` scripts carry the same operations as above for the upper house, starting with legislature 49 (i.e. one less legislature than the lower house) and ending with legislature 53. The download procedure is a bit more straightforward, but there are less data overall, and legislature 48 has no variance in seniority (see below).

Note that, for both chambers, two steps are necessary to pass the raw data to the network building routine. The `dossiers*.csv` are preprocessed versions of the `sponsors*.csv` bills datasets, where `*` is a two-digit legislature number: after adding new raw data, the preprocessed data should be refreshed by setting `update` to `TRUE` in `make.r`.

There are missing photos in both houses, so the download scripts will report a few 404 errors on every run.

The `build-ch.r` and `build-se.r` scripts assemble the edge lists and plot the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters in `make.r` to skip the plots or to change the node placement algorithm.

# DATA

## Bills

The bills data are quite different for each chamber. A common point is that there is no precise information on the date of introduction of the bills, just the legislature of introduction.

Chambre:

- `uid` -- unique identifier (`dossier` + unique number)
- `dossier` -- dossier id (first two digits are the legislature number)
- `document` -- document number (_dossiers_ have multiple items)
- `type` -- type of item, recoded to bills, amendments and others
- `topic` -- keywords (~ 2200 over the complete data, none missing)
- `status` -- outcome (simplified to adopted, rejected or missing)
- `n_a` -- total number of sponsors
- `authors` -- list of first authors, with hardcoded party affiliations
- `cosponsors` -- list of cosponsors, with hardcoded party affiliations
- `n_au` -- number of first authors
- `n_co` -- number of cosponsors

Sénat:

- `uid` -- unique identifier (`dossier` + unique number)
- `dossier` -- dossier id (first two digits are the legislature number)
- `document` -- document number (_dossiers_ have multiple items)
- `type` -- type of item, recoded to bills, amendments and others
- `topic` -- keywords (~ 58,000 over the complete data, none missing)
- `status` -- outcome (simplified to adopted, rejected or missing)
- `authors` -- list of sponsors as numeric ids (variable `sid` in sponsors data)
- `n_au` -- total number of sponsors (usually limited to 9 or 10)

## Sponsors

Chambre:

- `legislature` -- legislature number (int)
- `nom` -- name
- `mandate` -- legislatures and years, used to compute the `nyears` seniority variable
- `photo` -- photo URL, shortened to filename number
- `url` -- profile URL, shortened to numeric id
- `bio` -- biography text
- `sexe` -- gender (F/M), imputed from birth information: "Né(e)"
- `annee_naissance` -- year of birth (int)
- `constituency` -- constituency, stored as the string to its Wikipedia Francophone entry

Party affiliations are not in the data because they are hardcoded in the sponsorships.

Sénat:

- `sid` -- profile URL, shortened to numeric id
- `nom` -- name
- `mandate` -- mandate year ranges, used to compute the `nyears` seniority variable
- `from` and `to` -- minimum and maximum values, extracted from `mandate`
- `parti` -- party affiliation, recoded as explained in the next section
- `photo` -- photo URL, shortened to filename number
- `sex` -- gender (F/M), imputed from photos and first names
- `born` -- year of birth (int), imputed from Wikipedia when missing

The `mandate` variable does not capture mandate years before 1995. As a result, imputed seniority for senators sitting in legislature 48 has null variance.

Fun fact -- there are two "Philippe Charlier" in the data, and both are born in 1951.

## Parties

There are many political parties in Belgium, with many name changes during the sample period. The code uses simplified groups that preserve the main political families and the two linguistic groups:

- parties are replaced with acronyms for Flemish (V) and Francophone (F) groups:
  - Socialists: SP, sp.a, sp.a-SPIRIT, Vl.-Pro, (SOC-V), PS (SOC-F)
  - Christian-Democrats: CVP, CD&V (CDEM-V), PSC, cdH (CDEM-F)
  - Liberals: PVV, VLD, Open Vld (LIB-V), PRL, PRL-FDF, MR (LIB-F)
- Flemish nationalists are coded in two different ways:
  - Volksunie, previous VU/VU-ID and now N-VA, is coded "VOLKS"
  - the lower chamber CD&V + Volksunie coalition of legislature 52 is coded "CDEM-V/VOLKS"
- Greens from both linguistic areas work together and are coded as a single group:
  - Flemish Greens (Agalev-Ecolo, Ecolo-Groen!) are coded "ECOLO"
  - Francophone Greens (Écologistes) are also coded "ECOLO"
- a few minor parties are coded separately:
  - "LDD" stands for "List Dedecker", now "Libertarian, Direct, Democratic"
  - "ROSSEM", Jean-Pierre Van Rossem's list, appears once in legislature 48 of the lower chamber
  - the (Francophone) Front national, coded "FN", appears once in legislature 51 of the Senate

Including the minor parties in the modularity computations does not affect the results (checked at two digits precision on all sample legislatures).

|abbreviation  |full.name                         |
|:-------------|:---------------------------------|
|ECOLO         |Greens                            |
|SOC-V         |Francophone Conservatives         |
|SOC-F         |Flemish Conservatives             |
|CDEM-V        |Flemish Conservatives + Volksunie |
|CDEM-F        |Francophone Liberals              |
|CDEM-V/VOLKS  |Flemish Liberals                  |
|LIB-F         |Francophone Socialists            |
|LIB-V         |Flemish Socialists                |
|VOLKS         |Front National                    |
|VLAAMS        |ROSSEM                            |
|FN            |Vlaams Blok                       |
|LDD           |Volksunie                         |
|ROSSEM        |Libertair, Direct, Democratisch   |
|IND           |Independent                       |

# CREDITS

Thanks to [Bram](https://twitter.com/brabram) for feedback.
