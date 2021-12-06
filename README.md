# ukbbhelpr

A collection of helper functions for working with [UK Biobank](https://www.ukbiobank.ac.uk/) data, focussing on the linked primary care EHR data.

## Installation

Install from GitHub using:

```r
devtools::install_github("philipdarke/ukbbhelpr", dependencies = TRUE)
```

## Reference

The main functionality is summarised below. See the [manual](ukbbhelpr_0.2.0.pdf) for more details.

Functions for use with primary care EHR data start `ehr_`. Functions for use with data collected at UK Biobank assessment centre visits start `visit_`. Functions starting `get_` fetch data e.g. UK Biobank dictionaries.

### EHR data

#### `ehr_extract(ehr_data, read_codes)`

Extracts observations/test results from linked EHR data (`ehr_data`) from records matching the provided `read_codes`. Data is extracted from the `value1` field with the exception of data provider 2 where values are extracted from `value2` if `value1` is empty. Units are taken from `value3` for data provider 2 (otherwise units are unavailable). `NA`, zero and duplicate values are dropped.

### UK Biobank visit data

#### `visit_extract(visit_data, field)`

Extracts all instances/arrays of a UK Biobank field(s) from `visit_data`. See https://biobank.ndph.ox.ac.uk/showcase/ to identify `field` codes.

#### `visit_conditions(visit_data)`

Extracts self-reported non-cancer medical history from `visit_data` in a "long" format that is easier to work with than "wide" as provided by UK Biobank.

#### `visit_cancer(visit_data)`

Extracts self-reported cancer history from `visit_data` in a "long" format that is easier to work with than "wide" as provided by UK Biobank.

#### `visit_family_history(visit_data, fields, condition)`

Determines presence of a specified `condition` in the self-reported family history data (`visit_data`). If multiple history `fields` are provided (e.g. history of mother and father), presence of the condition in any field determines a positive family history.

### Fetch data from other sources

#### `get_hdr_concept(id, version_id = NULL, api = NULL, read = FALSE)`

Downloads and returns a concept from the HDR UK [phenotype library](https://phenotypes.healthdatagateway.org/home/) as a data table. `id` is the concept ID. The most recent version is downloaded unless a `version_id` is specified. `api` can be used to pass a `connect_to_API` object from the [`ConceptLibraryClient`](https://github.com/SwanseaUniversityMedical/ConceptLibraryClient) package.

If the concept is a list of Read v2 codes and `read` is TRUE, additional columns are added to the concept to map the code to CTV3. `read_2` are the 5 digit Read v2 codes and `read_3` are the equivalent CTV3 codes mapped using UK Biobank [mapping dictionaries](https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=592). Read v2 codes shorter than 5 digits are padded with "." characters.

```R
> get_hdr_concept("C1120", read = TRUE)

trying URL 'https://biobank.ndph.ox.ac.uk/showcase/ukb/auxdata/primarycare_codings.zip'
Content type 'application/zip' length 70625113 bytes (67.4 MB)
==================================================
downloaded 67.4 MB

       code                                        description  ...  read_2  term_2  read_3  term_3
 1: C108.12                           Type 1 diabetes mellitus  ...   C108.      12   X40J4   Y41PV
 2: C108.13                           Type I diabetes mellitus  ...   C108.      13   X40J4   Yagv5
 3: C108011  Type I diabetes mellitus with renal complications  ...   C1080      11   C1080   Yagv4
```

A similar function `get_hdr_phenotype(id, version_id = NULL, api = NULL)` is provided to download and return phenotypes.

#### `get_coding(id, overwrite = FALSE)`

Returns the UK Biobank coding table for a data field. The file is downloaded if it is unavailable locally or if `overwrite` is TRUE. Obtain the `id` for the field from the data dictionary for your application, or by looking up the field in the [showcase](https://biobank.ndph.ox.ac.uk/showcase/).

```R
> get_coding(100508)

trying URL 'https://biobank.ctsu.ox.ac.uk/ukb/codown.cgi?id=100508'
Content type 'text/tab-separated-values' length 86 bytes
==================================================
downloaded 86 bytes

   coding              meaning
1:     -3 Prefer not to answer
2:     -1          Do not know
3:      1            Excellent
```

## Licence

Made available under the [MIT Licence](https://mit-license.org/).
