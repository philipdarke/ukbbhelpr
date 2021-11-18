# ukbbhelpr

A collection of helper functions for working with [UK Biobank](https://www.ukbiobank.ac.uk/) data, focussing on the linked primary care EHR data.

## Installation

Install from GitHub using:

```r
devtools::install_github("philipdarke/ukbbhelpr", dependencies = TRUE)
```

## Reference

The main functionality is summarised below. See the [manual](ukbbhelpr_0.1.0.pdf) for more details.

Functions for use with data collected at UK Biobank assessment centre visits start `visit_`. Functions for use with primary care EHR data start `ehr_`.

### `ehr_extract(ehr_data, read_codes)`

Extracts observations/test results from linked EHR data (`ehr_data`) from records matching the provided `read_codes`. Data is extracted from the `value1` field with the exception of data provider 2 where values are extracted from `value2` if `value1` is empty. Units are taken from `value3` for data provider 2 (otherwise units are unavailable). `NA`, zero and duplicate values are dropped.

### `visit_extract(visit_data, field)`

Extracts all instances/arrays of a UK Biobank field(s) from `visit_data`. See https://biobank.ndph.ox.ac.uk/showcase/ to identify `field` codes.

### `visit_conditions(visit_data)`

Extracts self-reported non-cancer medical history from `visit_data` in a "long" format that is easier to work with than "wide" as provided by UK Biobank.

### `visit_cancer(visit_data)`

Extracts self-reported cancer history from `visit_data` in a "long" format that is easier to work with than "wide" as provided by UK Biobank.

### `visit_family_history(visit_data, fields, condition)`

Determines presence of a specified `condition` in the self-reported family history data (`visit_data`). If multiple history `fields` are provided (e.g. history of mother and father), presence of the condition in any field determines a positive family history.

## Licence

Made available under the [MIT Licence](https://mit-license.org/).
