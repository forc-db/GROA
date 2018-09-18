# Cook-Patton(CP)-ForC variable mapping - 

ForC-CP variable mapping is given in [this document](https://github.com/forc-db/ForC_private/blob/master/data%20to%20add/Cook-Patton/CP-ForC%20variable%20mapping.csv). It can be sorted by either CP or ForC variable order.

## Metadata for [mapping document](https://github.com/forc-db/ForC_private/blob/master/data%20to%20add/Cook-Patton/CP-ForC%20variable%20mapping.csv)
field | def 
--- | ---
CP order | field order in SRDB
CP table | SRDB table
CP field | SRDB col heading
CP Description | SRDB variable description
ForC order | order in ForC 
ForC table | ForC table
ForC Column | ForC order within table
ForC field | ForC col heading
FOrC Description | ForC variable desc
FOrC Storage.Type | ForC storage type
ForC Variable.Codes | ForC variable codes (for categorical variables)
ForC Units | ForC units, when applicable
Value to assign - value entered | "value" - enter value as is; "value" included within formula - conversion to apply to numerical values, if any; string (in quotes) or numerical value- enter that value for all. In some cases, value to assign is dependent upon conditions (given in this field).
Value to assign - missing values | value to enter when CP database ahs a missing value (NA).
manual.review.needed | indicates when manual review of values will be needed following conversion to ForC format
Clarifications needed | clarifications needed on the CP database
notes | notes
KAT action needed | required action by KAT
ForC action needed | action required related to ForC database

## General guidance
Each row in CP measurements will often correspond to one row in ForC tables. 
1. Each record gets its own row in MEASUREMENTS
2. Each history event (for each plot; may be more than one plot per site) gets its own row in HISTORY. This is the most complex part. See [Instructions for populating ForC history table from CP.docx]() (to be created).
3. Each methodology / variable gets its own row in METHODOLOGY
4. Each site gets one row in SITES
5. Each citation gets one row in CITATIONS 
