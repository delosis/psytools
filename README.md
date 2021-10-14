Derivations for Psytools tasks
==============================

R library to process Psytools datasets exported as CSV files
from the [Delosis server](https://www.delosis.com).

The library contains a generic function to rotate simple questionnaires
from long to wide format and specific functions to aggregate and derive
data from more complex questionnaires such as:
- BART (Balloon Analogue Risk Task)
- CORSI (Corsi block-tapping test)
- DS (Digit Span Task)
- ERT (Emotion Recognition Task)
- ESPAD questionnaire (European School Survey Project on Alcohol and Other Drugs)
- IFCVS (Indian Family Violence and Control Scale)
- IRI (Interpersonal Reactivity Index)
- MAST (Michigan Alcoholism Screening Test)
- KIRBY (Now-or-later test)
- MID (Monetary Incentive Delay)
- NEO-FFI (NEO Five-Factor Inventory)
- SOCRATIS (Social Cognition Rating Tools in Indian Setting)
- SST (Stop Signal Task)
- SURPS (Substance Use Risk Profile Scale)
- TCI (Temperament and Character Inventory)
- TMT (Trail Making Test)
- WCST (Wisconsin Card Sorting Test)

Developer's notes
-----------------
After adding or modifying a function, remember to regenerate the documentation and NAMESPACE with `devtools::document()`.
