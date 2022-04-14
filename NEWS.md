# sbcdata 0.0

## Changes in 0.0.5

- Adapt "Diagnosis" labels for Leipzig data according to R57.2.

## Changes in 0.0.4

- Add Leipzig 2020-2021 Validation Set.
- Add "Set" column with "Training"/"Validation".
- Adapt to more detailed sender codes.
- Treat severe septic shock 785.52 and R65.2x (ICD9/10-CM) and
  R57.2 (ICD10-GM) as "Sepsis".

## Changes in 0.0.3

- `exclude_entries`: new argument `time` to filter specific time range.

## Changes in 0.0.2

- `import_mimic`: fix `Episode` calculation for `NA` in
  `hadm_id` and `transfer_id`.

## Changes in 0.0.1

- Initial version.
