# sbcdata 0.1

## Changes in 0.1.1

- Use lower-case "sepsis" in exclusion messages.

## Changes in 0.1.0

- Rename `exclude_entries` into `sbc_exclude_entries`.
- Add `sbc_label` for labeling into Sepsis/Control cases.
- Add `sbc_preprocess` as wrapper for `sbc_exclude_entries` and `sbc_label`.

# sbcdata 0.0

## Changes in 0.0.7

- Integrate new data query from Greifswald.

## Changes in 0.0.6

- Treat SIRS caused by an infection as "Sepsis" (ICD10-GM R65.1).

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
