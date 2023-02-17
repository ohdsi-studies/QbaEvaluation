SELECT
  cohort_definition_id,
	COUNT(DISTINCT subject_id) AS person_count,
	COUNT(*) AS cohort_entry_count,
	MIN(cohort_start_date) AS min_cohort_start_date,
	MAX(cohort_start_date) AS max_cohort_start_date
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id in (@cohort_ids)
GROUP BY cohort_definition_id
;
