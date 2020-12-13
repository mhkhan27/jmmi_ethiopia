# jmmi_ethiopia

This repository contains the scripts for data cleaning and analysis of the JMMI Ethiopia dataset.

### Steps

1. Export the dataset from Kobo (Select export type: XLS; Value and header format: XML values and headers; Include groups in header: FALSE)

2. Rename the file as YYYYMMDD_data_submission.xlsx using the export date and move the file into *data/*

3. Open config.R and update the parameters as requested based on the current assessment (e.g. assessment month, tool filename, raw data filename, etc.)

3. Run data_checking.R

4. Send the follow-up requests in *output/fu_requests/* to the partners by email

5. Save the received follow-up responses in *output/fu_responses/*

6. Run data_editing.R

7. Run data_analysis.R

### Notes

#### Survey duration
The script flags the surveys with a duration shorter than 10 minutes. To be noted that such a short duration is possible for surveys collected on paper and only later entered in Kobo, especially for vendors selling only 1 type of items. Audit files could be used as well to check how the enumerators filled the surveys.

#### GPS check
This check flags the surveys for which the reported GPS location is not within the reported Woreda. In such cases, a cleaning log is created to correct the Woreda with the one based on the GPS location.
Since often surveys are collected on paper and entered in Kobo possibly in another location (enumerator's office or home), by default this check does not apply any changes to the dataset.