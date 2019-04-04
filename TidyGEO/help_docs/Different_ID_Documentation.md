## Using a different variable as the ID column

The feature data that comes alongside the assay data is useful in annotating the values in the ID column. This makes these values more helpful in a specific research context. For example, if there were probe IDs in the ID column, it might be more useful to see the data as it corresponds to gene IDs, so we would map the probe IDs to the gene IDs to create a more informative ID.

Sometimes, columns in the feature data will not contain as many unique entries as rows in the assay data, so when mapping the assay IDs to a column in the feature data, it may be necessary to **summarize** the values that belong to a single ID. For example, if in the assay data, probes 1, 2, and 3 all correspond to gene A, it would be necessary to somehow combine the data from probes 1, 2, and 3 to get one value for gene A.

**Here is an example of using a different ID with GSEXXXX:**
