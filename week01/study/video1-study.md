# Study Guide — Video 1

## Topic: Extract, Transform, Load (ETL)

------------------------------------------------------------------------

## Learning Objectives

-   Understand the purpose and sequence of the ETL process.
-   Distinguish between raw, unstructured, and structured data.
-   Recognize why transformation is central to analysis.
-   Identify common data sources and loading destinations.

------------------------------------------------------------------------

## Key Takeaways

-   **Extract** = Getting the data from its source (database, website, sensor feed, file, etc.).
-   **Transform** = Cleaning, formatting, reshaping, and standardizing so it’s consistent.
-   **Load** = Bringing the cleaned data into the tool where analysis happens (Excel, R, Tableau, DBs).
-   Most analysis work happens in the **Transform** stage.
-   Data from third‑party sources often requires substantial cleanup.
-   Structured vs. unstructured sources influence how much transformation is needed.

------------------------------------------------------------------------

## Core Concepts

### Extract

Moving data from source → computer or tool. This could be downloads, APIs, database connections, or streaming sources.

### Transform

Ensuring variables are consistent: - date formats\
- numerical types\
- categorical codes\
- missingness\
- pivoting between tall/wide

This enables charts, summaries, and models to behave predictably.

### Load

Final storage in the analysis environment.\
Sometimes data is loaded back to the original DB; sometimes directly into R, Excel, etc.

------------------------------------------------------------------------

## Practice Questions

### Multiple Choice

1.  Which stage of ETL typically requires the most manual work?
    -   

        A)  Extract\

    -   

        B)  Transform\

    -   

        C)  Load\

    -   

        D)  Archive\
            **Correct answer:** B
2.  Which scenario describes *Extraction*?
    -   

        A)  Reformatting dates\

    -   

        B)  Downloading a CSV from the Census Bureau\

    -   

        C)  Creating new variables\

    -   

        D)  Writing a SQL summary\
            **Correct answer:** B
3.  Loading data means:
    -   

        A)  Cleaning and standardizing\

    -   

        B)  Moving data into the analysis tool\

    -   

        C)  Generating summaries\

    -   

        D)  Interpreting charts\
            **Correct answer:** B

### Short Answer

1.  Give an example of unstructured data and why it’s difficult to work with.\
2.  Why is the Transform step essential before analysis?\
3.  Describe a real-world example of ETL from your job or daily life.

------------------------------------------------------------------------

## AI‑Powered Study Prompts

Copy/paste into ChatGPT or Copilot:

-   “Explain ETL to me like I'm brand new to data analytics, using only everyday examples.”
-   “Give me 3 messy data scenarios and ask me how I would transform each one.”
-   “Quiz me with 5 increasingly challenging questions about ETL and explain why each answer is right or wrong.”
-   “Show me a before/after example of transforming poorly formatted data into tidy data ready for analysis.”
