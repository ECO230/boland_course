
# Study Guide — Video 3
## Topic: Tidy Data, Units of Analysis, Data Grouping, Tall/Wide Data

---

## Learning Objectives
- Identify the unit of analysis in any dataset.
- Distinguish cross‑sectional, time‑series, and panel data.
- Understand dimensions vs. measures.
- Recognize tall vs. wide data structures.
- Apply the four principles of tidy data.

---

## Key Takeaways
- **Unit of analysis** = what each row represents.
- **Cross‑sectional** = different subjects at one time.
- **Time‑series** = measurements over time (subjects may differ).
- **Panel** = *same subjects* over time.
- **Tall data** = many rows; good for grouping, pivoting, combining.
- **Wide data** = many columns; good for fast lookups.
- **Dimensions** = categories for slicing/filtering (e.g., person, month, location).
- **Measures** = numeric values to summarize (e.g., mean sleep minutes).
- **Tidy data principles**:
  1. Variables in columns  
  2. Observations in rows  
  3. One table per observational unit  
  4. Keys connect tables  

---

## Concept Definitions
### Unit of Analysis  
The entity described by each row (person, transaction, date, etc.).

### Dimensions  
Categorical descriptors used for slicing or grouping.

### Measures  
Numerical values used for summaries like average, count, standard deviation.

### Tall vs. Wide  
Two orientations of the same underlying information.

---

## Practice Questions

### Multiple Choice
1. Which dataset structure is ideal for pivoting between categories?
   - A) Wide  
   - B) Tall  
   **Correct answer:** B

2. A dataset tracking the same individuals every month is:
   - A) Cross‑sectional  
   - B) Time‑series  
   - C) Panel  
   **Correct answer:** C

3. “Month, Location, Person” in a sleep‑tracking dataset are:
   - A) Measures  
   - B) Dimensions  
   **Correct answer:** B

### Short Answer
1. Describe a dataset from real life and identify its unit of analysis.  
2. Explain why tall datasets are usually easier to join and aggregate.  
3. Describe when you would prefer a wide dataset.

---

## AI‑Powered Study Prompts
- “Convert a wide dataset to tall format and explain the impact on analysis.”
- “Give me examples of cross‑sectional, time‑series, and panel data from everyday life.”
- “Ask me 5 questions that require identifying unit of analysis errors.”
- “Give me messy untidy data and ask me to reorganize it into valid tidy format.”
