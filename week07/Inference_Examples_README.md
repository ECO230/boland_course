# Inference Examples

Open `Inference_Examples.qmd` in the prepared Posit Cloud project and select
**Render**.

The examples use the native R pipe `|>`, the course `eco230r` functions, built-in
tea/nap datasets, and the Chicago crash sample used in the Week 7 lecture.
The shared course environment is pinned in `renv.lock`.

For your own analysis, upload a clean CSV to `data/`, import it with
`readr::read_csv("data/your_filename.csv")`, and check `names()` before replacing
the example variables. The five interpretation steps appear at the end of the
document.
