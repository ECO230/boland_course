
# Bayesian Reasoning Visualization App (Shiny) – Full Context Documentation

This document summarizes the **purpose, pedagogy, structure, and implementation** of the Bayesian visualization Shiny app.  
It is designed to provide enough context for a **new LLM session** (or developer) to immediately understand the project and continue development.

---

# Core Purpose

The Shiny app teaches **Bayesian reasoning visually** using a simulated population and a diagnostic-style test framework.

Instead of presenting Bayes' theorem abstractly, the app builds the reasoning **step-by-step** through interactive tabs.

The key educational goals are:

- Demonstrate how **posterior probability depends on prior prevalence**
- Show why **false positives dominate when prevalence is low**
- Connect **sensitivity and specificity** to posterior probability
- Introduce the **Bayes Factor as the clean way to express evidence**
- Show that **the same Bayes Factor updates odds by the same amount regardless of prevalence**

The interface is intentionally narrative: each tab introduces **one new concept** while keeping the layout fixed.

---

# Conceptual Framing

The app uses a diagnostic‑test metaphor.

| Statistical Concept | App Label |
|--------------------|-----------|
| H_A | Practice |
| H_0 | No Practice |
| Positive test | Solved |
| Negative test | Did Not Solve |

The key posterior of interest is:

P(Practice | Positive)

The left group label (e.g. *Practice*) is parameterized in the controls so it can be changed.

---

# Simulation Model

The app simulates a population of size **N**.

User inputs:

Population size N  
Prevalence = P(H_A)  
Sensitivity = P(Positive | H_A)  
Specificity = P(Negative | H_0)

Derived counts:

Practice = N × prevalence  
No Practice = N − Practice  

True Positive = Practice × sensitivity  
False Negative = Practice − True Positive  

True Negative = No Practice × specificity  
False Positive = No Practice − True Negative

Posterior probability:

P(Practice | Positive) = TP / (TP + FP)

---

# Visual Language

Color is used consistently across the entire app.

Muted colors represent **population membership**.

Bright colors represent **observed evidence**.

Practice population = muted green (#CFE8D6)  
No Practice population = muted red (#F2D0D0)

True Positive = bright green (#00C853)  
False Positive = bright red (#FF1744)

Bayes Factor calculations = blue

Posterior values = orange

This visual language remains consistent across all tabs.

---

# Narrative Tab Structure

## Tab 1 – Population (Prior)

Shows only priors.

Displayed metrics:

Practice population  
No Practice population  
Prevalence  
Baseline rate

Purpose:

Establish the **starting belief**.

---

## Tab 2 – Apply Test

Introduces test accuracy.

New metrics appear:

True Positive  
True Negative  
Sensitivity  
Specificity  
False Negative Rate  
False Positive Rate

Displayed as fractions:

Sensitivity = TP / Practice  
Specificity = TN / No Practice

Purpose:

Show how the **test interacts with the population**.

---

## Tab 3 – Posterior

Everything except positives fades out.

Highlighted:

True Positives  
False Positives

Posterior shown as:

P(<left group label> | Positive)

Formula:

TP / (TP + FP)

Purpose:

Students visually see the **numerator and denominator** of the posterior.

---

## Tab 4 – Same Bayes Factor, Different Prevalence

This tab bridges posterior probability and Bayes Factors.

Top container (probabilities):

Prior Belief N% × Bayes Factor = New Belief N%

Example rows:

0.1% → 1%  
1% → 9%  
10% → 53%

Second container (odds):

Prior Odds × Bayes Factor = New Odds

Examples:

1:999 × 10 = 10:999  
1:99 × 10 = 10:99  
1:9 × 10 = 10:9

Important rule:

The **denominator stays fixed**.

The Bayes Factor multiplies the **odds**, not the denominator.

Purpose:

Show that **Bayes Factor is the consistent measure of evidence**.

---

## Tab 5 – Bayes Factor Interpretation

Displays a visual interpretation scale for Bayes Factors.

Evidence scale:

BF < 1/10 → Strong evidence for H0  
BF ≈ 1 → Anecdotal evidence  
BF ≈ 3 → Some evidence for HA  
BF ≈ 10 → Strong evidence for HA  
BF ≈ 100 → Decisive evidence

Displayed using an SVG interpretation scale centered in the panel.

Title:

Bayes Factor (bf10) – Interpretation

---

# Dynamic Labels

Several labels dynamically use the user‑defined group name.

Examples:

Tab 3 bottom label:

Probability <Left Group Label> Given Positive

Tab 4 probability column:

P(<Left Group Label> | Positive)

This ensures the app works with any label, not just “Practice”.

---

# Shiny Architecture

Single‑file Shiny app:

app.R

Structure:

sidebarLayout()

sidebarPanel()

controls for:

population size  
prevalence  
sensitivity  
specificity  
group labels

mainPanel()

tabsetPanel()

5 narrative tabs

Each tab uses:

fluidRow()  
column()

Left column = H_A metrics  
Center column = visual explanation  
Right column = H_0 metrics

---

# Reactive Engine

Core reactive object:

met()

Computes:

TP  
FP  
TN  
FN

These values drive:

posterior probability  
visual grid  
metric cards  
Bayes factor calculations

Outputs rendered with:

renderUI()  
renderPlot()

---

# Visualization Strategy

A dataframe of N individuals is generated.

Each observation is assigned a state:

practice  
true_positive  
false_positive  
true_negative  
false_negative

The dataframe feeds a tile grid visualization.

---

# Teaching Design Principles

Fixed layout across tabs

Numbers stay in the same physical location so students can trace them across steps.

Progressive disclosure

Each tab introduces **one concept only**.

Visual numerator/denominator

Posterior probability is visually:

green tiles  
over  
green + red tiles

---

# Default Demonstration Example

N = 1000  
Prevalence = 1%  
Sensitivity = 90%  
Specificity = 91%

Results:

Practice = 10  
True Positive = 9  

No Practice = 990  
False Positive = 89

Posterior:

9 / (9 + 89) = 9%

Key insight:

Even with a good test, most positives can be false when prevalence is low.

---

# Pedagogical Flow in Class

Instructor sequence:

1. Start with Tab 1 discussing prevalence
2. Move to Tab 2 explaining sensitivity and specificity
3. Ask students to predict the posterior
4. Reveal Tab 3
5. Transition to Tab 4 to explain Bayes Factor
6. Use Tab 5 to interpret strength of evidence

Students experience the **base rate fallacy** firsthand.

---

# Technology Stack

R  
Shiny  
Reactive expressions  
HTML / CSS styling inside Shiny  
SVG graphics for interpretation scale

---

# Key Idea of the Entire App

Posterior probability depends on:

prior belief  
test accuracy  
false positive rate

The Bayes Factor provides the clean mathematical description of how evidence updates beliefs.

