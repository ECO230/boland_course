
# Bayesian Visualization Shiny App — LLM Bootstrap Context

This document is a **compact technical briefing** for quickly bringing a new LLM or developer up to speed on the Bayesian visualization Shiny application.

It summarizes:

- conceptual goals
- UI structure
- simulation model
- tab logic
- color system
- key implementation details

This is intended to minimize onboarding time in a fresh development session.

---

# Core Idea

The app visually teaches **Bayesian updating** using a simulated population and a diagnostic‑test style framework.

Instead of starting with equations, it starts with **counts in a population** and gradually reveals the math.

The learning path is:

Population → Test Accuracy → Posterior Probability → Bayes Factor → Evidence Interpretation

---

# Key Posterior

The app focuses on the posterior:

P(Group | Positive)

Example in the default configuration:

P(Practice | Solved)

The left group label is **user‑defined** via controls.

---

# Simulation Model

Population size: N

Inputs:

prevalence = P(H_A)  
sensitivity = P(Positive | H_A)  
specificity = P(Negative | H_0)

Derived:

Practice = N × prevalence  
NoPractice = N − Practice

TP = Practice × sensitivity  
FN = Practice − TP

TN = NoPractice × specificity  
FP = NoPractice − TN

Posterior:

P(H_A | Positive) = TP / (TP + FP)

---

# Bayes Factor

Bayes factor is computed as:

BF10 = Sensitivity / FalsePositiveRate

FalsePositiveRate = 1 − Specificity

Posterior odds:

PosteriorOdds = PriorOdds × BF

PriorOdds = P(H_A) / P(H_0)

PosteriorProbability = PosteriorOdds / (1 + PosteriorOdds)

---

# Color System

Population membership uses muted colors.

Evidence uses bright colors.

Practice population: muted green (#CFE8D6)  
No Practice population: muted red (#F2D0D0)

True Positive: bright green (#00C853)  
False Positive: bright red (#FF1744)

Bayes factor math: blue

Posterior results: orange

These colors are consistent across all tabs.

---

# UI Layout

Shiny layout:

sidebarLayout()

sidebarPanel() → simulation parameters

mainPanel() → tabsetPanel()

Each tab uses a **three‑column layout**:

Left column → H_A metrics  
Center column → conceptual visualization  
Right column → H_0 metrics

Layout is intentionally **identical across tabs** so students track numbers as concepts change.

---

# Tabs

## Tab 1 — Population

Shows priors only.

Practice population  
No Practice population  
Prevalence  
Baseline rate

Purpose: establish starting beliefs.

---

## Tab 2 — Apply Test

Adds test performance metrics.

True Positive  
True Negative  
Sensitivity  
Specificity  
False Positive Rate

Shows fractions like:

Sensitivity = TP / Practice

---

## Tab 3 — Posterior

Fades out everything except:

True Positives  
False Positives

Posterior:

P(<left label> | Positive)

Students visually see the numerator and denominator.

---

## Tab 4 — Same Bayes Factor, Different Prevalence

Shows that the **same evidence produces different posteriors** when priors differ.

Two containers:

### Probability View

Prior Belief N% × Bayes Factor = New Belief N%

Example rows:

0.1% → 1%  
1% → 9%  
10% → 53%

### Odds View

Prior Odds × Bayes Factor = New Odds

Examples:

1:999 × 10 = 10:999  
1:99 × 10 = 10:99  
1:9 × 10 = 10:9

Key rule:

The **denominator remains fixed**.

BF multiplies odds.

---

## Tab 5 — Bayes Factor Interpretation

Displays an SVG evidence scale.

Title:

Bayes Factor (bf10) – Interpretation

Typical interpretation scale:

0.1 → evidence for H0  
1 → anecdotal  
3 → some evidence  
10 → strong evidence  
30+ → very strong evidence  
100+ → decisive evidence

SVG file is loaded from the app directory.

---

# Reactive Engine

Core reactive object:

met()

Produces:

TP  
FP  
TN  
FN

Derived metrics:

posterior probability  
posterior odds  
Bayes factor

Used across multiple renderUI outputs.

---

# Dynamic Labels

Some UI labels use the left‑group control.

Examples:

Probability <GroupLabel> Given Positive

P(<GroupLabel> | Positive)

This makes the app reusable across different scenarios.

---

# Visualization Strategy

Population visualization is built from a dataframe of N individuals.

Each row represents one person with a state:

practice  
true_positive  
false_positive  
true_negative  
false_negative

This dataframe feeds tile plots.

---

# Teaching Narrative

Instructor flow:

Tab1 → discuss prevalence

Tab2 → explain sensitivity / specificity

Ask students to guess posterior.

Tab3 → reveal base rate fallacy.

Tab4 → introduce Bayes Factor as evidence multiplier.

Tab5 → interpret strength of evidence.

---

# Technology

R  
Shiny  
Reactive expressions  
HTML/CSS inside Shiny UI  
External SVG graphics

Single‑file app:

app.R

---

# Key Teaching Insight

Posterior probability depends on:

prior belief  
test accuracy  
false positive rate

The **Bayes Factor describes how evidence updates odds**.

