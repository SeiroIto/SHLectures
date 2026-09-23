# The six 原典未確認 studies, read against Vendrell & Johnston's glosses

Date: 2026-09-14 | Session 8 Ivory Marmot

These six were cited in this project only through Vendrell & Johnston (2026,
*Computers and Education: AI* 10:100572) one-clause summaries. All six full texts were
obtained and read this session.

Reporting format is the project's six-field standard — a bare verb is not a finding.

---

## Headline

**Four of the six glosses misrepresent the paper.** Two (Yatani, Karataş) turn a paper
with no measurement into an empirical finding. Two (Pardos, Fan) report as harm evidence
a paper whose own result is positive or mixed. This repeats exactly the pattern found
when Deng and Heung & Chiu were opened in Session 6.

Only two of the six are randomised controlled experiments on learning (Darvishi n = 1625,
Pardos n = 274), and both of those favour AI assistance.

| study | gloss holds? | what breaks |
|---|---|---|
| Fisher et al. (2025) | yes | only the title is mangled in V&J; the gloss itself is accurate |
| Yatani et al. (2024) | **no** | 🚩 conceptual framework paper — no study, no sample, no statistic |
| Pardos & Bhandari (2024) | **no** | 🚩 construct mismatch — outcome is maths test gain, not creativity; 32% quality-check failure omitted |
| Darvishi et al. (2024) | yes | scale and the hybrid-condition null both dropped |
| Fan et al. (2024) | **no** | mixed result reported as pure harm; AI group *beat* all three comparison groups on the graded task |
| Karataş et al. (2024) | **no** | 🚩 qualitative interview study, n = 13, no test — cited as evidence of language development |

---

## 1. Fisher et al. (2025) — Biased LLMs can Influence Political Decision-Making

* **Design** — randomised online experiment, 3 × 2 (three model conditions × two
  participant partisanships), two tasks per participant, blind to the bias manipulation
* **Sample** — n = 299 US citizens via Prolific; 51% female, mean age 39.19 (SD 13.84);
  recruited as clear Democrats or Republicans (p.4, p.23 demographics table)
* **Treatment** — participants freely typed with an LLM prompted to "respond as a
  neutral / conservative / liberal citizen"; randomised to one of the three. Bias was
  validated following Feng et al. (2023)
* **Outcome + units** — (a) change in stated topic opinion on an ordinal agree–disagree
  scale, before vs after the conversation; (b) change in percentage-point budget
  allocation across Public Safety / Education / Veterans / Welfare
* **Statistic** — ordinal logistic regression, no controls (Table 1, p.3):
  * conservative-supported topic: Democrats exposed to the conservative model β = 0.98,
    t = 2.71, p = .007; Republicans exposed to the liberal model β = −0.79, t = −2.16,
    p = .03; Republicans × conservative model n.s. (β = 0.19, t = 0.55, p = .58) —
    authors read this as a ceiling effect
  * liberal-supported topic: the conservative model moved **both** sides —
    Democrats β = 1.44, t = 3.82, p < .001; Republicans β = 1.42, t = 3.91, p < .001;
    the liberal model moved neither
  * budget task (Table 2, p.5): all ANOVAs significant at p ≤ .001, Dunnett post-hoc vs
    control. The paper's summary sentence is "change toward the model's bias, p < .01 for
    all participants regardless of ideology" — but that is the authors' gloss, not the
    table. Four of Table 2's 16 Dunnett cells are non-significant or marginal
    (Democrat/Safety/Conserv. p = 0.13; Democrat/Welfare/Conserv. p = 0.08;
    Republican/Veterans/Liberal p = 0.60; Republican/Welfare/Liberal p = 0.06).
    Quote the table, not the sentence
  * **bias detection was measured**: 54% of Democrats (n = 51) and 54% of Republicans
    (n = 50) in a biased condition correctly identified the model as biased — and it made
    no difference. "We found no significant effect of bias detection in any condition for
    either task" (p.7); full results in Appendix E.3, Tables 19–20
  * prior AI knowledge as mitigator: **significant in one place, marginal in others.**
    Topic Opinion Task, Democrats, conservative-supported topics: coefficient −0.79,
    t = −2.51, p = .01. The Budget Allocation Task gave only branch-specific marginals
    (Veterans/Democrats p = .09; Safety/Republicans p = .08). The authors note the power
    analysis did not account for AI knowledge, so this analysis is
    "hypothesis-generating rather than conclusive" (p.6)
* **Provenance** — full text read (arXiv 2410.06415v4, 18 Mar 2026, 49 pp.)

**Against the gloss.** Only the citation is defective: V&J's reference entry mangles the
title into "g. Biased ai can influence political decision-makin" (V&J p.785 of the
extracted text) — do not repeat that string. The substance of V&J's gloss is right,
including the clause about users noticing: Fisher measured detection, found roughly half
of each party detected the bias, and found detection did not reduce the effect. The
result V&J omit is the cross-partisan one — a conservative-biased model moved Democrats
too, on both topic types.

---

## 2. Yatani, Sramek & Yang (2024) — AI as Extraherics

* **Design** — 🚩 **conceptual framework paper.** No experiment, no user study. It is not
  method-free, though: Section 4 reports a systematic survey of CHI 2023–2024 full papers
  from the ACM Digital Library, three screening criteria, two authors annotating
  independently, 50 papers categorised (p.3–4). Say "no empirical test", not "no method"
* **Sample** — no human participants (50 papers surveyed)
* **Treatment** — none
* **Outcome + units** — none
* **Statistic** — `no test`
* **Provenance** — full text read (arXiv 2409.09218v2, 19 Sep 2024, 16 pp.)

**Against the gloss.** V&J write "Yatani et al. (2024) noted that over-reliance on LLMs
may … [leave users vulnerable to] misinformation and hallucinated claims" (V&J p.140 of
the extracted text). That sentence is in Yatani's *introduction*, citing others, as
motivation for the framework they then propose. The paper's own contribution is
"extraheric AI" — a design concept for AI that asks questions and offers alternative
perspectives instead of giving answers, evaluated against cognitive load theory and
Bloom's taxonomy *as a proposal*, not as a tested intervention.

This is citable as "Yatani et al. argue", never "Yatani et al. show". It is the same
category error the Zhai deck was built to teach.

---

## 3. Pardos & Bhandari (2024) — ChatGPT-generated help vs human tutor help

* **Design** — randomised controlled experiment, 3 × 4 (three hint conditions ×
  four maths subject areas), pre-test / post-test
* **Sample** — n = 274; assigned across Elementary Algebra, Intermediate Algebra,
  College Algebra, Statistics
* **Treatment** — the hint text shown on a wrong answer: ChatGPT-generated, human
  tutor-authored, or none (control)
* **Outcome + units** — learning gain = post-test % minus pre-test % on a three-item
  repeated test; plus time-on-task in seconds
* **Statistic** (p.11–13):
  * ChatGPT condition gain significant vs control, p = 0.011
  * human tutor condition **not** separable from control, p = 0.087. Do not restate this
    as "human tutor hints produced no learning" — the human-tutor condition's own
    pre→post gain was 11.62% and significant (p = 0.001, Table 2). The null is
    between-condition, not within
  * ChatGPT vs human tutor not separable, p = 0.416 (ChatGPT's gain was 46.30% larger in
    magnitude, but not separably so)
  * the test is a two-way ANOVA **on ranked data** — Shapiro-Wilk rejected normality
    (p.11) — so call it that, not a plain ANOVA
  * ANOVA main effect of condition F(2, 262) = 5.037, p = 0.0071; of subject
    F(3, 262) = 6.737, p = 0.0002; interaction n.s. F(6, 262) = 0.901, p = 0.495
  * control's own gain 1.85%, not significant (p = 0.192)
  * **ChatGPT help failed quality checks on 32% of problems**, reduced to ~0% for
    algebra and 13% for statistics after applying self-consistency (abstract, p.1)
* **Provenance** — full text read (PLOS ONE 19(5) e0304013, open access, 18 pp.)

**Against the gloss.** V&J file this under benefits as "creativity and problem-solving
improvement, 記載なし (no statistic)". Both halves are wrong: 🚩 the outcome is a
mathematics test score, not creativity or problem-solving, and the paper reports a full
set of statistics. The direction is also the opposite of the deck's framing — this is a
randomised result in which **AI-generated help beat no help and human-authored help did
not.** The 32% quality-check failure is the caveat worth teaching, and V&J drop it.

---

## 4. Darvishi, Khosravi, Sadiq, Gašević & Siemens (2024) — Impact of AI assistance on student agency

* **Design** — randomised controlled experiment, 8 weeks, two phases
  (4 weeks AI prompts for everyone, then 4 weeks randomised into four conditions)
* **Sample** — n = 1625 students across 10 courses — the largest sample in this whole
  citation set. The 11,243 peer reviews on 3573 resources are the **second four weeks
  only** (Table 1, p.7); phase 1 added 16,007 reviews on 4501 resources (Appendix A,
  Table 6), so the study total is 27,250. Never quote 11,243 as the study total
* **Treatment** — in phase 2: AI (prompts continue, control), NR (no prompts),
  SR (self-monitoring checklist replaces prompts), SAI (both). AI prompts fired when a
  comment contained **no actionable suggestion** (rule-based detection), was
  **insufficiently related to the resource** (SBERT cosine similarity below threshold),
  or was **too similar to the reviewer's own previous comments** (GLEU) — §3.1.1, p.5.
  Comment length is an outcome measure, not a firing condition
* **Outcome + units** — six behavioural measures of peer-review quality: flag rate
  (proportion of reviews flagged as insufficient), similarity to the reviewer's own
  previous comments, relatedness to the resource under review, comment length in words,
  time in seconds, like rate
* **Statistic** — one-way ANOVA then Tukey HSD:
  * overall flag rate F(3, 1621) = 39.63, p < 0.001, η² = 0.068
  * every comparison below is **between groups during weeks 5–8** (AI N = 396 vs
    NR N = 409), not a within-person before/after change. The paper reports no
    phase-1→phase-2 within-group change; phase-1 values appear only as a balance check,
    all four groups n.s. (flag rate F(3,1621) = 0.866, p = 0.458). Write "AI 0.14 vs
    NR 0.31", never "0.14 → 0.31"
  * **RQ1, removing AI (AI vs NR)** — flag rate 0.14 (SD 0.23) vs 0.31 (SD 0.31),
    t = −8.70, p < 0.001, d = −0.61; similarity 0.28 vs 0.32, t = −4.22, p < 0.001,
    d = −0.30; relatedness 0.31 vs 0.27, t = 4.70, p < 0.001, d = 0.33; length 23.38 vs
    19.43 words, t = 4.03, p < 0.001, d = 0.28; time **not** different (p = 0.859)
  * **RQ2, checklist instead of AI (AI vs SR)** — flag rate 0.14 vs 0.26, t = −6.36,
    p < 0.001, d = −0.45. Better than nothing, worse than AI
  * **RQ3, AI plus checklist (AI vs SAI)** — flag rate 0.14 vs 0.14, t = −0.10,
    p = 1.000, d = −0.01; similarity identical, d = 7 × 10⁻⁵. The authors' own wording is
    that the hybrid was **"not more effective than AI assistance on its own"** (abstract)
    — use that, not "added nothing": comment length was marginal in SAI's favour
    (23.38 vs 25.67, t = −2.34, p = 0.090) and SAI had the longest comments of any group
* **Provenance** — full text read (Computers & Education 210:104967, CC BY, 18 pp.)

**Against the gloss.** V&J's "continued use as an effort substitute lowers agency" is
directionally right and the only gloss of the five that survives intact. What it drops
is worth having: the scale (1625, randomised), and the RQ3 null, which is the most
practically useful result in the paper — bolting self-regulation scaffolds onto AI
assistance did not help at all. Note also that "agency" here is operationalised purely
as peer-review comment quality, not a psychological agency scale; the construct is
behavioural.

---

## 5. Fan et al. (2024) — Beware of metacognitive laziness

* **Design** — randomised experiment, four groups, two-stage English reading and
  writing task, lab setting, July–September 2023
* **Sample** — n = 117 university students, mean age 22.61 (SD 3.39), 70% female,
  55% undergraduate, all L1 Chinese with English as L2. Groups: ChatGPT-4.0 (35),
  human expert (25), checklist tools (27), no support (30)
* **Treatment** — which agent was available during the revision stage
* **Outcome + units** — (1) intrinsic motivation on the IMI scale, (2) self-regulated
  learning process frequencies and sequences from multi-channel trace data,
  (3) essay score improvement (post-revision minus pre-revision), (4) knowledge gain
  (pre/post test on AI in education), (5) knowledge transfer (test on AI in healthcare)
* **Statistic**:
  * motivation — **no differences anywhere**: Interest/Enjoyment F = 1.087, p = 0.358,
    η² = 0.029; Perceived Competence F = 0.453, p = 0.716; Effort/Importance F = 1.152,
    p = 0.332; Pressure/Tension F = 0.546, p = 0.652. These rest on **n = 114**, not 117
    — Table 2 (p.12) gives CN 27 / AI 35 / HE 24 / CL 28. Do not pair these F values with
    the randomised group sizes
  * essay score improvement — omnibus F = 4.549, p = 0.005, η² = 0.108; the **AI group
    beat all three others** (Table 3, p.17): vs control mean diff 1.970, p-adj = 0.037;
    vs human expert 2.120, p-adj = 0.025; vs checklist 2.200, p-adj = 0.012. The three
    non-AI groups did not differ from each other (all p-adj ≥ 0.990)
  * knowledge gain — **no difference**: pre-test F = 1.294, p = 0.281; post-test
    F = 0.913, p = 0.438, η² = 0.030. Note the authors infer "no gain difference" from
    two separate ANOVAs; no ANOVA on gain scores themselves is reported
  * knowledge transfer — **no difference**: F = 0.019, p = 0.996, η² = 0.000
* **Provenance** — full text read (BJET 56(2):489–530, doi 10.1111/bjet.13544, 42 pp.)

**Against the gloss.** V&J render this as "metacognitive laziness — outsourcing even the
quality judgement", which reads as a measured harm. It is not. 🚩 "Metacognitive
laziness" is the authors' *interpretation* of the self-regulated-learning process traces;
no variable by that name was measured. The measured picture is mixed and mostly favours
AI on the graded outcome: the ChatGPT group produced the largest essay improvement of any
group, including the human-expert group, with motivation flat. The genuine negative
result is narrower and more interesting — that advantage **did not transfer**: knowledge
gain and transfer were statistically indistinguishable across all four groups, with
η² = 0.000 on transfer.

This is the same error class as the Gerlich β mislabel caught on 2026-09-10: an author's
narrative label read back as if it were an estimated quantity.

---

## 6. Karataş, Abedi, Ozek Gunyel, Karadeniz & Kuzgun (2024) — ChatGPT in foreign language education

* **Design** — 🚩 **qualitative case study.** Semi-structured interviews, thematic
  analysis. No control group, no pre/post measurement, no test
* **Sample** — n = 13 preparatory-class students at a School of Foreign Languages at a
  foundation university in Ankara, Turkey; 10 female, 3 male, all aged 18–20, English
  proficiency A2–B2 (p.19346–19348, Table 1). The authors state the sample is 13 because
  administrators permitted the study in a single classroom only (p.19346, repeated p.19362)
* **Treatment** — four weeks of ChatGPT-based learning activities delivered by the
  researcher acting as the students' language teacher. Note the researcher and the
  instructor are the same person. Classes were **online** (post-earthquake emergency
  distance education) and the tool was the **free ChatGPT 3.5** (p.19357)
* **Outcome + units** — interview themes about students' own perceptions: impact on
  specific language skills, motivation and engagement, strengths, challenges,
  recommendations (Tables 2 and 3)
* **Statistic** — `no test`. Every reported result is a theme drawn from interview
  transcripts
* **Provenance** — full text read (*Education and Information Technologies*
  29(15):19343–19366, open access © The Author(s) 2024, 24 pp.)

**Against the gloss.** V&J cite this under "language development" (V&J p.94 of the
extracted text) alongside quantitative studies. The paper cannot support a development
claim: 🚩 nothing was measured before or after, and the reported gains in writing,
grammar and vocabulary are what 13 students said in interviews with their own teacher.

The over-reliance concern V&J elsewhere attribute to this literature appears here too,
but as a **participant apprehension** raised by **2 of the 13** students (L1 and L2;
p.19355). L1: "I am afraid I will get used to it too much and my English will
deteriorate." The authors then support it by citing *other* papers, not their own data.
Citable as "2 of 13 students said they feared", never as "was found to reduce autonomy",
and the denominator must travel with the claim.

One sentence not to be misled by: the paper mentions students' bi-monthly exam scores
(p.19346), but those were used to pick a proficiency-spanning sample, not as an outcome.
No score is compared before and after.

---

## What this changes

* **Nothing in a live deck.** These six survive only in the frozen
  `AICriticalThinking_ja.qmd` baseline; `_verified` no longer cites any of them.
* The result strengthens the retired-Zhai lesson rather than contradicting it: the
  weakness was never that individual studies are bad, it is that **a review's one-clause
  gloss cannot be cited as the study's finding.** Six reads, four misrepresentations —
  two of which flip the sign, and two of which upgrade an unmeasured opinion into a result.
* Design census of the six: 3 randomised experiments (Fisher n = 299, Pardos n = 274,
  Fan n = 117), 1 randomised field experiment (Darvishi n = 1625), 1 conceptual framework
  (Yatani), 1 qualitative interview study (Karataş n = 13). V&J's prose gives all six the
  same evidential weight.
* If a slide ever needs a randomised result on AI and learning, Darvishi (n = 1625) and
  Pardos (n = 274) are both stronger evidence than anything the decks currently lean on,
  and Pardos points the opposite way from the deck's framing.

---

## Post-Flight verification (CoVe) — FAIL, then corrected

Two `claim-verifier` agents, fresh context, given the 54 claims and the extracted source
text only — never this draft. Both returned corrections. Zero fabricated numbers: every
error was a scope or label slip, the same failure mode as the 2026-09-10 Gerlich pass.

Three errors were mine and are fixed above:

* **Fisher — "the paper never measured whether participants noticed the bias."** False.
  It did: 54% of Democrats (n = 51) and 54% of Republicans (n = 50) in a biased condition
  correctly identified the bias, and detection did not reduce the effect (p.7, Appendix
  E.3, Tables 19–20). I had turned a reported null into an absence of measurement, and
  wrongly charged V&J with overstating a clause that is in fact correct.
* **Fisher — "prior AI knowledge mitigated only marginally (p = .09, .08)."** Scope error.
  Those two are Budget-Task branch-specific; the Topic Opinion Task gave a significant
  mitigation for Democrats (coefficient −0.79, t = −2.51, p = .01), which I had dropped.
* **Darvishi — AI prompts fired on comments "too short, too general, or too similar."**
  Wrong referent. That phrase is on p.2 describing Jia et al. (2021) and Xiong et al.
  (2012); Darvishi's own triggers (§3.1.1, p.5) are no actionable suggestion, low
  relatedness to the resource, and similarity to the reviewer's own previous comments.

Five further framing cautions were raised and are now written into the sections above:
Darvishi's 11,243 reviews are phase-2 only (study total 27,250); its comparisons are
between-group, not before/after; "the hybrid added nothing" overstates the authors'
"not more effective than AI assistance on its own"; Fan's motivation ANOVAs rest on
n = 114, not 117; Yatani is not method-free (a 50-paper systematic CHI survey with dual
independent annotation); Pardos's human-tutor condition was null only *against control*,
its own pre→post gain of 11.62% being significant at p = 0.001.
