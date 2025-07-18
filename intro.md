There are many ways to weight applicants in the lottery. Here you can pick a weighting formula and see how it would change results using the 2025, 2024, or 2023 High Lonesome data.
You can also look at synthetic data for 2026 and 2027 built on prediction models. You can see the modeling in a Jupyter notebook here.

**General goals of the lottery:**
- Equal numbers of men and women
- Mix of new and veteran runners, no guarantees for either
- Previous unsuccessful applications should be a major determinant of selection
- We value volunteering and trail work
- New entrants should have a reasonable chance to run within a couple years

**We award points for:**
- Volunteer shifts at High Lonesome or Freestone Endurance events
- Extra volunteer trailwork *beyond* the required hours
- Previous applications for the race
- Previous finishes of the race

**The current model is:**
```
Tickets = exp^(n + k + 1) + mult * ln(v + t + 1)
```
where:
- n = Previous Applications
- k = Finish Multiplier (0 finishes->0, 1f->0.5,  2f->1, 3f->=1.5, 4f or more->0.5)
- v = Volunteer Points
- t = Extra Trailwork Points