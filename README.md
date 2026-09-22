## Smith SDS Capstone Project

## Team members

- Zoe Khan (Sprint 1 Product Manager)
- Nicole Choi
- Bayansulu Tulepbayeva (Tulip)
- Ellie Zdancewic
 
## Team rules

1. AI Norms: We will only use AI as the LAST resort. We also believe there is a distinction between creating with AI vs. creating AI (ie. in the form of a chatbot for our dashboard), where the latter may be more justified. At the end of the day, we must be able to stand behind our own work. We collectively want to do as much of our own work as possible, and be transparent about any AI use that does occur. 

2. Group Meetings: For our baseline third standup meeting, we will have it in-person with the caveat that we can switch to Zoom depending on team members’ needs (ie. if one member is travelling). For meetings with our project sponsor, we will meet over Zoom and prepare a Meeting Agenda each week. Many members of the group also enjoy coworking, so we will work together on coding and other project tasks as much as time allows.

3. Communication: Outside of scheduled meeting times, communicate with the group frequently as needed. Overcommunication is better than undercommunication!
    - Example: if a meeting location/time has not been set up, you can take initiative and bring it up with other members. 
    - Example: if you made a pull request and noticed no one has reviewed it yet after a day or two, ask the group if someone can.
    - Example: if you know you have a crazy week ahead with multiple tests or a personal issue comes up, communicate with the group so expectations of work can be adjusted.

4. Navigating Conflict: We want to maintain psychological safety in the group. This means creating a safe space where everyone is respected and comfortable speaking their mind. If conflicts do arise, we agree to be direct and not let the issue fester. What does this look like? Let other group members know about the issue, use “I” statements, and try to understand everyone’s perspective. Remember: it’s not about winning or convincing, it's about finding a path forward as a collective.

5. Avoiding Crises: As a group, we want to avoid one or multiple members facing an ongoing crisis and having a “snowball” effect of missing work that piles up. As stated in communication example C, we will try to be as proactive as possible with our expected capacity for the week. If you find that you are repeatedly making promises that you can’t keep, it's better to acknowledge your current situation and ask for another team member for help than to continue struggling. As team members we will always assume best intent and volunteer to support each other if possible. 

6. Pull Request Workflow: The rough workflow of a change to the shared repository is:
    1. Creation of User Story/Issue detailing goals of work & acceptance criteria 
    2. Creation of a separate branch linked with this Issue 
    3. Frequent incremental commits of the work needed to address this Issue, pushed to the branch 
    4. When the change is tested and ready to be merged, the author will create a Pull Request. NEVER push to main.
    5. One other member of the group will review the change and when the change is approved, the author can merge 
        1. Merge conflicts are a fact of life will be addressed if necessary 
    6. Once the pull request is officially merged, the branch is deleted and the Issue is marked as complete. 


## Final paper

Your final paper is a Quarto Manuscript in the `quarto-manuscript/` directory. 
Please see the documentation for [Quarto Manuscripts: RStudio](https://quarto.org/docs/manuscripts/authoring/rstudio.html).

You can build the paper locally from the root directory of the project by the following command in a **Terminal**:

```bash
quarto render quarto-manuscript
```

The [GitHub Action](https://quarto.org/docs/publishing/github-pages.html#github-action) in `.github/workflows/publish.yml` will trigger a build of the Quarto Manuscript on each commit. 
The badge above is linked to the status of that action. 

