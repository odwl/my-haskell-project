---
name: cite-source-documents
description: Systematically append a dedicated 'Document References & Source Archives' section to all generated markdown artifacts, briefings, reports, and summaries, compiling clickable links to all internal go/ links, PRDs, emails, SQL queries, and local archived documentation used in the synthesis.
---

# Cite Source Documents & References

When generating, structuring, or updating markdown documents, executive briefings, technical design reports, or walkthrough artifacts for the user, **you must systematically include a dedicated reference section at the culmination of the document** titled `## 📚 Document References & Source Archives` (or similar appropriate heading).

## Mandatory Reference Section Structure

Organize the citations cleanly using GitHub Flavored Markdown lists or tables, categorizing sources into intuitive sub-sections based on where they originate:

### 1. Internal Shortlinks & Quick-Links (`go/` links)
* List all relevant internal company URLs or shortlinks referenced during the task (e.g., `go/granular-locations-adstube`, `go/ycp-promotion`, `go/gbpxyt-dd`).
* Provide a clean Markdown hyperlink format (e.g., `[go/link-name](http://go/link-name)`) paired with a brief 10–15 word description of what the link points to (e.g., PRD, engineering dashboard, onboarding checklist).

### 2. Cloud Documents, Presentations & Pastes
* Include clickable URLs to external or online collaboratively edited documents (e.g., Google Docs, Google Sheets, Google Slides, GooglePlex Pastes, GitHub PRs/issues).
* Include the primary author(s) or PM/engineering owner(s) alongside the document title whenever that context is known or available in the text.
* Example: `* **[Proposal: Automatic Brand Detection & Scaled Video Reuse Rights](https://docs.google.com/...)**: Core architecture for automated Video Brand Intelligence (Authors: Nicolas Marchal, Olivier De Wolf, François Montay).`

### 3. Local Workspace & Conversation Archives (`file://` scheme)
* Whenever you synthesize insights, historical data, code, or email thread extracts from existing files stored on the local machine (such as previous agent conversation archives in `<appDataDir>/brain/` or user repo documents in `/Documents/`), you must cite them using GitHub-style markdown links with the literal absolute path and `file://` scheme.
* Use readable basenames for the link text rather than raw UUIDs or messy relative paths.
* Example: `* **[adstube_local_boosting_strategic_overview.md](file:///usr/local/google/home/odwl/.gemini/jetski/brain/0673e843-94cf-4c17-bc0a-5591054fa9ee/adstube_local_boosting_strategic_overview.md)**: Empirical production analysis of local CPV efficiency and In-Stream engagement.`

## Core Operational Guidance
* **Transparency & Traceability:** The goal of this skill is to empower the user to easily audit, re-verify, share, or click into the exact underlying sources that informed your conclusions without searching through chat logs.
* **No Orphan Claims:** If a specific numerical metric (such as ARR, CPV, view rate, or survey percentages), engineering milestone, or PM quote is highlighted prominently in the body of the markdown document, ensure its primary source document is represented in this section.
* **Formatting Cleanliness:** Never wrap link text in backticks (which breaks clickable Markdown formatting). Keep descriptions concise and punchy so the reference section functions as a professional bibliography.
