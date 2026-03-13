---
description: How to view and update outline and post artifacts
---
# Outline and Post Live Artifact Workflow

1. Navigate to the artifacts directory in the current session.
2. Open `outline_artifact.md` or any of the part-specific artifacts (`part1_artifact.md`, `part2_artifact.md`, `part3_artifact.md`, `part4_artifact.md`) to see the live rendering of the document.
3. If changes are needed, you can ask the agent to apply updates directly to both the source files (`docs/outline.md`, `docs/01_the_atoms.md`, `docs/02_the_evolution.md`, `docs/03_the_aggregators.md`, or `docs/04_the_deep_math.md`) and their corresponding artifact files (`outline_artifact.md`, etc.).
4. **CRITICAL INSTRUCTION FOR AGENT**: When synchronizing content from the source files over to the artifact files, the agent MUST use proper file manipulation tools (like `replace_file_content` or `write_to_file` with IsArtifact:true). The agent must **never** use bash commands like `cp` to sync the files, because doing so bypasses the UI and prevents the live artifact panel from refreshing for the user.
5. All generated artifacts contain 100% of the content from the original files on a single page, meaning no page-flips or scroll interruptions are needed.
