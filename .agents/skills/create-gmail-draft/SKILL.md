---
name: create-gmail-draft
description: Create a draft email in Gmail with a specified recipient, subject, and body using local credentials.
---

# Create Gmail Draft

Use this skill when you need to programmatically draft an email in Gmail. This is useful for drafting follow-ups, confirmations, or other communications directly in the user's Gmail client.

## Setup Requirements
1. The user must have a valid `token.pickle` containing Gmail OAuth credentials in the workspace root: `/usr/local/google/home/odwl/Documents/dev/my-haskell-project/token.pickle`.

## Usage
Run the helper Python script located at `scripts/create_draft.py` in the workspace root directory:

```bash
python3 .agents/skills/create-gmail-draft/scripts/create_draft.py --to "<recipient>" --subject "<subject>" --body "<body>"
```

### Arguments
* `--to`: Recipient email address(es) (e.g. `user@example.com` or comma-separated `a@example.com, b@example.com`).
* `--subject`: The subject of the email.
* `--body`: The text content of the email.
* `--token`: (Optional) Path to the `token.pickle` credentials file. Defaults to `token.pickle` in the workspace root.
