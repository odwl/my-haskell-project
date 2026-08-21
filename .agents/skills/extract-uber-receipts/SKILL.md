---
name: extract-uber-receipts
description: Extract Uber receipts from Gmail, convert them to PDF using Puppeteer, and upload them to Google Drive.
---

# Extract Uber Receipts Skill

This skill provides automated extraction of Uber receipt emails from the authenticated user's Gmail inbox. It uses headless Chrome via Puppeteer to convert the exact HTML styling into high-fidelity PDF screenshots, and then automatically uploads those PDFs to the user's Google Drive.

## Requirements
- `puppeteer` Node.js package installed locally.
- Google OAuth credentials with Gmail and Google Drive scopes (`odewolf_token.json`).

## Usage
1. First, search for the target Uber receipts in Gmail to extract the message IDs.
2. Next, extract the HTML content of the target email using the Gmail API.
3. Run the Puppeteer script to render the HTML file into a PDF.
4. Upload the generated PDF to Google Drive using the Drive API.
