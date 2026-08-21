---
name: manage-google-photos
description: Access, search, download, inspect, and organize Google Photos and cloud media items via Google Drive and Photos APIs.
---

# Manage Google Photos Skill

This skill provides step-by-step instructions and tools to search, list, download, and inspect Google Photos and media items.

## Authentication Overview

Google Photos and Drive media files require an OAuth 2.0 access token with the Google Drive scope (`https://www.googleapis.com/auth/drive.readonly`).

### Generating a Token
1. Go to [Google OAuth 2.0 Playground](https://developers.google.com/oauthplayground).
2. Under **Step 1 (Select & authorize APIs)**, paste:
   `https://www.googleapis.com/auth/drive.readonly`
3. Click **Authorize APIs** and grant permission.
4. Under **Step 2**, click **Exchange authorization code for tokens**.
5. Copy the **Access Token** (starts with `ya29...`).

---

## Python Helper Script Usage

Use the included helper script `scripts/photos_client.py` to query photos.

### Set Token Environment Variable
```bash
export GOOGLE_DRIVE_TOKEN="<your_access_token>"
```

### Commands

1. **List Recent Photos**:
   ```bash
   python3 .agents/skills/manage-google-photos/scripts/photos_client.py list --limit 10
   ```

2. **Fetch Latest Photo**:
   ```bash
   python3 .agents/skills/manage-google-photos/scripts/photos_client.py latest
   ```

3. **Search Photos by Keyword or Type**:
   ```bash
   python3 .agents/skills/manage-google-photos/scripts/photos_client.py search --query "Screenshot"
   ```

4. **Download Photo by File ID**:
   ```bash
   python3 .agents/skills/manage-google-photos/scripts/photos_client.py download --file-id "<file_id>" --output "/tmp/photo.jpg"
   ```
