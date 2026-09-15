---
name: manage-drive-backups
description: Operate the full disaster-recovery backup system that archives Jetski conversations, brain artifacts, skills, knowledge base, OAuth tokens, Gemini config and the project git repo to Google Drive, incrementally syncs the books PDF library, and restores a complete working environment onto a new Cloudtop or laptop with one command.
---

# Manage Google Drive Backups & Disaster Recovery Skill

This skill operates the three-layer backup system protecting the entire Antigravity/Jetski
working environment. The design goal is **prompt-readiness on a new machine**: download one
archive, run one command, and immediately resume work with full conversation history, skills,
knowledge base, and authenticated Gmail/Drive/Calendar access.

---

## 1. Backup Account — Corporate Drive Only

All Drive backups go to **corporate Drive (`odwl@google.com`)**, authenticated with
`~/.gemini/jetski/gmail_mcp/token.json`.

**Personal Drive (`odewolf@gmail.com`) is NOT a backup target.** Never write archives there.
The personal token (`odewolf_token.json`) remains in use for Gmail/Calendar/Docs *content*
skills, but must not be used as a backup destination.

> [!IMPORTANT]
> The current archive is unsplit: it contains both corporate material (HR dossiers,
> legal memos, financial analyses, `odwl@` mail) and personal material (family medical
> records, estate planning, personal OAuth tokens). Everything therefore inherits
> corporate retention, admin access, and eDiscovery. A future fine-grained split into
> `work_backup_*` and `personal_backup_*` is planned — see §7.

---

## 2. Architecture — Protection Layers

| Layer | Cadence | Target | Protects Against |
| :--- | :--- | :--- | :--- |
| **Cloudtop hot mirror** | Every 30 min | `odwl3:~/.gemini/jetski/central_backup/` | Local machine unavailability |
| **GitHub auto-push** | Daily 20:30 | `git@github.com:odwl/my-haskell-project.git` | Code/skill loss, gives full commit history |
| **Drive versioned snapshot** | Daily 20:30 | `Jetski Backups/Snapshots/` (14-day rolling) | Corruption, deletion, total machine loss |
| **Books library sync** | Weekly Sun 21:00 | `Jetski Backups/Books/` | Loss of the 1.1 GB PDF reference library |

### Drive Folder Layout

Everything lives under a single parent folder on corporate Drive — never create
additional top-level backup folders.

```
Jetski Backups/                          (1EtcgqB0wlLD8aWVdlVdHZCGvFXi9Yjnf)
├── Snapshots/                           daily jetski_backup_YYYY-MM-DD.tar.gz, 14 retained
│     └── jetski_backup_2026-09-14.tar.gz
└── Books/                               mirrored PDF library, sub-foldered by topic
      ├── physics/  algebra/  category/  ...
```

Both scripts resolve their destination by walking a folder *path* (`BACKUP_FOLDER_PATH`,
`ROOT_FOLDER_PATH`), creating any missing level. To relocate or rename, change the path
constant in all three scripts — `backup_to_drive_and_git.py`, `sync_books_to_drive.py`,
and `restore_from_drive.py` — they must agree or restore will not find the archives.

> A `rsync` mirror is NOT a backup: corruption propagates. Only the Drive snapshots are
> versioned and therefore recoverable from a bad state.

### Why books are synced separately but artifacts are not

| | `books/` | `brain/` artifacts |
| :--- | :--- | :--- |
| Size | 1.1 GB | 533 MB |
| Churn (7 days) | ~0 files | 467 files / 37 MB |
| Nature | Static, immutable PDFs | Append-only, actively growing |
| Benefit from versioning | None | High — point-in-time recovery |
| Strategy | Incremental file sync (upload once) | Included in the daily versioned archive |


---

## 3. Scripts & Canonical Locations

| Script | Location | Purpose |
| :--- | :--- | :--- |
| `backup_to_drive_and_git.py` | `~/.local/bin/` | Daily: git push + full DR archive to Drive |
| `sync_books_to_drive.py` | `~/.local/bin/` | Incremental mirror of `books/` PDFs to Drive |
| `restore_from_drive.py` | `~/.local/bin/` | One-command restore onto a new machine |
| `jetski-sync.sh` | `~/.local/bin/` | 30-minute hot mirror to Cloudtop `odwl3` |

Versioned copies live in this skill's `scripts/` directory so they are tracked in git.

---

## 4. Archive Layout (`jetski_backup_YYYY-MM-DD.tar.gz`)

```
jetski/    ~/.gemini/jetski/          conversations, brain artifacts, MCP servers,
                                      OAuth tokens (gmail_mcp/), knowledge, prompting, lib
gemini/    ~/.gemini/                 config, plugins, skills, policies, settings, history
project/   ~/Documents/dev/...        full git repo incl. .git history, gitignored
                                      docs/knowledge_base/ and .agents/skills/
local/     ~/.local/bin/              all sync & backup tooling
system/                               crontab.txt, requirements.txt, manifest.json, RESTORE.md
```

### Deliberate Exclusions

| Excluded | Reason / Recovery |
| :--- | :--- |
| `books/` | Static 1.1 GB; synced separately as individual files (never re-uploaded daily) |
| `venv/`, `new_venv/`, `node_modules/` | Regenerate via `pip install -r system/requirements.txt` |
| `dist-newstyle/`, `.stack-work/`, `target/`, `.lake/` | Build outputs; rebuild from source |
| `tempmediaStorage/`, `__pycache__/`, `*.db-wal` | Transient caches and SQLite journals |
| SSH private keys | Security: never archived. Generate new keys on the restored machine. |

> [!CAUTION]
> The archive **contains live OAuth tokens** (`gmail_mcp/*.json`) — required for instant
> prompt-readiness. Treat it as a secret. It must remain in private Drive and must never
> be shared, emailed, or moved to a shared drive.

---

## 5. Operating Commands

```bash
# Run a full backup immediately (git push + Drive snapshot)
python3 ~/.local/bin/backup_to_drive_and_git.py

# Sync the books library (incremental; safe to re-run, resumable)
python3 ~/.local/bin/sync_books_to_drive.py

# Inspect scheduled jobs
crontab -l

# Follow the most recent backup log
tail -n 40 /tmp/backup_to_drive_and_git.log
tail -n 40 /tmp/books_sync.log
```

### Verifying a Backup Succeeded

A healthy run ends with `=== BACKUP FINISHED SUCCESSFULLY ===`. Always confirm:
1. `GitHub push completed successfully.`
2. `Snapshot archive created successfully (NNN MB).` — expect **~600-650 MB**; a sudden drop
   below ~400 MB indicates a missing backup set and must be investigated.
3. `OK - all 10 critical paths present (N entries).` — the archive self-check.
4. `Upload complete!` with a `webViewLink`.

### Archive Self-Verification

Before uploading, `verify_archive()` runs `tar -tzf` over the finished archive and asserts
every entry in `CRITICAL_PATHS` is present:

| Path | Why it matters |
| :--- | :--- |
| `jetski/gmail_mcp/token.json` | Corporate Drive/Gmail auth — without it, restore cannot even fetch a backup |
| `jetski/gmail_mcp/odewolf_token.json` | Personal Gmail/Calendar auth for content skills |
| `jetski/conversations/` | Chat history |
| `jetski/brain/` | Artifacts and session logs |
| `gemini/config/mcp_config.json` | MCP server wiring |
| `project/.../docs/knowledge_base/` | Gitignored — the Drive archive is its ONLY backup |
| `project/.../.agents/skills/` | Skills |
| `project/.../.git/` | Commit history |
| `local/bin/` | The backup tooling itself |
| `system/RESTORE.md` | Recovery instructions |

If any are missing the script raises and **does not upload**, on the principle that a
silently incomplete backup is more dangerous than a visibly failed one. When adding a new
`BACKUP_SETS` entry, add a matching `CRITICAL_PATHS` assertion.

---

## 6. Restore Procedure (New Cloudtop or Laptop)

### Bootstrap problem
`restore_from_drive.py` needs Drive credentials, which live *inside* the backup. Resolve with
either path:

**Path A — credentials available** (copy `odewolf_token.json` from any existing machine to
`~/` or `~/Downloads/`):
```bash
python3 restore_from_drive.py
```

**Path B — no credentials** (download the `.tar.gz` manually from drive.google.com):
```bash
python3 restore_from_drive.py --archive ~/Downloads/jetski_backup_2026-09-14.tar.gz
```

Always preview first:
```bash
python3 restore_from_drive.py --dry-run
```

### What restore performs
1. Downloads the newest snapshot (or uses `--archive`).
2. Restores `jetski/`, `gemini/`, `project/`, `local/` — moving any pre-existing directory
   aside as `<path>.pre-restore-<timestamp>` rather than overwriting.
3. Re-links the symlinks Jetski expects:
   `~/.gemini/jetski/mcp_config.json` → `~/.gemini/config/mcp_config.json`
   `~/.gemini/jetski/plugins` → `~/.gemini/config/plugins`
4. Installs Python dependencies from `system/requirements.txt`.
5. Reinstalls the crontab from `system/crontab.txt`.
6. Copies `RESTORE.md` to the home directory.

### Post-restore verification
```bash
ls ~/.gemini/jetski/conversations | wc -l                   # conversation history present
ls ~/Documents/dev/my-haskell-project/docs/knowledge_base   # briefings present
crontab -l                                                  # schedules re-registered
python3 -c "from google.oauth2.credentials import Credentials; \
  Credentials.from_authorized_user_file('$HOME/.gemini/jetski/gmail_mcp/odewolf_token.json'); \
  print('OAuth OK')"
```

Then open the workspace in Antigravity and resume prompting.

---

## 7. Mandatory Operating Rules

1. **Never delete a Drive snapshot manually.** Retention is automatic — the newest 14 are
   kept and older ones pruned by `enforce_retention_policy()`.
2. **Never commit the archive or OAuth tokens to GitHub.** The repo's `.gitignore` blocks
   `credentials.json` and `*.pickle`; keep it that way.
3. **After adding a new skill or knowledge-base artifact**, confirm it is captured. Skill
   scripts are force-added past the repo-wide `*.py` ignore rule via `GIT_FORCE_ADD_PATHS`;
   `docs/knowledge_base/` is intentionally gitignored and is protected by the Drive archive only.
4. **When adding a new directory worth protecting**, register it in `BACKUP_SETS` inside
   `backup_to_drive_and_git.py` — do not rely on it being picked up implicitly.
5. **Run a `--dry-run` restore on a spare machine quarterly.** An untested backup is not a backup.
6. **Report drift**: if a scheduled run is missing from `/tmp/backup_to_drive_and_git.log` for
   more than 48 hours, surface it to the user proactively.

---

## 8. Planned: Fine-Grained Split (Not Yet Implemented)

The archive is currently unsplit and lives entirely on corporate Drive. The intended
end state separates it by data class so neither account holds the other's sensitive data:

| Archive | Destination | Contents |
| :--- | :--- | :--- |
| `work_backup_*.tar.gz` | Corporate Drive (`odwl@google.com`) | Corporate knowledge base, work conversations, corporate token |
| `personal_backup_*.tar.gz` | Personal Drive (`odewolf@gmail.com`) | Personal docs, personal conversations, personal token, books |
| Neutral | Either | Skills, tooling, Clifford/Haskell work, config |

Implementing this requires a file-by-file classification of `docs/knowledge_base/` and of
conversation history, reviewed and corrected by the user. Obvious cases:
`synthese_medicale_*`, `planification_successorale_*`, `master_wealth_tax_*`, `norway_trial*`
are personal; `*_promo_dossier`, `*_endorsement_request`, `precalibration_*`,
`legal_escalation_*`, `*_arr_analysis` are corporate. Mixed cases such as
`travel_itinerary_2026.md` need an explicit decision.
