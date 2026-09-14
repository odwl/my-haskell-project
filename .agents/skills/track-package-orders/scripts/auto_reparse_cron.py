#!/usr/bin/env python3
"""
auto_reparse_cron.py - OS-level reboot-proof background cron script.

Runs every 4 hours (08:00, 12:00, 16:00, 20:00) via Linux crontab to:
1. Scan personal (odewolf@gmail.com) and corporate (odwl@google.com) Gmail accounts
   for new package orders, carrier tracking updates, and travel/dining updates.
2. Synchronize the master markdown itinerary and package catalog to the live Google Doc.
"""

import os
import sys
import subprocess
from datetime import datetime

PROJECT_ROOT = "/usr/local/google/home/odwl/Documents/dev/my-haskell-project"
SCAN_ORDERS_SCRIPT = os.path.join(PROJECT_ROOT, ".agents/skills/track-package-orders/scripts/scan_orders.py")
SYNC_DOC_SCRIPT = os.path.join(PROJECT_ROOT, ".agents/skills/track-package-orders/scripts/sync_google_doc.py")

def main():
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"[{now_str}] Starting scheduled 4-hour Gmail reparse & Google Doc sync...")
    
    # 1. Run order scanner
    if os.path.exists(SCAN_ORDERS_SCRIPT):
        subprocess.run([sys.executable, SCAN_ORDERS_SCRIPT], check=False)
        
    # 2. Sync master markdown to Google Doc
    if os.path.exists(SYNC_DOC_SCRIPT):
        subprocess.run([sys.executable, SYNC_DOC_SCRIPT], check=False)
        
    print(f"[{now_str}] Scheduled sync complete.")

if __name__ == "__main__":
    main()
