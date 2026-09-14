#!/usr/bin/env python3
"""
scan_orders.py - Package Orders & Delivery Tracking Extraction Engine

Scans personal (odewolf@gmail.com) and corporate (odwl@google.com) Gmail accounts
for online retail orders, parcel dispatches, and delivery status updates across
all major carriers (Swiss Post, DHL Express/Parcel, UPS, DPD, Planzer, SendCloud)
and merchants (Bergzeit, Running Warehouse Europe, Zalando, Galaxus/Digitec,
Travelpro, James Perse, OTTO'S, Globus, etc.).
"""

import os
import sys
import json
import base64
import re
from datetime import datetime
from bs4 import BeautifulSoup
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

ACCOUNTS = [
    {
        "name": "Personal (odewolf@gmail.com)",
        "token_path": "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/odewolf_token.json",
    },
    {
        "name": "Corporate (odwl@google.com)",
        "token_path": "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json",
    },
]

def get_body_parts(payload):
    parts = []
    def recurse(p):
        mime = p.get("mimeType", "")
        data = p.get("body", {}).get("data")
        if data:
            try:
                decoded = base64.urlsafe_b64decode(data).decode("utf-8", errors="ignore")
                parts.append((mime, decoded))
            except Exception:
                pass
        for sub in p.get("parts", []):
            recurse(sub)
    recurse(payload)
    return parts

def scan_orders(since_date="2026/08/01"):
    results = []
    for acc in ACCOUNTS:
        tpath = acc["token_path"]
        if not os.path.exists(tpath):
            continue
        try:
            creds = Credentials.from_authorized_user_file(tpath)
            svc = build("gmail", "v1", credentials=creds)
        except Exception as e:
            print(f"Error loading credentials for {acc['name']}: {e}", file=sys.stderr)
            continue

        queries = [
            f"after:{since_date} (order OR Bestellung OR commande OR versandt OR expédié OR tracking OR \"Paket\" OR \"colis\")",
            f"after:{since_date} from:(post.ch OR dhl OR ups OR dpd OR galaxus OR digitec OR bergzeit OR runningwarehouse OR zalando)",
        ]

        seen_ids = set()
        for q in queries:
            try:
                res = svc.users().messages().list(userId="me", q=q, maxResults=50).execute()
                for m in res.get("messages", []):
                    mid = m["id"]
                    if mid in seen_ids:
                        continue
                    seen_ids.add(mid)
                    msg = svc.users().messages().get(userId="me", id=mid, format="full").execute()
                    hdrs = {h["name"]: h["value"] for h in msg["payload"]["headers"]}
                    subj = hdrs.get("Subject", "")
                    sender = hdrs.get("From", "")
                    date_str = hdrs.get("Date", "")
                    
                    results.append({
                        "id": mid,
                        "account": acc["name"],
                        "date": date_str,
                        "from": sender,
                        "subject": subj,
                    })
            except Exception as e:
                print(f"Query error '{q}': {e}", file=sys.stderr)

    return results

if __name__ == "__main__":
    orders = scan_orders()
    print(f"Total matching order/shipment messages: {len(orders)}")
    for o in orders[:20]:
        print(f"[{o['id']}] {o['date'][:25]} | {o['from'][:30]} | {o['subject'][:60]}")
