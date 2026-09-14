import re
from datetime import datetime, timedelta
import json
import os
import base64
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build

DOC_PATH = "/usr/local/google/home/odwl/Documents/dev/my-haskell-project/docs/knowledge_base/travel_itinerary_2026.md"
TOKEN_PATH = "/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/odewolf_token.json"

def get_calendar_service():
    creds = Credentials.from_authorized_user_file(TOKEN_PATH)
    return build('calendar', 'v3', credentials=creds)

def get_gmail_service():
    creds = Credentials.from_authorized_user_file(TOKEN_PATH)
    return build('gmail', 'v1', credentials=creds)

def parse_date_time(date_str, time_str):
    formats = [
        "%b %d, %Y %I:%M %p",
        "%B %d, %Y %I:%M %p",
        "%b %d %Y %I:%M %p",
        "%B %d %Y %I:%M %p",
        "%b %d, %Y %H:%M",
        "%B %d, %Y %H:%M",
    ]
    date_str = re.sub(r'(\d+)(st|nd|rd|th)', r'\1', date_str)
    for fmt in formats:
        try:
            return datetime.strptime(f"{date_str} {time_str}".strip(), fmt)
        except ValueError:
            pass
    return None

def parse_flights():
    with open(DOC_PATH, "r") as f:
        text = f.read()
    lines = text.split('\n')
    
    flights = []
    
    # Simple state machine to carry over date and PNR to sub-flights
    current_date_str = None
    current_pnr = None
    
    for b in lines:
        b = b.strip()
        if not b.startswith("-") and not b.startswith("*"):
            continue
            
        # Check for date anywhere in bullet or parent header
        date_match = re.search(r'(?:Friday|Saturday|Sunday|Monday|Tuesday|Wednesday|Thursday),?\s+([A-Z][a-z]{2,8}\s+\d{1,2}(?:,\s+\d{4})?)', b)
        if not date_match:
            date_match = re.search(r': (?:[A-Z][a-z]+, )?([A-Z][a-z]{2,8} \d{1,2}(?:, \d{4})?)', b)
        if not date_match:
            date_match = re.search(r'([A-Z][a-z]{2,8} \d{1,2}, \d{4})', b)
            
        if date_match:
            date_str = date_match.group(1)
            if "," not in date_str and len(date_str.split()) == 2:
                date_str += ", 2026"
            elif len(date_str.split()) == 3 and not "," in date_str:
                parts = date_str.split()
                date_str = f"{parts[0]} {parts[1]}, {parts[2]}"
            current_date_str = date_str

        pnr_match = re.search(r'(?:PNR|Ref|Code)[^\*]*\*\*([A-Z0-9]{5,8})\*\*', b)
        if pnr_match:
            current_pnr = pnr_match.group(1)

        time_match = re.search(r'(\d{1,2}:\d{2}(?:\s?[AP]M)?)\s*(?:–|-)\s*(\d{1,2}:\d{2}(?:\s?[AP]M)?)', b)
        route_match = re.search(r'\(([A-Z]{3} ➔ [A-Z]{3}.*?)\)', b)
        if not route_match:
            route_match = re.search(r'\(([A-Z]{3} to [A-Z]{3}.*?)\)', b)

        if not time_match or not route_match:
            continue
            
        route = route_match.group(1)

        t_start = time_match.group(1).replace(" AM", " AM").replace(" PM", " PM")
        t_end = time_match.group(2).replace(" AM", " AM").replace(" PM", " PM")

        if current_date_str:
            dt_start = parse_date_time(current_date_str, t_start)
            dt_end = parse_date_time(current_date_str, t_end)
            if dt_start and dt_end:
                if dt_end < dt_start:
                    dt_end += timedelta(days=1)
                flights.append({
                    "route": route,
                    "start": dt_start.isoformat(),
                    "end": dt_end.isoformat(),
                        "pnr": current_pnr,
                        "raw": b
                    })
    return flights

def extract_text(payload):
    text = ""
    if "data" in payload.get("body", {}):
        try:
            text += base64.urlsafe_b64decode(payload["body"]["data"]).decode("utf-8", errors="ignore")
        except Exception:
            pass
    if "parts" in payload:
        for part in payload["parts"]:
            text += extract_text(part)
    return text

def clean_text(html_content):
    no_style = re.sub(r'<style[^>]*>.*?</style>', ' ', html_content, flags=re.DOTALL)
    no_script = re.sub(r'<script[^>]*>.*?</script>', ' ', no_style, flags=re.DOTALL)
    no_tags = re.sub(r'<[^>]+>', ' ', no_script)
    clean = re.sub(r'\s+', ' ', no_tags).strip()
    return clean

def main():
    print("=== SYNCHRONIZING CALENDAR WITH MASTER ITINERARY ===")
    service = get_calendar_service()
    flights = parse_flights()
    print(f"Found {len(flights)} flights to sync.")
    
    for f in flights:
        query = f['pnr'] if f['pnr'] else f['route']
        events_result = service.events().list(calendarId='primary', q=query, singleEvents=True).execute()
        events = events_result.get('items', [])
        
        target_summary = f'Travelling: Flight {f["route"]}'
        
        exists = False
        for e in events:
            # Check if this event matches the flight
            # The start time of the event from the API looks like '2026-09-28T18:55:00+02:00'
            event_start_str = e.get('start', {}).get('dateTime')
            if not event_start_str:
                continue
                
            # If the date matches and it's our target PNR or title matches roughly
            if event_start_str.startswith(f['start'][:10]):
                if e.get('summary') != target_summary:
                    print(f"Deleting locked/incorrect event '{e.get('summary')}' for {f['route']} (PNR {f['pnr']})...")
                    try:
                        service.events().delete(calendarId='primary', eventId=e['id']).execute()
                        exists = False
                    except Exception as err:
                        print(f"Failed to delete {e['id']}: {err}")
                        exists = True
                else:
                    print(f"Already correct: {f['route']} (PNR: {f['pnr']})")
                    exists = True
                break
                
        if not exists:
            event = {
                'summary': target_summary,
                'description': f'{f["raw"]}',
                'start': {'dateTime': f['start'], 'timeZone': 'Europe/Zurich'},
                'end': {'dateTime': f['end'], 'timeZone': 'Europe/Zurich'},
            }
            try:
                service.events().insert(calendarId='primary', body=event).execute()
                print(f"Added new event: {f['route']} on {f['start']} (PNR: {f['pnr']})")
            except Exception as e:
                print(f"Error adding {f['route']}: {e}")
            
    print("\n=== CANCELLATION & REFUND SCRUBBING ===")
    gmail = get_gmail_service()
    try:
        res = gmail.users().messages().list(userId='me', q='(subject:"Your flight cancellation is complete" OR subject:"reservation has been canceled" OR subject:"cancellation")', maxResults=20).execute()
        messages = res.get('messages', [])
        for msg in messages:
            mdata = gmail.users().messages().get(userId='me', id=msg['id'], format='full').execute()
            headers = {x['name'].lower(): x['value'] for x in mdata.get('payload', {}).get('headers', [])}
            subj = headers.get('subject', '')
            body_text = clean_text(extract_text(mdata.get('payload', {})))
            
            pnr = None
            subj_match = re.search(r'\(([A-Z0-9]{6})\)', subj)
            if subj_match:
                pnr = subj_match.group(1)
            else:
                pnr_match = re.search(r'(?:Confirmation number|reservation|confirmation):\s*([A-Z0-9]{6})', body_text, re.IGNORECASE)
                if pnr_match:
                    pnr = pnr_match.group(1)
            
            if pnr:
                cal_res = service.events().list(calendarId='primary', q=pnr, singleEvents=True).execute()
                cal_events = cal_res.get('items', [])
                for ce in cal_events:
                    desc = ce.get('description', '')
                    summ = ce.get('summary', '')
                    if re.search(r'\b' + pnr + r'\b', desc) or re.search(r'\b' + pnr + r'\b', summ):
                        print(f"  Deleting cancelled event for PNR {pnr}: {summ} (ID: {ce['id']})")
                        try:
                            service.events().delete(calendarId='primary', eventId=ce['id']).execute()
                        except Exception as err:
                            print(f"    Failed to delete {ce['id']}: {err}")
    except Exception as e:
        print(f"Error checking cancellations: {e}")

if __name__ == '__main__':
    main()
