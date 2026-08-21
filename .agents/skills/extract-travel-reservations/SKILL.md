---
name: extract-travel-reservations
description: Accurately scan personal and work Gmail accounts for flights, hotels, car rentals, corporate Silicon Valley lodgings (Bay View / Mountain View), and dining reservations across multiple languages, enforcing exact prices, timestamps, interactive portal/Google Maps links, maintaining self-cleaning calendar sync, and generating native executive-formatted Google Docs via Google Docs API.
---

# Extract & Update Travel Reservations Skill

This skill provides an automated methodology, strict metadata and interactive link verification rules, and dedicated extraction and synchronization engines (`scripts/scan_travel.py` & companion Google Docs API formatting scripts) to monitor, extract, verify, and maintain travel itineraries across personal and corporate Gmail accounts, private Google Calendar schedules, and natively styled live Google Docs.

## Mandatory Data Extraction & Interactive Link Rules (No Exceptions)

Whenever extracting itineraries, updating walkthrough artifacts, or generating travel briefings, every individual reservation MUST explicitly satisfy these completeness and formatting standards:

### 1. 🌐 Interactive Link & Media Standards (User Preference Enforcement)
Every accommodation, dining reservation, vehicle rental, and transit record MUST embed the following interactive hyperlinks in Markdown and Google Docs formatting:
- **Booking Management Portals**: Include direct links to view or modify bookings on aggregator portals (e.g., `https://www.booking.com/myreservations.html` or `https://ch.hotels.com/trips` or direct resort management URLs).
- **Google Maps Navigation Links (Mandatory Search API URLs)**: Must embed a direct Google Maps Search API URL for every hotel, rental pickup station, transit destination, and restaurant. Due to a bug where other formats route to the user's current location on mobile, you MUST format it exactly as `https://www.google.com/maps/search/?api=1&query=<Exact+Property+Name+and+Full+Street+Address>`, and NEVER use `https://www.google.com/maps/place/...` or `https://maps.google.com/?q=...`.
- **Online Check-in Hyperlinks**: Whenever a property or airline provides digital pre-registration or online check-in URLs (e.g., Casablanca guest app links), explicitly format them as interactive hyperlinks (e.g., `🎟️ [Complete Online Check-in](URL)`).
- **Mandatory Clickable Expense Invoice & Carrier Linking Rule (No Superfluous Baggage Receipts!)**: For EVERY single flight reservation, eTicket confirmation, mobility taxi ride, vehicle rental invoice, and dining bill, you MUST explicitly embed a direct clickable viewing hyperlink to the official digital expense invoice artifact or file in Google Drive (`[View Certified Expense Invoice on Google Drive](https://drive.google.com/file/d/...)`) AND the official airline or carrier management website (e.g., `https://www.swiss.com/manage-my-booking` or `https://www.united.com`) directly alongside the financial total. Do NOT clutter flight entries with superfluous baggage check receipts; strictly provide the financial expense invoice and official carrier website!
- **Mandatory Dual-Currency Restaurant Billing Rule (Original + USD Conversion)**: For every restaurant dining receipt or café bill, you MUST explicitly display both the original local currency amount (e.g., CHF, PLN, EUR, GBP) AND its accurate conversion into United States Dollars (**USD / $**) using prevailing commercial exchange rates (e.g., CHF 1.00 = $1.16 USD, PLN 1.00 = $0.26 USD). Always display as: **`<Original Currency Amount> ($<USD Amount> USD)`** (e.g., `CHF 84.40 ($97.90 USD)` or `PLN 218.00 ($56.68 USD)`). Never report a restaurant bill solely in local currency or Euros!
- **Strict Chronological Ordering for All Items**: Within every section of an itinerary, ALL items (including flights, accommodations, car rentals, dining reservations, transit receipts, mobility rides, and shopping transactions) MUST be arranged in strict forward chronological order by exact start date and timestamp. Never present items out of temporal sequence! Whenever a date or time is modified or updated for an existing reservation, you MUST immediately re-sort and re-number the items in that section to maintain this strict chronological order.
- **Past Trips & Google Doc Annex**: When a travel is in the past, move it to an "Annex: Past Trips" section at the very bottom of the Markdown artifact. When syncing to the Google Doc, you must include this Annex at the bottom of the document, separated by a clear visual divider (e.g., `===== PAST TRIPS =====`), to keep past trips organized without relying on native Google Doc tabs (which our automated sync tool does not support).
- **Mandatory Parsing of "Travel Receipts" Google Drive Folder & Concur Focus**: During every itinerary check or update, you MUST explicitly authenticate against personal Google Drive (`odewolf@gmail.com` via `odewolf_token.json`), query the dedicated **"Travel Receipts" folder (Folder ID: `1aKpW04rznAjQINaoWi7EyDcRKVadsw4p`)**, download all uploaded receipt pictures or invoice files, execute visual OCR parsing to extract exact vendor names, timestamps, and financial totals, and synchronize them directly into the master schedule and Live Google Doc! When reporting or summarizing SAP Concur manifests, focus strictly on active travel reports like Poland (July 22) and brand-new upcoming trip confirmations!
- **Clean Document Styling Rule**: When writing to Google Docs, NEVER display ugly raw URLs in plain text; use native Google Docs API styling to embed clickable hyperlinks directly into descriptive text titles.

### 2. 🏨 Hotels, Corporate Guest Housing & Bay View Lodging Rules
Must explicitly record:
- **Corporate & Silicon Valley Stays (No Missed Bay View Stays!)**: Always actively search for and incorporate corporate accommodations, Concur itineraries, and Silicon Valley guest housing—specifically querying for **"Bay View Hotel"**, **"Bayview"**, **"Mountain View"**, and corporate trip record locators. NEVER omit Bay View accommodations!
- **Aggregator & Third-Party Platforms**: Ensure you explicitly search for booking confirmations from platforms like **Hotels.com**, Expedia, and Booking.com when correlating dates with flights or car rentals, so that no accommodation is missed.
- **Exact Start Date & Check-in Time** (e.g., *November 15, 2026 at 15:00*) and **Exact End Date & Check-out Time** (e.g., *November 18, 2026 at 11:00*) and Total Nights.
- **Multi-Hotel Stay Dates**: Whenever an itinerary or trip segment includes two or more sequential accommodations (e.g., Hotel Maloja Palace followed by Chasa Castello), you MUST explicitly record the exact check-in and check-out start/end dates for *each individual hotel* rather than just listing nights or totals.
- **Property Name, Location & Booking Confirmation Number**
- **Exact Price & Amount Paid / Rate**: Explicitly state financial total and currency (in CHF, GBP, USD, ZAR, EUR, etc.) or corporate Concur billing status.

### 3. ✈️ Flights & Air Travel
Must explicitly record:
- **Mandatory PDF Receipt Generation**: For ALL flights, you MUST extract the flight email receipt, generate a PDF using the provided script (`python3 /usr/local/google/home/odwl/Documents/dev/my-haskell-project/.agents/skills/extract-uber-receipts/scripts/extract_uber.py <email_message_id>`), upload it to the "Travel Receipts" Google Drive folder, and embed the clickable Google Drive link in the itinerary! If a booking has multiple legs (e.g., outbound and return), the PDF receipt link MUST be explicitly attached to EVERY individual flight leg entry.
- **Companion Flights (No Missed Companion Legs!)**: When checking flights for a specific date range or car rental period, always explicitly search for companion flights (e.g., for "Natalie Adamec" or origin/destination pairs like "PRG") that may have been booked separately from the primary user's itinerary.
- **Exact Departure Date & Time** and **Arrival Date & Time**
- **Operating Airline & Flight Number** (e.g., *SWISS Flight LX 1488*, *United Flight UA 9728*)
- **Passenger Name ("Who")**: Fully identify assigned traveler(s) (e.g., *Olivier de Wolf*, *Natalie Adamec*)
- **Booking Reference Code (PNR)** and associated eTicket numbers (`016...`) or cabin/seat assignments.

### 4. 🚗 Car Rentals & Ground Mobility
Must explicitly record:
- **Rental Provider / Agency**: (e.g., *Europcar*, *Sixt*, *Hertz*, *Avis*, *Enterprise*, *Uber*)
- **Exact Pick-up Start Date & Time** and **Station Location** (with Google Maps link)
- **Exact Return End Date & Time** and **Station Location**
- **Vehicle Class / Model**, **Driver Name**, and **Reservation Reference Number**
- **Exact Price & Total Rate Paid**: Explicitly record total hire cost and currency (e.g., *CHF 410.92 / GBP £373.02* or *PLN 107.14*).
- **EXCLUSION RULE**: Do NOT capture or report local commuter rail marketing, general public transit promotions, or bike-share discount mailers (such as **SBB Rail** or **SwissPass** promotions). Purge these from travel briefings.

### 5. 🍽️ Restaurant & Dining Reservations
Must explicitly record:
- **Restaurant Name & City/Location** as a distinct, top-level itinerary bullet point (never bury dining confirmations in parenthetical notes).
- **Exact Date & Time** (in 24-hour format, e.g., `18:30` or `19:00` or `20:00`).
- **Booking Platform & Reference** and table party size.
- **Location Link**: Clickable Google Maps URL.
- **No Dish Details Rule**: Do NOT include details of the dishes or items ordered. Only include the total price and the exact date.

---

## Executive Google Docs API Formatting & Self-Cleaning Calendar Sync

1. **Native Google Docs API Styling (No Raw Conversions!)**:
   - Never use raw plain-text Drive file conversion for Google Docs, as it produces atrocious formatting and broken tables.
   - Use the authenticated **Google Docs API** (`https://www.googleapis.com/auth/documents`) to construct executive-ready documents featuring:
     - **Live Google Doc Link & Update Date Header**: Always include the official Google Doc URL link and a prominent update timestamp at the very top of the Travel Itinerary document right beneath the main title (e.g., `🌐 [Live Google Docs Itinerary](https://docs.google.com/document/d/1N4YjroxgRAdQPOrMrNQYGiRiMMyX4GkNAat9yOWvpk4/edit) | 📅 **Last Updated**: July 21, 2026`).
     - **Professional Typography**: Clean modern fonts with bold headings and structural hierarchy.
     - **Native Tables**: Structured Executive Timeline tables with custom shaded header rows and padded cells.
     - **Embedded Hyperlinks**: Interactive links embedded cleanly into readable anchor text.
2. **Bi-Directional Private Calendar Sync**:
   - Insert valid upcoming flights, hotel stays, vehicle pickups/returns, and dining reservations directly into the user's primary private calendar using explicit event titles (using proper English, e.g., *"travelling"*).
   - Embed booking reference numbers and PNRs directly inside the event Description or Title to serve as stable tracking keys.
3. **Automated Cancellation & Refund Scrubbing (Self-Cleaning Mode)**:
   - Actively parse incoming cancellation notices, flight refund receipts, or revoked bookings.
   - Whenever a booking reference or PNR is identified as cancelled, query the Calendar API (`service.events().list(q=<PNR_OR_REF>)`) and automatically execute an API deletion (`service.events().delete(...)`).
