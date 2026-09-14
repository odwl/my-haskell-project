---
name: track-package-orders
description: Automatically scan personal and corporate Gmail accounts for online retail orders, parcel shipments, and deliveries (Bergzeit, Running Warehouse, Galaxus, Digitec, Zalando, Otto, Travelpro, James Perse, Amazon, etc.), extracting exact order dates, product photos/images, financial totals, carrier tracking numbers (Swiss Post, DHL, UPS, DPD), delivery timelines, and merchant portals.
---

# Track Package Orders & Delivery Shipments Skill

This skill provides an automated methodology, strict metadata extraction standards, and a dedicated extraction engine (`scripts/scan_orders.py`) to systematically monitor, extract, verify, and maintain records of online orders, package dispatches, and carrier deliveries across personal (`odewolf@gmail.com`) and corporate (`odwl@google.com`) Gmail accounts.

---

## Mandatory Data Fields & Verification Rules

Whenever extracting, updating, or displaying online orders and package tracking, every order entry MUST satisfy the following completeness criteria:

### 1. 📅 Order Date & Timestamp
- Record the exact date and local time when the order was placed (e.g., *Saturday, September 12, 2026 at 15:10 CEST*).
- If an order has distinct confirmation and payment timestamps, record the primary order placement timestamp.

### 2. 🛍️ Merchant / Vendor & Direct Portal Link
- Identify the merchant or platform (e.g., **Bergzeit**, **Running Warehouse Europe**, **Zalando**, **Galaxus / Digitec**, **Travelpro**, **James Perse**, **OTTO'S**, **Globus**).
- Embed a direct clickable hyperlink to the merchant website or order management / customer portal (e.g., `[Bergzeit.ch](https://www.bergzeit.ch)` or `[Galaxus Order Detail](https://www.galaxus.ch)`).

### 3. 📦 Product Details (Name, Variant, Size, SKU)
- Explicitly list each item ordered, including product model, size, colorway, and article / SKU number (e.g., *NNormal Tomir 2 Schuhe, Blue-Black, Size 47|47.5* or *HOKA Speedgoat 7, Orbit/Blue, US 12.5*).

### 4. 🖼️ Product Photo / Image
- Extract embedded or catalog thumbnail/image URLs from the confirmation emails or vendor product pages.
- Present product images using standard Markdown image tags (`![Product Name](image_url)`) or HTML image tags (`<img src="..." width="120"/>`) within structured tables or cards so the user can visually recognize the product immediately.

### 5. 💰 Exact Total Price & Currency
- Explicitly record the financial subtotal, shipping fee, tax, and final total paid in the original currency (e.g., **CHF 169.95**, **€102.07 EUR**, **CHF 330.00**, **CHF 380.00**).
- If discounts or vouchers were applied, note them.

### 6. 🚚 Shipping, Carrier & Package ID / Tracking Link
- **Carrier Identification**: Accurately detect the handling logistics carrier:
  - **Die Schweizerische Post (Swiss Post)**: `https://www.post.ch/swisspost-tracking?formattedParcelCodes=<PARCEL_CODE>`
  - **DHL Express / Deutsche Post**: `https://www.dhl.com/en/express/tracking.html?AWB=<AWB>`
  - **UPS**: `https://www.ups.com/track?loc=en_CH&Requester=SBN&tracknum=<TRACKING_NUM>`
  - **DPD**: `https://tracking.dpd.de/status/de_CH/parcel/<PARCEL_NUM>`
  - **Planzer Paket / SendCloud**: direct tracking link.
- **Dispatch Date**: Exact date and time when the parcel was handed to the carrier or dispatched from the warehouse.
- **Delivery Date & Status**:
  - If delivered: exact delivery timestamp (e.g., *Delivered: Saturday, September 5, 2026 at 10:32 CEST*).
  - If in transit: current transit step and carrier estimated delivery window (e.g., *In transit, expected in 2–4 business days*).
  - If processing: order received, awaiting warehouse packing/label creation.

### 7. ⏱️ Strict Chronological Ordering
- Arrange all orders in reverse chronological order (newest orders first) or forward chronological order depending on whether viewing an active pipeline or historical log. By default, present current/active in-flight orders at the top, followed by recent delivered orders.

---

## Token & Account Configuration

- **Personal Account**:
  `odewolf@gmail.com` token: `/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/odewolf_token.json`
- **Corporate Account**:
  `odwl@google.com` token: `/usr/local/google/home/odwl/.gemini/jetski/gmail_mcp/token.json`

## CLI Execution

To run an automated scan across both accounts:
```bash
python3 .agents/skills/track-package-orders/scripts/scan_orders.py
```

---

## 🌐 Executive Google Docs Synchronization (Mandatory Rule)

Whenever orders are scanned, updated, or modified:
1. **Master Google Doc Target**:
   Synchronize the updated catalog into the live Google Doc:
   - **Document URL**: `https://docs.google.com/document/d/1N4YjroxgRAdQPOrMrNQYGiRiMMyX4GkNAat9yOWvpk4/edit`
   - **Document ID**: `1N4YjroxgRAdQPOrMrNQYGiRiMMyX4GkNAat9yOWvpk4`
2. **Automated Synchronization Script**:
   Run the dedicated synchronization script:
   ```bash
   python3 .agents/skills/track-package-orders/scripts/sync_google_doc.py
   ```
   Or invoke the `workspace` MCP tool `update_document` directly via `/google/bin/releases/codemind-mcp-servers/workspace_server.par`.
3. **Synchronization Consistency**:
   Ensure both local artifacts (`docs/knowledge_base/online_orders_and_deliveries_2026.md` and the `## Online Orders & Package Deliveries` section of `docs/knowledge_base/travel_itinerary_2026.md`) are kept in sync with the live Google Doc.

---

## ⏰ Automated Recurring Schedule (4-Hour Cadence)

By user preference, package orders and travel updates are automatically polled **every 4 hours during daytime (`08:00, 12:00, 16:00, 20:00` CEST)** via the `schedule` cron tool (`CronExpression: "0 8,12,16,20 * * *"`):
- **Delta-Only Reporting**: Each scheduled run checks both `odewolf@gmail.com` and `odwl@google.com` for new order confirmations, carrier tracking updates, or travel/dining reservations.
- **Silent When Unchanged**: Only triggers a Google Doc update and user summary when new emails or tracking state changes are detected.
