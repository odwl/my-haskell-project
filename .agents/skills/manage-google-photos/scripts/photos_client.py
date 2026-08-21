#!/usr/bin/env python3
import sys
import os
import argparse
import json
import urllib.request
import urllib.parse

def get_headers():
    token = os.environ.get("GOOGLE_DRIVE_TOKEN")
    if not token:
        print("ERROR: Please set GOOGLE_DRIVE_TOKEN environment variable.")
        sys.exit(1)
    return {"Authorization": f"Bearer {token}"}

def cmd_list(args):
    headers = get_headers()
    params = {
        "q": "mimeType contains 'image/' or mimeType contains 'video/'",
        "pageSize": args.limit,
        "orderBy": "createdTime desc",
        "fields": "files(id, name, mimeType, createdTime, webViewLink)"
    }
    url = "https://www.googleapis.com/drive/v3/files?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req) as resp:
        data = json.loads(resp.read().decode())
        files = data.get("files", [])
        print(f"Retrieved {len(files)} recent media items:\n")
        for idx, f in enumerate(files, 1):
            print(f"{idx}. {f.get('name')} ({f.get('mimeType')}) - {f.get('createdTime')} | ID: {f.get('id')}")

def cmd_latest(args):
    headers = get_headers()
    params = {
        "q": "mimeType contains 'image/'",
        "pageSize": 1,
        "orderBy": "createdTime desc",
        "fields": "files(id, name, mimeType, createdTime, webViewLink)"
    }
    url = "https://www.googleapis.com/drive/v3/files?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req) as resp:
        data = json.loads(resp.read().decode())
        files = data.get("files", [])
        if not files:
            print("No photo files found.")
            return
        top = files[0]
        print(f"Latest Photo: {top.get('name')} | Created: {top.get('createdTime')} | ID: {top.get('id')}")
        out_path = args.output or f"/tmp/latest_{top.get('name')}"
        dl_url = f"https://www.googleapis.com/drive/v3/files/{top.get('id')}?alt=media"
        with urllib.request.urlopen(urllib.request.Request(dl_url, headers=headers)) as dl_resp, open(out_path, "wb") as out_f:
            out_f.write(dl_resp.read())
        print(f"Downloaded to {out_path}")

def cmd_search(args):
    headers = get_headers()
    q = f"mimeType contains 'image/' and name contains '{args.query}'"
    params = {
        "q": q,
        "pageSize": args.limit,
        "orderBy": "createdTime desc",
        "fields": "files(id, name, mimeType, createdTime, webViewLink)"
    }
    url = "https://www.googleapis.com/drive/v3/files?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers=headers)
    with urllib.request.urlopen(req) as resp:
        data = json.loads(resp.read().decode())
        files = data.get("files", [])
        print(f"Found {len(files)} items matching query '{args.query}':\n")
        for idx, f in enumerate(files, 1):
            print(f"{idx}. {f.get('name')} | {f.get('createdTime')} | ID: {f.get('id')}")

def cmd_download(args):
    headers = get_headers()
    dl_url = f"https://www.googleapis.com/drive/v3/files/{args.file_id}?alt=media"
    out_path = args.output or f"/tmp/download_{args.file_id}.jpg"
    with urllib.request.urlopen(urllib.request.Request(dl_url, headers=headers)) as dl_resp, open(out_path, "wb") as out_f:
        out_f.write(dl_resp.read())
    print(f"Downloaded file {args.file_id} to {out_path}")

def main():
    parser = argparse.ArgumentParser(description="Google Photos & Media Client CLI")
    subparsers = parser.add_subparsers(dest="command")

    list_p = subparsers.add_parser("list")
    list_p.add_argument("--limit", type=int, default=10)
    list_p.set_defaults(func=cmd_list)

    latest_p = subparsers.add_parser("latest")
    latest_p.add_argument("--output", type=str)
    latest_p.set_defaults(func=cmd_latest)

    search_p = subparsers.add_parser("search")
    search_p.add_argument("--query", type=str, required=True)
    search_p.add_argument("--limit", type=int, default=10)
    search_p.set_defaults(func=cmd_search)

    dl_p = subparsers.add_parser("download")
    dl_p.add_argument("--file-id", type=str, required=True)
    dl_p.add_argument("--output", type=str)
    dl_p.set_defaults(func=cmd_download)

    args = parser.parse_args()
    if hasattr(args, "func"):
        args.func(args)
    else:
        parser.print_help()

if __name__ == "__main__":
    main()
