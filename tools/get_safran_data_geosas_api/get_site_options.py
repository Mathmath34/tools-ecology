import csv

def get_site_options(site_metadata):
    """
    Read unique site_name values from the site_metadata CSV dataset
    and return them as a list of (display_name, value, selected) tuples
    for Galaxy dynamic_options.
    """
    if site_metadata is None:
        return []
    try:
        seen = set()
        options = []
        with open(site_metadata.file_name, newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            for row in reader:
                val = row.get("site_name", "").strip()
                if val and val not in seen:
                    seen.add(val)
                    options.append((val, val, False))
        return sorted(options, key=lambda x: x[0])
    except Exception:
        return []
