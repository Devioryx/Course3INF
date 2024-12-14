import json

def csv_format(data: list[dict]) -> str:
    lines = ["title,rating"]
    for item in data:
        line = f"{item['title']}, {item['rating']}"
        lines.append(line)
    return "\n".join(lines)

def json_format(data: list[dict]) -> str:
    return json.dumps(data)