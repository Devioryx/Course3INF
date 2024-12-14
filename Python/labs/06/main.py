from tmbd.arg_parser import parse_args
from tmbd.fetch import fetch_data
from tmbd.output import csv_format, json_format
import sys

if __name__ == "__main__":
    try :
        media_type, time, output_format = parse_args()
    except ValueError as e:
        print(f"An error occurred while parsing arguments: {e}")
        sys.exit(1)

    try:
        result = fetch_data(media_type, time)
    except Exception as e:
        print(f"An error occurred while fetching data: {e}")
        sys.exit(1)
    
    result.sort(key=lambda x: x["rating"], reverse=True)

    if output_format == "json":
        output = json_format(result)

    elif output_format == "csv":
        output = csv_format(result)

    print(output)