import sys

def parse_args() -> tuple:
    if len(sys.argv) != 4:
        print("You must provide 3 arguments")
        sys.exit(1)

    media_type = sys.argv[1]
    time = sys.argv[2]
    output = sys.argv[3]

    if media_type not in ["movie", "tv", "all"]:
        raise ValueError("Type must be one of 'movie', 'tv' or 'all'.")

    if time not in ["day", "week"]:
        raise ValueError("Time must be one of 'day' or 'week'.")

    if output not in ["json", "csv"]:
        raise ValueError("Output must be one of 'json' or 'csv'.")

    return media_type, time, output