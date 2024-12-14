from .client import get_trending
from .async_fetch import fetch_all, fetch_trending
import asyncio
from enum import Enum

class MediaType(Enum):
    MOVIE = "movie"
    TV = "tv"


def fetch_data(media_type: str, window:str) -> list[dict]:
    if media_type in {"tv", "movie"}:
        data = get_trending(media_type, window)
        return extract_result(data, MediaType(media_type))
    
    elif media_type == "all":
        movie_data, tv_data = asyncio.run(fetch_all(window))
        result = extract_result(movie_data, MediaType.MOVIE) + extract_result(tv_data, MediaType.TV)
        return result
    
    else:
        raise ValueError("Invalid media type")
    

def extract_result(data: dict, type: MediaType) -> list[dict]:
    result = []
    try:
        for item in data.get("results", []):
            title_key = "title" if type == MediaType.MOVIE else "name"
            title = item.get(title_key, "")
            rating = item.get("vote_average", 0)
            result.append({"title": title, "rating": rating})
    except Exception as e:
            print(f"Error parsing results: {e}")

    return result