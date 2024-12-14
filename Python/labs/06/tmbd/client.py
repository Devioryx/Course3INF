import os
import requests
from dotenv import load_dotenv

load_dotenv()

API_TOKEN = os.getenv("TOKEN")
BASE_URL = "https://api.themoviedb.org/3"

def get_trending(media_type: str, time: str) -> dict:
    url = f"{BASE_URL}/trending/{media_type}/{time}"

    headers = {
        "Authorization": f"Bearer {API_TOKEN}"
    }

    try:
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        return response.json()
    except requests.exceptions.RequestException as e:
        raise RuntimeError(f"Error fetching data from TMDB: {e}")