import os
import asyncio
import aiohttp
from dotenv import load_dotenv

load_dotenv()

API_TOKEN = os.getenv("TOKEN")
BASE_URL = "https://api.themoviedb.org/3"

async def fetch_trending(session: aiohttp.ClientSession, media_type: str, time: str) -> dict:
    url = f"{BASE_URL}/trending/{media_type}/{time}"

    try:
        async with session.get(url) as response:
            response.raise_for_status()
            return await response.json()
    except aiohttp.ClientError as e:
        raise RuntimeError(f"Network or request error for {media_type}: {e}")
    
async def fetch_all(time: str) -> tuple:

    headers = {
        "Authorization": "Bearer {API_TOKEN}",
        "Content-Type" : "application/json"
    }

    async with aiohttp.ClientSession(headers=headers) as session:
        try:
            movie_task = asyncio.create_task(fetch_trending(session, "movie", time))
            tv_task = asyncio.create_task(fetch_trending(session, "tv", time))

            movie_data, tv_data = await asyncio.gather(movie_task, tv_task)
            return movie_data, tv_data
        except Exception as e:
            raise RuntimeError(f"Error during asynchronous fetching: {e}")