"""Market data loading with on-disk caching.

Uses yfinance for real OHLCV history. Caches to parquet so repeated
backtests don't hammer the network (and so it works offline once cached).
"""
from __future__ import annotations
import os
import pandas as pd

CACHE_DIR = os.path.join(os.path.dirname(__file__), ".cache")


def get_ohlcv(ticker: str, start: str, end: str, interval: str = "1d",
              use_cache: bool = True) -> pd.DataFrame:
    """Return a clean OHLCV DataFrame indexed by date.

    Columns: open, high, low, close, volume (lowercase, single level).
    """
    os.makedirs(CACHE_DIR, exist_ok=True)
    key = f"{ticker}_{start}_{end}_{interval}".replace(":", "-")
    path = os.path.join(CACHE_DIR, key + ".pkl")
    if use_cache and os.path.exists(path):
        return pd.read_pickle(path)

    import yfinance as yf
    raw = yf.download(ticker, start=start, end=end, interval=interval,
                      progress=False, auto_adjust=True)
    if raw is None or len(raw) == 0:
        raise RuntimeError(f"No data returned for {ticker} {start}..{end}")

    # yfinance returns a MultiIndex column frame (field, ticker). Flatten it.
    if isinstance(raw.columns, pd.MultiIndex):
        raw.columns = raw.columns.get_level_values(0)
    df = raw.rename(columns=str.lower)[["open", "high", "low", "close", "volume"]].copy()
    df = df.dropna()
    df.index.name = "date"
    df.to_pickle(path)
    return df
