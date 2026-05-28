"""Publicly-documented strategies, implemented from their published rules.

Each strategy is a function df -> target-position Series in {-1,0,+1}.
Signals at bar t use only information available at or before t (we shift
breakout reference levels by 1 bar to avoid look-ahead).

Sources of the rules (all published free by their authors / firms):
  * Turtle / Donchian breakout: the Original Turtle Trading Rules
    (Richard Dennis / William Eckhardt, released free), Donchian channels.
  * RSI-2 mean reversion: Larry Connors & Cesar Alvarez,
    "Short Term Trading Strategies That Work" (the RSI(2) method is
    widely published and discussed publicly).
  * MA crossover trend following: classic, ubiquitous.
"""
from __future__ import annotations
import numpy as np
import pandas as pd


def _rsi(close: pd.Series, period: int) -> pd.Series:
    delta = close.diff()
    up = delta.clip(lower=0).ewm(alpha=1 / period, adjust=False).mean()
    down = (-delta.clip(upper=0)).ewm(alpha=1 / period, adjust=False).mean()
    rs = up / down.replace(0, np.nan)
    return 100 - (100 / (1 + rs))


def turtle(df: pd.DataFrame, entry: int = 20, exit: int = 10,
           long_only: bool = False) -> pd.Series:
    """Donchian breakout. Long on N-day high, exit on shorter-period low
    (and symmetrically for shorts). This is the System-1 Turtle core."""
    high = df["high"]
    low = df["low"]
    # reference levels exclude the current bar (shift 1) -> no look-ahead
    entry_hi = high.rolling(entry).max().shift(1)
    entry_lo = low.rolling(entry).min().shift(1)
    exit_hi = high.rolling(exit).max().shift(1)
    exit_lo = low.rolling(exit).min().shift(1)

    pos = pd.Series(0, index=df.index, dtype=int)
    state = 0
    c = df["close"].values
    eh, el = entry_hi.values, entry_lo.values
    xh, xl = exit_hi.values, exit_lo.values
    out = np.zeros(len(df), dtype=int)
    for i in range(len(df)):
        if np.isnan(eh[i]):
            out[i] = state
            continue
        if state == 0:
            if c[i] > eh[i]:
                state = 1
            elif c[i] < el[i] and not long_only:
                state = -1
        elif state == 1:
            if c[i] < xl[i]:
                state = 0
        elif state == -1:
            if c[i] > xh[i]:
                state = 0
        out[i] = state
    pos[:] = out
    return pos


def ma_crossover(df: pd.DataFrame, fast: int = 20, slow: int = 50,
                 long_only: bool = True) -> pd.Series:
    c = df["close"]
    f = c.rolling(fast).mean()
    s = c.rolling(slow).mean()
    pos = np.where(f > s, 1, -1)
    pos = pd.Series(pos, index=df.index, dtype=int)
    pos[s.isna()] = 0
    if long_only:
        pos = pos.clip(lower=0)
    return pos


def rsi2_meanrev(df: pd.DataFrame, trend: int = 200, rsi_buy: int = 10,
                 rsi_exit: int = 70) -> pd.Series:
    """Connors RSI(2): only go long when price is above its long-term
    trend MA and 2-period RSI is oversold; exit when RSI recovers.
    Long-only mean reversion -- a documented retail favorite."""
    c = df["close"]
    sma = c.rolling(trend).mean()
    rsi = _rsi(c, 2)
    pos = pd.Series(0, index=df.index, dtype=int)
    state = 0
    cv, sv, rv = c.values, sma.values, rsi.values
    out = np.zeros(len(df), dtype=int)
    for i in range(len(df)):
        if np.isnan(sv[i]) or np.isnan(rv[i]):
            out[i] = 0
            continue
        if state == 0:
            if cv[i] > sv[i] and rv[i] < rsi_buy:
                state = 1
        elif state == 1:
            if rv[i] > rsi_exit or cv[i] < sv[i]:
                state = 0
        out[i] = state
    pos[:] = out
    return pos


REGISTRY = {
    "turtle": turtle,
    "ma_crossover": ma_crossover,
    "rsi2": rsi2_meanrev,
}
