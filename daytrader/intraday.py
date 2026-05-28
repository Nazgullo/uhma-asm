"""Actual day trading: intraday bars, flat by the close, no overnight risk.

Data: yfinance intraday (5m/1m), up to ~60 days of history.

Strategies (the documented intraday playbook):
  * Opening Range Breakout (ORB): take the high/low of the first N minutes;
    go long on a break above, short on a break below. One entry per day.
    Hard stop at the opposite side of the range. Flat at the close.
  * VWAP reversion: fade stretches away from session VWAP, exit back at VWAP.

Every position is force-closed on the last bar of its session. Commission
and slippage are charged on every fill -- intraday, costs dominate, which is
exactly why most day traders lose.
"""
from __future__ import annotations
import argparse
import numpy as np
import pandas as pd


def get_intraday(ticker: str, interval: str = "5m", period: str = "60d") -> pd.DataFrame:
    import yfinance as yf
    raw = yf.download(ticker, period=period, interval=interval,
                      progress=False, auto_adjust=True)
    if raw is None or len(raw) == 0:
        raise RuntimeError(f"no intraday data for {ticker}")
    if isinstance(raw.columns, pd.MultiIndex):
        raw.columns = raw.columns.get_level_values(0)
    df = raw.rename(columns=str.lower)[["open", "high", "low", "close", "volume"]].dropna()
    df = df.tz_convert("America/New_York")
    df = df.between_time("09:30", "16:00")
    df["session"] = df.index.date
    return df


def session_vwap(g: pd.DataFrame) -> pd.Series:
    tp = (g["high"] + g["low"] + g["close"]) / 3.0
    cum_pv = (tp * g["volume"]).cumsum()
    cum_v = g["volume"].cumsum().replace(0, np.nan)
    return cum_pv / cum_v


# ---- per-session signal generators: return target pos per bar in {-1,0,1} ----

def orb_session(g: pd.DataFrame, open_bars: int = 6) -> pd.Series:
    """Opening Range Breakout. open_bars*interval defines the opening range."""
    pos = np.zeros(len(g), dtype=int)
    if len(g) <= open_bars:
        return pd.Series(pos, index=g.index)
    or_hi = g["high"].iloc[:open_bars].max()
    or_lo = g["low"].iloc[:open_bars].min()
    state = 0
    c = g["close"].values
    for i in range(open_bars, len(g)):
        if state == 0:
            if c[i] > or_hi:
                state = 1
            elif c[i] < or_lo:
                state = -1
        # stop: price crosses back through the opposite range edge
        elif state == 1 and c[i] < or_lo:
            state = 0
        elif state == -1 and c[i] > or_hi:
            state = 0
        pos[i] = state
    return pd.Series(pos, index=g.index)


def vwap_session(g: pd.DataFrame, k: float = 1.5) -> pd.Series:
    """Fade deviations of k * rolling std from session VWAP; exit at VWAP."""
    pos = np.zeros(len(g), dtype=int)
    vwap = session_vwap(g).values
    c = g["close"].values
    dev = c - vwap
    band = pd.Series(dev).expanding(min_periods=6).std().values
    state = 0
    for i in range(len(g)):
        if np.isnan(band[i]) or band[i] == 0:
            pos[i] = state
            continue
        z = dev[i] / band[i]
        if state == 0:
            if z < -k:
                state = 1     # too far below VWAP -> buy
            elif z > k:
                state = -1    # too far above -> sell
        elif state == 1 and z >= 0:
            state = 0
        elif state == -1 and z <= 0:
            state = 0
        pos[i] = state
    return pd.Series(pos, index=g.index)


INTRADAY = {"orb": orb_session, "vwap": vwap_session}


def backtest_intraday(df: pd.DataFrame, sig_fn, *, capital=100_000.0,
                      risk_frac=0.005, commission_bps=1.0, slippage_bps=2.0,
                      **kw):
    """Day-trading backtest. Each session is independent and forced flat
    on its final bar. Returns (equity_series, trades, stats)."""
    fee = (commission_bps + slippage_bps) / 10_000.0
    equity = capital
    eq_points = []
    trades = []

    for _, g in df.groupby("session", sort=True):
        if len(g) < 8:
            continue
        target = sig_fn(g, **kw).values
        closes = g["close"].values
        idx = g.index
        pos, qty, entry = 0, 0.0, 0.0
        # size per trade so a ~0.5% adverse move ~ risk_frac of equity
        for i in range(len(g)):
            px = closes[i]
            force_flat = (i == len(g) - 1)
            want = 0 if force_flat else int(target[i])
            if pos != 0 and want != pos:
                exec_px = px * (1 - fee * pos)
                pnl = pos * (exec_px - entry) * qty
                equity += pnl
                trades.append(pnl)
                pos, qty, entry = 0, 0.0, 0.0
            if pos == 0 and want != 0:
                qty = (risk_frac * equity) / (0.005 * px)   # vol-scaled notional
                qty = min(qty, equity / px)                  # cap at 1x
                exec_px = px * (1 + fee * want)
                pos, entry = want, exec_px
            unreal = pos * (px - entry) * qty if pos else 0.0
            eq_points.append((idx[i], equity + unreal))

    eq = pd.Series(dict(eq_points)).sort_index()
    rets = eq.pct_change().dropna()
    wins = [t for t in trades if t > 0]
    gl = -sum(t for t in trades if t <= 0)
    stats = {
        "total_return": eq.iloc[-1] / capital - 1 if len(eq) else 0,
        "num_trades": len(trades),
        "win_rate": len(wins) / len(trades) if trades else 0,
        "profit_factor": (sum(wins) / gl) if gl > 0 else float("inf"),
        "avg_trade_$": np.mean(trades) if trades else 0,
        "max_drawdown": (eq / eq.cummax() - 1).min() if len(eq) else 0,
        "sessions": df["session"].nunique(),
    }
    return eq, trades, stats


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ticker", default="SPY")
    ap.add_argument("--interval", default="5m")
    ap.add_argument("--period", default="60d")
    ap.add_argument("--risk", type=float, default=0.005)
    args = ap.parse_args()

    df = get_intraday(args.ticker, args.interval, args.period)
    print(f"\n{args.ticker} INTRADAY  {args.interval} bars  "
          f"{df['session'].nunique()} sessions  "
          f"({df.index.min().date()} -> {df.index.max().date()})   flat-at-close")
    print("=" * 84)
    print(f"{'strategy':<10}{'totRet':>10}{'trades':>9}{'win%':>8}{'PF':>8}"
          f"{'avg$/trade':>13}{'maxDD':>9}")
    print("-" * 84)
    for name, fn in INTRADAY.items():
        eq, trades, s = backtest_intraday(df, fn, risk_frac=args.risk)
        pf = "inf" if s["profit_factor"] == float("inf") else f"{s['profit_factor']:.2f}"
        print(f"{name:<10}{s['total_return']*100:>9.1f}%{s['num_trades']:>9}"
              f"{s['win_rate']*100:>7.0f}%{pf:>8}{s['avg_trade_$']:>12.2f} "
              f"{s['max_drawdown']*100:>8.1f}%")
    print("=" * 84)
    print("Note: ~60 sessions is a tiny sample. Positive here is NOT an edge -- "
          "it's noise until proven over years and out-of-sample.")


if __name__ == "__main__":
    main()
