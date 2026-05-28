"""Portfolio backtest: run a strategy across a basket and combine equity.

Trend-following's real edge is diversification across uncorrelated markets
(the Turtles traded ~20+ futures). This allocates risk equally across the
basket and sums the per-asset equity curves.
"""
from __future__ import annotations
import argparse
import numpy as np
import pandas as pd
from data import get_ohlcv
from engine import backtest, Costs
from strategies import REGISTRY

DEFAULT_BASKET = ["SPY", "QQQ", "GLD", "TLT", "USO", "EEM", "EFA", "IWM"]


def fmt_pct(x):
    return "n/a" if x is None else f"{x*100:6.1f}%"


def run_portfolio(strategy_fn, tickers, start, end, risk, costs, capital):
    per_asset = capital / len(tickers)
    curves = []
    n_trades = 0
    for t in tickers:
        try:
            df = get_ohlcv(t, start, end)
        except Exception as e:
            print(f"  (skip {t}: {e})")
            continue
        res = backtest(df, strategy_fn(df), starting_equity=per_asset,
                       risk_frac=risk, costs=costs)
        n_trades += len(res.trades)
        curves.append(res.equity.rename(t))
    port = pd.concat(curves, axis=1).ffill().dropna().sum(axis=1)
    return port, n_trades, len(curves)


def stats_from_equity(eq, n_trades):
    rets = eq.pct_change().dropna()
    years = max(len(eq) / 252.0, 1e-9)
    total = eq.iloc[-1] / eq.iloc[0] - 1
    cagr = (eq.iloc[-1] / eq.iloc[0]) ** (1 / years) - 1
    vol = rets.std() * np.sqrt(252)
    sharpe = (rets.mean() * 252) / vol if vol > 0 else 0
    dd = (eq / eq.cummax() - 1).min()
    return total, cagr, sharpe, dd, n_trades


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tickers", default=",".join(DEFAULT_BASKET))
    ap.add_argument("--start", default="2010-01-01")
    ap.add_argument("--end", default="2024-12-31")
    ap.add_argument("--risk", type=float, default=0.01)
    ap.add_argument("--capital", type=float, default=100_000.0)
    args = ap.parse_args()
    tickers = [t.strip() for t in args.tickers.split(",") if t.strip()]
    costs = Costs()

    print(f"\nPortfolio: {tickers}")
    print(f"{args.start} -> {args.end}   risk/trade={args.risk:.1%}")
    print("=" * 78)
    print(f"{'strategy':<14}{'totRet':>10}{'CAGR':>9}{'Sharpe':>9}{'maxDD':>10}{'trades':>9}")
    print("-" * 78)
    for name, fn in REGISTRY.items():
        port, nt, nused = run_portfolio(fn, tickers, args.start, args.end,
                                        args.risk, costs, args.capital)
        total, cagr, sharpe, dd, _ = stats_from_equity(port, nt)
        print(f"{name:<14}{fmt_pct(total):>10}{fmt_pct(cagr):>9}{sharpe:>9.2f}"
              f"{fmt_pct(dd):>10}{nt:>9}")

    # equal-weight buy & hold of the basket as benchmark
    bh_curves = []
    for t in tickers:
        try:
            df = get_ohlcv(t, args.start, args.end)
            bh_curves.append((df["close"] / df["close"].iloc[0]).rename(t))
        except Exception:
            pass
    bh = pd.concat(bh_curves, axis=1).ffill().dropna().mean(axis=1)
    total, cagr, sharpe, dd, _ = stats_from_equity(bh * args.capital, 0)
    print("-" * 78)
    print(f"{'buy&hold(EW)':<14}{fmt_pct(total):>10}{fmt_pct(cagr):>9}{sharpe:>9.2f}{fmt_pct(dd):>10}")
    print("=" * 78)


if __name__ == "__main__":
    main()
