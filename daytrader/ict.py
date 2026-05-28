"""ICT / Smart Money Concepts -- the non-canonical 'guru' method.

This is NOT the textbook playbook. It implements the mechanical core of
the Inner Circle Trader (Michael Huddleston) approach, which he released
publicly for free and which has a cult following. The thesis: "smart money"
runs retail stops (a liquidity sweep), then reverses, and price re-enters
through a Fair Value Gap (FVG) imbalance. We only trade the NY kill zone
and go flat by the close.

Mechanical rules implemented here (per the publicly published definitions):
  * Liquidity sweep: current bar's high takes out the prior swing high
    (buy-side liquidity) then closes back below it -> expect DOWN reversal;
    symmetrically for sell-side -> UP reversal.
  * Fair Value Gap (3-bar imbalance) confirms displacement in the reversal
    direction:
        bullish FVG: low[i] > high[i-2]   (gap between bar i-2 and bar i)
        bearish FVG: high[i] < low[i-2]
  * Kill zone: only enter inside the New York session window.
  * Risk: stop just beyond the sweep extreme; fixed reward multiple (R).
    Flat at session close. No overnight.
"""
from __future__ import annotations
import argparse
import numpy as np
import pandas as pd
from intraday import get_intraday


def _swing_levels(high, low, lookback):
    """Prior swing high/low over a trailing window (excludes current bar)."""
    sh = pd.Series(high).rolling(lookback).max().shift(1).values
    sl = pd.Series(low).rolling(lookback).min().shift(1).values
    return sh, sl


def ict_signals(g: pd.DataFrame, *, lookback=12, rr=2.0, fvg_window=5,
                kz_start="09:30", kz_end="11:30", _diag=None):
    """Sequential ICT setup, per the published method:
       1) liquidity sweep of a prior swing extreme (stop run),
       2) within `fvg_window` bars, a Fair Value Gap forms in the reversal
          direction (displacement = institutions stepping in),
       3) enter at the close of the FVG bar; stop beyond the sweep extreme;
          target at R:R multiple. One position at a time, flat at close.
    Returns list of (entry_idx, side, entry, stop, target)."""
    h, l, c = g["high"].values, g["low"].values, g["close"].values
    sh, sl = _swing_levels(h, l, lookback)
    in_kz = (g.index.time >= pd.Timestamp(kz_start).time()) & \
            (g.index.time <= pd.Timestamp(kz_end).time())
    n = len(g)
    signals = []
    n_sweep = n_fvg = 0
    # pending sweep: (direction, sweep_extreme, expiry_bar)
    pending = None
    for i in range(2, n):
        # detect a sweep at bar i
        swept_low = (l[i] < sl[i]) if not np.isnan(sl[i]) else False
        swept_high = (h[i] > sh[i]) if not np.isnan(sh[i]) else False
        if swept_low:
            pending = (1, l[i], i + fvg_window); n_sweep += 1
        elif swept_high:
            pending = (-1, h[i], i + fvg_window); n_sweep += 1

        if pending is None or i > pending[2]:
            pending = None if (pending and i > pending[2]) else pending
            continue
        side, sweep_ext, _ = pending
        # look for an FVG in the reversal direction at bar i (uses i, i-2)
        bull_fvg = l[i] > h[i - 2]
        bear_fvg = h[i] < l[i - 2]
        if side == 1 and bull_fvg and in_kz[i]:
            n_fvg += 1
            entry, stop = c[i], sweep_ext
            if entry - stop > 0:
                signals.append((i, 1, entry, stop, entry + rr * (entry - stop)))
                pending = None
        elif side == -1 and bear_fvg and in_kz[i]:
            n_fvg += 1
            entry, stop = c[i], sweep_ext
            if stop - entry > 0:
                signals.append((i, -1, entry, stop, entry - rr * (stop - entry)))
                pending = None
    if _diag is not None:
        _diag["sweeps"] += n_sweep
        _diag["fvg_after_sweep"] += n_fvg
    return signals


def backtest_ict(df, *, capital=100_000.0, risk_frac=0.005,
                 commission_bps=1.0, slippage_bps=2.0, **kw):
    fee = (commission_bps + slippage_bps) / 10_000.0
    equity = capital
    trades = []
    eq_curve = []
    diag = {"sweeps": 0, "fvg_after_sweep": 0}
    for _, g in df.groupby("session", sort=True):
        if len(g) < 12:
            continue
        sigs = ict_signals(g, _diag=diag, **kw)
        h, l, c = g["high"].values, g["low"].values, g["close"].values
        n = len(g)
        used_until = -1
        for (i, side, entry, stop, target) in sigs:
            if i <= used_until:
                continue  # one position at a time
            risk_per_unit = abs(entry - stop)
            qty = (risk_frac * equity) / risk_per_unit
            qty = min(qty, equity / entry)  # cap leverage
            entry_px = entry * (1 + fee * side)
            exit_px, exit_j = None, n - 1
            for j in range(i + 1, n):
                if side == 1:
                    if l[j] <= stop:
                        exit_px = stop; exit_j = j; break
                    if h[j] >= target:
                        exit_px = target; exit_j = j; break
                else:
                    if h[j] >= stop:
                        exit_px = stop; exit_j = j; break
                    if l[j] <= target:
                        exit_px = target; exit_j = j; break
            if exit_px is None:
                exit_px = c[n - 1]  # forced flat at close
            exit_px *= (1 - fee * side)
            pnl = side * (exit_px - entry_px) * qty
            equity += pnl
            trades.append(pnl)
            eq_curve.append((g.index[exit_j], equity))
            used_until = exit_j
    eq = pd.Series(dict(eq_curve)).sort_index() if eq_curve else pd.Series([capital])
    wins = [x for x in trades if x > 0]
    gl = -sum(x for x in trades if x <= 0)
    return {
        "total_return": equity / capital - 1,
        "num_trades": len(trades),
        "win_rate": len(wins) / len(trades) if trades else 0,
        "profit_factor": (sum(wins) / gl) if gl > 0 else float("inf"),
        "avg_trade_$": np.mean(trades) if trades else 0,
        "expectancy_$": np.mean(trades) if trades else 0,
        "max_drawdown": (eq / eq.cummax() - 1).min() if len(eq) > 1 else 0,
        "sessions": df["session"].nunique(),
        "sweeps": diag["sweeps"],
        "fvg_after_sweep": diag["fvg_after_sweep"],
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tickers", default="SPY,QQQ,TSLA,NVDA")
    ap.add_argument("--interval", default="5m")
    ap.add_argument("--period", default="60d")
    ap.add_argument("--rr", type=float, default=2.0, help="reward:risk")
    ap.add_argument("--lookback", type=int, default=12)
    args = ap.parse_args()

    print(f"\nICT / Smart Money Concepts  ({args.interval}, NY kill zone, R:R={args.rr})")
    print("=" * 84)
    print(f"{'ticker':<8}{'totRet':>10}{'trades':>9}{'win%':>8}{'PF':>8}"
          f"{'avg$/trade':>13}{'maxDD':>9}")
    print("-" * 84)
    for tk in [x.strip() for x in args.tickers.split(",")]:
        try:
            df = get_intraday(tk, args.interval, args.period)
        except Exception as e:
            print(f"{tk:<8} (skip: {e})"); continue
        s = backtest_ict(df, rr=args.rr, lookback=args.lookback)
        pf = "inf" if s["profit_factor"] == float("inf") else f"{s['profit_factor']:.2f}"
        print(f"{tk:<8}{s['total_return']*100:>9.1f}%{s['num_trades']:>9}"
              f"{s['win_rate']*100:>7.0f}%{pf:>8}{s['avg_trade_$']:>12.2f} "
              f"{s['max_drawdown']*100:>8.1f}%")
    print("=" * 84)
    print(f"({df['session'].nunique()} sessions of data -- tiny sample, treat as a "
          "mechanism test, not proof of edge.)")


if __name__ == "__main__":
    main()
