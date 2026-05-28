"""Run the documented strategies on real data and report honest stats.

Usage:
    python3 run.py                       # default: SPY 2010-2024, all strategies
    python3 run.py --ticker QQQ --start 2015-01-01 --end 2025-01-01
"""
from __future__ import annotations
import argparse
from data import get_ohlcv
from engine import backtest, Costs
from strategies import REGISTRY


def fmt_pct(x):
    return "n/a" if x is None else f"{x*100:6.1f}%"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ticker", default="SPY")
    ap.add_argument("--start", default="2010-01-01")
    ap.add_argument("--end", default="2024-12-31")
    ap.add_argument("--risk", type=float, default=0.01, help="risk fraction per trade")
    ap.add_argument("--commission-bps", type=float, default=1.0)
    ap.add_argument("--slippage-bps", type=float, default=2.0)
    args = ap.parse_args()

    df = get_ohlcv(args.ticker, args.start, args.end)
    costs = Costs(commission_bps=args.commission_bps, slippage_bps=args.slippage_bps)

    print(f"\n{args.ticker}  {args.start} -> {args.end}   "
          f"({len(df)} daily bars)   risk/trade={args.risk:.1%}   "
          f"costs={args.commission_bps+args.slippage_bps:.0f}bps/side")
    print("=" * 92)
    hdr = (f"{'strategy':<14}{'totRet':>9}{'CAGR':>8}{'Sharpe':>8}"
           f"{'maxDD':>9}{'trades':>8}{'win%':>7}{'PF':>7}{'vsBuy&Hold':>13}")
    print(hdr)
    print("-" * 92)

    bh_ret = None
    for name, fn in REGISTRY.items():
        target = fn(df)
        res = backtest(df, target, risk_frac=args.risk, costs=costs)
        s = res.stats()
        bh_ret = s["buy_hold_return"]
        edge = s["total_return"] - (bh_ret or 0)
        pf = s["profit_factor"]
        pf_s = "inf" if pf == float("inf") else f"{pf:5.2f}"
        print(f"{name:<14}{fmt_pct(s['total_return']):>9}{fmt_pct(s['cagr']):>8}"
              f"{s['sharpe']:>8.2f}{fmt_pct(s['max_drawdown']):>9}"
              f"{s['num_trades']:>8}{s['win_rate']*100:>6.0f}%{pf_s:>7}"
              f"{fmt_pct(edge):>13}")

    print("-" * 92)
    print(f"{'buy & hold':<14}{fmt_pct(bh_ret):>9}   "
          f"(the benchmark every strategy above is trying, and mostly failing, to beat)")
    print("=" * 92)


if __name__ == "__main__":
    main()
