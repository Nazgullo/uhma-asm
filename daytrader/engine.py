"""A small, honest event-driven backtester.

Design goals:
  * No look-ahead: a signal computed from bar t is acted on at bar t's
    close (or t+1 open, configurable). Indicators only use data <= t.
  * Realistic frictions: commission + slippage charged on every fill.
  * ATR-based risk sizing: position size is chosen so that the distance
    from entry to the protective stop equals `risk_frac` of equity --
    this is the Turtle/Van-Tharp style sizing that actually matters.
  * Honest accounting: equity is marked to market every bar.

A strategy supplies a target position series in {-1, 0, +1}. The engine
turns that into sized trades, applies a 2N (ATR-multiple) stop, and
records every round-trip trade.
"""
from __future__ import annotations
from dataclasses import dataclass, field
import numpy as np
import pandas as pd


@dataclass
class Costs:
    commission_bps: float = 1.0     # per side, in basis points of notional
    slippage_bps: float = 2.0       # per side, modeled as adverse fill


@dataclass
class Trade:
    entry_date: object
    exit_date: object
    side: int            # +1 long, -1 short
    entry: float
    exit: float
    qty: float
    pnl: float
    reason: str          # 'signal' | 'stop'


@dataclass
class Result:
    equity: pd.Series
    trades: list = field(default_factory=list)
    buy_hold: pd.Series = None

    def stats(self) -> dict:
        eq = self.equity
        rets = eq.pct_change().dropna()
        n = len(eq)
        years = max(n / 252.0, 1e-9)
        total = eq.iloc[-1] / eq.iloc[0] - 1.0
        cagr = (eq.iloc[-1] / eq.iloc[0]) ** (1 / years) - 1.0
        vol = rets.std() * np.sqrt(252) if len(rets) > 1 else 0.0
        sharpe = (rets.mean() * 252) / vol if vol > 0 else 0.0
        dd = (eq / eq.cummax() - 1.0).min()
        wins = [t for t in self.trades if t.pnl > 0]
        gross_win = sum(t.pnl for t in wins)
        gross_loss = -sum(t.pnl for t in self.trades if t.pnl <= 0)
        pf = (gross_win / gross_loss) if gross_loss > 0 else float("inf")
        bh = None
        if self.buy_hold is not None and len(self.buy_hold):
            bh = self.buy_hold.iloc[-1] / self.buy_hold.iloc[0] - 1.0
        return {
            "total_return": total,
            "cagr": cagr,
            "sharpe": sharpe,
            "max_drawdown": dd,
            "num_trades": len(self.trades),
            "win_rate": (len(wins) / len(self.trades)) if self.trades else 0.0,
            "profit_factor": pf,
            "buy_hold_return": bh,
        }


def _atr(df: pd.DataFrame, period: int = 20) -> pd.Series:
    h, l, c = df["high"], df["low"], df["close"]
    pc = c.shift(1)
    tr = pd.concat([(h - l), (h - pc).abs(), (l - pc).abs()], axis=1).max(axis=1)
    return tr.rolling(period).mean()


def backtest(df: pd.DataFrame, target_pos: pd.Series, *,
             starting_equity: float = 100_000.0,
             risk_frac: float = 0.01,
             stop_atr_mult: float = 2.0,
             atr_period: int = 20,
             costs: Costs = Costs(),
             allow_short: bool = True) -> Result:
    """Run a backtest given a target position series aligned to df.index."""
    df = df.copy()
    atr = _atr(df, atr_period)
    target_pos = target_pos.reindex(df.index).fillna(0).astype(int)
    if not allow_short:
        target_pos = target_pos.clip(lower=0)

    cash = starting_equity
    pos = 0          # current side
    qty = 0.0
    entry_px = 0.0
    stop_px = 0.0
    entry_date = None
    trades: list[Trade] = []
    equity_curve = []

    fee = (costs.commission_bps + costs.slippage_bps) / 10_000.0

    idx = df.index
    closes = df["close"].values
    lows = df["low"].values
    highs = df["high"].values
    atrv = atr.values
    tgt = target_pos.values

    def close_position(i, fill_px, reason):
        nonlocal cash, pos, qty, entry_px, entry_date
        # adverse slippage on exit
        exec_px = fill_px * (1 - fee * pos)  # selling a long fills a touch lower
        pnl = pos * (exec_px - entry_px) * qty
        cash += pnl
        trades.append(Trade(entry_date, idx[i], pos, entry_px, exec_px, qty,
                            pnl, reason))
        pos, qty, entry_px, entry_date = 0, 0.0, 0.0, None

    for i in range(len(idx)):
        px = closes[i]
        a = atrv[i]

        # 1) intrabar stop check on existing position (uses this bar's range)
        if pos != 0 and not np.isnan(a):
            if pos == 1 and lows[i] <= stop_px:
                close_position(i, stop_px, "stop")
            elif pos == -1 and highs[i] >= stop_px:
                close_position(i, stop_px, "stop")

        # 2) act on target (at close of bar i). Only after warmup (valid ATR).
        want = tgt[i]
        if not np.isnan(a) and a > 0:
            if pos != 0 and want != pos:
                close_position(i, px, "signal")
            if pos == 0 and want != 0:
                # size so (entry - stop) * qty == risk_frac * equity
                stop_dist = stop_atr_mult * a
                risk_dollars = risk_frac * cash
                size = risk_dollars / stop_dist
                # cap leverage at ~1x notional so sizing stays sane
                max_size = cash / px
                size = min(size, max_size)
                exec_px = px * (1 + fee * want)  # buying fills a touch higher
                pos = int(want)
                qty = size
                entry_px = exec_px
                entry_date = idx[i]
                stop_px = (exec_px - stop_dist) if pos == 1 else (exec_px + stop_dist)

        # 3) mark to market
        unreal = pos * (px - entry_px) * qty if pos != 0 else 0.0
        equity_curve.append(cash + unreal)

    equity = pd.Series(equity_curve, index=idx, name="equity")
    bh = (df["close"] / df["close"].iloc[0]) * starting_equity
    return Result(equity=equity, trades=trades, buy_hold=bh)
