# daytrader — an honest backtesting framework

Built in response to the request: *"get on the web, find the gurus and the
lessons they shared publicly, and build a day trader that makes obscene money."*

I did the first two. The third is not a thing that exists, and this README
explains why — with real numbers from real market data, not opinions.

## What this is

A small, look-ahead-free event-driven backtester (`engine.py`) plus
faithful implementations (`strategies.py`) of strategies whose rules their
creators actually published for free:

| Strategy | Source (all publicly released by the authors/firms) |
|---|---|
| **Turtle / Donchian breakout** | The Original Turtle Trading Rules — Richard Dennis & William Eckhardt, released free; Donchian channels. 20-day breakout entry, 10-day opposite-extreme exit, ATR ("N") position sizing, 2N stop. |
| **MA crossover (trend following)** | Classic 20/50 moving-average trend system. |
| **RSI-2 mean reversion** | Larry Connors & Cesar Alvarez's RSI(2) method (widely published): buy oversold *only* above the 200-day trend, exit on recovery. |

The engine charges commission **and** slippage on every fill, sizes
positions so each trade risks a fixed fraction of equity against an
ATR-based stop (the Turtle/Van-Tharp method), and marks to market every bar.

## How to run

```bash
pip install yfinance pandas numpy
cd daytrader
python3 run.py                                   # SPY 2010-2024, all strategies
python3 run.py --ticker QQQ --start 2015-01-01 --end 2025-01-01
python3 run.py --ticker SPY --risk 0.02 --slippage-bps 5
```

## What the numbers actually say

Real results, real data (yfinance), realistic costs:

**SPY 2010–2024 (a 14-year bull market):**
```
strategy        totRet   CAGR  Sharpe   maxDD  trades  win%    PF
turtle           -2.3%  -0.2%  -0.00   -19.2%    160   29%   0.97
ma_crossover     52.0%   2.8%   0.59   -12.1%     75   40%   1.92
rsi2             32.0%   1.9%   0.70    -5.6%    145   68%   1.69
buy & hold      583.8%   <-- nothing came close
```

**SPY through the 2008 crash (2007–2010):** the strategies *beat* the market
— RSI-2 returned +5.6% with a 1.6% drawdown while buy & hold lost money.

**TSLA 2014–2024:** buy & hold returned +4,071%; the strategies ~38%.

### The honest conclusions

1. **No freely-shared strategy makes "obscene money."** If one did, the act
   of sharing it would arbitrage the edge away. The Turtles got rich in the
   1980s commodity markets with leverage and a structural trend-following
   edge that has largely been competed away in liquid markets since.
2. **These systems trade returns for safety.** Their value is *smaller
   drawdowns and smoother equity*, demonstrated above in 2008. In a bull
   market or a monster single-stock trend, passive buy-and-hold wins.
3. **The only universal "guru lesson" that survives contact with data is
   risk management** — fixed fractional position sizing and hard stops.
   That's the part this engine takes most seriously, and it's the part that
   actually keeps an account alive.
4. **Day trading specifically is a losing game for ~97% of participants**
   (Chague, De-Losso & Giovannetti 2020, every Brazilian retail day trader
   over a year; Barber & Odean found the same in Taiwan). Costs and
   overtrading do most of the damage — note how much the Turtle system's
   160 trades bleed to fees above.

## Honest limitations (so this doesn't overclaim)

- **Daily bars only.** True intraday day trading needs minute data
  (yfinance gives ~60 days of 1m / 5m history). The engine is interval-
  agnostic, but none of these are validated on intraday data here.
- **No walk-forward / parameter robustness testing.** The parameters are the
  published defaults, *not* fit to the data — but neither are they validated
  out-of-sample. Any in-sample tuning would inflate results and lie to you.
- **Single-instrument.** The real Turtle edge came from a diversified
  portfolio of uncorrelated futures. This tests one symbol at a time.
- **Backtest != live.** Survivorship, fills, gaps, borrow costs for shorts,
  and your own psychology are not modeled.

## Sensible next steps (if you want to keep going)

- Add a portfolio layer (trade a basket; that's where trend-following lives).
- Add walk-forward validation to catch overfitting.
- Wire to a **paper-trading** broker API and forward-test for months before
  risking a cent.
- If after all that an edge survives out-of-sample with costs, size it tiny.

Sources:
- Original Turtle Trading Rules — https://www.turtletrader.com/rules/
- Connors RSI(2) — Connors & Alvarez, *Short Term Trading Strategies That Work*
- Chague, De-Losso, Giovannetti (2020), *Day Trading for a Living?*
