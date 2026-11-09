"""Three unemployment-flow figures from cps_monthly_hazards.csv."""
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

# Put this script and the CSV in the same folder, or change CSV_PATH.
CSV_PATH = Path(__file__).resolve().parent / 'cps_monthly_hazards.csv'
OUTPUT_DIR = Path(__file__).resolve().parent

# US NBER recession dates; COVID recession was February–April 2020.
RECESSIONS = [
    (pd.Timestamp('2007-12-01'), pd.Timestamp('2009-06-30'), 'Great Recession'),
    (pd.Timestamp('2020-02-01'), pd.Timestamp('2020-04-30'), 'COVID-19 recession'),
]

df = pd.read_csv(CSV_PATH)
df['date'] = pd.to_datetime(dict(year=df['YEAR'], month=df['MONTH'], day=1))
df = df.sort_values('date').reset_index(drop=True)

# Convert probabilities (0–1) to percent.
df['finding_pct'] = 100 * df['f_shimer']
df['separation_pct'] = 100 * df['s_t']
df['unemployment_pct'] = 100 * df['U_t'] / (df['E_t'] + df['U_t'])


def shade_recessions(ax, label=True):
    for start, end, name in RECESSIONS:
        ax.axvspan(start, end, color='#999999', alpha=0.23,
                   label=name if label else None)


def format_time_axis(ax):
    ax.xaxis.set_major_locator(mdates.YearLocator(2))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.grid(alpha=0.25)


# FIGURE 1: Three time-series plots across the full dataset.
fig, axes = plt.subplots(3, 1, figsize=(14, 11), sharex=True)
series = [
    ('finding_pct', 'Job-finding rate', '#2474a6'),
    ('separation_pct', 'Job-separation rate', '#bd4b43'),
    ('unemployment_pct', 'Unemployment share', '#32836e'),
]
for ax, (column, title, color) in zip(axes, series):
    ax.plot(df['date'], df[column], color=color, linewidth=1.6)
    shade_recessions(ax, label=False)
    ax.set_ylabel('Percent (%)')
    ax.set_title(title, loc='left', fontweight='bold')
    format_time_axis(ax)
axes[-1].set_xlabel('Year')
axes[-1].set_xlim(df['date'].min(), df['date'].max())
fig.suptitle('US Labor-market Transitions and Unemployment, 2005–2026',
             fontsize=16, fontweight='bold')
fig.text(0.5, 0.015, 'Gray shading: NBER recession periods (Dec 2007–Jun 2009; Feb–Apr 2020)',
         ha='center', fontsize=10)
fig.tight_layout(rect=[0, 0.035, 1, 0.96])
fig.savefig(OUTPUT_DIR / '01_full_period_rates.png', dpi=200, bbox_inches='tight')
plt.show()


# FIGURE 2: Two-by-two closeups of the major shocks.
fig, axes = plt.subplots(2, 2, figsize=(15, 9))
shock_windows = [
    ('Great Recession', '2006-01-01', '2011-12-01', RECESSIONS[0]),
    ('COVID-19', '2019-01-01', '2022-12-01', RECESSIONS[1]),
]
for col, (shock_name, window_start, window_end, recession) in enumerate(shock_windows):
    window = df[df['date'].between(window_start, window_end)]
    for row, (var, rate_name, color) in enumerate([
        ('finding_pct', 'Job-finding rate', '#2474a6'),
        ('separation_pct', 'Job-separation rate', '#bd4b43'),
    ]):
        ax = axes[row, col]
        ax.plot(window['date'], window[var], color=color, linewidth=1.8)
        ax.axvspan(recession[0], recession[1], color='#999999', alpha=0.25)
        ax.set_title(f'{shock_name}: {rate_name}', fontweight='bold')
        ax.set_ylabel('Percent (%)')
        ax.set_xlabel('Year')
        ax.xaxis.set_major_locator(mdates.YearLocator(1))
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
        ax.grid(alpha=0.25)
fig.suptitle('Job-finding and Separation Rates Around Major Shocks',
             fontsize=16, fontweight='bold')
fig.tight_layout(rect=[0, 0, 1, 0.95])
fig.savefig(OUTPUT_DIR / '02_recession_covid_closeups.png', dpi=200, bbox_inches='tight')
plt.show()


# FIGURE 3: Monthly scatter plot. Color is the unemployment share.
fig, ax = plt.subplots(figsize=(11, 8))
scatter = ax.scatter(df['finding_pct'], df['separation_pct'],
                     c=df['unemployment_pct'], cmap='viridis',
                     alpha=0.8, s=38, edgecolors='none')
colorbar = fig.colorbar(scatter, ax=ax)
colorbar.set_label('Unemployment share (%)')
ax.set_xlabel('Job-finding rate (%)')
ax.set_ylabel('Job-separation rate (%)')
ax.set_title('Monthly Job-finding vs. Separation Rates, 2005–2026',
             fontsize=15, fontweight='bold')
ax.grid(alpha=0.25)
fig.tight_layout()
fig.savefig(OUTPUT_DIR / '03_finding_vs_separation_scatter.png',
            dpi=200, bbox_inches='tight')
plt.show()

print('Saved all three figures to:', OUTPUT_DIR)
