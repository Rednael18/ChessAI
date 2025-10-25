#!/usr/bin/env python3
import re
import argparse
from pathlib import Path

import numpy as np
import matplotlib.pyplot as plt

LINE_RE = re.compile(
    r"^Epoch\s+(\d+)/\d+:\s+Best = (-?[0-9.]+),\s*Avg = (-?[0-9.]+),\s*Worst = (-?[0-9.]+)\s*$"
)

def parse_logs(path: Path):
    epochs = []
    best = []
    avg = []
    worst = []

    with path.open("r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            m = LINE_RE.match(line.strip())
            if m:
                e, b, a, w = m.groups()
                epochs.append(int(e))
                best.append(float(b))
                avg.append(float(a))
                worst.append(float(w))

    if not epochs:
        raise ValueError("No epoch lines found. Make sure logs.txt has lines like: "
                         "'Epoch   1/1000: Best = 0.5890, Avg = 0.5843, Worst = 0.5780'")
    # Ensure sorted by epoch (in case)
    order = np.argsort(epochs)
    epochs = np.array(epochs, dtype=int)[order]
    best = np.array(best, dtype=float)[order]
    avg = np.array(avg, dtype=float)[order]
    worst = np.array(worst, dtype=float)[order]
    return epochs, best, avg, worst


def linear_fit(x: np.ndarray, y: np.ndarray):
    # y ≈ m*x + c
    m, c = np.polyfit(x, y, 1)
    y_hat = m * x + c
    # R^2
    ss_res = np.sum((y - y_hat) ** 2)
    ss_tot = np.sum((y - np.mean(y)) ** 2)
    r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else float("nan")
    return m, c, r2, y_hat


def main():
    parser = argparse.ArgumentParser(description="Plot Best/Avg/Worst fitness per epoch from logs.txt with linear fit.")
    parser.add_argument("--log", default="logs.txt", help="Path to logs file (default: logs.txt)")
    parser.add_argument("--out", default="fitness_over_epochs.png", help="Output image filename")
    args = parser.parse_args()

    log_path = Path(args.log)
    if not log_path.exists():
        raise FileNotFoundError(f"Could not find '{log_path.resolve()}'")

    epochs, best, avg, worst = parse_logs(log_path)
    m, c, r2, best_fit = linear_fit(epochs.astype(float), best)

    # Plot
    plt.figure(figsize=(10, 5))
    plt.plot(epochs, best, label="Best", linewidth=1.5)
    plt.plot(epochs, avg, label="Avg", linewidth=1.2)
    plt.plot(epochs, worst, label="Worst", linewidth=1.2)
    plt.plot(epochs, best_fit, label=f"Best (linear fit)  m={m:.4f}, c={c:.4f}, R²={r2:.4f}", linestyle="--")

    plt.title("Fitness per Epoch")
    plt.xlabel("Epoch")
    plt.ylabel("Fitness")
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    #plt.savefig(args.out, dpi=150)
    print(f"Saved plot to {args.out}")
    print(f"Linear fit to Best: slope={m:.6f}, intercept={c:.6f}, R^2={r2:.6f}")

    try:
        # Also show the plot interactively if possible
        plt.show()
    except Exception:
        pass


if __name__ == "__main__":
    main()
