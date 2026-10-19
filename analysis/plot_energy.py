#!/usr/bin/env python3
"""Time evolution of the energy per toroidal mode number n, from diagnostics.bp.

Replaces the old chain  ekenc_00* -> cat -> ekenc_all -> energy_reader.f90 ->
energy_vs_tor_mode.dat -> tor_mode_energy_reader.m.  diagnostics.bp already
holds every run of a restart chain, so no concatenation is needed, and the
sum over poloidal modes m for each n is done by label (mode_n), not by
hard-coded column positions.

    plot_energy.py [diagnostics.bp] [--var ekenc] [--tau-a 2.4249e-07]
                   [--out energy.png] [--csv energy_vs_tor_mode.csv] [--show]

Variables of diagnostics.bp usable with --var (all per-mode, labelled by
mode_m / mode_n): ekenc, eke, emenc, eme, evprlfnc, evprlf, evalpnc, evalp.
"""
import argparse
import sys

import numpy as np
from adios2 import FileReader


def read_series(path, var):
    """Returns (time[nt], n_values[], energy[nt, n]) with the energy summed over
    all poloidal modes m of each toroidal mode number n, for every energy step."""
    with FileReader(path) as f:
        avail = f.available_variables()
        for need in ("time", "mode_n", var):
            if need not in avail:
                sys.exit(f"{path}: variable '{need}' not found (has: {sorted(avail)[:8]}...)")
        nt = int(avail["time"]["AvailableStepsCount"])
        times = np.empty(nt)
        per_step = []
        for i in range(nt):
            times[i] = np.ravel(f.read("time", step_selection=[i, 1]))[0]
            n = f.read("mode_n", step_selection=[i, 1]).ravel()
            e = f.read(var, step_selection=[i, 1]).ravel()
            per_step.append((n, e))
    nvals = np.unique(np.concatenate([n for n, _ in per_step]))
    out = np.full((nt, nvals.size), np.nan)   # NaN: this n is absent from the step (e.g. restart with fewer modes)
    for i, (n, e) in enumerate(per_step):
        for k, nv in enumerate(nvals):
            if np.any(n == nv):
                out[i, k] = e[n == nv].sum()
    return times, nvals, out


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("file", nargs="?", default="diagnostics.bp")
    ap.add_argument("--var", default="ekenc", help="energy variable (default ekenc, as the old plots)")
    ap.add_argument("--tau-a", type=float, default=2.4249e-07,
                    help="Alfven time in s used to scale time (default: ITER steady state, 2.4249e-07)")
    ap.add_argument("--out", default="energy.png")
    ap.add_argument("--csv", help="also write the per-n sums as a table")
    ap.add_argument("--show", action="store_true", help="open an interactive window")
    a = ap.parse_args()

    t, nvals, e = read_series(a.file, a.var)
    t = t * a.tau_a

    if a.csv:
        np.savetxt(a.csv, np.column_stack([t, e]), delimiter="\t", comments="",
                   header="time\t" + "\t".join(str(int(n)) for n in nvals))

    import matplotlib
    if not a.show:
        matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(12, 7))
    for k, nv in enumerate(nvals):
        ax.semilogy(t, e[:, k], lw=2, label=f"n = {int(nv)}")
    ax.set_xlabel("time (sec)", fontsize=18, fontweight="bold")
    ax.set_ylabel("Volume integrated Magnetic Energy", fontsize=18, fontweight="bold")
    ax.tick_params(labelsize=16)
    ax.legend()
    fig.tight_layout()
    fig.savefig(a.out, dpi=150)
    print(f"{a.file}: {len(t)} steps, n = {[int(n) for n in nvals]} -> {a.out}")
    if a.show:
        plt.show()


if __name__ == "__main__":
    main()
