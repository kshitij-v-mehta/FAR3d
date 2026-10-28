#!/usr/bin/env python3
"""Register a FAR3d run directory in an hpc-campaign archive (.aca).

    make_campaign.py RUN_DIR ARCHIVE [--campaign-store DIR] [--hostname NAME]
                     [--keyfile KEY] [--input FILE ...] [--log FILE ...]
                     [--max-store-mb 64] [--label TAG] [--binary far3d.x]

Catalog     every ADIOS2 dataset in RUN_DIR (fs*_dumps.bp, fs*z.bp, diagnostics.bp,
            field_snapshots.bp, hifreq.bp, geom_bzr_to_cyl.bp) is added by reference;
            the archive stores only metadata and the host/directory it lives in.
Inputs      farin, fareq, profiles.dat, orb_data, taefl_data.txt, adios2.xml, the
            job script and geom_bzr_to_cyl.dat are stored INSIDE the archive
            (compressed), so the run can be reproduced from the .aca alone.
            Files over --max-store-mb are only referenced and checksummed.
Logs        out/restart logs and slurm-*.out are stored the same way.
Provenance  a W3C PROV-JSON record (inputs -> run -> outputs, restart lineage,
            code version, host, user) is stored as provenance/prov_<label>.json,
            and the parsed farin namelist as run/parameters.json.

Calling it again on the same ARCHIVE after a restart updates/extends it: dataset
IDs are derived from host+path, so unchanged files are not duplicated, and each
call adds its own provenance record.
"""
import argparse
import getpass
import glob
import hashlib
import json
import os
import platform
import re
import socket
import contextlib
import io
import subprocess
import sys
from datetime import datetime, timezone

from hpc_campaign import Manager

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

INPUT_FILES = ["farin", "fareq", "profiles.dat", "orb_data", "taefl_data.txt",
               "adios2.xml", "geom_bzr_to_cyl.dat"]
INPUT_GLOBS = ["job*", "*.sbatch", "*.slurm", "submit*"]
LOG_GLOBS = ["out.log", "*.log", "slurm-*.out", "farprt*"]


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for blk in iter(lambda: f.read(1 << 24), b""):
            h.update(blk)
    return h.hexdigest()


def iso(ts):
    return datetime.fromtimestamp(ts, timezone.utc).isoformat(timespec="seconds")


def git(*args):
    try:
        return subprocess.check_output(["git", "-C", REPO, *args], text=True,
                                       stderr=subprocess.DEVNULL).strip()
    except (OSError, subprocess.CalledProcessError):
        return ""


def parse_namelist(path):
    """key=value pairs of farin's namelists (scalars and short lists, as strings)."""
    with open(path, errors="replace") as f:
        lines = f.read().splitlines()
    text = " ".join(lines[1:])  # first line holds the restart/file-name tokens
    out = {}
    for m in re.finditer(r"([A-Za-z_]\w*)\s*=\s*([^=]*?)(?=,\s*[A-Za-z_]\w*\s*=|/|&|$)", text):
        out[m.group(1)] = m.group(2).strip().rstrip(",").strip()
    return {"header": lines[0].split() if lines else [], "namelist": out}


def adios_version(bpls):
    try:
        return subprocess.check_output([bpls, "-V"], text=True,
                                       stderr=subprocess.STDOUT).splitlines()[0]
    except (OSError, subprocess.CalledProcessError, IndexError):
        return ""


def uniq(seq):
    seen = set()
    return [x for x in seq if not (x in seen or seen.add(x))]


def find_files(run, names, globs):
    found = [os.path.join(run, n) for n in names if os.path.isfile(os.path.join(run, n))]
    for g in globs:
        found += [p for p in sorted(glob.glob(os.path.join(run, g))) if os.path.isfile(p)]
    return uniq(found)


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("run_dir")
    ap.add_argument("archive", help="campaign archive name (relative to the campaign store)")
    ap.add_argument("--campaign-store", default="", help="default: ~/.config/hpc-campaign/config.yaml")
    ap.add_argument("--hostname", default="", help="logical location name, not 'localhost'")
    ap.add_argument("--keyfile", default="", help="encrypt metadata (hpc_campaign genkey)")
    ap.add_argument("--input", action="append", default=[], help="extra input file (repeatable)")
    ap.add_argument("--log", action="append", default=[], help="extra log file (repeatable)")
    ap.add_argument("--max-store-mb", type=float, default=64,
                    help="larger inputs/logs are referenced, not stored (default 64)")
    ap.add_argument("--label", default="", help="tag for this provenance record (default: timestamp)")
    ap.add_argument("--binary", default="", help="far3d.x used for the run (checksummed)")
    ap.add_argument("--verbose", action="store_true", help="show hpc_campaign's own output")
    ap.add_argument("--truncate", action="store_true", help="start the archive from scratch")
    a = ap.parse_args()

    run = os.path.abspath(a.run_dir)
    if not os.path.isdir(run):
        sys.exit(f"{run}: not a directory")
    a.binary = os.path.abspath(a.binary) if a.binary else ""
    a.input, a.log = [os.path.abspath(p) for p in a.input], [os.path.abspath(p) for p in a.log]
    a.campaign_store = os.path.abspath(a.campaign_store) if a.campaign_store else ""
    a.keyfile = os.path.abspath(a.keyfile) if a.keyfile else ""
    os.chdir(run)  # hpc_campaign records the cwd as the data location; keep names relative
    label = a.label or datetime.now().strftime("%Y%m%dT%H%M%S")

    bp = sorted(p for p in glob.glob(os.path.join(run, "*.bp")) if os.path.isdir(p))
    if not bp:
        sys.exit(f"{run}: no *.bp datasets found")
    inputs = find_files(run, INPUT_FILES, INPUT_GLOBS) + [os.path.abspath(p) for p in a.input]
    logs = find_files(run, [], LOG_GLOBS) + [os.path.abspath(p) for p in a.log]
    inputs, logs = uniq(inputs), [p for p in uniq(logs) if p not in inputs]
    rel = lambda p: os.path.relpath(p, run) if p.startswith(run + os.sep) else p  # noqa: E731
    bp, inputs, logs = [rel(p) for p in bp], [rel(p) for p in inputs], [rel(p) for p in logs]
    limit = a.max_store_mb * 1e6

    host = a.hostname or socket.gethostname().split(".")[0]
    now = datetime.now(timezone.utc).isoformat(timespec="seconds")
    params = parse_namelist(os.path.join(run, "farin")) if os.path.isfile(os.path.join(run, "farin")) else {}

    quiet = contextlib.nullcontext() if a.verbose else contextlib.redirect_stdout(io.StringIO())
    stack = contextlib.ExitStack()
    stack.enter_context(quiet)
    mgr = Manager(a.archive, hostname=a.hostname, campaign_store=a.campaign_store, keyfile=a.keyfile)
    os.makedirs(os.path.dirname(os.path.join(os.path.expanduser(mgr.args.campaign_store), a.archive)), exist_ok=True)
    if a.truncate:
        mgr.open(create=True, truncate=True)

    # --- datasets (by reference) -----------------------------------------------------
    ds_name = {}
    for p in bp:
        name = os.path.basename(p)[:-3]
        mgr.data(p, name=name)
        ds_name[p] = name
    restarts = [ds_name[p] for p in bp if re.fullmatch(r"fs\d+z", ds_name[p])]
    if len(restarts) > 1:
        mgr.add_time_series("restart_checkpoints", restarts, replace=True)

    # --- inputs and logs (stored in the archive) -----------------------------------------
    def add_text(paths, group):
        names = {}
        for p in paths:
            nm = f"{group}/{os.path.basename(p)}"
            names[p] = nm
            store = os.path.getsize(p) <= limit
            mgr.text(p, name=nm, store=store)
            if not store:
                print(f"  {nm}: {os.path.getsize(p)/1e6:.0f} MB > limit, referenced only")
        return names

    in_name, log_name = add_text(inputs, "inputs"), add_text(logs, "logs")

    # --- provenance (W3C PROV-JSON) --------------------------------------------------------
    commit, dirty = git("rev-parse", "HEAD"), bool(git("status", "--porcelain", "--untracked-files=no"))
    bpls = os.path.join(os.environ.get("ADIOS2_DIR", ""), "bin", "bpls") if os.environ.get("ADIOS2_DIR") else "bpls"
    first_log = min((os.path.getmtime(p) for p in logs), default=None)
    last_out = max(os.path.getmtime(p) for p in bp)
    ent, act, agent = {}, {}, {}
    used, gen, derived, assoc = {}, {}, {}, {}

    user = getpass.getuser()
    agent[f"user:{user}"] = {"prov:type": "prov:Person", "prov:label": user}
    code = {"prov:type": "prov:SoftwareAgent", "prov:label": "FAR3d",
            "far3d:git_commit": commit, "far3d:git_dirty": dirty,
            "far3d:git_describe": git("describe", "--always", "--dirty"),
            "far3d:git_branch": git("rev-parse", "--abbrev-ref", "HEAD"),
            "far3d:adios2": adios_version(bpls)}
    if a.binary and os.path.isfile(a.binary):
        code["far3d:binary"] = os.path.abspath(a.binary)
        code["far3d:binary_sha256"] = sha256(a.binary)
    agent["sw:far3d"] = code

    act_id = f"run:{label}"
    act[act_id] = {"prov:type": "far3d:simulation", "prov:label": f"FAR3d run in {run}",
                   "prov:endTime": iso(last_out),
                   "far3d:host": host, "far3d:platform": platform.platform(),
                   "far3d:run_dir": run, "far3d:registered_at": now}
    if first_log:
        act[act_id]["prov:startTime"] = iso(first_log)
    assoc["_:a1"] = {"prov:activity": act_id, "prov:agent": f"user:{user}"}
    assoc["_:a2"] = {"prov:activity": act_id, "prov:agent": "sw:far3d"}

    for i, p in enumerate(inputs):
        eid = f"in:{in_name[p]}"
        ent[eid] = {"prov:type": "far3d:input", "prov:label": in_name[p], "far3d:path": os.path.abspath(p),
                    "far3d:size": os.path.getsize(p), "far3d:sha256": sha256(p),
                    "far3d:stored_in_archive": os.path.getsize(p) <= limit,
                    "prov:generatedAtTime": iso(os.path.getmtime(p))}
        used[f"_:u{i}"] = {"prov:activity": act_id, "prov:entity": eid}
    for i, p in enumerate(bp):
        eid = f"out:{ds_name[p]}"
        size = sum(os.path.getsize(os.path.join(d, f)) for d, _, fs in os.walk(p) for f in fs)
        ent[eid] = {"prov:type": "far3d:dataset", "prov:label": ds_name[p], "far3d:path": os.path.abspath(p),
                    "far3d:size": size, "prov:generatedAtTime": iso(os.path.getmtime(p))}
        gen[f"_:g{i}"] = {"prov:entity": eid, "prov:activity": act_id}
        # restart lineage: a restart run reads <prev>z.bp and appends to <prev>_dumps.bp
        if re.fullmatch(r"fs\d+z", ds_name[p]):
            n = int(ds_name[p][2:-1])
            if n > 0 and f"out:fs{n-1:04d}z" in ent:
                derived[f"_:d{i}"] = {"prov:generatedEntity": eid, "prov:usedEntity": f"out:fs{n-1:04d}z"}
    for i, p in enumerate(logs):
        eid = f"log:{log_name[p]}"
        ent[eid] = {"prov:type": "far3d:log", "prov:label": log_name[p], "far3d:path": os.path.abspath(p),
                    "far3d:size": os.path.getsize(p)}
        gen[f"_:l{i}"] = {"prov:entity": eid, "prov:activity": act_id}

    prov = {"prefix": {"far3d": "https://github.com/far3d/FAR3d#", "run": "far3d:run/",
                       "in": "far3d:input/", "out": "far3d:output/", "log": "far3d:log/",
                       "user": "far3d:user/", "sw": "far3d:sw/"},
            "entity": ent, "activity": act, "agent": agent, "used": used,
            "wasGeneratedBy": gen, "wasDerivedFrom": derived, "wasAssociatedWith": assoc}

    meta = "campaign_meta"  # kept next to the data so the records can be regenerated/inspected
    os.makedirs(meta, exist_ok=True)
    pf, qf = os.path.join(meta, f"prov_{label}.json"), os.path.join(meta, "parameters.json")
    with open(pf, "w") as f:
        json.dump(prov, f, indent=1)
    mgr.text(pf, name=f"provenance/prov_{label}.json", store=True)
    if params:
        with open(qf, "w") as f:
            json.dump({"run_dir": run, "host": host, "label": label, **params}, f, indent=1)
        mgr.text(qf, name="run/parameters.json", store=True)

    mgr.close()
    stack.close()
    print(f"campaign '{a.archive}': {len(bp)} datasets, {len(inputs)} inputs, {len(logs)} logs, "
          f"provenance prov_{label}.json")


if __name__ == "__main__":
    main()
