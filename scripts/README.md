# Campaign management (hpc-campaign)

`make_campaign.py` registers a finished (or restarted) run directory in an
hpc-campaign archive (`.aca`): the ADIOS2 outputs by reference, the input files
and logs stored inside the archive, and a W3C PROV-JSON provenance record.

## One-time setup

    pip install hpc-campaign          # needs ADIOS2 >= 2.12 built with campaign support
    mkdir -p ~/.config/hpc-campaign ~/campaign-store

`~/.config/hpc-campaign/config.yaml` (hostname names the *location* of the data
and must not be `localhost`; ADIOS2 only reads local files directly when it
matches the hostname recorded in the archive):

    Campaign:
      hostname: LAPTOP
      campaignstorepath: ~/campaign-store
      cachepath: /tmp/campaign
      verbose: 0

## Use

    ADIOS2_DIR=/path/to/adios2/install \
    scripts/make_campaign.py RUN_DIR far3d/iter_rev_001.aca --binary /path/to/far3d.x

Run it again on the same archive after each restart; datasets are not
duplicated and every call adds a `provenance/prov_<label>.json` record.

    hpc_campaign manager far3d/iter_rev_001.aca info
    bpls -l far3d/iter_rev_001.aca                       # diagnostics/*, fs0000z/*, ...
    bpls -d far3d/iter_rev_001.aca inputs/farin
    analysis/plot_energy.py far3d/iter_rev_001.aca --dataset diagnostics

Contents of an archive: datasets named after the files without `.bp`
(`diagnostics`, `field_snapshots`, `hifreq`, `fs0000_dumps`, `fs0000z`, ...),
`inputs/*`, `logs/*`, `run/parameters.json` (parsed `farin`),
`provenance/prov_*.json`, and the time series `restart_checkpoints`.
The same JSON records are kept in `RUN_DIR/campaign_meta/`.
