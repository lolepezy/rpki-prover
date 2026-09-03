#!/usr/bin/env python3
"""
rpki-compare: poll rpki-prover instances and alert on persistent regressions vs master.

Commands:
  poll    -- collect one snapshot from all configured instances and compare
  daemon  -- run poll in a loop (alternative to cron)
  report  -- show instance state and active/resolved alerts

Config file (JSON):
  {
    "db":            "~/.rpki-compare/history.db",
    "host":          "127.0.0.1",
    "master":        {"name": "master", "port": 34135},
    "branches":      [{"name": "sqlite", "port": 34136}],
    "warmup_cycles": 10,
    "alert_streak":  3,
    "poll_interval_s": 300,
    "thresholds": {
      "vrp_pct":       3.0,
      "objects_pct":   5.0,
      "errors_pct":    20.0,
      "resources_pct": 25.0
    }
  }
"""

import argparse
import json
import re
import sqlite3
import sys
import time
import urllib.request
from datetime import datetime, timezone
from pathlib import Path


# ── schema ────────────────────────────────────────────────────────────────────

SCHEMA = """
CREATE TABLE IF NOT EXISTS instances (
    id          INTEGER PRIMARY KEY,
    name        TEXT    NOT NULL UNIQUE,
    port        INTEGER NOT NULL,
    is_master   INTEGER NOT NULL DEFAULT 0,
    first_wv    INTEGER,
    cycle_count INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS snapshots (
    id            INTEGER PRIMARY KEY,
    instance_id   INTEGER NOT NULL REFERENCES instances(id),
    ts            TEXT    NOT NULL,
    world_version INTEGER NOT NULL,
    UNIQUE(instance_id, world_version)
);

CREATE TABLE IF NOT EXISTS snap_metrics (
    snapshot_id INTEGER PRIMARY KEY REFERENCES snapshots(id),
    vrp_unique  INTEGER,
    vrp_total   INTEGER,
    valid_cert  INTEGER,
    valid_roa   INTEGER,
    valid_mft   INTEGER,
    valid_crl   INTEGER,
    valid_aspa  INTEGER,
    valid_bgp   INTEGER,
    valid_gbr   INTEGER,
    valid_spl   INTEGER,
    error_count INTEGER,
    warn_count  INTEGER
);

CREATE TABLE IF NOT EXISTS snap_resources (
    id           INTEGER PRIMARY KEY,
    snapshot_id  INTEGER NOT NULL REFERENCES snapshots(id),
    tag          TEXT    NOT NULL,
    avg_cpu_ms_s REAL,
    avg_memory   INTEGER,
    max_memory   INTEGER,
    UNIQUE(snapshot_id, tag)
);

CREATE TABLE IF NOT EXISTS error_cats (
    id          INTEGER PRIMARY KEY,
    snapshot_id INTEGER NOT NULL REFERENCES snapshots(id),
    issue_type  TEXT    NOT NULL,
    category    TEXT    NOT NULL,
    count       INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS alerts (
    id          INTEGER PRIMARY KEY,
    created_ts  TEXT    NOT NULL,
    updated_ts  TEXT    NOT NULL,
    instance    TEXT    NOT NULL,
    metric      TEXT    NOT NULL,
    streak      INTEGER NOT NULL DEFAULT 1,
    master_val  REAL,
    branch_val  REAL,
    pct_diff    REAL,
    description TEXT,
    resolved    INTEGER NOT NULL DEFAULT 0,
    UNIQUE(instance, metric, resolved)
);
"""


# ── db ────────────────────────────────────────────────────────────────────────

def open_db(path: str) -> sqlite3.Connection:
    p = Path(path).expanduser()
    p.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(str(p))
    conn.row_factory = sqlite3.Row
    conn.executescript(SCHEMA)
    conn.commit()
    return conn


# ── fetch ─────────────────────────────────────────────────────────────────────

def fetch_json(host: str, port: int, path: str, timeout: int = 30):
    url = f"http://{host}:{port}/api{path}"
    try:
        with urllib.request.urlopen(url, timeout=timeout) as r:
            return json.loads(r.read())
    except Exception as e:
        return None


def fetch_all(host: str, port: int) -> dict | None:
    metrics     = fetch_json(host, port, "/metrics")
    validations = fetch_json(host, port, "/validations")
    vrps        = fetch_json(host, port, "/vrps-unique")
    system      = fetch_json(host, port, "/system")
    if metrics is None or validations is None:
        return None
    return {"metrics": metrics, "validations": validations, "vrps": vrps, "system": system}


# ── normalise ─────────────────────────────────────────────────────────────────

_URL_RE  = re.compile(r'(?:rsync|https?|rrdp)://\S+')
_HASH_RE = re.compile(r'\b[0-9A-Fa-f]{20,}\b')
_TIME_RE = re.compile(r'\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}(?:\.\d+)?Z?')


def categorise(msg: str) -> str:
    """Strip variable parts (URLs, hashes, timestamps) to produce a stable category key."""
    s = _URL_RE.sub('<url>', msg)
    s = _HASH_RE.sub('<hash>', s)
    s = _TIME_RE.sub('<ts>', s)
    dot = s.find('.')
    return (s[:dot].strip() if 0 < dot < 90 else s[:90].strip())


# ── aggregate raw API responses ───────────────────────────────────────────────

def aggregate_metrics(raw: dict) -> dict:
    totals = dict(valid_cert=0, valid_roa=0, valid_mft=0, valid_crl=0,
                  valid_aspa=0, valid_bgp=0, valid_gbr=0, valid_spl=0,
                  vrp_total=0)
    by_repo = ((raw.get("groupedValidations") or {}).get("byRepository") or {})
    for repo in by_repo.values():
        totals["valid_cert"]  += repo.get("validCertNumber",  0)
        totals["valid_roa"]   += repo.get("validRoaNumber",   0)
        totals["valid_mft"]   += repo.get("validMftNumber",   0)
        totals["valid_crl"]   += repo.get("validCrlNumber",   0)
        totals["valid_aspa"]  += repo.get("validAspaNumber",  0)
        totals["valid_bgp"]   += repo.get("validBgpNumber",   0)
        totals["valid_gbr"]   += repo.get("validGbrNumber",   0)
        totals["valid_spl"]   += repo.get("validSplNumber",   0)
        totals["vrp_total"]   += repo.get("vrpCounter",       0)
    return totals


def aggregate_validations(raw) -> tuple:
    items = raw if isinstance(raw, list) else (raw.get("validations") or [])
    errors, warns = 0, 0
    err_cats: dict[str, int] = {}
    warn_cats: dict[str, int] = {}
    for entry in items:
        for issue in (entry.get("issues") or []):
            if "error" in issue:
                errors += 1
                cat = categorise(issue["error"])
                err_cats[cat] = err_cats.get(cat, 0) + 1
            elif "warning" in issue:
                warns += 1
                cat = categorise(issue["warning"])
                warn_cats[cat] = warn_cats.get(cat, 0) + 1
    return errors, warns, err_cats, warn_cats


def aggregate_resources(raw_system) -> dict:
    """tag -> {avg_cpu, avg_memory, max_memory}, tag e.g. 'root', 'fetch', 'validation', 'cache-clean-up'."""
    out = {}
    if not raw_system:
        return out
    for r in (raw_system.get("resources") or []):
        tag = r.get("tag")
        if not tag:
            continue
        out[tag] = {
            "avg_cpu":    r.get("avgCpuTimeMsPerSecond", 0.0),
            "avg_memory": r.get("avgMemory", 0),
            "max_memory": r.get("maxMemory", 0),
        }
    return out


def extract_world_version(raw_metrics: dict) -> int | None:
    if "worldVersion" in raw_metrics:
        return raw_metrics["worldVersion"]
    # Fall back to most common validatedBy across repos
    by_repo = ((raw_metrics.get("groupedValidations") or {}).get("byRepository") or {})
    versions = [r.get("validatedBy") for r in by_repo.values() if r.get("validatedBy")]
    if versions:
        from collections import Counter
        return Counter(versions).most_common(1)[0][0]
    return None


def extract_vrp_count(raw_vrps) -> int:
    """Handle both list and {"vrps": [...]} response shapes."""
    if raw_vrps is None:
        return 0
    if isinstance(raw_vrps, list):
        return len(raw_vrps)
    if isinstance(raw_vrps, dict):
        v = raw_vrps.get("vrps") or raw_vrps.get("data") or []
        return len(v)
    return 0


# ── db writes ─────────────────────────────────────────────────────────────────

def ensure_instance(conn, name: str, port: int, is_master: bool) -> tuple:
    conn.execute(
        "INSERT INTO instances(name, port, is_master) VALUES (?,?,?)"
        " ON CONFLICT(name) DO UPDATE SET port=excluded.port",
        (name, port, 1 if is_master else 0)
    )
    conn.commit()
    row = conn.execute(
        "SELECT id, first_wv, cycle_count FROM instances WHERE name=?", (name,)
    ).fetchone()
    return row["id"], row["first_wv"], row["cycle_count"]


def store_snapshot(conn, instance_id: int, ts: str, wv: int,
                   metrics_agg: dict, vrp_unique: int,
                   errors: int, warns: int,
                   err_cats: dict, warn_cats: dict) -> tuple:
    """Insert snapshot if worldVersion is new. Returns (snap_id, is_new_cycle)."""
    existing = conn.execute(
        "SELECT id FROM snapshots WHERE instance_id=? AND world_version=?",
        (instance_id, wv)
    ).fetchone()
    if existing:
        return existing["id"], False

    conn.execute(
        "INSERT INTO snapshots(instance_id, ts, world_version) VALUES (?,?,?)",
        (instance_id, ts, wv)
    )
    snap_id = conn.execute("SELECT last_insert_rowid()").fetchone()[0]

    conn.execute("""
        INSERT INTO snap_metrics(snapshot_id, vrp_unique, vrp_total,
            valid_cert, valid_roa, valid_mft, valid_crl,
            valid_aspa, valid_bgp, valid_gbr, valid_spl,
            error_count, warn_count)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
    """, (snap_id, vrp_unique, metrics_agg["vrp_total"],
          metrics_agg["valid_cert"], metrics_agg["valid_roa"],
          metrics_agg["valid_mft"], metrics_agg["valid_crl"],
          metrics_agg["valid_aspa"], metrics_agg["valid_bgp"],
          metrics_agg["valid_gbr"], metrics_agg["valid_spl"],
          errors, warns))

    for cat, cnt in err_cats.items():
        conn.execute(
            "INSERT INTO error_cats(snapshot_id,issue_type,category,count) VALUES (?,?,?,?)",
            (snap_id, "error", cat, cnt)
        )
    for cat, cnt in warn_cats.items():
        conn.execute(
            "INSERT INTO error_cats(snapshot_id,issue_type,category,count) VALUES (?,?,?,?)",
            (snap_id, "warning", cat, cnt)
        )

    conn.commit()
    return snap_id, True


def store_resources(conn, snap_id: int, resources: dict):
    for tag, r in resources.items():
        conn.execute("""
            INSERT INTO snap_resources(snapshot_id, tag, avg_cpu_ms_s, avg_memory, max_memory)
            VALUES (?,?,?,?,?)
            ON CONFLICT(snapshot_id, tag) DO UPDATE SET
                avg_cpu_ms_s=excluded.avg_cpu_ms_s,
                avg_memory=excluded.avg_memory,
                max_memory=excluded.max_memory
        """, (snap_id, tag, r["avg_cpu"], r["avg_memory"], r["max_memory"]))
    conn.commit()


def update_instance_cycles(conn, instance_id: int, first_wv, wv: int, is_new: bool):
    if not is_new:
        return
    if first_wv is None:
        conn.execute(
            "UPDATE instances SET first_wv=?, cycle_count=1 WHERE id=?",
            (wv, instance_id)
        )
    else:
        conn.execute(
            "UPDATE instances SET cycle_count=cycle_count+1 WHERE id=?",
            (instance_id,)
        )
    conn.commit()


def latest_snapshot(conn, instance_id: int):
    return conn.execute("""
        SELECT s.id AS snapshot_id, s.world_version, m.*
        FROM snapshots s JOIN snap_metrics m ON m.snapshot_id = s.id
        WHERE s.instance_id = ?
        ORDER BY s.world_version DESC LIMIT 1
    """, (instance_id,)).fetchone()


def get_error_cats(conn, snap_id: int, issue_type: str) -> dict:
    rows = conn.execute(
        "SELECT category, count FROM error_cats WHERE snapshot_id=? AND issue_type=?",
        (snap_id, issue_type)
    ).fetchall()
    return {r["category"]: r["count"] for r in rows}


def get_resources(conn, snap_id: int) -> dict:
    rows = conn.execute(
        "SELECT tag, avg_cpu_ms_s, avg_memory, max_memory FROM snap_resources WHERE snapshot_id=?",
        (snap_id,)
    ).fetchall()
    return {r["tag"]: r for r in rows}


# ── diff & alerts ─────────────────────────────────────────────────────────────

def pct_diff(branch_val: float, master_val: float) -> float:
    if master_val == 0:
        return 0.0 if branch_val == 0 else float("inf")
    return 100.0 * (branch_val - master_val) / master_val


def compute_diffs(conn, branch_snap, master_snap, thresholds: dict) -> list:
    """Return list of (metric, master_val, branch_val, pct, description) for exceeded thresholds."""
    diffs = []
    vrp_t   = thresholds.get("vrp_pct",       3.0)
    obj_t   = thresholds.get("objects_pct",   5.0)
    err_t   = thresholds.get("errors_pct",    20.0)
    res_t   = thresholds.get("resources_pct", 25.0)

    def check(metric, bval, mval, thresh):
        if mval is None or bval is None:
            return
        p = pct_diff(float(bval), float(mval))
        if abs(p) > thresh:
            diffs.append((metric, float(mval), float(bval), p,
                          f"{metric}: master={mval} branch={bval} ({p:+.1f}%)"))

    check("vrp_unique",  branch_snap["vrp_unique"],  master_snap["vrp_unique"],  vrp_t)
    check("vrp_total",   branch_snap["vrp_total"],   master_snap["vrp_total"],   vrp_t)
    check("valid_cert",  branch_snap["valid_cert"],  master_snap["valid_cert"],  obj_t)
    check("valid_roa",   branch_snap["valid_roa"],   master_snap["valid_roa"],   obj_t)
    check("valid_mft",   branch_snap["valid_mft"],   master_snap["valid_mft"],   obj_t)
    check("error_count", branch_snap["error_count"], master_snap["error_count"], err_t)

    # New error categories in branch not seen in master → branch is stricter (possible regression)
    b_errs = get_error_cats(conn, branch_snap["snapshot_id"], "error")
    m_errs = get_error_cats(conn, master_snap["snapshot_id"], "error")
    new_cats  = sorted(set(b_errs) - set(m_errs))
    gone_cats = sorted(set(m_errs) - set(b_errs))
    if new_cats:
        diffs.append(("new_error_categories", 0.0, float(len(new_cats)), float("inf"),
                      "Branch has error categories not in master: " + "; ".join(new_cats[:3])))
    if gone_cats:
        diffs.append(("missing_error_categories", float(len(gone_cats)), 0.0, float("-inf"),
                      "Master error categories absent in branch (silent accept?): " + "; ".join(gone_cats[:3])))

    # Resource usage per tag (root, fetch, validation, cache-clean-up, ...)
    b_res = get_resources(conn, branch_snap["snapshot_id"])
    m_res = get_resources(conn, master_snap["snapshot_id"])
    for tag in sorted(set(b_res) & set(m_res)):
        check(f"avg_memory:{tag}", b_res[tag]["avg_memory"], m_res[tag]["avg_memory"], res_t)
        check(f"max_memory:{tag}", b_res[tag]["max_memory"], m_res[tag]["max_memory"], res_t)
        check(f"avg_cpu:{tag}",    b_res[tag]["avg_cpu_ms_s"], m_res[tag]["avg_cpu_ms_s"], res_t)

    return diffs


def upsert_alert(conn, ts: str, instance: str, metric: str,
                 master_val: float, branch_val: float, pct: float, description: str):
    existing = conn.execute(
        "SELECT id, streak FROM alerts WHERE instance=? AND metric=? AND resolved=0",
        (instance, metric)
    ).fetchone()
    if existing:
        conn.execute("""
            UPDATE alerts SET updated_ts=?, streak=streak+1,
                master_val=?, branch_val=?, pct_diff=?, description=?
            WHERE id=?
        """, (ts, master_val, branch_val, pct, description, existing["id"]))
    else:
        conn.execute("""
            INSERT INTO alerts(created_ts,updated_ts,instance,metric,streak,
                master_val,branch_val,pct_diff,description)
            VALUES (?,?,?,?,1,?,?,?,?)
        """, (ts, ts, instance, metric, master_val, branch_val, pct, description))
    conn.commit()


def resolve_stale_alerts(conn, ts: str, instance: str, still_triggered: set):
    active = conn.execute(
        "SELECT id, metric FROM alerts WHERE instance=? AND resolved=0", (instance,)
    ).fetchall()
    for row in active:
        if row["metric"] not in still_triggered:
            conn.execute(
                "UPDATE alerts SET resolved=1, updated_ts=? WHERE id=?",
                (ts, row["id"])
            )
    conn.commit()


# ── poll logic ────────────────────────────────────────────────────────────────

def poll_once(config: dict, conn, verbose: bool = True):
    host          = config.get("host", "127.0.0.1")
    thresholds    = config.get("thresholds", {})
    warmup_cycles = config.get("warmup_cycles", 10)
    alert_streak  = config.get("alert_streak", 3)
    master_cfg    = config["master"]
    branches      = config.get("branches", [])
    ts            = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # ── master ───────────────────────────────────────────────────────────────
    m_id, m_first_wv, _ = ensure_instance(conn, master_cfg["name"], master_cfg["port"], True)
    m_data = fetch_all(host, master_cfg["port"])
    if m_data is None:
        print(f"[{ts}] WARN: master ({master_cfg['name']}) unreachable", file=sys.stderr)
        return

    m_wv  = extract_world_version(m_data["metrics"])
    m_agg = aggregate_metrics(m_data["metrics"])
    m_vrp = extract_vrp_count(m_data["vrps"])
    m_err, m_warn, m_ecats, m_wcats = aggregate_validations(m_data["validations"])
    m_res = aggregate_resources(m_data["system"])

    m_snap_id, m_is_new = store_snapshot(conn, m_id, ts, m_wv, m_agg, m_vrp, m_err, m_warn, m_ecats, m_wcats)
    if m_is_new:
        store_resources(conn, m_snap_id, m_res)
    update_instance_cycles(conn, m_id, m_first_wv, m_wv, m_is_new)
    m_cycles = conn.execute("SELECT cycle_count FROM instances WHERE id=?", (m_id,)).fetchone()["cycle_count"]

    if verbose and m_is_new:
        print(f"[{ts}] master({master_cfg['name']})  wv={m_wv}  cycle={m_cycles}"
              f"  vrps={m_vrp}  errs={m_err}  warns={m_warn}")

    master_snap = latest_snapshot(conn, m_id)

    # ── branches ─────────────────────────────────────────────────────────────
    for br in branches:
        b_id, b_first_wv, _ = ensure_instance(conn, br["name"], br["port"], False)
        b_data = fetch_all(host, br["port"])
        if b_data is None:
            print(f"[{ts}] WARN: branch '{br['name']}' unreachable", file=sys.stderr)
            continue

        b_wv  = extract_world_version(b_data["metrics"])
        b_agg = aggregate_metrics(b_data["metrics"])
        b_vrp = extract_vrp_count(b_data["vrps"])
        b_err, b_warn, b_ecats, b_wcats = aggregate_validations(b_data["validations"])
        b_res = aggregate_resources(b_data["system"])

        b_snap_id, b_is_new = store_snapshot(conn, b_id, ts, b_wv, b_agg, b_vrp, b_err, b_warn, b_ecats, b_wcats)
        if b_is_new:
            store_resources(conn, b_snap_id, b_res)
        update_instance_cycles(conn, b_id, b_first_wv, b_wv, b_is_new)
        b_cycles = conn.execute("SELECT cycle_count FROM instances WHERE id=?", (b_id,)).fetchone()["cycle_count"]

        if verbose and b_is_new:
            print(f"[{ts}] branch({br['name']:<14})  wv={b_wv}  cycle={b_cycles}"
                  f"  vrps={b_vrp}  errs={b_err}  warns={b_warn}")

        # Comparison only makes sense on new cycles after both sides have warmed up.
        if not b_is_new:
            continue
        if b_cycles < warmup_cycles or m_cycles < warmup_cycles:
            remaining = max(warmup_cycles - b_cycles, warmup_cycles - m_cycles)
            if verbose:
                print(f"[{ts}] {br['name']}: warming up ({remaining} cycles remaining)")
            continue

        branch_snap = latest_snapshot(conn, b_id)
        diffs = compute_diffs(conn, branch_snap, master_snap, thresholds)
        triggered = {d[0] for d in diffs}

        for metric, mval, bval, pct, desc in diffs:
            upsert_alert(conn, ts, br["name"], metric, mval, bval, pct, desc)

        resolve_stale_alerts(conn, ts, br["name"], triggered)

        # Print alerts that have reached the streak threshold
        if verbose:
            fired = conn.execute("""
                SELECT metric, streak, pct_diff, description
                FROM alerts WHERE instance=? AND resolved=0 AND streak >= ?
                ORDER BY streak DESC
            """, (br["name"], alert_streak)).fetchall()
            for a in fired:
                print(f"[{ts}] ALERT(streak={a['streak']}): [{br['name']}] {a['description']}")
            newly_resolved = conn.execute("""
                SELECT metric FROM alerts
                WHERE instance=? AND resolved=1 AND updated_ts=?
            """, (br["name"], ts)).fetchall()
            for r in newly_resolved:
                print(f"[{ts}] RESOLVED: [{br['name']}] {r['metric']}")


# ── report ────────────────────────────────────────────────────────────────────

def report(config: dict, conn):
    warmup      = config.get("warmup_cycles", 10)
    streak_th   = config.get("alert_streak", 3)

    print("Instance state")
    print("-" * 60)
    for row in conn.execute("SELECT name, port, is_master, cycle_count FROM instances ORDER BY is_master DESC, name"):
        role   = "MASTER" if row["is_master"] else "branch"
        cycles = row["cycle_count"] or 0
        state  = "warmed up" if cycles >= warmup else f"warming ({cycles}/{warmup})"
        print(f"  {role:<8}  {row['name']:<20}  port={row['port']}  cycles={cycles}  {state}")

    print(f"\nActive alerts (streak >= {streak_th} = alerting)")
    print("-" * 60)
    alerts = conn.execute("""
        SELECT * FROM alerts WHERE resolved=0 ORDER BY streak DESC, instance, metric
    """).fetchall()
    if not alerts:
        print("  (none)")
    for a in alerts:
        flag = "[ALERT]" if a["streak"] >= streak_th else "[watch]"
        print(f"  {flag}  [{a['instance']}]  {a['metric']:<28}  streak={a['streak']}")
        print(f"         {a['description']}")

    print("\nRecently resolved (last 10)")
    print("-" * 60)
    resolved = conn.execute("""
        SELECT * FROM alerts WHERE resolved=1 ORDER BY updated_ts DESC LIMIT 10
    """).fetchall()
    if not resolved:
        print("  (none)")
    for a in resolved:
        print(f"  [ok]  [{a['instance']}]  {a['metric']:<28}  resolved={a['updated_ts']}")


# ── cli ───────────────────────────────────────────────────────────────────────

DEFAULT_CONFIG: dict = {
    "db":             "~/.rpki-compare/history.db",
    "host":           "127.0.0.1",
    "master":         {"name": "master", "port": 8080},
    "branches":       [],
    "warmup_cycles":  10,
    "alert_streak":   3,
    "poll_interval_s": 300,
    "thresholds": {
        "vrp_pct":       3.0,
        "objects_pct":   5.0,
        "errors_pct":    20.0,
        "resources_pct": 25.0,
    },
}


def load_config(path: str | None) -> dict:
    cfg = dict(DEFAULT_CONFIG)
    if path:
        with open(path) as f:
            cfg.update(json.load(f))
    elif Path("compare.json").exists():
        with open("compare.json") as f:
            cfg.update(json.load(f))
    return cfg


def main():
    ap = argparse.ArgumentParser(description="Compare rpki-prover instances across branches")
    ap.add_argument("command", choices=["poll", "daemon", "report"])
    ap.add_argument("--config", "-c", default=None, help="JSON config file (default: ./compare.json)")
    ap.add_argument("--db",           default=None, help="Override DB path")
    ap.add_argument("--quiet",  "-q", action="store_true")
    args = ap.parse_args()

    config = load_config(args.config)
    if args.db:
        config["db"] = args.db

    conn = open_db(config["db"])
    verbose = not args.quiet

    if args.command == "poll":
        poll_once(config, conn, verbose=verbose)

    elif args.command == "daemon":
        interval = config.get("poll_interval_s", 300)
        print(f"Daemon started, polling every {interval}s  (Ctrl-C to stop)")
        while True:
            try:
                poll_once(config, conn, verbose=verbose)
            except Exception as e:
                print(f"ERROR: {e}", file=sys.stderr)
            time.sleep(interval)

    elif args.command == "report":
        report(config, conn)


if __name__ == "__main__":
    main()
