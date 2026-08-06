#!/usr/bin/env python3
"""Evidence-first normalization for the LoongNix topdown traces.

The command intentionally emits evidence records rather than inferred metrics.
Every extracted record retains its source path, source line number, and exact
source text.  Perf counters reported as zero are unavailable when perf could
not be opened, rather than measurements of zero cycles.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
import tempfile
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from typing import Dict, Iterable, Iterator, List, Optional, Sequence, TextIO, Tuple

SCHEMA_VERSION = 1
SINGLE_CORE_RELATIVE = Path("test/func/single_core/ref")
MULTI_CORE_RELATIVE = Path("test/func/multi_core")

GC_RE = re.compile(
    r"^\[(?P<timestamp>[0-9]+(?:\.[0-9]+)?)s\]\[info\]\[gc(?P<tags>[^]]*)\]\s+"
    r"GC\((?P<gc_id>\d+)\)\s+Using\s+(?P<workers>\d+)\s+workers?\s+of\s+"
    r"(?P<worker_total>\d+)\s+for\s+(?P<phase>.+?)\s*$"
)
ACCESS_RE = re.compile(
    r"\baccess\s+(?P<address>[0-9a-fA-F]+)\s+\((?P<size>\d+)\s+bytes\)\s+to\s+"
    r"(?P<operation>get|write)(?:\s+(?P<value>[0-9a-fA-F]+))?"
)
WORKER_RE = re.compile(r"\bworker_id\s+(?P<worker_id>\d+)\b")
THREAD_LOCAL_BOT_RE = re.compile(
    r"\bthread\s+(?P<thread_id>\d+),\s+localBot\s+is\s+(?P<local_bot>[0-9a-fA-F]+)"
)
PAR_RE = re.compile(r"\bpar\.(?P<field>\w+)\s*=\s*(?P<value>0x[0-9a-fA-F]+|\d+)\s*$")
PERF_FAILURE_RE = re.compile(
    r"perf_event_open\s+failed(?::|\s).*?(?:permission|权限|not permitted|denied)", re.I
)
COST_RE = re.compile(
    r"cost\s+time\s*=\s*(?P<nanoseconds>\d+)\s+ns,\s*"
    r"(?P<microseconds>[0-9.]+)\s+us,\s*cycles\s*=\s*(?P<cycles>\d+)"
)
BENCHMARK_RE = re.compile(
    r"^=+\s*(?P<name>.*?)\s+(?P<result>PASSED|FAILED)(?:\s+in\s+(?P<milliseconds>\d+)\s+msec)?\s*=+\s*$",
    re.I,
)


def utc_now() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def atomic_json_write(path: Path, value: object) -> None:
    """Write JSON using a same-directory temporary file and atomic replace."""
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp", dir=path.parent)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as output:
            json.dump(value, output, ensure_ascii=False, indent=2, sort_keys=True)
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def relative_source_path(path: Path, source_root: Path) -> str:
    try:
        return path.resolve().relative_to(source_root.resolve()).as_posix()
    except ValueError:
        return path.name


def _record(
    record_type: str,
    source_path: str,
    source_sha256: str,
    line_number: int,
    source_text: str,
    source_kind: str = "trace",
    **fields: object,
) -> Dict[str, object]:
    return {
        "record_type": record_type,
        "source_kind": source_kind,
        "source_path": source_path,
        "source_sha256": source_sha256,
        "source_line": line_number,
        "source_text": source_text,
        **fields,
    }


def iter_trace_records(
    path: Path, source_root: Optional[Path] = None, source_kind: str = "trace"
) -> Iterator[Dict[str, object]]:
    """Yield normalized evidence records from one trace, without changing it."""
    path = Path(path)
    source_root = source_root or path.parent
    source_path = relative_source_path(path, source_root)
    source_sha256 = sha256_file(path)
    def record(record_type: str, *args: object, **fields: object) -> Dict[str, object]:
        if len(args) >= 4:
            line_number, source_text = int(args[2]), str(args[3])
        else:
            line_number, source_text = int(args[0]), str(args[1])
        return _record(record_type, source_path, source_sha256, line_number, source_text, source_kind=source_kind, **fields)
    with path.open("r", encoding="utf-8", errors="replace") as trace:
        for line_number, raw_line in enumerate(trace, 1):
            text = raw_line.rstrip("\r\n")
            worker_match = WORKER_RE.search(text)
            worker_id = int(worker_match.group("worker_id")) if worker_match else None

            match = GC_RE.search(text)
            if match:
                yield record(
                    "gc",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    timestamp_seconds=float(match.group("timestamp")),
                    gc_id=int(match.group("gc_id")),
                    workers=int(match.group("workers")),
                    worker_total=int(match.group("worker_total")),
                    phase=match.group("phase"),
                )

            match = THREAD_LOCAL_BOT_RE.search(text)
            if match:
                yield record(
                    "thread",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    thread_id=int(match.group("thread_id")),
                    local_bot=match.group("local_bot"),
                )

            match = ACCESS_RE.search(text)
            if match:
                value = match.group("value")
                yield record(
                    "access",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    worker_id=worker_id,
                    address=match.group("address").lower(),
                    size_bytes=int(match.group("size")),
                    operation=match.group("operation"),
                    value=value.lower() if value is not None else None,
                )

            match = PAR_RE.search(text)
            if match:
                yield record(
                    "parameter",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    worker_id=worker_id,
                    field=match.group("field"),
                    value=match.group("value"),
                )

            if PERF_FAILURE_RE.search(text):
                yield record(
                    "perf_status",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    status="unavailable",
                    reason="perf_event_open_failed_permission",
                )

            match = COST_RE.search(text)
            if match:
                cycles = int(match.group("cycles"))
                yield record(
                    "perf_measurement",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    nanoseconds=int(match.group("nanoseconds")),
                    microseconds=float(match.group("microseconds")),
                    cycles=cycles if cycles > 0 else None,
                    cycles_available=cycles > 0,
                    status="available" if cycles > 0 else "unavailable",
                    unavailable_reason="zero_after_permission_failure" if cycles == 0 else None,
                )

            match = BENCHMARK_RE.search(text)
            if match:
                milliseconds = match.group("milliseconds")
                yield record(
                    "benchmark",
                    source_path,
                    source_sha256,
                    line_number,
                    text,
                    name=match.group("name").strip(),
                    result=match.group("result").upper(),
                    milliseconds=int(milliseconds) if milliseconds else None,
                )


def normalize_trace(path: Path, source_root: Optional[Path] = None, source_kind: str = "trace") -> Tuple[List[Dict[str, object]], Dict[str, object]]:
    """Parse one trace into records and a compact, evidence-linked summary."""
    records = list(iter_trace_records(path, source_root, source_kind=source_kind))
    counts = Counter(str(item["record_type"]) for item in records)
    measurements = [item for item in records if item["record_type"] == "perf_measurement"]
    failures = [item for item in records if item["record_type"] == "perf_status"]
    summary: Dict[str, object] = {
        "record_type": "summary",
        "source_path": records[0]["source_path"] if records else relative_source_path(path, source_root or path.parent),
        "source_sha256": sha256_file(path),
        "source_line_count": sum(1 for _ in path.open("r", encoding="utf-8", errors="replace")),
        "record_counts": dict(sorted(counts.items())),
        "perf_status": "unavailable" if failures or any(not x["cycles_available"] for x in measurements) else ("available" if measurements else "not_observed"),
        "perf_measurement_count": len(measurements),
        "perf_available_count": sum(1 for item in measurements if item["cycles_available"]),
        "perf_permission_failure_count": len(failures),
        "benchmark_results": [
            {"result": item["result"], "name": item["name"], "source_line": item["source_line"]}
            for item in records if item["record_type"] == "benchmark"
        ],
        "access_count": counts.get("access", 0),
        "gc_count": counts.get("gc", 0),
    }
    return records, summary


def parse_gc_log(path: Path) -> Dict[str, object]:
    """Optional GC log hook; trace GC records are parsed by the main pipeline."""
    return {"status": "stub", "parser": "gc", "path": str(path), "records": [], "message": "optional GC log parser not enabled"}


def parse_perf_stat(path: Path) -> Dict[str, object]:
    """Optional perf-stat hook; embedded trace perf records use iter_trace_records."""
    return {"status": "stub", "parser": "perf", "path": str(path), "records": [], "message": "optional perf-stat parser not enabled"}


def parse_jfr(path: Path) -> Dict[str, object]:
    """Optional JFR hook reserved for a future JFR event adapter."""
    return {"status": "stub", "parser": "jfr", "path": str(path), "records": [], "message": "optional JFR parser not enabled"}


def parse_host_software_log(path: Path, source_root: Optional[Path] = None) -> Tuple[List[Dict[str, object]], Dict[str, object]]:
    """Parse a host-side benchmark log as separate provenance, never as hardware evidence."""
    return normalize_trace(path, source_root, source_kind="host_software")


def atomic_jsonl_write(path: Path, records: Iterable[Dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=f".{path.name}.", suffix=".tmp", dir=path.parent)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as output:
            for item in records:
                output.write(json.dumps(item, ensure_ascii=False, sort_keys=True, separators=(",", ":")))
                output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, path)
    except BaseException:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def source_slug(source_path: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]+", "_", source_path).strip("_") or "source"


def discover_sources(repository_root: Path) -> List[Path]:
    base = repository_root / "test/func"
    single = base / "single_core/ref"
    multi = base / "multi_core"
    paths = [single] if single.is_file() else []
    paths.extend(sorted(p for p in multi.glob("0_*") if p.is_file()))
    return paths


def run_pipeline(
    repository_root: Path,
    output_dir: Path,
    source_paths: Sequence[Path],
    resume: bool = True,
    host_source_paths: Sequence[Path] = (),
) -> Dict[str, object]:
    output_dir.mkdir(parents=True, exist_ok=True)
    source_dir = output_dir / "sources"
    source_dir.mkdir(parents=True, exist_ok=True)
    checkpoint_path = output_dir / "checkpoint.json"
    status_path = output_dir / "status.json"
    old_checkpoint = {}
    if resume and checkpoint_path.is_file():
        try:
            old_checkpoint = json.loads(checkpoint_path.read_text(encoding="utf-8")).get("sources", {})
        except (OSError, ValueError, TypeError):
            old_checkpoint = {}

    status: Dict[str, object] = {"schema_version": SCHEMA_VERSION, "state": "running", "started_at": utc_now(), "updated_at": utc_now(), "sources": []}
    atomic_json_write(status_path, status)
    checkpoint: Dict[str, object] = {"schema_version": SCHEMA_VERSION, "updated_at": utc_now(), "sources": dict(old_checkpoint)}
    source_summaries: List[Dict[str, object]] = []

    all_sources = [(path, "trace") for path in source_paths] + [(path, "host_software") for path in host_source_paths]
    for path, source_kind in all_sources:
        path = path.resolve()
        source_path = relative_source_path(path, repository_root)
        digest = sha256_file(path)
        target = source_dir / f"{source_slug(source_path)}.jsonl"
        prior = old_checkpoint.get(source_path, {}) if isinstance(old_checkpoint, dict) else {}
        entry: Dict[str, object] = {"source_path": source_path, "source_sha256": digest, "status": "running"}
        status["sources"].append(entry)
        status["updated_at"] = utc_now()
        atomic_json_write(status_path, status)
        try:
            if resume and prior.get("source_sha256") == digest and target.is_file():
                summary_lines = target.read_text(encoding="utf-8").splitlines()
                summary = json.loads(summary_lines[-1]) if summary_lines else {}
                entry.update({"status": "skipped", "records": max(0, len(summary_lines) - 1), "summary": summary})
            else:
                records, summary = normalize_trace(path, repository_root, source_kind=source_kind)
                atomic_jsonl_write(target, [*records, summary])
                entry.update({"status": "complete", "records": len(records), "summary": summary})
            checkpoint["sources"][source_path] = {"source_sha256": digest, "file": target.name, "status": "complete"}
        except Exception as error:  # preserve partial progress and continue evidence collection
            error_record = _record("error", source_path, digest, 0, "", error=str(error))
            atomic_jsonl_write(target, [error_record, {"record_type": "summary", "source_path": source_path, "source_sha256": digest, "status": "error"}])
            entry.update({"status": "error", "error": str(error)})
            checkpoint["sources"][source_path] = {"source_sha256": digest, "file": target.name, "status": "error"}
        checkpoint["updated_at"] = utc_now()
        atomic_json_write(checkpoint_path, checkpoint)
        status["updated_at"] = utc_now()
        atomic_json_write(status_path, status)
        source_summaries.append(entry)

    def aggregate() -> Iterator[Dict[str, object]]:
        for entry in source_summaries:
            target = source_dir / f"{source_slug(str(entry['source_path']))}.jsonl"
            if not target.is_file():
                continue
            with target.open("r", encoding="utf-8") as source:
                for line in source:
                    if line.strip():
                        yield json.loads(line)

    atomic_jsonl_write(output_dir / "normalized.jsonl", aggregate())
    status.update({"state": "completed", "updated_at": utc_now(), "completed_at": utc_now(), "normalized_file": "normalized.jsonl"})
    atomic_json_write(status_path, status)
    checkpoint["updated_at"] = utc_now()
    atomic_json_write(checkpoint_path, checkpoint)
    return status


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Normalize LoongNix topdown trace evidence")
    subparsers = parser.add_subparsers(dest="command")
    run = subparsers.add_parser("run", help="parse the reference and five multi-core traces")
    run.add_argument("--repository-root", type=Path, default=None, help="loongnix directory (default: inferred)")
    run.add_argument("--output-dir", type=Path, default=None, help="output directory (default: topdown/output)")
    run.add_argument("--no-resume", action="store_true", help="reparse sources even when checkpoint fingerprints match")
    run.add_argument("--source", type=Path, action="append", help="additional/alternate trace; repeatable")
    run.add_argument("--host-log", type=Path, action="append", default=[], help="optional host GC/benchmark log; repeatable")
    subparsers.add_parser("status", help="show the last atomic status document")
    parser.set_defaults(command="run")
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = build_parser().parse_args(argv)
    script_dir = Path(__file__).resolve().parent
    repository_root = (args.repository_root if getattr(args, "repository_root", None) else script_dir.parents[3]).resolve()
    output_dir = (args.output_dir if getattr(args, "output_dir", None) else script_dir / "output").resolve()
    if args.command == "status":
        status_path = script_dir / "output/status.json"
        if status_path.is_file():
            print(status_path.read_text(encoding="utf-8"), end="")
            return 0
        print(json.dumps({"state": "not_started"}, indent=2))
        return 0
    paths = [p.resolve() for p in args.source] if args.source else discover_sources(repository_root)
    if not paths:
        print(f"no traces found below {repository_root / 'test/func'}", file=sys.stderr)
        return 2
    status = run_pipeline(repository_root, output_dir, paths, resume=not args.no_resume, host_source_paths=[p.resolve() for p in args.host_log])
    print(json.dumps({"state": status["state"], "output_dir": str(output_dir), "sources": len(paths)}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
