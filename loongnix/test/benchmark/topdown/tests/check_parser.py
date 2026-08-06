#!/usr/bin/env python3
"""Small dependency-free checks for the topdown parser."""
from __future__ import annotations

import importlib.util
from pathlib import Path

HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("topdown_pipeline", HERE.parent / "pipeline.py")
assert SPEC and SPEC.loader
pipeline = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(pipeline)

records, summary = pipeline.normalize_trace(HERE / "fixtures/single.log", HERE / "fixtures")
gc = next(item for item in records if item["record_type"] == "gc")
assert gc["source_line"] == 1 and gc["source_kind"] == "trace"
assert summary["gc_count"] == 1

records, summary = pipeline.normalize_trace(HERE / "fixtures/multi.log", HERE / "fixtures")
measurement = next(item for item in records if item["record_type"] == "perf_measurement")
assert measurement["cycles"] is None
assert measurement["cycles_available"] is False
assert measurement["unavailable_reason"] == "zero_after_permission_failure"
assert summary["perf_status"] == "unavailable"
assert summary["perf_permission_failure_count"] == 1

root = HERE.parents[3]
sources = pipeline.discover_sources(root)
assert len(sources) == 6
assert {path.name for path in sources} == {
    "ref", "0_114_1_105", "0_123_1_74", "0_64_1_78", "0_64_1_81", "0_64_1_87"
}
print("topdown parser checks: PASS")
