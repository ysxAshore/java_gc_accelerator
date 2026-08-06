"""Tests for the evidence-first topdown parser."""
from __future__ import annotations

import importlib.util
from pathlib import Path


HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("topdown_pipeline", HERE.parent / "pipeline.py")
assert SPEC and SPEC.loader
pipeline = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(pipeline)


def test_single_trace_preserves_provenance_and_gc() -> None:
    records, summary = pipeline.normalize_trace(HERE / "fixtures/single.log", HERE / "fixtures")
    gc = next(record for record in records if record["record_type"] == "gc")
    assert gc["source_line"] == 1
    assert gc["source_text"].startswith("[0.100s]")
    assert gc["source_kind"] == "trace"
    assert summary["gc_count"] == 1


def test_permission_failure_makes_zero_cycles_unavailable() -> None:
    records, summary = pipeline.normalize_trace(HERE / "fixtures/multi.log", HERE / "fixtures")
    measurement = next(record for record in records if record["record_type"] == "perf_measurement")
    assert measurement["cycles"] is None
    assert measurement["cycles_available"] is False
    assert measurement["unavailable_reason"] == "zero_after_permission_failure"
    assert summary["perf_status"] == "unavailable"
    assert summary["perf_permission_failure_count"] == 1


def test_all_current_sources_are_discovered() -> None:
    root = HERE.parents[3]
    sources = pipeline.discover_sources(root)
    assert len(sources) == 6
    assert sources[0].name == "ref"
    assert {path.name for path in sources[1:]} == {
        "0_114_1_105", "0_123_1_74", "0_64_1_78", "0_64_1_81", "0_64_1_87"
    }
