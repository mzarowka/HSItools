# HSI Calibration Tool — Interface Contract

**Purpose:** This document defines the interface between the scanning protocol, the hsical Shiny app, and the HSItools R package. It is the single source of truth: when the protocol changes, this contract is updated first, then propagated to the software. As of version 3.0, the machine-readable record of every scan is the **HSItools metadata sidecar** — hsical owns no schema and no serialization machinery of its own.

**Date:** 2026-07-15
**Contract version:** 3.1 (app-structure reconciliation; supersedes 3.0 of 2026-07-11)
**Protocol version:** 2.0 (unchanged)
**Sidecar schema version:** 1.1.0 (defined and enforced by HSItools — unchanged since 3.0)

---

## Context: scanning workflow

The laboratory operates a Specim Single Core Scanner with two cameras (VNIR sCMOS PFD and SWIR, on separate PCs) controlled by *Lumo Scanner* software, imaging split sediment cores with a push-broom approach. The key constraint is a **dual-exposure** strategy:

- The **white reference** (BaSO₄ slab) is scanned at a low exposure (**ET_white**) so peak DN reaches 80–85 % of detector maximum.
- The **target** is scanned at a higher exposure (**ET_target**) chosen the same way; ET_target is typically several times ET_white.
- Lumo's built-in dark/white corrections are **never applied**; all data is captured raw.
- Reflectance is calculated in R by *HSItools* with one formula for both cameras; the dark reference is always acquired at ET_white (ET_dark = ET_white), handled via the `tint_dark` parameter. (Formula unchanged from contract 2.0.)

A **session** is a continuous set of scans sharing identical physical and software parameters (camera position, lens, FOV, ET_target, binning). One white reference scan at ET_white closes the session. Changing any parameter opens a new session.

---

## Division of responsibility (the core of this contract)

| Concern | Owner |
|---|---|
| Sidecar schema (field names, types, constraints, `schema_version`) | **HSItools** — `hsi_create_metadata()` / `validate_hsi_metadata()` |
| Writing sidecars to disk (YAML, full schema, NULLs as `~`) | **HSItools** — `hsi_write_metadata()` |
| Reading and validating sidecars (single-version schema gate) | **HSItools** — `hsi_read_metadata()` |
| Collecting field values from the operator and `.hdr`/`.log` files | **hsical** — GUI + `parse_hdr()` / `parse_log()` |
| Geometry QC (ideal FOV, aspect ratio) at the scanner | **hsical** — derived live in the Scan panel |
| Reflectance, masking, saturation screening, co-registration | **HSItools** (R pipeline), never the app |

hsical maps GUI inputs onto `hsi_create_metadata()` arguments and pipes the result to `hsi_write_metadata()`. It never assembles YAML, never defines fields, never validates beyond what HSItools raises. If HSItools' schema changes, hsical's form changes in lockstep — this document first, then both sides.

---

## The sidecar schema (v1.1.0)

One YAML sidecar per capture, **flat** (no nested groups), written by HSItools. All fields optional and `NULL` by default except `name`. Full key list in schema order, with hsical's source for each:

| Key | Type | hsical source |
|---|---|---|
| `schema_version` | stamped by HSItools | — (never a form field) |
| `name` | **required** string | `.hdr` filename sans extension, editable |
| `sensor_type` | string | `.hdr` `sensor type`; GUI camera selector (VNIR/SWIR/free text) writes here — **there is no separate camera field** |
| `manufacturer` | string | manual |
| `lens` | string (free text, e.g. "18.5 mm") | manual |
| `calibration_pack` | string | `.hdr` `calibration pack` |
| `session_id` | string | manual (e.g. `LAZ-26-S1`) |
| `operator` | string (full first + last name) | manual |
| `campaign_prefix` | string | manual (Lumo Setup value) |
| `dataset_name` | string | manual (Lumo Capture value, `CC-SS`) |
| `nrow` | positive scalar | `.hdr` `lines` |
| `ncol` | positive scalar | `.hdr` `samples` |
| `nlyr` | positive scalar | `.hdr` `bands` |
| `xres`, `yres` | positive scalars, µm | **derived** in the Scan panel from FOV, motor positions, and raster dimensions (`fov·1000/ncol`, `length·1000/nrow`) — measured geometry, never nominal, never typed |
| `spectral_resolution_nm` | positive scalar | manual |
| `frame_rate_hz` | positive scalar | `.hdr` `fps` |
| `et_target_ms` | positive scalar | `.hdr` `tint` of the target scan |
| `et_white_ms` | positive scalar | `tint` of the **separately loaded** white-reference `.hdr` |
| `target_start_mm`, `target_stop_mm` | positive scalars | manual (motor positions — in no Lumo output file) |
| `fov_mm` | positive scalar | manual (Lumo FOV setting) |
| `camera_position_mm`, `stage_position_mm` | scalars | manual (enables future focus-signature QC) |
| `scanning_speed_mm_s` | positive scalar | manual |
| `aspect_ratio` | positive scalar | **derived** in the Scan panel (`yres/xres`), never typed |
| `spectral_binning`, `spatial_binning` | positive scalars | `.hdr` `binning = {x, y}` |
| `dropped_frames` | scalar, **zero valid** | `.log` via `parse_log()` |
| `gcp_count` | scalar, **zero valid** | manual |
| `wavelengths`, `fwhm` | positive numeric vectors, nm, length = `nlyr` | `.hdr` `wavelength = {}` / `fwhm = {}` blocks — autofill only, never typed |
| `notes` | string — **pending Maury's gate decision (2026-07-11 hsical handoff); if approved, amended into 1.1.0 without a bump** | manual free text |

Constraint details (positivity, cross-field length checks, string/numeric error classes) are normative in HSItools' validator, not duplicated here.

**Removed relative to contract 2.0, deliberately:**

- `saturation_ratio_pct` — the screening criterion remains protocol (see below) but was never implemented as a logged field; dropped from the machine record.
- Test-scan motor positions and `scan_length_mm` — the durable QC result is `aspect_ratio`; test positions are scratch work for the paper notebook; scan length is derivable (`target_stop_mm − target_start_mm`) and derived fields are not stored.
- The **master CSV log** — replaced by reading sidecars back (`hsi_read_metadata()` mapped over a directory). Sidecars travel with the data; there is no app-centric aggregate file.
- Sample/location fields (`core_id`, `material_type`, site fields, …) — material-specific, not pipeline-essential; the paper notebook and campaign documentation carry them. The sidecar schema stays material-agnostic.

---

## App structure: one Scan panel + Review

Contract 3.0 described two modes (Calibrate | Log). As of hsical **v2.1 (2026-07-14)**
those are unified: geometry QC and logging are the same act, because "test",
"confirmation" and "target" scans were never different objects — only different things
to look at. The app is now a single **Scan** panel plus a **Review** panel. The
division of responsibility and the schema are unchanged; only the presentation is.

### Scan

One panel that both checks geometry and collects the sidecar. Loading one capture
`.hdr` discovers the whole capture folder in a single pick — the `WHITEREF` / `DARKREF`
siblings and the `.log` — and autofills every value the files already carry (`lines`,
`samples`, `bands`, `tint`, `fps`, binning, calibration pack, dropped frames, and the
`wavelength` / `fwhm` axes).

A scan is **five numbers** — `nrow` (lines), `ncol` (samples), `target_start_mm`,
`target_stop_mm`, `fov_mm`. Everything else is derived live and never typed:

- scan length (`target_stop_mm − target_start_mm`) and estimated scan time,
- `yres` (along-track) and `xres` (across-track) pixel size in µm,
- ideal FOV for square pixels (for Lumo's *Scanning speed calculation* panel),
- `aspect_ratio` with a three-tier indicator (0.95–1.05 green / 0.90–0.94 or
  1.06–1.10 amber / outside red).

Each derived value computes independently, so a half-filled form shows what it can (the
ideal FOV from a test scan before any FOV is set; the aspect ratio the moment one is).
The remaining fields are visually grouped in an accordion (Session / Instrument /
Acquisition / QC — presentation only; the sidecar stays flat). **Save sidecar** writes
one sidecar per capture via `hsi_write_metadata()`, default path `<hdr basename>.yaml`
beside the scan, operator-editable, with an explicit overwrite toggle. Session-stable
fields carry forward in memory between saves; **Clear session** resets them. Nothing
persists to disk except sidecars.

### Review

Loads one existing sidecar via `hsi_read_metadata()`, renders every scalar field as an
editable input (schema version and the spectral vectors are shown read-only and carried
through untouched), and writes the edits back to the same file via
`hsi_write_metadata(overwrite = TRUE)`. HSItools validates on write, so an out-of-range
edit aborts with its own message. Fields absent from a sidecar still render, so a value
forgotten at save time can be added here.

> Not yet built: a directory-level session table that maps `hsi_read_metadata()` over a
> folder to list many sidecars at once (contract 3.0 anticipated this under "Log"). The
> current Review panel is single-sidecar, edit-in-place. Left as a future addition.

---

## Unchanged from contract 2.0

- **ET_white mandatory per session** (every session ends with a white reference; ET_dark = ET_white by protocol). The old "Additional whiteref taken?" toggle stays dead.
- **Motor positions, not scan length**, are what the operator records (mm, from Lumo's display).
- **Session concept and manual `session_id`** — downstream code groups by it to reconstruct white-reference sharing.
- **Naming convention**: `PREFIX_CC-SS_TIMESTAMP`, zero-padded for natural sorting (`LAZ-26_01-01_...`).
- **Saturation screening criterion** (protocol, executed in R, no app involvement): any band at detector ceiling per core-mask pixel; threshold 0.1 % of core-area positions; exceed → reduce ET_target and re-scan; within → flagged positions masked during reflectance calculation.
- **GCP pins**: ≥ 10 per section, alternating left–right, ~10 cm spacing, within the SWIR FOV; digitised in GIS for co-registration in R. The app logs only `gcp_count`.
- **What the app does NOT do**: control Lumo, touch `.raw`/`.hdr` data cubes, calculate reflectance, screen saturation, co-register.

---

## Summary of changes from contract 2.0

1. hsical owns no schema — the HSItools flat sidecar (v1.1.0) **is** the scan record; hsical wraps `hsi_create_metadata()` / `hsi_write_metadata()` / `hsi_read_metadata()`.
2. Three tabs → two modes (Calibrate | Log).
3. Master CSV removed; sidecar-per-capture, read back on demand.
4. Camera dropdown collapsed into `sensor_type`.
5. `saturation_ratio_pct`, test-scan positions, and derived `scan_length_mm` removed from the machine record.
6. Sample/location fields removed (paper notebook territory; schema stays material-agnostic).
7. New logged fields since 2.0: positions/FOV/speed (`fov_mm`, `camera_position_mm`, `stage_position_mm`, `scanning_speed_mm_s`), acquisition identity (`operator`, `campaign_prefix`, `dataset_name`, `lens`, `calibration_pack`), QC (`aspect_ratio`, `dropped_frames`, `gcp_count`), and full spectral axes (`wavelengths`, `fwhm`) autofilled from the `.hdr`.
8. `notes` pending one open gate decision.

---

## Summary of changes from contract 3.0 → 3.1 (2026-07-15)

Documentation reconciliation to hsical **as built at v2.1 (2026-07-14)**. **Schema
1.1.0 and protocol 2.0 unchanged — no field added, removed, or retyped.** Presentation
and field provenance only:

1. **Two modes (Calibrate | Log) → one Scan panel + a Review panel.** Geometry QC and
   logging are the same act in one place; the mode split is retired.
2. **`xres` / `yres` are derived, not manual — ratified 2026-07-15.** This is the
   whole point of the calibration tool: scan length comes from the motor positions
   (`target_stop_mm − target_start_mm`), and dividing by the raster dimensions gives the
   pixel size — `yres = length·1000/nrow` (along-track), `xres = fov·1000/ncol`
   (across-track). These are *measured* geometry, exactly the "calibrated, never
   nominal" value the field asks for; a typed nominal was never the intent, so 3.0's
   "manual" framing is superseded rather than a competing option. `aspect_ratio` is
   likewise `yres/xres`.
3. **Review is single-sidecar, edit-in-place.** The directory-level session table 3.0
   anticipated under "Log" is not yet built; recorded as a future addition, not a
   regression.
