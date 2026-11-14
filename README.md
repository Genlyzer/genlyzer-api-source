# Genlyzer API

This repository publishes the Genlyzer API's analysis request, data-handling,
and calculation pipelines. Genlyzer users and researchers can inspect how
this API code handles analysis inputs.

## Source-release scope

The API source, tests, OpenAPI contract, and public demo fixtures in
`api/demo-data/` are included, along with CI and maintainer deployment
automation. The UI, annotation databases and population scripts, VPS
systemd/nginx configuration, server secrets, and runtime data (sessions,
queues, caches, and logs) are excluded. The API uses locally provisioned
annotation databases; this source copy does not include those databases. This
repository is intended for code inspection, not as a self-contained
installation or reproduction package.

The API source accepts count matrices and optional sample sheets, validates
them, queues analysis jobs, runs calculations, and returns results. Supported
pipelines include differential expression (DE), data exploration, gene set
enrichment (GSEA), over-representation analysis (ORA), and correlation ORA.

## Request and calculation flow

1. `POST /v1/analyses` creates an analysis session.
2. Count data and, optionally, a sample sheet are uploaded to the API. The API
   validates them and stores them under `SESSION_DATA_DIR` (default:
   `api/session_data`).
3. Run requests are written to a local SQLite queue. A separate worker process
   consumes jobs and dispatches calculations to local `mirai` workers.
4. The worker reads the analysis inputs and locally configured annotation
   databases, saves results, and the API serves status and results.

The relevant implementation is primarily in:

- `api/routes/analyses.R` — request handling and result endpoints.
- `api/helpers/` — validation, file handling, session storage, and encryption.
- `api/tasks/` — queueing, worker dispatch, and DE/GSEA/ORA/exploration tasks.
- `openapi.yaml` — API contract.

Routes cover analysis creation/status/configuration, count and sample-sheet
uploads, and running or retrieving DE, exploration, GSEA, ORA, and correlation
ORA results. See the route definitions and OpenAPI contract for exact request
and response details.

## Calculations and scientific limitations

The API currently supports human and mouse analyses. The calculation
implementations and their defaults are part of the source and should be checked
alongside the versions of the R/Bioconductor packages and annotation database
used by a deployment.

- **Differential expression (DE):** The selectable backends are R DESeq2,
  RustDESeq2, and Limma-Voom. The standard design compares reference and
  comparison groups; an optional sample sheet supports advanced designs with
  covariates and blocking factors. DESeq2 backends
  round fractional raw counts to integers. Only unnormalized counts are accepted. DESeq2 results use an absolute
  log-fold-change test with a default alpha of 0.05, and the R DESeq2 backend
  applies `apeglm` shrinkage to reported log2 fold changes. Counts
  filters are configurable. The API checks that there are at least three
  samples in total and at least one per group. The RustDESeq2 backend currently uses the
  condition-only design; advanced covariates and blocking are not applied by
  that backend. Its results can differ slightly from R DESeq2.
- **Exploration:** Users can select DESeq2 size-factor, TMM, CPM, or no
  normalization, and VST, rlog, log2, or no transformation. Defaults are
  DESeq2 normalization and VST, with a count-sum filter of 10 and no
  variance-based reduction. DESeq2 normalization and VST/rlog require integer
  counts and round fractional values (only raw counts).
- **GSEA:** The API ranks genes from completed DE results (default: the DE
  statistic) and evaluates gene sets from the local annotation database using
  `clusterProfiler::GSEA`. The default adjusted-p-value cutoff is 0.25; sets
  smaller than 5 or larger than 2,000 genes are excluded. Available ranking
  choices include the DE statistic, p-value, log2 fold change, signed
  `-log10(p-value)`, and correlation with a selected gene using VST values.
  The `pvalue` ranking uses `-log10(p-value)`, so the most significant genes
  are ranked first.
- **ORA:** The default input list is genes with DE adjusted p-value at or
  below 0.05; the background is the genes tested in that DE analysis. Enrichment
  uses the selected local annotation collection, a hypergeometric test, and
  Benjamini-Hochberg adjustment; the default result cutoff is adjusted
  p-value 0.05.
- **Correlation ORA:** The API does not calculate correlations or their
  p-values. It accepts gene IDs and adjusted p-values calculated by the client,
  checks that the IDs belong to the current exploration result, applies the
  requested correlation adjusted-p-value cutoff (default 0.05), and performs
  ORA against the submitted tested-gene universe. The quality of the
  correlation statistics and the chosen universe therefore depend on the
  client-side calculation and inputs.

These are implementation details, not a claim that every analysis design or
input is scientifically appropriate. Users should inspect
method assumptions and validate results for their study.

## Data handling and privacy boundaries

- The worker monitor runs cleanup hourly and removes analysis sessions that
  have been inactive for more than 7 days. Sessions marked as running are
  excluded. Analysis sessions are not backed up; if server or storage failure
  causes session data to be lost, it cannot be restored. The service can be
  redeployed from its source repository, but user sessions and results will
  not be recovered.
- For non-demo analyses, count matrices, sample sheets, and FST result files
  are encrypted using a random 256-bit key generated
  by the browser. The browser stores the key in IndexedDB and sends it in the
  `X-Analysis-Key` header; the API and worker therefore can access the key.
  The worker receives it through a transient loopback-only handoff. The key is
  not included in the SQLite task queue, but the API briefly writes the
  handoff payload to a restricted temporary file before removing it.
- The FST format uses AES-256-CTR encryption with HMAC-SHA-256 authentication
  and separately derived encryption and authentication keys. This protects
  those saved files if a copy of the storage is exposed without the analysis
  key. This is a useful at-rest protection, but not end-to-end encryption:
  server-side calculations require the API and worker to access the key and
  plaintext while processing.
- This encryption covers FST files, **not all session data**. Metadata,
  configuration, and some summary files are stored as JSON, and the SQLite
  queue stores task parameters.
- Data and keys are available in API/worker memory while requests and
  calculations run. Decrypted FST staging and the transient handoff file must
  use a RAM-backed directory, configured with `SESSION_DECRYPTED_TMP_DIR`.
  The current API and worker systemd services disable core dumps
  (`LimitCORE=0`); the service unit files are outside this API-only source.
- Demo analyses use public demo data. Their FST files are unencrypted, and
  demo results may use the shared results cache. Private analyses do not use
  that shared cache.
- API and worker logs omit uploaded values, analysis IDs, file paths, and raw
  calculation or validation errors. Errors needed by the user are returned in
  the API response or analysis status instead.

### Verifying a deployed build

This section helps correlate the public source with the release identified by
the running service.

`GET /v1/build-info` reports optional build metadata supplied to the API
process:

- `GENLYZER_SOURCE_REVISION`: full Git commit hash (40 or 64 hexadecimal
  characters).
- `GENLYZER_ARTIFACT_DIGEST`: immutable artifact digest in
  `sha256:<64 hexadecimal characters>` format.
- `GENLYZER_BUILD_PROVENANCE_URL`: HTTPS URL to the GitHub Actions run that
  built and attested the artifact.
- `GENLYZER_BUILD_TIMESTAMP`: optional UTC timestamp.

The API systemd unit reads these values from `/etc/genlyzer/build-info`; the
deployment script writes that file after checking the artifact digest. The
endpoint marks metadata complete when the revision, digest, and provenance
URL are well-formed. It does **not** verify the signature, inspect its own
executable/source, or independently prove that the claimed artifact is still
running. Treat the response as a convenient identifier, not proof.

On pushes to `main`, the workflow runs tests, packages the API source and demo
fixtures, creates a signed GitHub artifact attestation, and waits for approval
in the `production` environment before deploying that same artifact without
rebuilding it. The VPS deployment script checks the artifact digest, blocks
deployment while analyses are queued or running, preserves runtime data, and
rolls code back if health checks fail. Because this repository is public,
GitHub artifact attestations are enabled. Verify the attestation independently
against the downloaded workflow artifact with GitHub's `gh attestation verify`
command, and compare the artifact digest with `/v1/build-info` and the
deployment run. A digest reported only by the application is not independent
proof.

Encryption at rest, server-side confidentiality, and code transparency are
different properties:

- Encryption at rest helps protect stored files from someone who obtains a
  copy of the storage without access to the running service.
- It does not protect data from the Genlyzer server itself or its administrator:
  the server needs the key and plaintext to perform calculations.
- Publishing source code and identifying the deployed build helps users inspect
  implementation and correlate a service release with source, but does not by
  itself hide data from the server or prove that production is running that
  code. A server-reported hash alone is not independent proof; meaningful
  verification requires an identifier tied to a CI-built artifact and
  independently verifiable build provenance or signature.

## Tests

API-focused tests are in `api/tests/`. They cover encryption, worker key
handoff and queue behavior, demo handling, configuration validation, and
analysis helpers. GitHub Actions runs these tests on pull requests and pushes
to `main`; the workflow parses the R source and OpenAPI YAML before executing
the test scripts. The tests do not require the annotation database files.
Production deployment is only considered for a push to `main`, after tests
pass and a reviewer approves the `production` environment.
