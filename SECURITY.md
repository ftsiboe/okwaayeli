# Security policy

`okwaayeli` is a research package that distributes harmonized survey data and
econometric tooling. It does not handle authentication or personally
identifiable information beyond the de-identified records released by the
[Ghana Statistical Service](https://statsghana.gov.gh/) through the Ghana
Living Standards Surveys. Nonetheless, we take the security of the code
base and its consumers seriously.

## Supported versions

We provide security fixes only for the latest tagged release on `main`.
Once a new version is released, prior versions are no longer patched.

| Version | Supported |
|---------|-----------|
| `main` (development) | :white_check_mark: |
| Latest tagged release | :white_check_mark: |
| Older releases | :x: |

## Reporting a vulnerability

If you believe you have found a security vulnerability in `okwaayeli` —
for example a remote-code-execution risk in one of the `R/` functions, a
path-traversal issue in `study_setup()`, or a credential leak in the data
download path — please **do not open a public issue**.

Instead, send a private report to
[ftsiboe@hotmail.com](mailto:ftsiboe@hotmail.com) with:

- A description of the vulnerability and its impact.
- Steps (or a minimal reproducer) to demonstrate the issue.
- Any suggested mitigation.

You can expect:

- An acknowledgement within 5 business days.
- A status update within 14 days, including either a fix timeline or a
  decision that the report does not constitute a security issue.
- Public disclosure (with credit, if you wish) once a fix is shipped.

## Non-security report categories

Please do **not** use this channel for:

- Statistical questions about the methodology — use
  [GitHub Discussions](https://github.com/ftsiboe/okwaayeli/discussions).
- Bugs that do not have a security impact — use the
  [bug report template](./.github/ISSUE_TEMPLATE/bug_report.yml).
- Data harmonization requests — use the
  [data request template](./.github/ISSUE_TEMPLATE/data_request.yml).

## Learning more

The `okwaayeli` codebase deliberately avoids:

- Reading or writing files outside the user-specified `study_setup()` tree
  unless explicitly authorised.
- Storing credentials. The optional `GHProdLab_TOKEN` environment variable
  is read only at function call sites and is never written to disk.
- Phoning home for telemetry.

If you spot a deviation from any of these properties, please follow the
private-disclosure path above.
