# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## Release [1.5.0] - 2025-08-15

### Added

- Exposed existing `TStringKit` functionality as string type helpers in `TStringHelperEx` (e.g., `Soundex`, `Metaphone`, `CountWords`, formatting, case conversions, splitting/joining, etc.) for instance-style usage like `'text'.Soundex`.
- Added comprehensive unit tests for Roman numeral conversion (`FromRoman`, `ToRoman`), including boundary and invalid cases.
- Added Base64 encoding/decoding (`Encode64`, `Decode64`) to both `TStringKit` and `TStringHelperEx`.

### Changed

- Renamed delimiter-centric helper method `Join(const Strings: TMatchStrings)` to `JoinWith(const Strings: TMatchStrings)` in `TStringHelperEx` for clarity. Usage example: `', '.JoinWith(Arr)`.
- Updated tests and examples to prefer the delimiter-first, instance-style `JoinWith` over array-first calls.

### Fixed

- Soundex implementation aligned with standard rules (handle H/W non-reset and first-letter code), fixing cases like `"Ashcraft" -> A261` and `"Tymczak" -> T522`.
- Metaphone implementation adjusted for initial `WR` (silent W), `CK` collapsing, and soft `C` duplicate suppression, fixing cases like `"wrack" -> RK` and `"science" -> SNS`.
- Tests updated to reflect correct behavior where punctuation splits words (e.g., `CountWords` on `'This is a test.'` expects 4).
- Base64 decoding strictness: `TStringKit.Decode64` now validates padding strictly (length % 4, '=' only at the end, 1–2 '='), returning an empty string on invalid input. This resolves previously failing Base64 decode tests.

## Release [1.0.0] - 2025-08-12

### Changed

- Separated from TidyKit-FP as a separate library for ease of maintenance.
- Updated documentation to reflect this change.

