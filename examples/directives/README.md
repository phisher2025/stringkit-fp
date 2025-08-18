# Compiler Directives Examples (Feature Flags)

These examples show how to compile `TStringHelperEx` with only selected feature groups using conditional defines.

As noted in `README.md`, defines must be set at the project/build level so they apply when compiling `src/StringKitHelper.pas`. Putting `{$DEFINE ...}` inside your program does not affect the separate unit.

## Available groups

- `SK_MANIP` — trim, pad, collapse whitespace, reverse, length, substring
- `SK_MATCH` — regex match/extract, contains/starts/ends, words, counts
- `SK_COMPARE` — Levenshtein, Hamming, Jaro/Jaro-Winkler, LCS, fuzzy
- `SK_CASE` — title, camel, pascal, snake, kebab
- `SK_VALIDATE` — email, URL, IP (v4/v6), date
- `SK_FORMAT` — truncate, file size, number/float formatting
- `SK_NUMERIC` — roman, ordinal, number-to-words, from-roman
- `SK_ENCODE` — hex, base64, HTML, URL encode/decode
- `SK_SPLIT` — split, join
- `SK_PHONETIC` — soundex, metaphone, readability, ngrams, basic counts

## Lazarus (FPC)

Project Options → Compiler Options → Other → Custom Options

- Encode-only demo: `-dSK_ANY -dSK_ENCODE`
- Case+Encode demo: `-dSK_ANY -dSK_CASE -dSK_ENCODE`

Also ensure your project can find the units and includes:

- Add `-Fu..\..\src -Fu..\..\src\inc` to "Other units" search paths or project options.

Ensure the source and includes are on the search path.

## fpc (CLI)

From the repo root:

```bash
fpc -dSK_ANY -dSK_ENCODE \
  -Fu./src \
  examples/directives/EncodeOnlyDemo.pas
```

```bash
fpc -dSK_ANY -dSK_CASE -dSK_ENCODE \
  -Fu./src \
  examples/directives/CaseAndEncodeDemo.pas
```

## Notes

- If you call a helper method from a group you didn’t enable, you’ll get a compile-time unknown identifier error. That’s expected and confirms the selection is working.
- With no defines provided, `SK_ALL` is assumed and all groups are enabled.
