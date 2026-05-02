"""Regenerate the boilerplate token blocks inside the .y parser sources.

Currently scoped to a single block: the reserved-keyword tokens of TsParser.y,
listed one per line in src/TyKeywords.txt (next to TsParser.y itself). The
block in TsParser.y is delimited by two single-line marker comments

    -- reserved keywords start
    -- reserved keywords end

Everything strictly between those two markers is replaced with one token line
per keyword, in the order they appear in TyKeywords.txt:

    'Identifier' { AlexTokenTag AlexRawToken_Identifier _ _ }

The script is idempotent: running it after no edits to TyKeywords.txt leaves
TsParser.y byte-identical (same line endings, same trailing newline).

Usage:
    python mk_parsers.py
"""

from __future__ import annotations

from pathlib import Path

HERE = Path(__file__).resolve().parent

START_MARKER = "-- reserved keywords start"
END_MARKER = "-- reserved keywords end"


def render_keyword_line(keyword: str) -> str:
    lbrace = "{"
    rbrace = "}"
    return f"'{keyword}' {lbrace} AlexTokenTag AlexRawToken_{keyword} _ _ {rbrace}"


def regenerate_block(parser_path: Path, keywords_path: Path) -> int:
    keywords = [
        line.strip()
        for line in keywords_path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]

    raw = parser_path.read_bytes()
    newline_bytes = b"\r\n" if b"\r\n" in raw else b"\n"
    newline = newline_bytes.decode("ascii")
    has_trailing_newline = raw.endswith(newline_bytes)

    text = raw.decode("utf-8")
    lines = text.split(newline)
    if has_trailing_newline and lines and lines[-1] == "":
        lines.pop()

    start_idx = end_idx = None
    for i, line in enumerate(lines):
        stripped = line.strip()
        if stripped == START_MARKER and start_idx is None:
            start_idx = i
        elif stripped == END_MARKER and start_idx is not None and end_idx is None:
            end_idx = i
            break

    if start_idx is None or end_idx is None:
        raise SystemExit(
            f"Could not find both markers in {parser_path}: "
            f"'{START_MARKER}' and '{END_MARKER}'"
        )

    rendered = [render_keyword_line(k) for k in keywords]
    new_lines = lines[: start_idx + 1] + rendered + lines[end_idx:]

    out = newline.join(new_lines)
    if has_trailing_newline:
        out += newline

    parser_path.write_bytes(out.encode("utf-8"))
    return len(rendered)


def main() -> None:
    parser_path = HERE / "src" / "TsParser.y"
    keywords_path = HERE / "src" / "TyKeywords.txt"
    n = regenerate_block(parser_path, keywords_path)
    print(f"Wrote {n} keyword lines into {parser_path.relative_to(HERE)}")


if __name__ == "__main__":
    main()
