# AGENTS

## Improving Parsers Coverage

**Goal**: parse every source code file. A single parse error drops the entire file from the framework's analysis, so any coverage improvement directly increases what we can scan.

1. **Regression repo**: a fixed set of source files acts as the coverage benchmark. Each file is either parsed successfully or fails at some location; record this baseline.
2. **One rule adjustment at a time**: each iteration adds or adjusts at most one production in the relevant `.y` file (currently `TsParser.y`).
3. **Acceptance per iteration**:
   1. Every file that previously parsed must still parse.
   2. Every file that previously failed at column `c` must now either parse, or fail strictly past `c`. (Native ASTs are single-line, so column is the only progress metric.)
4. **Guardrails — must pass before the regression check**:
   1. `cabal build` succeeds.
   2. Happy reports **no new shift/reduce or reduce/reduce conflicts** vs the previous baseline. This guarantees that every input the previous grammar accepted is still accepted with the same reductions, i.e. existing rules keep their meaning.

### Example adjustments

Each example below makes the grammar strictly more permissive. If Happy reports no new conflicts, the change is a strict superset — every previous input still parses with the same reductions, and at least one new shape now also parses.

1. **Widen a list to allow empty**: replace `commalistof(a)` with `possibly_empty_commalistof(a)` in some production. Non-empty lists still parse; empty lists now parse too.
2. **Make a symbol optional**: replace `a` with `optional(a)` in some production. Inputs containing `a` still parse; inputs missing `a` now parse too.

### Active cleanup conventions

The grammar is being prepared so each per-iteration prompt stays small. Until that's done, edits should follow these conventions:

1. **Action code lives in `TsParserActions.hs`**. Each production's `{ ... }` body shrinks to a single call `{ Actions.helperName $i $j ... }`; the helper is a smart constructor in `TsParserActions.hs`, imported as `import qualified TsParserActions as Actions`.
2. **Consolidate when possible**. When two near-identical productions differ only in an optional / repeated / alternative position, collapse them using a shared helper:
   - one with a list, one without → `possibly_empty_commalistof(a)`
   - one with an extra symbol, one without → `optional(a)`
   - one accepting a single form, one accepting an aggregate → a sum nonterminal (e.g. `stmtOrBlock: stmt { [$1] } | block { $1 }`)
3. **camelCase = handled, snake_case = TODO**. A production starts in snake_case (`stmt_if`, `else_part`). Once it's consolidated and its action moved into `Actions.*`, rename it to camelCase (`stmtIf`, `elsePart`). The mix is the at-a-glance progress indicator inside `TsParser.y`.
4. **Handled so far**: `stmtIf`, `elsePart`, `expCall`, `stmtImport`, `stmtFunction`, `stmtDecvar`, `stmtReturn`, `stmtAssign`, `stmtExp`, `expBinop`, `expBool` (incl. `expTrue`/`expFalse`), `expArrowFunction`, `expNull` — plus support helpers `block` (was `body`), `stmtOrBlock`, `decvarLhs`, `stmtExpBody`, `trueKeyword`, `falseKeyword`, `possibly_empty_commalistof` (was `possibly_empty_listof`).
5. **Reusable list/option helpers** (use these before introducing new ones): `optional(a)`, `listof(a)`, `commalistof(a)`, `possibly_empty_commalistof(a)`, `commalistof_with_optional_trailing_comma(a)`, `possibly_empty_commalistof_with_optional_trailing_comma(a)`, `barlistof(a)` (bar-separated, for union types), `ampersandlistof(a)` (ampersand-separated, for intersection types), `stmtOrBlock`.
