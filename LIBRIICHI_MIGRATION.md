# Implementation Status

## Completed Features

### Phase 1-3: Core Components
- ✅ Tiles module (`lib/tiles.ml`) - Complete
- ✅ Hand parsing (`lib/hand.ml`) - Complete
- ✅ Shanten calculation (`lib/shanten.ml`) - Complete with table generation
- ✅ Agari calculation (`lib/agari.ml`) - Complete
- ✅ Point calculation (`lib/point.ml`) - Complete
- ✅ Mjai protocol (`lib/mjai.ml`) - Complete
12→- ✅ State management (`lib/state.ml`) - Complete, with migrated tests

### Phase 4: SP Calculator (Complete - Parity with Mortal)
**Location:** `lib/sp.ml`

**Status:** ✅ **COMPLETE** - Full parity with Mortal's `libriichi`

**Implemented:**
- ✅ Full recursive search with multi-turn probability arrays (17 turns)
- ✅ Recursive `draw` and `discard` functions matching `calc.rs`
- ✅ State caching using `Hashtbl` for efficient repeated calculations
- ✅ Tegawari (hand improvement) paths consideration
- ✅ Shanten down consideration in search (with multi-turn recursive evaluation)
- ✅ Accurate probability table computation (tsumo/not-tsumo)
- ✅ Agari scoring integration with uradora probability modeling
- ✅ Correct red tile handling in both discard and draw phases
- ✅ Robust state management with guaranteed restoration using `with_discard`/`with_deal`

**How It Works:**
```
Recursive Backtracking Search:
1. discard_recursive(state, shanten):
   - Check cache for (state, shanten)
   - For each legal discard:
     - evaluate EV/WinProb using recursive draw_recursive
     - Select discard maximizing value at current turn
2. draw_recursive(state, shanten):
   - Check cache for (state, shanten)
   - If calc_tegawari: evaluate all draw outcomes (improvement and same-shanten)
   - Else: evaluate only shanten-improving draws (optimized search)
3. get_score(state, win_tile):
   - Calculate real points including riichi, ippatsu, haitei, and dora
   - Strictly model uradora probabilities for riichi hands
```

**Test Coverage:**
- ✅ Basic type tests (required_tile, candidate, comparison)
- ✅ State manipulation tests (backtracking safeguards, red tile awareness)
- ✅ Config tests (all search options)
- ✅ Nanikiru tests (discard decisions, shanten down, EV maximization)
- ✅ Tsumo-only tests (verify unknown tile marker and probabilities)
- ✅ Probability table tests (mathematical correctness verification)
- ✅ Integration tests (multi-turn calc, tenpai with agari, dora indicators)

**Results:** 11/11 tests passing.

**Reference:** `algo/sp/` directory in Mortal
- `algo/sp/calc.rs` - Main recursive algorithm
- `algo/sp/state.rs` - State operations
- `algo/sp/candidate.rs` - Candidate types and sorting

### obs_repr: Version 4 SP Table (Integrated)
**Location:** `lib/state.ml:2150`

**Status:** ✅ **COMPLETE**

**Implemented:**
- ✅ version 4 observations now correctly encode SP Calculator results
- ✅ Encodes EV (scaled to 100k and 30k)
- ✅ Encodes required tiles (presence and counts)
- ✅ Encodes full SP table (tenpai_probs, win_probs, exp_values for 17 turns)
- ✅ Proactive `akas_seen` tracking in `PlayerState` for better SP accuracy
- ✅ Added `witness_tile` helper to update visibility flags across all MJAI events

**Reference:** `state/obs_repr.rs:644` (encode_sp_table) in Mortal

### Phase 8: Python Integration
pyml bindings for OCaml-Python bridge.

**Reference:** Python integration bindings

---

## Design Notes

### Code Style Guidelines

**IMPORTANT**: Do not reference external implementations (Rust, Mortal, reference implementations, etc.) in code comments. All code comments should focus on what the code does and why, not where it came from or what other implementations exist.

### Module Structure
- **Flattened modules**: `algo/point.rs` → `lib/point.ml` (not `lib/algo/point.ml`)
- Follows OCaml convention: all modules in `lib/` directory

### Data Structures
- **Arrays vs lists**: Use arrays for known-length, fixed-size data; use lists for variable-length data
- **Mutable state**: PlayerState uses mutable fields for performance
- **Table generation**: Build-time generation for shanten/agari lookup tables

### Error Handling
- Uses `Result` type consistently throughout
- No exceptions for expected error conditions

### Serialization
- **JSON**: Yojson for MJAI protocol
- **Observation encoding**: Direct array encoding for ML (no numpy dependency)

### State Module Test Parity

The OCaml state tests in [`test/test_state.ml`](test/test_state.ml) now mirror all Rust tests from `Mortal/libriichi/src/state/test.rs`, including:

- Waits calculation
- Chi options
- Furiten tracking (temporary and riichi-furiten)
- Dora counting after kans
- Rule-based agari decisions in all-last situations
- Kakan from hand
- Discard candidates that keep unconditional tenpai
- Double chankan ron scenarios

The OCaml implementation now achieves full behavioral parity with Rust, matching exact shanten values and wait arrays in all tested scenarios, including subtle differences in shanten updates for non-riichi players.
