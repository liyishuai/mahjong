# Plan: Port mjx to OCaml

## Goal
Translate the `mjx` Mahjong library to OCaml, using Cap'n Proto for serialization/RPC and Dune for building.

## Source Reference
The original C++ implementation is located at: `/Users/yishuai.li/mjx`

**CRITICAL: All OCaml code must align with the C++ reference implementation.**
- Never design our own algorithms or data structures
- All code must match the C++ behavior exactly
- Use C++ tests as the source of truth
- Reference: `/Users/yishuai.li/mjx/include/mjx/internal/*.cpp` and `/Users/yishuai.li/mjx/tests_cpp/*`


**OCaml Code Style:**
- Prefer lists over arrays for sequential iteration
- Use arrays only when indexed access is required
- Use `begin ... end` delimiters to avoid pattern matching ambiguity in nested expressions
- Keep functions pure and side-effect-free when possible
- Use `Option` and `Result` types for error handling instead of exceptions

## Strategy: Implement & Test
We will strictly follow an iterative "Implement -> Test" cycle. No feature is considered complete until a corresponding test case in `test/` verifies it.
**Crucially**, our tests must strictly align with the tests in `/Users/yishuai.li/mjx/tests_cpp/`. We will verify behavior against the original implementation's expectations.

## Migration Schedule

### Phase 1: Foundation (Completed)
- [x] Project initialization (`dune-project`, `mahjong.opam`, `.ocamlformat`).
- [x] Cap'n Proto schema definition (`src/mjx_proto.capnp`).
- [x] OCaml type generation via `capnpc-ocaml`.
- [x] Basic `Tile` module implementation (`src/tile.ml`).
- [x] **Test** `Tile` module (Align with `mjx/tests/tile_test.cpp`).

### Phase 2: Core Data Structures (Completed)
- [x] `Open` module: Port meld (Chi, Pon, Kan) logic (`src/open.ml`).
- [x] **Test** `Open` module (Align with `/Users/yishuai.li/mjx/tests_cpp/internal_open_test.cpp`).
- [x] `Hand` module: Port hand management (Draw, Discard, ApplyOpen) (`src/hand.ml`).
- [x] **Test** `Hand` module (Align with `/Users/yishuai.li/mjx/tests_cpp/internal_hand_test.cpp`).
- [x] `Action` module: Port action validation, creation, and encoding (src/action.ml).
  - Action types: Discard, Tsumogiri, Riichi, Tsumo, Ron, Chi, Pon, Kan variants, No, NineTiles, Dummy
  - Encode/Decode: 0-180 action code mapping
  - Validation: who (0-3), tile (0-135), open bits
- [x] **Test** `Action` module (Align with `/Users/yishuai.li/mjx/tests_cpp/internal_action_test.cpp`).
  - All 16 encoding tests passed
- [x] `Wall` module: Port deck management and dora logic (src/wall.ml).
  - 136-tile deck with proper indexing
  - Initial hand calculation (13 tiles per player, round-dependent)
  - Normal draws (70 tiles available without kan)
  - Kan draws (4 special tiles at end of deck)
  - Dora indicators (visible and hidden ura dora)
  - Align with `/Users/yishuai.li/mjx/include/mjx/internal/wall.cpp`
- [x] **Test** `Wall` module (Align with `/Users/yishuai.li/mjx/tests_cpp/internal_wall_test.cpp`).
  - All 8 tests passed (initial hands, draws, kan, doras)

### Phase 3: Mahjong Algorithms (Completed)
- [x] **Shanten calculation**: Port the shanten calculator.
    - [x] Analyze `mjx` Shanten logic (Cache vs. Calculation).
    - [x] Implement `Shanten` module.
    - [x] **Test** Shanten calculator (Strictly verify against `mjx` outputs).
- [x] **Win/Yaku evaluation**: Port the rules engine for scoring and win conditions.
    - [x] Analyze `mjx` Yaku logic.
    - [x] Implement `Yaku_evaluator` module.
    - [x] **Test** Win/Yaku evaluation.
    - [x] Port `WinHandCacheGenerator` for offline cache generation (`src/win_cache_generator.ml`).
    - [x] **Refactor to idiomatic OCaml**:
        - Changed from bytes encoding to `int list list` (natural representation)
        - Keys: `string` (abstract hands like "111,4142")
        - Values: `int list list list` (decompositions as lists)
        - Generated cache embedded in source code (not binary)
        - Build-time generation via dune rules
        - Tenpai cache generation (34,539 hands)
        - Performance: ~4s parallel generation with 8 domains

### Phase 4: Game Engine (Current Focus)
- [x] `State` module (Initial Skeleton): Manage the complete game state (`src/state.ml`).
- [ ] `Wall` module: Implement wall logic (draws, dora indicators).
- [ ] **Test** State transitions.
- [ ] `Environment` module: Implement the game loop and rule enforcement.
- [ ] **Test** Environment loop.

### Phase 5: RPC and Integration
- [ ] Cap'n Proto RPC: Implement the `Agent` interface for remote interaction.
- [ ] CLI: Provide tools to run games or interact with the engine.

## Current Status Summary

**Completed Modules:**
- `Tile`: Full implementation with red five support
- `Open`: Chi/Pon/Kan logic
- `Hand`: Hand management (draw, discard, open)
- `Shanten`: Shanten calculation
- `Win_cache`: Win detection + decomposition (embedded data)
- `Yaku_evaluator`: Core yaku evaluation (subset of yakus)

**Test Status:**
- ✅ Tile tests passing
- ✅ Open tests passing
- ✅ Hand tests passing
- ✅ Action tests passing (16/16)
- ✅ Wall tests passing (8/8)
- ✅ Event tests passing (5/5)
- ✅ Shanten tests passing
- ✅ Win_cache tests passing
- ✅ Yaku tests passing (subset)

**Build System:**
- Two-stage build: generate cache → compile library
- `win_cache_data.ml` generated in `_build/` (not checked in)
- Parallel cache generation (~4s with 8 domains)
- 9,362 winning patterns, 18,426 decompositions

## Migration Roadmap

### High Priority (Core Game Engine)
1. ~~**Action module** (`action.ml`): Action validation, creation, encoding~~ **[COMPLETED]**
   - Align with: `tests_cpp/internal_action_test.cpp`
   - 16 tests passed, all encoding variants working
2. ~~**Wall module** (`wall.ml`): Deck management, draw/dora logic~~ **[COMPLETED]**
   - Align with: `tests_cpp/internal_wall_test.cpp`
   - 8 tests passed, all wall operations working
   - Features: 136-tile deck, initial hand calculation, draws, kan draws, dora indicators
   - Uses `Stdlib.Array.shuffle` and `Stdlib.Random.full_init [| round; honba; game_seed |]`
   - Note: Simplified seed approach from C++ (which uses mt19937_64 with 512 wall seeds)
3. ~~**Event module** (`event.ml`): Game event representation~~ **[COMPLETED]**
   - 20 event types (Draw, Discard, Tsumogiri, Riichi, Chi, Pon, Kan variants, Tsumo, Ron, NewDora, AbortiveDraw, ExhaustiveDraw)
   - Factory functions for all event types
   - Validation function matching C++ behavior
   - 5 tests passed
4. **State module (complete)** (`state.ml`): Full game state
   - Align with: `tests_cpp/internal_state_test.cpp`
5. **Environment module** (`environment.ml`): Game loop, rule enforcement
   - Align with: `tests_cpp/internal_environment_test.cpp`

### Medium Priority (Agent Interface)
6. **Observation module** (`observation.ml`): Agent view of game state
7. **Agent interface** (`agent.ml`): Strategy interface
8. **Strategy** (`strategy.ml`): Rule-based baseline agent

### Low Priority (Utilities)
9. **Game seed** (`game_seed.ml`): Reproducible RNG
10. **Game result** (`game_result.ml`): Result summarization
11. **Utils** (`utils.ml`): Helper functions

## Next Steps
1. Port **Action module** (depends on: Tile, Open, Hand ✓)
2. Port **Wall module** (independent)
3. Port **Event module** (independent)
4. Complete **State module** (depends on: Hand ✓, Wall, Event)
5. Port **Environment module** (depends on: State, Action)