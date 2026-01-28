# Plan: Port Yaku Evaluation

## Overview
Port the `YakuEvaluator` class and its dependencies (`WinScore`, `WinHandCache`, `WinInfo`) from C++ to OCaml.
The Yaku evaluation logic is extensive and relies on:
1.  Checking for specific Yakus (Pinfu, Tanyao, etc.).
2.  Maximizing Fan/Fu.
3.  Using a pre-computed cache for valid winning hands (`WinHandCache`).

## Status: Core Complete, Refinement Needed

### Completed Components

**Win Cache System:**
- `src/win_cache_generator.ml`: Parallel cache generator (8 domains, ~4s)
  - Generates 9,362 winning hand patterns
  - Generates 18,426 pattern decompositions
  - Generates 34,539 tenpai (ready) hands
  - **Refactored to idiomatic OCaml**:
    - Natural data types: `(string, int list list list) Hashtbl.t`
    - No manual byte encoding (was C++ habit)
    - Embedded source code generation (not binary marshaling)
    - Build-time generation via dune
    - Arrays used only for indexed access, lists for iteration

- `src/win_cache.ml`: Win detection & decomposition
  - `has`: Check if hand is winning
  - `tenpai`: Check if hand is ready (1-tile away)
  - `machi`: Get waiting tiles
  - `sets_and_heads`: Decompose into sets (mentsu) and heads (jantou)
  - Special case handling: Kokushi musou (13 orphans)
  - Cache loaded from embedded `Win_cache_data.create_win_cache()`

**Yaku Evaluation:**
- `src/yaku_types.ml`: Type definitions
  - `Yaku.t`: All yaku types (50+ variants)
  - `WinInfo.t`: Hand context for evaluation
  - `Wind.t`: Seat/prevalent wind

- `src/yaku_evaluator.ml`: Scoring logic
  - `judge_yakuman`: Big patterns (13 orphans, all honours, etc.)
  - `judge_simple_yaku`: Standard yaku (tanyao, yakuhai, flush, etc.)
  - `judge_dora`: Dora counting (regular, reversed, red fives)
  - `maximize_total_fan`: Pattern-based yaku (pinfu, all pons, seven pairs)
  - `evaluate`: Main entry point

### Currently Implemented Yaku

**Yakuman (1 han = limit):**
- Big Three Dragons
- All Honours
- All Terminals
- Thirteen Orphans

**Standard (1-6 han):**
- Fully Concealed Hand (menzen tsumo)
- Riichi / Double Riichi
- Ippatsu
- All Simples (tanyao)
- Yakuhai (dragons, seat/prevalent winds)
- Half Flush / Full Flush
- Dora / Reversed Dora / Red Dora
- Pinfu (pattern-based)
- All Pons (pattern-based)
- Seven Pairs (pattern-based)

### Pending Work

**Missing Yaku (30+ variants):**
- Sequences: Iipeikou, Ryanpeikou, Sanshoku doujun, Ittsuu
- Triplets: Sanshoku doukou, Sankantsu, Toitoi
- Terminals/Honours: Chanta, Junchan, Honroutou
- Special: Rinshan, Chankan, Haitei, Houtei
- More yakuman: Four winds, Four kans, etc.

**Fu Calculation:**
- Currently placeholder (25 for seven pairs, 30 default)
- Need proper fu counting:
  - Base fu (20/25/30)
  - Pair fu (dragons, winds)
  - Triplet fu (concealed/open, simples/terminals)
  - Wait type fu (tanki, kanchan, penchan, ryanmen)
  - Tsumo fu, menzen ron fu

**Integration:**
- Connect to State module for game context
- Win score calculation (han + fu → points)
- Payment calculation (dealer/non-dealer)