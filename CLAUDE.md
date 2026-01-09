# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Mahjong AI system with a hybrid OCaml/Python architecture. OCaml handles game logic, rules, and simulation, while Python (with optional MLX for Apple Silicon) handles neural network training and the web UI.

## Build and Development Commands

### OCaml Build System (Dune)

```bash
# Build the entire project
dune build

# Run the main executable (shows info and demo)
./mahjong

# Build and run tests
dune test

# Run a specific test
dune exec test/test_simulation.exe
dune exec test/test_mahjong.exe

# Format OCaml code (uses Jane Street profile)
dune build @fmt --auto-promote

# Clean build artifacts
dune clean
```

### Python Components

```bash
# Install Python dependencies
pip install -r requirements.txt

# Run training (MLX on Apple Silicon)
python -m mlx_training.train --games 10000 --mode 4p-half

# Run training (NumPy fallback for non-Mac)
python -m mlx_training.train --games 10000 --no-mlx

# Start web server
python -m web.server --port 8080

# Run OCaml training simulation
./mahjong --train --games 1000
```

### OCaml Executable Modes

```bash
./mahjong              # Show system info and demo
./mahjong --train      # Run training simulation
./mahjong --tenhou     # Show Tenhou bot configuration
./mahjong --web        # Show web server instructions
./mahjong --games N    # Set number of games for training
```

## Architecture Overview

### Core OCaml Libraries (lib/)

The OCaml library is organized into distinct modules with clear responsibilities:

1. **tiles.ml** - Base tile types and random wall generation
   - Defines tile types: `Man`, `So`, `Pin`, `Honor`
   - Number types include `Aka` for red dora fives
   - `random_set` generates shuffled 136-tile walls with Fisher-Yates shuffle

2. **hand.ml** - Hand representation and meld structures
   - Defines `hand` type with tiles and open/closed melds
   - Chi, pon, kan structures

3. **rules.ml** - Game rule configurations
   - Defines point rules, dora rules, yaku rules
   - Default configurations: `default_four_player`, `default_three_player`, `default_four_player_east`, `default_three_player_east`
   - 3-player rules automatically remove Man 2-8 tiles (28 tiles total)
   - Supports red dora (aka), ura dora, kan dora, kiriage mangan

4. **state.ml** - Game state management
   - Defines `game_state`, `round_state`, `player_state`
   - Tracks wall position, dora indicators, riichi status, furiten
   - Player rivers track discards with metadata (riichi, tsumogiri, called)
   - Wind management for both seat and round winds

5. **simulation.ml** - Game simulation engine
   - Action types: `DrawAction`, `DiscardAction`, `CallChi`, `CallPon`, `CallKan`, `DeclareRiichi`, `DeclareTsumo`, `DeclareRon`, `Pass`
   - Action results: `Continue`, `RoundEnd`, `GameEnd`, `Invalid`
   - Handles wall exhaustion, turn progression, action validation

6. **training.ml** - Training infrastructure
   - Feature extraction for ML: hand encoding, river features, score features, dora features
   - Training samples pair features with actions
   - Interfaces with Python training via JSON or WebSocket

7. **nn_interface.ml** - Neural network interface
   - Defines `nn_input` feature vectors (~800+ dimensions)
   - Feature categories: hand (136), discards (544), game state (~50), strategic hints (~100)
   - Output action probabilities: discard, riichi, chi, pon, kan, tsumo, ron, pass

8. **tenhou_protocol.ml** - Tenhou protocol encoding/decoding
   - Tile encoding/decoding for Tenhou format
   - Message encoding: discard, reach, noop, etc.
   - Handles Tenhou's integer tile codes

9. **tenhou_bot.ml** - Tenhou bot client
   - Default configuration for Tenhou server (133.242.10.78:10080)
   - Lobby levels: 0=般, 1=上, 2=特, 3=鳳
   - Game types (e.g., 9 = 4人東)
   - Strategy interface for AI integration

### Python Components

1. **mlx_training/train.py** - Neural network training
   - Uses MLX on Apple Silicon for GPU acceleration
   - NumPy fallback for non-Mac systems
   - Feature extraction class `MahjongFeatures` (matches OCaml nn_interface dimensions)
   - Training configuration: batch size, learning rate, discount factor, exploration rate
   - Game modes: "4p-half", "4p-east", "3p-half", "3p-east"

2. **web/server.py** - Web UI server (aiohttp)
   - Training dashboard with real-time statistics
   - Human vs AI gameplay interface
   - Tenhou bot control panel
   - WebSocket support for live updates

### OCaml-Python Communication

The OCaml and Python components communicate via:
- JSON files for batch training data
- WebSocket for real-time web UI updates
- Feature vectors must match dimensions between OCaml's `nn_interface.ml` and Python's `MahjongFeatures` class

## Game Rules Implementation

### Supported Game Variants

- **4-player half-game (四人半荘)**: 8 rounds (East + South)
- **4-player East only (四人東風)**: 4 rounds (East only)
- **3-player half-game (三麻半荘)**: 6 rounds, Man 2-8 removed
- **3-player East only (三麻東風)**: 3 rounds, Man 2-8 removed

### Important Rule Details

- Starting points: 25,000 per player
- Red dora (aka): Man5, So5, Pin5 (configurable)
- Ura dora revealed on riichi win
- Kan dora added on kan calls
- Kiriage mangan (optional round-up)
- Furiten tracking (both temporary and permanent)
- Ippatsu window tracking

### 3-Player (Sanma) Specifics

When using 3-player rules, the wall generation in `tiles.ml` removes Man 2-8 tiles:
- Original wall: 136 tiles
- 3-player wall: 108 tiles (136 - 28)
- This is validated in `test_sanma_wall` test

## Testing

Tests are located in `test/` directory:

- **test_mahjong.ml** - Basic tile and hand tests
- **test_simulation.ml** - Comprehensive simulation tests including:
  - Rules configuration validation
  - Game state initialization
  - Wall generation (including sanma wall verification)
  - Draw mechanics
  - Feature extraction

Run tests with `dune test` or individually with `dune exec test/test_simulation.exe`.

## Code Style

- OCaml code follows Jane Street profile (`ocamlformat version 0.27.0`)
- Format code with `dune build @fmt --auto-promote`
- Type annotations are preferred for public interfaces
- Documentation comments use OCaml doc syntax `(** ... *)`

## Common Development Workflows

### Adding New Game Features

1. Define data types in appropriate module (e.g., `tiles.ml`, `hand.ml`, `rules.ml`)
2. Update `state.ml` if new state tracking needed
3. Add action handling in `simulation.ml`
4. Update feature extraction in `training.ml` and `nn_interface.ml`
5. Sync Python `MahjongFeatures` class dimensions if features changed
6. Add tests in `test/test_simulation.ml`

### Modifying Neural Network Features

When changing feature dimensions:
1. Update `nn_interface.ml` feature vector definitions
2. Update `training.ml` feature extraction
3. Update Python `mlx_training/train.py` `MahjongFeatures` class
4. Ensure total dimension counts match between OCaml and Python
5. Retrain models with new feature set

### Working with Tenhou Protocol

- Tile codes are in `tenhou_protocol.ml`
- Bot configuration in `tenhou_bot.ml`
- Encoding/decoding functions convert between OCaml tiles and Tenhou integer codes
- See `bin/main.ml` show_info() for encoding examples

## Architecture Invariants

- **Wall immutability**: Once generated, wall array is not modified, only `wall_index` advances
- **Feature dimension consistency**: OCaml `nn_interface.ml` and Python `MahjongFeatures` must match
- **Turn progression**: Only `current_player` index changes, player array order is fixed
- **Furiten semantics**: Permanent furiten persists across turns, temporary furiten clears on turn end
- **3-player tile removal**: Man 2-8 removal happens during wall generation, not during gameplay
