# Mahjong AI

A comprehensive Mahjong AI system with support for game simulation, training, and Tenhou bot integration.

## Features

### Game Simulation
- **4-player standard** (四人麻雀)
- **3-player sanma** (三人麻雀)
- **East wind only** (東風) and **half-game** (半荘) variants
- Configurable rules: red dora, ura dora, kan dora, kiriage mangan, etc.

### Tenhou Bot
- Complete Tenhou protocol implementation
- Automatic tile encoding/decoding
- Strategy interface for AI integration
- Real-time game state tracking

### Web UI
- **Training Dashboard**: Visualize training progress with real-time statistics
- **Play vs AI**: Interactive game interface for human vs AI gameplay
- **Tenhou Control Panel**: Configure and monitor Tenhou bot

### MLX Training (Apple Silicon)
- Neural network training optimized for Apple Silicon
- NumPy fallback for non-Mac systems
- Experience replay buffer
- Configurable hyperparameters

## Quick Start

### Prerequisites

**OCaml (for game logic):**
```bash
opam install dune
```

**Python (for training and web UI):**
```bash
pip install -r requirements.txt
```

### Build OCaml

```bash
dune build
```

### Run Training

Using MLX (Apple Silicon):
```bash
python -m mlx_training.train --games 10000 --mode 4p-half
```

Using NumPy (all platforms):
```bash
python -m mlx_training.train --games 10000 --no-mlx
```

### Start Web UI

```bash
python -m web.server --port 8080
```

Then open http://localhost:8080 in your browser.

### Run OCaml Demo

```bash
./mahjong              # Show info
./mahjong --train      # Run training simulation
./mahjong --tenhou     # Start Tenhou bot
./mahjong --web        # Show web server instructions
```

## Architecture

```
┌─────────────────┐     ┌─────────────────┐
│   OCaml Core    │────▶│  Python/MLX     │
│  (Game Logic)   │◀────│  (Training)     │
└────────┬────────┘     └────────┬────────┘
         │                       │
         │    ┌─────────────┐    │
         └───▶│   Web UI    │◀───┘
              │  (aiohttp)  │
              └─────────────┘
```

- **OCaml**: Game rules, state management, Tenhou protocol
- **MLX/NumPy**: Neural network training and inference
- **aiohttp**: Web server and WebSocket communication

## Directory Structure

```
mahjong/
├── bin/              # OCaml executable
│   └── main.ml
├── lib/              # OCaml library
│   ├── tiles.ml          # Tile types and utilities
│   ├── hand.ml           # Hand representation
│   ├── rules.ml          # Game rules configuration
│   ├── state.ml          # Game state management
│   ├── simulation.ml     # Game simulation engine
│   ├── training.ml       # OCaml training infrastructure
│   ├── tenhou_protocol.ml # Tenhou protocol parser
│   ├── tenhou_bot.ml     # Tenhou bot client
│   └── nn_interface.ml   # Neural network interface
├── test/             # Tests
├── web/              # Web UI
│   ├── server.py         # Web server
│   ├── static/           # CSS, JavaScript
│   └── templates/        # HTML templates
├── mlx_training/     # MLX training module
│   └── train.py          # Training loop
└── requirements.txt  # Python dependencies
```

## Game Modes

| Mode | Players | Rounds | Japanese |
|------|---------|--------|----------|
| 4P Half | 4 | 8 | 四人半荘 |
| 4P East | 4 | 4 | 四人東風 |
| 3P Half | 3 | 6 | 三麻半荘 |
| 3P East | 3 | 3 | 三麻東風 |

## Configuration

### Training Configuration

```python
TrainingConfig(
    num_games=10000,      # Number of games to train
    batch_size=64,        # Batch size for training
    learning_rate=0.001,  # Learning rate
    discount_factor=0.99, # Reward discount
    exploration_rate=0.1, # Exploration rate
    game_mode="4p-half",  # Game mode
    use_mlx=True,         # Use MLX (Apple Silicon)
)
```

### Tenhou Bot Configuration

```ocaml
{
  username = "NoName";
  auth_token = "";
  server = "133.242.10.78";
  port = 10080;
  lobby = 0;           (* 0=般, 1=上, 2=特, 3=鳳 *)
  game_type = 9;       (* 4人東 *)
}
```

## Web UI Screenshots

### Training Dashboard
- Real-time training statistics
- Win rate and loss graphs
- Configuration panel

### Play vs AI
- Interactive tile selection
- Action buttons (riichi, chi, pon, kan, etc.)
- Opponent AI visualization

### Tenhou Bot
- Connection status
- Game log
- Strategy selection

## License

MPL-2.0

## Author

Yishuai Li
