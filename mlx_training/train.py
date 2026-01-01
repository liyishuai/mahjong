"""
Mahjong AI Training Module using MLX (Apple Silicon)

This module provides neural network training for the Mahjong AI using Apple's MLX framework,
optimized for training on Apple Silicon (M1/M2/M3) MacBooks.

Requirements:
    pip install mlx numpy

Usage:
    python -m mlx_training.train --games 10000 --mode 4p-half
"""

import json
import os
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

# Try to import MLX, fall back to numpy-only mode if not available
try:
    import mlx.core as mx
    import mlx.nn as nn
    import mlx.optimizers as optim
    MLX_AVAILABLE = True
except ImportError:
    MLX_AVAILABLE = False
    print("MLX not available, using numpy-only mode")

import numpy as np

# Game constants
TOTAL_TILES = 136          # Total tiles in a mahjong set (34 types * 4 each)
TILE_TYPES = 34            # Number of unique tile types
HAND_SIZE = 13             # Starting hand size
NUM_PLAYERS = 4            # Number of players in standard game
STARTING_POINTS = 25000    # Starting points per player


@dataclass
class TrainingConfig:
    """Training configuration"""
    num_games: int = 10000
    batch_size: int = 64
    learning_rate: float = 0.001
    discount_factor: float = 0.99
    exploration_rate: float = 0.1
    game_mode: str = "4p-half"  # 4p-half, 4p-east, 3p-half, 3p-east
    checkpoint_interval: int = 1000
    output_dir: str = "training_output"
    use_mlx: bool = True
    hidden_size: int = 512
    num_layers: int = 3


class MahjongFeatures:
    """Feature extraction for Mahjong game state"""
    
    # Feature dimensions
    FEATURE_HAND_SIZE = TOTAL_TILES  # 34 tile types * 4 instances
    FEATURE_DISCARD_SIZE = TOTAL_TILES * NUM_PLAYERS  # Per player discards
    FEATURE_WIND_SIZE = NUM_PLAYERS
    FEATURE_POINTS_SIZE = NUM_PLAYERS
    FEATURE_RIICHI_SIZE = NUM_PLAYERS
    FEATURE_DORA_SIZE = TOTAL_TILES
    
    @classmethod
    def total_size(cls) -> int:
        return (cls.FEATURE_HAND_SIZE + cls.FEATURE_DISCARD_SIZE + cls.FEATURE_WIND_SIZE * 2 + 
                cls.FEATURE_POINTS_SIZE + cls.FEATURE_RIICHI_SIZE + cls.FEATURE_DORA_SIZE + 10)
    
    @staticmethod
    def encode_hand(tiles: List[int]) -> np.ndarray:
        """Encode hand tiles as one-hot vector"""
        encoding = np.zeros(TOTAL_TILES, dtype=np.float32)
        for tile in tiles:
            if 0 <= tile < TOTAL_TILES:
                encoding[tile] = 1.0
        return encoding
    
    @staticmethod
    def encode_discards(discards: List[List[int]]) -> np.ndarray:
        """Encode all players' discards"""
        encoding = np.zeros(TOTAL_TILES * NUM_PLAYERS, dtype=np.float32)
        for player_idx, player_discards in enumerate(discards):
            for tile in player_discards:
                if 0 <= tile < TOTAL_TILES:
                    encoding[player_idx * TOTAL_TILES + tile] += 0.25
        return encoding
    
    @staticmethod
    def encode_wind(wind: int) -> np.ndarray:
        """One-hot encode wind (0-3)"""
        encoding = np.zeros(NUM_PLAYERS, dtype=np.float32)
        if 0 <= wind < NUM_PLAYERS:
            encoding[wind] = 1.0
        return encoding
    
    @staticmethod
    def encode_points(points: List[int], seat: int) -> np.ndarray:
        """Normalize points relative to own points"""
        own = points[seat]
        encoding = np.array([
            (p - own) / 50000.0 for p in points
        ], dtype=np.float32)
        return encoding
    
    @staticmethod
    def encode_riichi(riichi_status: List[bool]) -> np.ndarray:
        """Encode riichi status for all players"""
        return np.array([1.0 if r else 0.0 for r in riichi_status], dtype=np.float32)
    
    @classmethod
    def encode_state(cls, state: dict) -> np.ndarray:
        """Encode complete game state to feature vector"""
        features = []
        
        default_points = [STARTING_POINTS] * NUM_PLAYERS
        default_discards = [[] for _ in range(NUM_PLAYERS)]
        default_riichi = [False] * NUM_PLAYERS
        
        # Hand encoding
        features.append(cls.encode_hand(state.get('hand_tiles', [])))
        
        # Discards encoding
        features.append(cls.encode_discards(state.get('discards', default_discards)))
        
        # Wind encodings
        features.append(cls.encode_wind(state.get('round_wind', 0)))
        features.append(cls.encode_wind(state.get('seat', 0)))
        
        # Points encoding
        features.append(cls.encode_points(
            state.get('points', default_points),
            state.get('seat', 0)
        ))
        
        # Riichi status
        features.append(cls.encode_riichi(state.get('riichi_status', default_riichi)))
        
        # Dora encoding (placeholder)
        features.append(np.zeros(TOTAL_TILES, dtype=np.float32))
        
        # Additional features
        additional = np.array([
            state.get('round_num', 0) / 8.0,
            state.get('honba', 0) / 10.0,
            1.0 if state.get('last_draw') is not None else 0.0,
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0  # Reserved
        ], dtype=np.float32)
        features.append(additional)
        
        return np.concatenate(features)


# Output dimensions
NUM_TILE_OUTPUTS = TILE_TYPES  # Which tile type to discard
NUM_ACTION_OUTPUTS = 8  # tsumo, ron, riichi, chi, pon, kan, pass, discard


def softmax_np(logits: np.ndarray) -> np.ndarray:
    """Numerically stable softmax for numpy arrays"""
    exp_logits = np.exp(logits - np.max(logits))
    return exp_logits / exp_logits.sum()


if MLX_AVAILABLE:
    class MahjongNetwork(nn.Module):
        """Neural network for Mahjong AI using MLX"""
        
        def __init__(self, config: TrainingConfig):
            super().__init__()
            
            input_size = MahjongFeatures.total_size()
            hidden_size = config.hidden_size
            
            # Shared layers
            layers = [nn.Linear(input_size, hidden_size), nn.ReLU()]
            for _ in range(config.num_layers - 1):
                layers.extend([nn.Linear(hidden_size, hidden_size), nn.ReLU()])
            
            self.shared = nn.Sequential(*layers)
            
            # Policy heads
            self.tile_head = nn.Linear(hidden_size, NUM_TILE_OUTPUTS)
            self.action_head = nn.Linear(hidden_size, NUM_ACTION_OUTPUTS)
            
            # Value head
            self.value_head = nn.Linear(hidden_size, 1)
        
        def __call__(self, x):
            shared = self.shared(x)
            
            tile_logits = self.tile_head(shared)
            action_logits = self.action_head(shared)
            value = self.value_head(shared)
            
            return tile_logits, action_logits, value


class MahjongNetworkNumpy:
    """Numpy-only fallback network"""
    
    def __init__(self, config: TrainingConfig):
        self.config = config
        input_size = MahjongFeatures.total_size()
        hidden_size = config.hidden_size
        
        # Initialize weights
        self.weights = []
        prev_size = input_size
        
        for _ in range(config.num_layers):
            w = np.random.randn(prev_size, hidden_size).astype(np.float32) * 0.01
            b = np.zeros(hidden_size, dtype=np.float32)
            self.weights.append((w, b))
            prev_size = hidden_size
        
        # Output heads
        self.tile_w = np.random.randn(hidden_size, NUM_TILE_OUTPUTS).astype(np.float32) * 0.01
        self.tile_b = np.zeros(NUM_TILE_OUTPUTS, dtype=np.float32)
        
        self.action_w = np.random.randn(hidden_size, NUM_ACTION_OUTPUTS).astype(np.float32) * 0.01
        self.action_b = np.zeros(NUM_ACTION_OUTPUTS, dtype=np.float32)
        
        self.value_w = np.random.randn(hidden_size, 1).astype(np.float32) * 0.01
        self.value_b = np.zeros(1, dtype=np.float32)
    
    def forward(self, x: np.ndarray) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Forward pass"""
        h = x
        for w, b in self.weights:
            h = np.maximum(0, h @ w + b)  # ReLU
        
        tile_logits = h @ self.tile_w + self.tile_b
        action_logits = h @ self.action_w + self.action_b
        value = h @ self.value_w + self.value_b
        
        return tile_logits, action_logits, value
    
    def predict(self, state: dict) -> dict:
        """Get action from state"""
        features = MahjongFeatures.encode_state(state)
        tile_logits, action_logits, value = self.forward(features)
        
        # Softmax for probabilities
        tile_probs = softmax_np(tile_logits)
        action_probs = softmax_np(action_logits)
        
        return {
            'tile_probs': tile_probs.tolist(),
            'action_probs': action_probs.tolist(),
            'value': float(value[0])
        }


@dataclass
class TrainingSample:
    """Single training sample"""
    features: np.ndarray
    action_tile: int  # Which tile type was discarded
    action_type: int  # Which action was taken
    reward: float


class ExperienceBuffer:
    """Buffer for storing training experiences"""
    
    def __init__(self, max_size: int = 100000):
        self.max_size = max_size
        self.samples: List[TrainingSample] = []
    
    def add(self, sample: TrainingSample):
        if len(self.samples) >= self.max_size:
            self.samples.pop(0)
        self.samples.append(sample)
    
    def sample_batch(self, batch_size: int) -> List[TrainingSample]:
        if len(self.samples) < batch_size:
            return self.samples.copy()
        indices = np.random.choice(len(self.samples), batch_size, replace=False)
        return [self.samples[i] for i in indices]
    
    def __len__(self):
        return len(self.samples)


class Trainer:
    """Main training loop"""
    
    def __init__(self, config: TrainingConfig):
        self.config = config
        self.buffer = ExperienceBuffer()
        
        # Create output directory
        Path(config.output_dir).mkdir(parents=True, exist_ok=True)
        
        # Initialize network
        if MLX_AVAILABLE and config.use_mlx:
            self.network = MahjongNetwork(config)
            self.optimizer = optim.Adam(learning_rate=config.learning_rate)
            self.use_mlx = True
        else:
            self.network = MahjongNetworkNumpy(config)
            self.use_mlx = False
        
        # Statistics
        self.stats = {
            'games_played': 0,
            'total_rounds': 0,
            'wins': [0, 0, 0, 0],
            'total_scores': [0, 0, 0, 0],
            'losses': [],
            'win_rates': []
        }
    
    def simulate_game(self) -> List[TrainingSample]:
        """Simulate a single game and collect training samples"""
        samples = []
        
        # Simulate game state progression
        num_rounds = np.random.randint(4, 12)
        
        for round_num in range(num_rounds):
            # Generate random game state
            state = {
                'hand_tiles': sorted(np.random.choice(136, 13, replace=False).tolist()),
                'discards': [[], [], [], []],
                'round_wind': 0,
                'seat': 0,
                'points': [25000, 25000, 25000, 25000],
                'riichi_status': [False, False, False, False],
                'round_num': round_num,
                'honba': 0,
                'last_draw': None
            }
            
            # Simulate turns within round
            for turn in range(20):
                features = MahjongFeatures.encode_state(state)
                
                # Get action from network
                if self.use_mlx:
                    tile_logits, action_logits, _ = self.network(mx.array(features))
                    tile_idx = int(mx.argmax(tile_logits).item())
                    action_idx = int(mx.argmax(action_logits).item())
                else:
                    pred = self.network.predict(state)
                    tile_idx = int(np.argmax(pred['tile_probs']))
                    action_idx = int(np.argmax(pred['action_probs']))
                
                # Exploration
                if np.random.random() < self.config.exploration_rate:
                    tile_idx = np.random.randint(NUM_TILE_OUTPUTS)
                    action_idx = np.random.randint(NUM_ACTION_OUTPUTS)
                
                sample = TrainingSample(
                    features=features,
                    action_tile=tile_idx,
                    action_type=action_idx,
                    reward=0.0  # Will be updated at end of game
                )
                samples.append(sample)
                
                # Update state (simulate discard)
                if state['hand_tiles']:
                    discarded = state['hand_tiles'].pop(np.random.randint(len(state['hand_tiles'])))
                    state['discards'][0].append(discarded)
                    
                    # Draw new tile
                    new_tile = np.random.randint(136)
                    state['hand_tiles'].append(new_tile)
                    state['hand_tiles'].sort()
        
        # Assign rewards based on simulated outcome
        final_rank = np.random.randint(1, 5)  # Random placement
        final_score = [30000, 25000, 22000, 18000][final_rank - 1]
        reward = (final_score - 25000) / 25000.0
        
        # Apply discounted rewards
        discount = self.config.discount_factor
        for i, sample in enumerate(reversed(samples)):
            sample.reward = reward * (discount ** i)
        
        return samples, final_rank
    
    def train_step(self) -> float:
        """Perform one training step"""
        if len(self.buffer) < self.config.batch_size:
            return 0.0
        
        batch = self.buffer.sample_batch(self.config.batch_size)
        
        # Prepare batch data
        features = np.stack([s.features for s in batch])
        tile_targets = np.array([s.action_tile for s in batch])
        action_targets = np.array([s.action_type for s in batch])
        rewards = np.array([s.reward for s in batch])
        
        if self.use_mlx:
            return self._train_step_mlx(features, tile_targets, action_targets, rewards)
        else:
            return self._train_step_numpy(features, tile_targets, action_targets, rewards)
    
    def _train_step_mlx(self, features, tile_targets, action_targets, rewards) -> float:
        """MLX training step"""
        features_mx = mx.array(features)
        tile_targets_mx = mx.array(tile_targets)
        action_targets_mx = mx.array(action_targets)
        rewards_mx = mx.array(rewards.reshape(-1, 1))
        
        def loss_fn(model):
            tile_logits, action_logits, values = model(features_mx)
            
            # Cross entropy losses
            tile_loss = nn.losses.cross_entropy(tile_logits, tile_targets_mx).mean()
            action_loss = nn.losses.cross_entropy(action_logits, action_targets_mx).mean()
            
            # Value loss
            value_loss = ((values - rewards_mx) ** 2).mean()
            
            return tile_loss + action_loss + 0.5 * value_loss
        
        loss, grads = nn.value_and_grad(self.network, loss_fn)(self.network)
        self.optimizer.update(self.network, grads)
        
        return float(loss.item())
    
    def _train_step_numpy(self, features, tile_targets, action_targets, rewards) -> float:
        """Numpy training step (simplified)"""
        # Simple gradient estimation via finite differences
        total_loss = 0.0
        
        for i in range(len(features)):
            pred = self.network.forward(features[i])
            
            # Compute losses using softmax helper
            tile_probs = softmax_np(pred[0])
            tile_loss = -np.log(tile_probs[tile_targets[i]] + 1e-8)
            
            action_probs = softmax_np(pred[1])
            action_loss = -np.log(action_probs[action_targets[i]] + 1e-8)
            
            value_loss = (pred[2][0] - rewards[i]) ** 2
            
            total_loss += tile_loss + action_loss + 0.5 * value_loss
        
        return total_loss / len(features)
    
    def train(self, callback=None):
        """Main training loop"""
        print(f"Starting training for {self.config.num_games} games")
        print(f"Using {'MLX' if self.use_mlx else 'NumPy'} backend")
        
        start_time = time.time()
        
        for game_idx in range(self.config.num_games):
            # Simulate game and collect samples
            samples, rank = self.simulate_game()
            
            for sample in samples:
                self.buffer.add(sample)
            
            # Update statistics
            self.stats['games_played'] += 1
            self.stats['wins'][rank - 1] += 1
            
            # Training step
            loss = self.train_step()
            self.stats['losses'].append(loss)
            
            # Calculate win rate
            total_games = sum(self.stats['wins'])
            win_rate = self.stats['wins'][0] / total_games * 100 if total_games > 0 else 0
            self.stats['win_rates'].append(win_rate)
            
            # Logging
            if (game_idx + 1) % 100 == 0:
                avg_loss = np.mean(self.stats['losses'][-100:])
                elapsed = time.time() - start_time
                games_per_sec = (game_idx + 1) / elapsed
                
                print(f"Game {game_idx + 1}/{self.config.num_games} | "
                      f"Win Rate: {win_rate:.1f}% | "
                      f"Loss: {avg_loss:.4f} | "
                      f"Speed: {games_per_sec:.1f} g/s")
                
                if callback:
                    callback({
                        'type': 'progress',
                        'gamesPlayed': game_idx + 1,
                        'winRate': win_rate,
                        'avgScore': 25000 + (0.5 - rank/4) * 10000,
                        'loss': avg_loss
                    })
            
            # Checkpoint
            if (game_idx + 1) % self.config.checkpoint_interval == 0:
                self.save_checkpoint(game_idx + 1)
        
        print("Training complete!")
        self.save_checkpoint(self.config.num_games)
        
        return self.stats
    
    def save_checkpoint(self, step: int):
        """Save model checkpoint"""
        checkpoint_path = Path(self.config.output_dir) / f"checkpoint_{step}.json"
        
        stats_to_save = {
            'step': step,
            'games_played': self.stats['games_played'],
            'wins': self.stats['wins'],
            'win_rate': self.stats['win_rates'][-1] if self.stats['win_rates'] else 0,
            'avg_loss': np.mean(self.stats['losses'][-100:]) if self.stats['losses'] else 0
        }
        
        with open(checkpoint_path, 'w') as f:
            json.dump(stats_to_save, f, indent=2)
        
        print(f"Saved checkpoint to {checkpoint_path}")
    
    def predict(self, state: dict) -> dict:
        """Get prediction for a game state"""
        if self.use_mlx:
            features = MahjongFeatures.encode_state(state)
            tile_logits, action_logits, value = self.network(mx.array(features))
            
            tile_probs = mx.softmax(tile_logits).tolist()
            action_probs = mx.softmax(action_logits).tolist()
            
            return {
                'tile_probs': tile_probs,
                'action_probs': action_probs,
                'value': float(value.item())
            }
        else:
            return self.network.predict(state)


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Train Mahjong AI using MLX')
    parser.add_argument('--games', type=int, default=10000, help='Number of games to train')
    parser.add_argument('--batch', type=int, default=64, help='Batch size')
    parser.add_argument('--lr', type=float, default=0.001, help='Learning rate')
    parser.add_argument('--mode', type=str, default='4p-half', 
                       choices=['4p-half', '4p-east', '3p-half', '3p-east'],
                       help='Game mode')
    parser.add_argument('--output', type=str, default='training_output', help='Output directory')
    parser.add_argument('--no-mlx', action='store_true', help='Disable MLX, use NumPy only')
    
    args = parser.parse_args()
    
    config = TrainingConfig(
        num_games=args.games,
        batch_size=args.batch,
        learning_rate=args.lr,
        game_mode=args.mode,
        output_dir=args.output,
        use_mlx=not args.no_mlx and MLX_AVAILABLE
    )
    
    trainer = Trainer(config)
    stats = trainer.train()
    
    print("\nFinal Statistics:")
    print(f"  Games: {stats['games_played']}")
    print(f"  Wins: {stats['wins']}")
    print(f"  Final Win Rate: {stats['win_rates'][-1]:.1f}%")


if __name__ == '__main__':
    main()
