"""
Mahjong AI Web Server

Provides web interface for:
- Training visualization
- Human vs AI gameplay
- Tenhou bot control

Requirements:
    pip install aiohttp numpy

Usage:
    python -m web.server --port 8080
"""

import asyncio
import json
import os
import subprocess
import sys
from pathlib import Path
from typing import Optional

try:
    from aiohttp import web
    AIOHTTP_AVAILABLE = True
except ImportError:
    AIOHTTP_AVAILABLE = False
    print("aiohttp not available. Install with: pip install aiohttp")

# Game constants
TOTAL_TILES = 136          # Total tiles in a mahjong set
HAND_SIZE = 13             # Starting hand size
NUM_PLAYERS = 4            # Number of players
STARTING_POINTS = 25000    # Starting points per player
TOTAL_DEALT_CARDS = HAND_SIZE * NUM_PLAYERS  # Total cards dealt at start

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    from mlx_training.train import Trainer, TrainingConfig, MahjongFeatures
    TRAINING_AVAILABLE = True
except ImportError:
    TRAINING_AVAILABLE = False
    print("Training module not available")


class WebServer:
    """Main web server"""
    
    def __init__(self, host: str = "0.0.0.0", port: int = 8080):
        self.host = host
        self.port = port
        self.app = web.Application() if AIOHTTP_AVAILABLE else None
        self.trainer: Optional[Trainer] = None
        self.training_task: Optional[asyncio.Task] = None
        self.ws_clients = set()
        
        if self.app:
            self.setup_routes()
    
    def setup_routes(self):
        """Set up HTTP and WebSocket routes"""
        self.app.router.add_get('/', self.handle_index)
        self.app.router.add_static('/static', Path(__file__).parent / 'static')
        self.app.router.add_get('/ws/training', self.handle_training_ws)
        self.app.router.add_get('/ws/game', self.handle_game_ws)
        self.app.router.add_get('/ws/tenhou', self.handle_tenhou_ws)
        
        # API routes
        self.app.router.add_get('/api/status', self.handle_status)
        self.app.router.add_post('/api/predict', self.handle_predict)
    
    async def handle_index(self, request):
        """Serve main HTML page"""
        html_path = Path(__file__).parent / 'templates' / 'index.html'
        return web.FileResponse(html_path)
    
    async def handle_status(self, request):
        """Return server status"""
        return web.json_response({
            'status': 'ok',
            'training_available': TRAINING_AVAILABLE,
            'training_active': self.training_task is not None and not self.training_task.done()
        })
    
    async def handle_predict(self, request):
        """Get AI prediction for game state"""
        try:
            data = await request.json()
            
            if not self.trainer:
                self.trainer = Trainer(TrainingConfig())
            
            prediction = self.trainer.predict(data)
            return web.json_response(prediction)
            
        except Exception as e:
            return web.json_response({'error': str(e)}, status=500)
    
    async def handle_training_ws(self, request):
        """WebSocket handler for training"""
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        
        self.ws_clients.add(ws)
        
        try:
            async for msg in ws:
                if msg.type == web.WSMsgType.TEXT:
                    data = json.loads(msg.data)
                    await self.handle_training_message(ws, data)
                elif msg.type == web.WSMsgType.ERROR:
                    print(f"WebSocket error: {ws.exception()}")
        finally:
            self.ws_clients.discard(ws)
        
        return ws
    
    async def handle_training_message(self, ws, data):
        """Handle training WebSocket messages"""
        msg_type = data.get('type')
        
        if msg_type == 'start':
            config_data = data.get('config', {})
            
            config = TrainingConfig(
                num_games=config_data.get('numGames', 10000),
                batch_size=config_data.get('batchSize', 64),
                learning_rate=config_data.get('learningRate', 0.001),
                game_mode=config_data.get('gameMode', '4p-half'),
                use_mlx=config_data.get('useMlx', True)
            )
            
            # Start training in background
            self.training_task = asyncio.create_task(
                self.run_training(ws, config)
            )
            
        elif msg_type == 'stop':
            if self.training_task:
                self.training_task.cancel()
                self.training_task = None
                await ws.send_json({'type': 'log', 'message': 'Training stopped', 'level': 'warning'})
    
    async def run_training(self, ws, config: TrainingConfig):
        """Run training with progress updates"""
        try:
            await ws.send_json({'type': 'log', 'message': 'Initializing training...', 'level': 'info'})
            
            self.trainer = Trainer(config)
            
            def progress_callback(data):
                # Schedule sending message on event loop
                asyncio.get_event_loop().call_soon_threadsafe(
                    lambda: asyncio.create_task(ws.send_json(data))
                )
            
            # Run training
            await ws.send_json({'type': 'log', 'message': 'Training started', 'level': 'info'})
            
            # Run in executor to not block
            loop = asyncio.get_event_loop()
            stats = await loop.run_in_executor(
                None,
                lambda: self.trainer.train(callback=progress_callback)
            )
            
            await ws.send_json({
                'type': 'complete',
                'stats': {
                    'gamesPlayed': stats['games_played'],
                    'wins': stats['wins'],
                    'winRate': stats['win_rates'][-1] if stats['win_rates'] else 0
                }
            })
            
        except asyncio.CancelledError:
            await ws.send_json({'type': 'log', 'message': 'Training cancelled', 'level': 'warning'})
        except Exception as e:
            await ws.send_json({'type': 'error', 'message': str(e)})
    
    async def handle_game_ws(self, request):
        """WebSocket handler for game play"""
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        
        game_state = self.create_new_game()
        await ws.send_json({'type': 'game_state', 'state': game_state})
        
        try:
            async for msg in ws:
                if msg.type == web.WSMsgType.TEXT:
                    data = json.loads(msg.data)
                    response = await self.handle_game_message(game_state, data)
                    await ws.send_json(response)
        finally:
            pass
        
        return ws
    
    def create_new_game(self):
        """Create new game state"""
        import random
        
        # Generate random tiles
        all_tiles = list(range(TOTAL_TILES))
        random.shuffle(all_tiles)
        
        # Deal hands
        hands = [sorted(all_tiles[i*HAND_SIZE:(i+1)*HAND_SIZE]) for i in range(NUM_PLAYERS)]
        wall = all_tiles[TOTAL_DEALT_CARDS:]
        
        return {
            'hands': hands,
            'wall': wall,
            'wall_index': 0,
            'discards': [[] for _ in range(NUM_PLAYERS)],
            'current_player': 0,
            'round_wind': 0,
            'round_num': 1,
            'honba': 0,
            'riichi_sticks': 0,
            'points': [STARTING_POINTS] * NUM_PLAYERS,
            'riichi_status': [False] * NUM_PLAYERS,
            'dora_indicators': [wall[0]]
        }
    
    async def handle_game_message(self, game_state, data):
        """Handle game action"""
        action = data.get('action')
        
        if action == 'new_game':
            return {'type': 'game_state', 'state': self.create_new_game()}
        
        elif action == 'discard':
            tile_idx = data.get('tile_idx', 0)
            player = game_state['current_player']
            
            if player == 0:  # Human player
                if tile_idx < len(game_state['hands'][0]):
                    tile = game_state['hands'][0].pop(tile_idx)
                    game_state['discards'][0].append(tile)
                    
                    # AI turns
                    await self.ai_turns(game_state)
                    
                    # Draw for human
                    if game_state['wall_index'] < len(game_state['wall']):
                        new_tile = game_state['wall'][game_state['wall_index']]
                        game_state['wall_index'] += 1
                        game_state['hands'][0].append(new_tile)
                        game_state['hands'][0].sort()
            
            return {'type': 'game_state', 'state': game_state}
        
        elif action == 'get_ai_suggestion':
            if self.trainer:
                state = {
                    'hand_tiles': game_state['hands'][0],
                    'discards': game_state['discards'],
                    'round_wind': game_state['round_wind'],
                    'seat': 0,
                    'points': game_state['points'],
                    'riichi_status': game_state['riichi_status']
                }
                prediction = self.trainer.predict(state)
                return {'type': 'suggestion', 'prediction': prediction}
            return {'type': 'error', 'message': 'AI not initialized'}
        
        return {'type': 'error', 'message': 'Unknown action'}
    
    async def ai_turns(self, game_state):
        """Process AI player turns"""
        import random
        
        for player_idx in [1, 2, 3]:  # AI players
            if game_state['wall_index'] >= len(game_state['wall']):
                break
            
            # Draw
            new_tile = game_state['wall'][game_state['wall_index']]
            game_state['wall_index'] += 1
            game_state['hands'][player_idx].append(new_tile)
            
            # Discard (AI logic)
            if self.trainer:
                state = {
                    'hand_tiles': game_state['hands'][player_idx],
                    'discards': game_state['discards'],
                    'round_wind': game_state['round_wind'],
                    'seat': player_idx,
                    'points': game_state['points'],
                    'riichi_status': game_state['riichi_status']
                }
                prediction = self.trainer.predict(state)
                
                # Find best tile to discard based on prediction
                hand = game_state['hands'][player_idx]
                tile_probs = prediction['tile_probs']
                
                best_idx = 0
                best_score = -1
                for i, tile in enumerate(hand):
                    tile_type = tile // 4
                    if tile_type < len(tile_probs) and tile_probs[tile_type] > best_score:
                        best_score = tile_probs[tile_type]
                        best_idx = i
                
                discard_idx = best_idx
            else:
                # Random discard
                discard_idx = random.randint(0, len(game_state['hands'][player_idx]) - 1)
            
            tile = game_state['hands'][player_idx].pop(discard_idx)
            game_state['discards'][player_idx].append(tile)
            
            await asyncio.sleep(0.3)  # Add slight delay for visual effect
    
    async def handle_tenhou_ws(self, request):
        """WebSocket handler for Tenhou bot"""
        ws = web.WebSocketResponse()
        await ws.prepare(request)
        
        tenhou_process = None
        
        try:
            async for msg in ws:
                if msg.type == web.WSMsgType.TEXT:
                    data = json.loads(msg.data)
                    msg_type = data.get('type')
                    
                    if msg_type == 'connect':
                        config = data.get('config', {})
                        await ws.send_json({
                            'type': 'log',
                            'message': f"Connecting to Tenhou as {config.get('username', 'NoName')}...",
                            'level': 'info'
                        })
                        
                        # Note: Actual Tenhou connection would require OCaml binary
                        # This is a placeholder for the web interface
                        await ws.send_json({
                            'type': 'log',
                            'message': 'Tenhou connection requires running the OCaml bot binary',
                            'level': 'warning'
                        })
                        
                    elif msg_type == 'disconnect':
                        if tenhou_process:
                            tenhou_process.terminate()
                            tenhou_process = None
                        await ws.send_json({
                            'type': 'log',
                            'message': 'Disconnected from Tenhou',
                            'level': 'info'
                        })
        finally:
            if tenhou_process:
                tenhou_process.terminate()
        
        return ws
    
    def run(self):
        """Start the web server"""
        if not AIOHTTP_AVAILABLE:
            print("Cannot start server: aiohttp not available")
            return
        
        print(f"Starting Mahjong AI Web Server on http://{self.host}:{self.port}")
        print("Press Ctrl+C to stop")
        
        web.run_app(self.app, host=self.host, port=self.port)


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description='Mahjong AI Web Server')
    parser.add_argument('--host', type=str, default='0.0.0.0', help='Host to bind')
    parser.add_argument('--port', type=int, default=8080, help='Port to listen on')
    
    args = parser.parse_args()
    
    server = WebServer(host=args.host, port=args.port)
    server.run()


if __name__ == '__main__':
    main()
