// Mahjong AI Web Application

// Constants
const TOTAL_TILES = 136;           // Total tiles in a mahjong set
const HAND_SIZE = 13;              // Starting hand size
const NUM_PLAYERS = 4;             // Number of players
const STARTING_POINTS = 25000;     // Starting points per player
const RED_DORA_MAN5 = 16;          // Red Man 5 tile code
const RED_DORA_PIN5 = 52;          // Red Pin 5 tile code  
const RED_DORA_SOU5 = 88;          // Red Sou 5 tile code

// State management
const state = {
    training: {
        isRunning: false,
        gamesPlayed: 0,
        winRate: 0,
        avgScore: 0,
        loss: 0,
        history: []
    },
    game: {
        isPlaying: false,
        hand: [],
        selectedTile: null,
        river: [],
        opponents: [{}, {}, {}],
        roundWind: 'East',
        roundNum: 1,
        honba: 0,
        points: [STARTING_POINTS, STARTING_POINTS, STARTING_POINTS, STARTING_POINTS],
        canActions: {
            tsumo: false,
            ron: false,
            riichi: false,
            chi: false,
            pon: false,
            kan: false
        }
    },
    tenhou: {
        connected: false,
        gameInProgress: false
    }
};

// WebSocket connection
let ws = null;
let trainingWs = null;

// Page navigation
function showPage(pageName) {
    document.querySelectorAll('.page').forEach(p => p.classList.remove('active'));
    document.querySelectorAll('.nav-btn').forEach(b => b.classList.remove('active'));
    
    document.getElementById(`${pageName}-page`).classList.add('active');
    document.getElementById(`nav-${pageName}`).classList.add('active');
}

// Tile utilities
const TILE_NAMES = {
    'm': ['1m', '2m', '3m', '4m', '5m', '6m', '7m', '8m', '9m'],
    'p': ['1p', '2p', '3p', '4p', '5p', '6p', '7p', '8p', '9p'],
    's': ['1s', '2s', '3s', '4s', '5s', '6s', '7s', '8s', '9s'],
    'z': ['1z', '2z', '3z', '4z', '5z', '6z', '7z']
};

function tenhouCodeToTile(code) {
    if (code < 36) {
        return TILE_NAMES['m'][Math.floor(code / 4)];
    } else if (code < 72) {
        return TILE_NAMES['p'][Math.floor((code - 36) / 4)];
    } else if (code < 108) {
        return TILE_NAMES['s'][Math.floor((code - 72) / 4)];
    } else {
        return TILE_NAMES['z'][Math.floor((code - 108) / 4)];
    }
}

function createTileElement(tileCode, isBack = false) {
    const tile = document.createElement('div');
    tile.className = 'tile' + (isBack ? ' back' : '');
    if (!isBack) {
        const tileName = typeof tileCode === 'number' ? tenhouCodeToTile(tileCode) : tileCode;
        tile.setAttribute('data-tile', tileName);
        tile.dataset.code = tileCode;
        
        // Check for red dora
        if (tileCode === RED_DORA_MAN5 || tileCode === RED_DORA_PIN5 || tileCode === RED_DORA_SOU5) {
            tile.classList.add('red');
        }
    }
    return tile;
}

// Training functions
function startTraining() {
    const config = {
        numGames: parseInt(document.getElementById('num-games').value),
        batchSize: parseInt(document.getElementById('batch-size').value),
        learningRate: parseFloat(document.getElementById('learning-rate').value),
        gameMode: document.getElementById('game-mode').value,
        useMlx: document.getElementById('use-mlx').checked
    };
    
    state.training.isRunning = true;
    addTrainingLog('Starting training with configuration:', 'info');
    addTrainingLog(`  Games: ${config.numGames}, Batch: ${config.batchSize}, LR: ${config.learningRate}`, 'info');
    addTrainingLog(`  Mode: ${config.gameMode}, MLX: ${config.useMlx}`, 'info');
    
    // Connect to training WebSocket
    try {
        trainingWs = new WebSocket(`ws://${window.location.host}/ws/training`);
        
        trainingWs.onopen = () => {
            addTrainingLog('Connected to training server', 'info');
            trainingWs.send(JSON.stringify({ type: 'start', config }));
        };
        
        trainingWs.onmessage = (event) => {
            const data = JSON.parse(event.data);
            handleTrainingMessage(data);
        };
        
        trainingWs.onerror = (error) => {
            addTrainingLog(`WebSocket error: ${error}`, 'error');
            // Fallback to simulation mode
            simulateTraining(config);
        };
        
        trainingWs.onclose = () => {
            addTrainingLog('Training connection closed', 'info');
            state.training.isRunning = false;
        };
    } catch (e) {
        // Fallback to simulation mode if WebSocket fails
        simulateTraining(config);
    }
}

function simulateTraining(config) {
    addTrainingLog('Running in simulation mode (no server connection)', 'warning');
    
    let gamesComplete = 0;
    const interval = setInterval(() => {
        if (!state.training.isRunning || gamesComplete >= config.numGames) {
            clearInterval(interval);
            state.training.isRunning = false;
            addTrainingLog('Training complete!', 'info');
            return;
        }
        
        // Simulate training progress
        gamesComplete += config.batchSize;
        const winRate = 25 + Math.random() * 10 + (gamesComplete / config.numGames) * 5;
        const avgScore = 24000 + Math.random() * 2000 + (gamesComplete / config.numGames) * 1000;
        const loss = 1.0 - (gamesComplete / config.numGames) * 0.5 + Math.random() * 0.1;
        
        updateTrainingStats({
            gamesPlayed: gamesComplete,
            winRate: winRate,
            avgScore: avgScore,
            loss: loss
        });
        
        if (gamesComplete % 100 === 0) {
            addTrainingLog(`Games: ${gamesComplete}, Win Rate: ${winRate.toFixed(1)}%`, 'info');
        }
    }, 100);
}

function stopTraining() {
    state.training.isRunning = false;
    if (trainingWs) {
        trainingWs.send(JSON.stringify({ type: 'stop' }));
        trainingWs.close();
    }
    addTrainingLog('Training stopped', 'warning');
}

function handleTrainingMessage(data) {
    switch (data.type) {
        case 'progress':
            updateTrainingStats(data);
            break;
        case 'log':
            addTrainingLog(data.message, data.level || 'info');
            break;
        case 'complete':
            state.training.isRunning = false;
            addTrainingLog('Training complete!', 'info');
            break;
        case 'error':
            addTrainingLog(data.message, 'error');
            break;
    }
}

function updateTrainingStats(stats) {
    state.training.gamesPlayed = stats.gamesPlayed;
    state.training.winRate = stats.winRate;
    state.training.avgScore = stats.avgScore;
    state.training.loss = stats.loss;
    
    document.getElementById('games-played').textContent = stats.gamesPlayed;
    document.getElementById('win-rate').textContent = `${stats.winRate.toFixed(1)}%`;
    document.getElementById('avg-score').textContent = Math.round(stats.avgScore);
    document.getElementById('training-loss').textContent = stats.loss.toFixed(4);
    
    // Update chart
    state.training.history.push(stats);
    updateChart();
}

function addTrainingLog(message, level = 'info') {
    const logContainer = document.getElementById('training-log');
    const entry = document.createElement('div');
    entry.className = `log-entry ${level}`;
    entry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
    logContainer.appendChild(entry);
    logContainer.scrollTop = logContainer.scrollHeight;
}

// Chart setup
let chart = null;

function updateChart() {
    const canvas = document.getElementById('training-chart');
    const ctx = canvas.getContext('2d');
    
    const history = state.training.history;
    if (history.length === 0) return;
    
    // Simple line chart implementation
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    
    const padding = 40;
    const width = canvas.width - padding * 2;
    const height = canvas.height - padding * 2;
    
    // Draw axes
    ctx.strokeStyle = '#4a5568';
    ctx.beginPath();
    ctx.moveTo(padding, padding);
    ctx.lineTo(padding, height + padding);
    ctx.lineTo(width + padding, height + padding);
    ctx.stroke();
    
    // Draw win rate line
    if (history.length > 1) {
        ctx.strokeStyle = '#4ade80';
        ctx.lineWidth = 2;
        ctx.beginPath();
        
        const xStep = width / (history.length - 1);
        const maxWinRate = 50;
        
        history.forEach((point, i) => {
            const x = padding + i * xStep;
            const y = height + padding - (point.winRate / maxWinRate) * height;
            
            if (i === 0) {
                ctx.moveTo(x, y);
            } else {
                ctx.lineTo(x, y);
            }
        });
        
        ctx.stroke();
    }
    
    // Draw loss line
    if (history.length > 1) {
        ctx.strokeStyle = '#f87171';
        ctx.lineWidth = 2;
        ctx.beginPath();
        
        const xStep = width / (history.length - 1);
        
        history.forEach((point, i) => {
            const x = padding + i * xStep;
            const y = height + padding - (1 - point.loss) * height;
            
            if (i === 0) {
                ctx.moveTo(x, y);
            } else {
                ctx.lineTo(x, y);
            }
        });
        
        ctx.stroke();
    }
    
    // Legend
    ctx.fillStyle = '#4ade80';
    ctx.fillRect(width - 100, 10, 15, 15);
    ctx.fillStyle = '#e8e8e8';
    ctx.font = '12px sans-serif';
    ctx.fillText('Win Rate', width - 80, 22);
    
    ctx.fillStyle = '#f87171';
    ctx.fillRect(width - 100, 30, 15, 15);
    ctx.fillStyle = '#e8e8e8';
    ctx.fillText('1 - Loss', width - 80, 42);
}

// Game functions
function newGame() {
    state.game.isPlaying = true;
    state.game.hand = [];
    state.game.river = [];
    state.game.points = [STARTING_POINTS, STARTING_POINTS, STARTING_POINTS, STARTING_POINTS];
    
    // Generate random starting hand
    const tiles = [];
    for (let i = 0; i < HAND_SIZE; i++) {
        tiles.push(Math.floor(Math.random() * TOTAL_TILES));
    }
    state.game.hand = tiles.sort((a, b) => a - b);
    
    renderGame();
}

function renderGame() {
    // Render player hand
    const handContainer = document.getElementById('self-hand');
    handContainer.innerHTML = '';
    
    state.game.hand.forEach((tileCode, index) => {
        const tile = createTileElement(tileCode);
        tile.addEventListener('click', () => selectTile(index));
        if (state.game.selectedTile === index) {
            tile.classList.add('selected');
        }
        handContainer.appendChild(tile);
    });
    
    // Render opponent hands (hidden)
    ['top', 'left', 'right'].forEach((pos, i) => {
        const container = document.getElementById(`opp-${pos}-hand`);
        container.innerHTML = '';
        for (let j = 0; j < HAND_SIZE; j++) {
            container.appendChild(createTileElement(0, true));
        }
    });
    
    // Update points
    document.getElementById('self-points').textContent = state.game.points[0];
    document.getElementById('opp-top-points').textContent = state.game.points[2];
    document.getElementById('opp-left-points').textContent = state.game.points[3];
    document.getElementById('opp-right-points').textContent = state.game.points[1];
}

function selectTile(index) {
    if (state.game.selectedTile === index) {
        // Discard selected tile
        discardTile(index);
    } else {
        state.game.selectedTile = index;
        renderGame();
    }
}

function discardTile(index) {
    const tile = state.game.hand.splice(index, 1)[0];
    state.game.river.push(tile);
    state.game.selectedTile = null;
    
    // Add river tile
    const riverContainer = document.getElementById('self-river');
    riverContainer.appendChild(createTileElement(tile));
    
    // Draw new tile (simulate)
    if (state.game.isPlaying) {
        setTimeout(() => {
            const newTile = Math.floor(Math.random() * TOTAL_TILES);
            state.game.hand.push(newTile);
            state.game.hand.sort((a, b) => a - b);
            renderGame();
        }, 500);
    }
}

function gameAction(action) {
    console.log('Game action:', action);
    // TODO: Implement game actions
    switch (action) {
        case 'tsumo':
            alert('ツモ！');
            break;
        case 'ron':
            alert('ロン！');
            break;
        case 'riichi':
            alert('リーチ！');
            break;
        case 'chi':
            alert('チー');
            break;
        case 'pon':
            alert('ポン');
            break;
        case 'kan':
            alert('カン');
            break;
        case 'pass':
            // Do nothing
            break;
    }
}

// Tenhou functions
function connectTenhou() {
    const config = {
        username: document.getElementById('tenhou-username').value || 'NoName',
        auth: document.getElementById('tenhou-auth').value,
        lobby: parseInt(document.getElementById('tenhou-lobby').value),
        gameType: parseInt(document.getElementById('tenhou-game-type').value),
        strategy: document.getElementById('tenhou-strategy').value
    };
    
    addTenhouLog('Connecting to Tenhou...', 'info');
    
    try {
        ws = new WebSocket(`ws://${window.location.host}/ws/tenhou`);
        
        ws.onopen = () => {
            ws.send(JSON.stringify({ type: 'connect', config }));
            updateTenhouStatus(true);
            addTenhouLog('Connected! Waiting for game...', 'info');
        };
        
        ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            handleTenhouMessage(data);
        };
        
        ws.onerror = (error) => {
            addTenhouLog(`Connection error: ${error}`, 'error');
            updateTenhouStatus(false);
        };
        
        ws.onclose = () => {
            addTenhouLog('Disconnected from Tenhou', 'info');
            updateTenhouStatus(false);
        };
    } catch (e) {
        addTenhouLog('WebSocket not available - server not running', 'error');
    }
}

function disconnectTenhou() {
    if (ws) {
        ws.send(JSON.stringify({ type: 'disconnect' }));
        ws.close();
    }
    updateTenhouStatus(false);
}

function handleTenhouMessage(data) {
    switch (data.type) {
        case 'connected':
            addTenhouLog('Successfully logged in to Tenhou', 'info');
            break;
        case 'game_start':
            addTenhouLog(`Game started! Players: ${data.players.join(', ')}`, 'info');
            state.tenhou.gameInProgress = true;
            break;
        case 'round_start':
            addTenhouLog(`Round ${data.round}: ${data.wind}${data.num}局 ${data.honba}本場`, 'info');
            break;
        case 'draw':
            addTenhouLog(`Drew: ${tenhouCodeToTile(data.tile)}`, 'info');
            break;
        case 'discard':
            addTenhouLog(`P${data.player} discarded: ${tenhouCodeToTile(data.tile)}`, 'info');
            break;
        case 'win':
            addTenhouLog(`Win! ${data.winner} ${data.type}`, 'info');
            break;
        case 'error':
            addTenhouLog(data.message, 'error');
            break;
        case 'log':
            addTenhouLog(data.message, data.level || 'info');
            break;
    }
    
    // Update game info panel
    if (data.gameInfo) {
        document.getElementById('tenhou-game-info').innerHTML = `
            <p>Round: ${data.gameInfo.round}</p>
            <p>Seat: ${data.gameInfo.seat}</p>
            <p>Points: ${data.gameInfo.points.join(' / ')}</p>
        `;
    }
}

function updateTenhouStatus(connected) {
    state.tenhou.connected = connected;
    const statusEl = document.getElementById('tenhou-status');
    const dot = statusEl.querySelector('.status-dot');
    const text = statusEl.querySelector('span:last-child');
    
    dot.className = `status-dot ${connected ? 'connected' : 'disconnected'}`;
    text.textContent = connected ? 'Connected' : 'Disconnected';
    
    document.getElementById('tenhou-connect').disabled = connected;
    document.getElementById('tenhou-disconnect').disabled = !connected;
}

function addTenhouLog(message, level = 'info') {
    const logContainer = document.getElementById('tenhou-log');
    const entry = document.createElement('div');
    entry.className = `log-entry ${level}`;
    entry.textContent = `[${new Date().toLocaleTimeString()}] ${message}`;
    logContainer.appendChild(entry);
    logContainer.scrollTop = logContainer.scrollHeight;
}

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    // Set up chart canvas
    const canvas = document.getElementById('training-chart');
    canvas.width = canvas.offsetWidth;
    canvas.height = canvas.offsetHeight;
    
    // Initialize with some demo tiles in hand
    newGame();
});

// Resize handler for chart
window.addEventListener('resize', () => {
    const canvas = document.getElementById('training-chart');
    canvas.width = canvas.offsetWidth;
    canvas.height = canvas.offsetHeight;
    updateChart();
});
