'use strict';

//Set up express
const express = require('express');
const app = express();

//Setup socket.io
const server = require('http').Server(app);
const io = require('socket.io')(server);

let players = new Map();
let playersToSockets = new Map();
let socketsToPlayers = new Map();

let nextPlayerNumber = 0;
let lastPlayer = null;
let state = {state: 0, light: null, countdown: 90};
let timer = null;

//Setup static page handling
app.set('view engine', 'ejs');
app.use('/static', express.static('public'));

//Handle client interface on /
app.get('/', (req, res) => {
  res.render('client');
});

//Start the server
function startServer() {
    const PORT = process.env.PORT || 8080;
    server.listen(PORT, () => {
        console.log(`Server listening on port ${PORT}`);
    });
}

//Chat message
function handleChat(player, message) {
    console.log('Handling chat: ' + message); 
    io.emit('chat',message);
}

// Handle join
function handleJoin(socket) {
  
  if (state.state > 0) {
    error(socket, 'The game has already started.', true);
    return ;
  }

  nextPlayerNumber++;
  console.log('Welcome to player ' + nextPlayerNumber);
  announce('Welcome player ' + nextPlayerNumber);

  players.set(nextPlayerNumber, { name: nextPlayerNumber, state: 1, score: 0 });
  playersToSockets.set(nextPlayerNumber, socket);
  socketsToPlayers.set(socket, nextPlayerNumber);
}

// Handle quit
function handleQuit(socket) {
  
  if (!socketsToPlayers.has(socket)) {
    console.log('Handling quit');
    return ;
  }

  const player = socketsToPlayers.get(socket);
  socketsToPlayers.delete(socket);
  playersToSockets.delete(player);
  killPlayer(player);

  console.log('Handling quit from player ' + player);
  announce('Goodbye player ' + player);
}

function killPlayer(player) {
  console.log('Killing player ' + player);
  announce('Player ' + player + ' eliminated');
  const thePlayer = players.get(player);
  thePlayer.state = -1;
}

// Handle admin action
function handleAdmin(player, action) {
  if (player != 1) {
    console.log('Failed admin action from player ' + player + ' for ' + action);
    return ;
  }

  if (action == 'start' && state.state == 0) {
    startGame();
  } else if (action == 'light' && state.state == 1) {
    gameLight();
  } else {
    console.log('Unknown admin action ' + action);
  }
}

function startGame() {
  console.log('Game starting');
  announce('Let the game begin');

  for (const [playerNumber, player] of players) {
    player.state = 0;
  }

  console.log('Starting the timer: ' + state.countdown);
  timer = setInterval(() => {
    tickGame();
  }, 1000);

  state.state = 1;

  state.light = 'red';
}

function endGame() {
  state.state = 2;
  console.log('Game ending');
  for (let [playerNumber, player] of players) {
    if (player.score < 100) {
      killPlayer(playerNumber);
    }
  }
}

function tickGame() {
  if (state.countdown > 1) {
    state.countdown--;
    console.log('Tick ' + state.countdown);
  } else {
    clearInterval(timer);
    timer = null;
    endGame();
  }
  updateAll();
}

function gameLight() {
  if (state.light != 'green')
    state.light = 'green';
  else
    state.light = 'red';

  announce(state.light.charAt(0).toUpperCase() + state.light.slice(1) + ' light!');
}

// Handle player action
function handleAction(player, action) {
  if (state.state != 1) return;

  const thePlayer = players.get(player);
  if (thePlayer.state != 0) return;

  console.log('Handling action: ' + action + ' from player ' + player);
  if (state.light == 'red') {
    killPlayer(player);
  } else if (lastPlayer == player) {
    tripPlayer(player);
  } else {
    advancePlayer(player);
  }

  lastPlayer = player;
}

function tripPlayer(player) {
  announce('Player ' + player + ' tripped over!');
  const thePlayer = players.get(player);
  thePlayer.score = 0;
}

function advancePlayer(player) {
  const thePlayer = players.get(player);

  if (thePlayer.state > 0) {
    error(playersToSockets.get(player), 'You have already finished!');
    return ;
  }

  thePlayer.score += 1;
  announce('Player ' + player + ' moved forwards');
  console.log('Player ' + player + ' is not at ' + thePlayer.score + ' (+1)');
  if (thePlayer.score >= 100) {
    announce('Player ' + player + ' is safe!');
    thePlayer.state = 1;
  }
}

function updateAll() {
  console.log('Updating all players');
  for (let [playerNumber, socket] of playersToSockets) {
    updatePlayer(socket);
  }
}

function updatePlayer(socket) {
  const playerNumber = socketsToPlayers.get(socket);
  const thePlayer = players.get(playerNumber);
  const data = {state: state, me: thePlayer, players: Object.fromEntries(players) };
  socket.emit('state', data);
}

function error(socket, message, halt) {
  console.log('Error: ' + message);
  socket.emit('fail', message);
  if (halt) {
    socket.disconnect();
  }
}

function announce(message) {
  console.log('Announcement: ' + message);
  io.emit('chat', message);
}

//Handle new connection
io.on('connection', socket => { 
  console.log('New connection');

  //Handle on chat message received
  socket.on('chat', message => {
    if (!socketsToPlayers.has(socket)) return;
    handleChat(socketsToPlayers.get(socket), message);
  });

  //Handle disconnection
  socket.on('disconnect', () => {
    console.log('Dropped connection');
  });

  // Handle join
  socket.on('join', () => {
    if (socketsToPlayers.has(socket)) return;
    handleJoin(socket);
    updateAll();
  });

  // Handle admin commands
  socket.on('admin', action => {
    if (!socketsToPlayers.has(socket)) return;
    handleAdmin(socketsToPlayers.get(socket), action);
    updateAll();
  });
  
  // Handle player actions
  socket.on('action', action => {
    if (!socketsToPlayers.has(socket)) return;
    handleAction(socketsToPlayers.get(socket), action);
    updateAll();
  });
});

//Start server
if (module === require.main) {
  startServer();
}

module.exports = server;
