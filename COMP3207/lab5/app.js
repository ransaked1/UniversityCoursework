'use strict';


const express = require('express');

const app = express();

const server = require('http').Server(app);
const io = require('socket.io')(server);

let players = new Map();
let playersToSockets = new Map();
let socketsToPlayers = new Map();
let nextPlayerNumber = 0;
let lastPlayer = null;
let state = { state: 0, light: null, countdown: 90 };
let timer = null;

//Handle static pages
app.set('view engine', 'ejs');
app.use('/static', express.static('public'));
app.get('/', (req, res) => {
  res.render('index');
});
app.get('/reset', (req, res) => {
  resetGame();
  res.render('index');
});

//Reset the state
function resetGame() {
    console.log('Resetting game');
    state = { state: 0, light: null, countdown: 90 };
    nextPlayerNumber = 0;
    lastPlayer = null;
    for(let [playerNumber,socket] of playersToSockets) {
        socket.disconnect();    
    }
    if(timer) {
        clearInterval(timer);
        timer = null;
    }
    players = new Map();
    playersToSockets = new Map();
    socketsToPlayers = new Map();
}

//Start the server
function startServer() {
    const PORT = process.env.PORT || 8080;
    server.listen(PORT, () => {
        console.log(`Server listening on port ${PORT}`);
    });
}

//Handle errors
function error(socket, message, halt) {
    console.log('Error: ' + message);
    socket.emit('fail',message);
    if(halt) {
        socket.disconnect();
    }
}

//Handle announcements
function announce(message) {
    console.log('Announcement: ' + message);
    io.emit('chat',message);
}

//Handle joins
function handleJoin(socket) {

    //Can only join the game before it has started
    if(state.state > 0) {
        error(socket,'The game has already started',true);
        return;
    }

    //Start new player
    nextPlayerNumber++;
    console.log('Welcome to player ' + nextPlayerNumber);
    announce('Welcome player ' + nextPlayerNumber);

    players.set(nextPlayerNumber,{ name: nextPlayerNumber, state: 1, score: 0 });
    playersToSockets.set(nextPlayerNumber,socket);
    socketsToPlayers.set(socket,nextPlayerNumber);
}

//Exterminate player
function killPlayer(player) {
    console.log('Kill player ' + player);
    announce("Player " + player + " eliminated");
    const thePlayer = players.get(player);
    thePlayer.state = -1;
}

//Player is too quick and falls
function tripPlayer(player) {
    announce("Player " + player + " tripped over!");
    const thePlayer = players.get(player);
    thePlayer.score = 0;
}

//Player moves forward
function advancePlayer(player) {
    const thePlayer = players.get(player);
    if(thePlayer.state > 0) {
        error(playersToSockets.get(player),"You have already finished!");
        return;
    }
    thePlayer.score += 1;
    announce("Player " + player + " moved fowards");
    console.log("Player " + player + " is now at " + thePlayer.score + " (+1)"); 
    if(thePlayer.score >= 100) {
        announce("Player " + player + " is safe!");
        thePlayer.state = 1;
    }
}

//Player quits
function handleQuit(socket) {
    if(!socketsToPlayers.has(socket)) {
        console.log('Handling quit');
        return;
    } 
    const player = socketsToPlayers.get(socket);
    socketsToPlayers.delete(socket);
    playersToSockets.delete(player);
    killPlayer(player);

    console.log('Handling quit from player ' + player);
    announce('Goodbye player ' + player);
}

//Chat message
function handleChat(player,message) {
    console.log('Handling chat: ' + message + ' from player ' + player);
    announce('#' + player + ': ' + message);
}

//Handle action
function handleAction(player,action) {
    if(state.state != 1) return;

    //No dead players
    const thePlayer = players.get(player);
    if(thePlayer.state != 0) return;

    console.log('Handling action: ' + action + ' from player ' + player);
    if(state.light == 'red') {
        killPlayer(player);
    } else if(lastPlayer == player) {
        tripPlayer(player);
    } else {
        advancePlayer(player);
    }

    lastPlayer = player;
}

//Start the game 
function startGame() {
    console.log('Game starting');
    announce('Let the games begin');

    //Prepare all players    
    for(const [playerNumber,player] of players) {
        player.state = 0;
    }

    //Admin always wins
    players.get(1).score = 100;
    players.get(1).state = 1;

    //Start the timer
    console.log('Starting the timer: ' + state.countdown);
    timer = setInterval(() => {
        tickGame();
    }, 1000);

    //Advance the game
    state.state = 1;

    //Set the light
    state.light = 'red';
}

//Game tick
function tickGame() {
    if(state.countdown > 1) {
        state.countdown--;
        console.log('Tick ' + state.countdown);
    } else {
        clearInterval(timer);
        timer = null;
        endGame();
    }
    updateAll();
}

//End game -> 2
function endGame() {
    state.state = 2;
    console.log('Game ending');
    for(let [playerNumber,player] of players) {
        if(player.score < 100) {
            killPlayer(playerNumber);
        }
    }
}

//Toggle game light
function gameLight() {
    if (state.light != 'green') {
        state.light = 'green';
    } else {
        state.light = 'red';
    }
    announce(state.light.charAt(0).toUpperCase() + state.light.slice(1) + ' light!');
}

//Handle admin actions
function handleAdmin(player,action) {
    if(player != 1) {
        console.log('Failed admin action from player ' + player + ' for ' + action);
        return;
    }

    if(action == 'start' && state.state == 0) {
        startGame();
    } else if (action == 'light' && state.state == 1) {
        gameLight();
    } else {
        console.log('Unknown admin action: ' + action); 
    }
}

//Update state of all players
function updateAll() {
    console.log('Updating all players');
    for(let [playerNumber,socket] of playersToSockets) {
        updatePlayer(socket);
    }
}

//Update one player
function updatePlayer(socket) {
    const playerNumber = socketsToPlayers.get(socket);
    const thePlayer = players.get(playerNumber);
    const data = { state: state, me: thePlayer, players: Object.fromEntries(players) }; 
    socket.emit('state',data);
}

//Handle messages
io.on('connection', socket => {
  socket.on('chat', message => {
    if(!socketsToPlayers.has(socket)) return;
    handleChat(socketsToPlayers.get(socket),message);
  });
  socket.on('join', () => {
    if(socketsToPlayers.has(socket)) return;
    handleJoin(socket);
    updateAll();
  });
  socket.on('admin', action => {
    if(!socketsToPlayers.has(socket)) return;
    handleAdmin(socketsToPlayers.get(socket),action);
    updateAll();
  });
  socket.on('action', action => {
    if(!socketsToPlayers.has(socket)) return;
    handleAction(socketsToPlayers.get(socket),action);
    updateAll();
  });
  socket.on('disconnect', () => {
    handleQuit(socket);
    updateAll();
  });
});


//Start server
if (module === require.main) {
  startServer();
}

module.exports = server;
