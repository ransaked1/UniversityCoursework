var socket = null;

//Prepare game
var app = new Vue({
    el: '#game',
    data: {
        error: null,
        chatmessage: '',
        me: { name: '', state: 0, score: 0 },
        state: { state: false },
        players: {},
    },
    mounted: function() {
        connect(); 
    },
    methods: {
        admin(command) {
            socket.emit('admin',command)
        },
        join() {
            socket.emit('join');
        },
        update(data) {
            this.me = data.me;
            this.state = data.state;
            this.players = data.players;
        },
        action() {
            socket.emit('action','advance');
        },
        chat() {
            socket.emit('chat',this.chatmessage);
            this.chatmessage = '';
        },
        announce(message) {
            const messages = document.getElementById('messages');
            var item = document.createElement('li');
            item.textContent = message;
            messages.prepend(item);
        },
        fail(message) {
            this.error = message;
            setTimeout(clearError, 3000);
        },
        capitalise(text) {
            return text.charAt(0).toUpperCase() + text.slice(1);
        }
    }
});

function clearError() {
    app.error = null;
}

function connect() {
    //Prepare web socket
    socket = io();

    socket.on('connect', function() {
        app.state.state = 0;
    });

    socket.on('state', function(data) {
        app.update(data);
    });

    socket.on('fail', function(message) {
        app.fail(message);
    });

    socket.on('chat', function(message) {
        app.announce(message);
    });

    socket.on('connect_error', function(message) {
        alert('Unable to connect: ' + message);
    });

    socket.on('disconnect', function() {
        alert('Disconnected');
        app.state = { state: -1 }; 
    });
}



