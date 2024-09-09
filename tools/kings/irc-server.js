const net = require('net');
const tls = require('tls');
const dns = require('dns');
const fs = require('fs');
const path = require('path');
const UPnP = require('nat-upnp');
const NatPMP = require('nat-pmp');

// Configuration
const serverPort = 6667; // Default port
const serverName = 'localhostd';
const internalIp = '192.168.1.2'; // Replace with your internal IP
const useSSL = serverPort === 6697;
const clients = {};
const channels = {};
const xdccPacks = {}; // Store XDCC packs

let server;

// Setup UPnP port mapping
function setupUPnP() {
    return new Promise((resolve, reject) => {
        const client = UPnP.createClient();
        client.portMapping(serverPort, serverPort, 'TCP', (err) => {
            if (err) {
                reject(err);
            } else {
                resolve();
            }
        });
    });
}

// Map port using NAT-PMP
function mapPortNatPMP() {
    return new Promise((resolve, reject) => {
        const client = new NatPMP();
        client.portMapping(serverPort, serverPort, (err) => {
            if (err) {
                reject(err);
            } else {
                resolve();
            }
        });
    });
}

// Create a server
if (useSSL) {
    const options = {
        key: fs.readFileSync('server-key.pem'),
        cert: fs.readFileSync('server-cert.pem'),
        ciphers: 'HIGH:!aNULL:!MD5',
        secureProtocol: 'TLSv1_2_method',
    };
    server = tls.createServer(options, handleClient);
} else {
    server = net.createServer(handleClient);
}

server.listen(serverPort, () => {
    console.log(`IRC Server started on port ${serverPort}`);
});

// Handle client connections
function handleClient(socket) {
    console.log(`Accepted new client from ${socket.remoteAddress}:${socket.remotePort}`);
    clients[socket] = {
        socket: socket,
        address: socket.remoteAddress,
        port: socket.remotePort,
        nick: null,
        user: null,
        realname: null,
        channels: [],
    };

    socket.on('data', (data) => {
        const lines = data.toString().split('\r\n');
        lines.forEach(line => {
            if (line.trim()) {
                handleCommand(socket, line.trim());
            }
        });
    });

    socket.on('error', (err) => {
        console.error('Socket error:', err);
    });

    socket.on('end', () => {
        console.log(`Client ${socket.remoteAddress}:${socket.remotePort} disconnected`);
        delete clients[socket];
    });
}

function handleCommand(socket, line) {
    const clientInfo = clients[socket];

    console.log(`Received: ${line}`);

    if (/^NICK\s+(\S+)/.test(line)) {
        const nick = RegExp.$1;
        clientInfo.nick = nick;
        socket.write(`:${serverName} 001 ${nick} :Welcome to ${serverName}, ${nick}\r\n`);
    } else if (/^USER\s+(\S+)\s+\S+\s+\S+\s+:(.*)/.test(line)) {
        clientInfo.user = RegExp.$1;
        clientInfo.realname = RegExp.$2;
    } else if (/^JOIN\s+(#\S+)/.test(line)) {
        const channel = RegExp.$1;
        joinChannel(socket, clientInfo, channel);
    } else if (/^PART\s+(#\S+)/.test(line)) {
        const channel = RegExp.$1;
        partChannel(socket, clientInfo, channel);
    } else if (/^PING\s+(.*)/.test(line)) {
        socket.write(`PONG ${RegExp.$1}\r\n`);
    } else if (/^QUIT/.test(line)) {
        socket.write('QUIT :Client disconnected\r\n');
        socket.end();
    } else if (/^PRIVMSG\s+(\S+)\s+:(.*)/.test(line)) {
        const target = RegExp.$1;
        const message = RegExp.$2;
        broadcastMessage(socket, target, message);
    } else if (/^TOPIC\s+(#\S+)\s+:(.*)/.test(line)) {
        const channel = RegExp.$1;
        const topic = RegExp.$2;
        channels[channel] = channels[channel] || {};
        channels[channel].topic = topic;
        broadcastTopic(channel, topic);
    } else if (/^WHOIS\s+(\S+)/.test(line)) {
        const nick = RegExp.$1;
        handleWhois(socket, nick);
    } else if (/^DNSLOOKUP\s+(\S+)/.test(line)) {
        const host = RegExp.$1;
        dns.lookup(host, (err, address) => {
            if (err) {
                socket.write(`PRIVMSG ${clientInfo.nick || 'unknown'} :No DNS record found.\r\n`);
            } else {
                socket.write(`PRIVMSG ${clientInfo.nick || 'unknown'} :${address}\r\n`);
            }
        });
    } else if (/^DCC\s+SEND\s+(\S+)\s+(\S+)/.test(line)) {
        const filePath = RegExp.$1;
        const fileSize = RegExp.$2;
        sendDCCFile(socket, filePath, fileSize);
    } else if (/^XDCC\s+SEND\s+(\d+)/.test(line)) {
        const packNumber = RegExp.$1;
        sendXDCCPack(socket, packNumber);
    } else if (/^XDCC\s+LIST/.test(line)) {
        listXDCCPacks(socket);
    } else if (/^TEST/.test(line)) {
        socket.write('TEST :Server is online\r\n');
    } else {
        socket.write('ERROR :Unknown command\r\n');
    }
}

function joinChannel(socket, clientInfo, channel) {
    channels[channel] = channels[channel] || {};
    channels[channel][socket] = clientInfo.nick || 'unknown';
    clientInfo.channels.push(channel);
    socket.write(`:${serverName} 001 ${clientInfo.nick || 'unknown'} :Welcome to the ${channel} channel\r\n`);
    broadcastChannelMessage(channel, `${clientInfo.nick || 'unknown'} has joined ${channel}`);
}

function partChannel(socket, clientInfo, channel) {
    if (channels[channel]) {
        delete channels[channel][socket];
        clientInfo.channels = clientInfo.channels.filter(ch => ch !== channel);
        socket.write(`:${serverName} PART ${channel} :Leaving channel\r\n`);
        broadcastChannelMessage(channel, `${clientInfo.nick || 'unknown'} has left ${channel}`);
    }
}

function broadcastMessage(senderSocket, target, message) {
    Object.keys(clients).forEach((sock) => {
        if (sock !== senderSocket) {
            if (channels[target] && channels[target][sock]) {
                sock.write(`:${clients[senderSocket].nick || 'unknown'}!${clients[senderSocket].user || 'unknown'}@${clients[senderSocket].address} PRIVMSG ${target} :${message}\r\n`);
            }
        }
    });
}

function broadcastTopic(channel, topic) {
    if (channels[channel]) {
        Object.keys(channels[channel]).forEach((sock) => {
            sock.write(`:${serverName} TOPIC ${channel} :${topic}\r\n`);
        });
    }
}

function handleWhois(socket, nick) {
    Object.keys(clients).forEach((sock) => {
        if (clients[sock].nick === nick) {
            socket.write(`:${serverName} 311 ${clients[sock].nick} ${nick} ${clients[sock].user} ${clients[sock].address} * :${clients[sock].realname}\r\n`);
        }
    });
}

function sendDCCFile(socket, filePath, fileSize) {
    fs.readFile(filePath, (err, data) => {
        if (err) {
            socket.write('DCC FAIL :File not found\r\n');
        } else {
            socket.write(`DCC SEND ${filePath} ${fileSize}\r\n`);
            socket.write(data);
        }
    });
}

function sendXDCCPack(socket, packNumber) {
    if (xdccPacks[packNumber]) {
        const { path: filePath, size: fileSize } = xdccPacks[packNumber];
        fs.readFile(filePath, (err, data) => {
            if (err) {
                socket.write('XDCC FAIL :Pack not found\r\n');
            } else {
                socket.write(`XDCC SEND ${filePath} ${fileSize}\r\n`);
                socket.write(data);
            }
        });
    } else {
        socket.write('XDCC FAIL :Pack not found\r\n');
    }
}

function listXDCCPacks(socket) {
    Object.keys(xdccPacks).forEach((packNumber) => {
        const { path: filePath, size: fileSize } = xdccPacks[packNumber];
        const fileName = path.basename(filePath);
        socket.write(`XDCC PACK ${packNumber} ${fileName} ${size}\r\n`);
    });
}
