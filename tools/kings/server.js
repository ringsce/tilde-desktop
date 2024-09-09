const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const fs = require('fs');
const path = require('path');
const marked = require('marked');

const app = express();
const server = http.createServer(app);
const io = socketIo(server);

// Static list of files for demonstration
let files = [
  { path: '/path/to/file1.ext', name: 'File1.ext', packNumber: 1 },
  { path: '/path/to/file2.ext', name: 'File2.ext', packNumber: 2 },
];

io.on('connection', (socket) => {
  console.log('A user connected');
  socket.emit('update', files);

  // Simulate a file update after 10 seconds
  setTimeout(() => {
    files.push({ path: '/path/to/file3.ext', name: 'File3.ext', packNumber: 3 });
    io.emit('update', files);
  }, 10000);

  socket.on('disconnect', () => {
    console.log('User disconnected');
  });
});

// Middleware to serve static files
app.use(express.static(path.join(__dirname, 'public')));

// Route for the home page
app.get('/', (req, res) => {
  res.send('Hello World!');
});

// Route for serving markdown files as HTML
app.get('/:filename', (req, res) => {
  const filename = req.params.filename;
  const markdownPath = path.join(__dirname, 'posts', `${filename}.md`);

  fs.readFile(markdownPath, 'utf8', (err, data) => {
    if (err) {
      res.status(404).send('File not found');
    } else {
      const html = marked(data.toString());
      res.send(html);
    }
  });
});

// Start the server
const PORT = 3000;
server.listen(PORT, () => {
  console.log(`Server listening on port ${PORT}`);
});
