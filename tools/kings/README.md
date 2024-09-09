```markdown
# IRC Server

This project is a Node.js-based IRC server with support for SSL, NAT-PMP, and UPnP port mapping.

## Table of Contents

1. [Setup](#setup)
2. [Configuration](#configuration)
3. [Running the Server](#running-the-server)
4. [Additional Information](#additional-information)
5. [Astro Starter Kit](#astro-starter-kit)

## Setup

To set up the IRC server, follow these steps:

1. **Clone the Repository**

   If you haven’t cloned the repository yet, do so with the following command:

   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```

2. **Install Dependencies**

   Install the necessary Node.js packages by running:

   ```bash
   npm install
   ```

3. **Generate SSL Certificates**

   If you plan to use SSL for secure connections, you need to generate SSL certificates. Use the following commands to create self-signed certificates for testing purposes:

   ```bash
   openssl genrsa -out server-key.pem 2048
   openssl req -new -key server-key.pem -out server-csr.pem
   openssl x509 -req -days 365 -in server-csr.pem -signkey server-key.pem -out server-cert.pem
   ```

   Place the generated `server-key.pem` and `server-cert.pem` files in the root directory of your project.

## Configuration

Open `irc-server.js` to configure the following settings:

- **Server Port**: Adjust the `server_port` variable to set the port on which the server will listen (default is `6667`).

- **Server Name**: Update the `server_name` variable to set the name of your IRC server.

- **Internal IP**: Change the `internal_ip` variable to match your internal network IP address.

- **SSL Settings**: If using SSL, ensure that `server-cert.pem` and `server-key.pem` files are present and correctly configured.

- **Port Mapping**: Uncomment and configure the UPnP and NAT-PMP sections if you want the server to handle automatic port mapping.

## Running the Server

To start the IRC server, navigate to your project directory in the terminal and run:

```bash
node irc-server.js
```

The server will start and listen on the specified port. You should see a message indicating that the IRC server is running.

## Additional Information

- **Handling Commands**: The server supports various IRC commands including `NICK`, `USER`, `JOIN`, `PART`, `PING`, `PRIVMSG`, `TOPIC`, `WHOIS`, `DCC`, `XDCC`, and `TEST`. Commands are handled according to standard IRC protocol specifications.

- **Logging and Debugging**: The server prints incoming commands and errors to the console. Use these logs for debugging and monitoring server activity.

- **Port Mapping**: If port mapping is required, ensure that UPnP and NAT-PMP configurations are properly set up. The default implementation for these features is commented out; uncomment and configure as needed.

## Astro Starter Kit: Basics

If you're also working with Astro, you can initialize a basic Astro project with the following command:

```sh
npm create astro@latest -- --template basics
```

Explore the project structure and commands for Astro:

- **Project Structure**

  ```text
  /
  ├── public/
  │   └── favicon.svg
  ├── src/
  │   ├── components/
  │   │   └── Card.astro
  │   ├── layouts/
  │   │   └── Layout.astro
  │   └── pages/
  │       └── index.astro
  └── package.json
  ```

- **Commands**

  | Command                   | Action                                           |
  | :------------------------ | :----------------------------------------------- |
  | `npm install`             | Installs dependencies                            |
  | `npm run dev`             | Starts local dev server at `localhost:4321`      |
  | `npm run build`           | Build your production site to `./dist/`          |
  | `npm run preview`         | Preview your build locally, before deploying     |
  | `npm run astro ...`       | Run CLI commands like `astro add`, `astro check` |
  | `npm run astro -- --help` | Get help using the Astro CLI                     |

For further details on Astro, check out [the documentation](https://docs.astro.build) or join the [Astro Discord server](https://astro.build/chat).

---

Feel free to reach out if you have any questions or need further assistance.

Happy chatting!
```
