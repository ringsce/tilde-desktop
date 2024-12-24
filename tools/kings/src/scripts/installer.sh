#!/bin/sh

# Define a function to install packages
install_packages() {
    echo "Installing necessary packages..."

    # For FreeBSD
    if [ "$(uname -s)" = "FreeBSD" ]; then
        pkg install -y npm mysql-client postgresql13-client sqlite3
    # For NetBSD
    elif [ "$(uname -s)" = "NetBSD" ]; then
        pkgin install -y npm mysql-client postgresql-client sqlite3
    # For OpenBSD
    elif [ "$(uname -s)" = "OpenBSD" ]; then
        pkg_add npm mysql-client postgresql-client sqlite3
    else
        echo "Unsupported OS"
        exit 1
    fi
}

# Run database migrations
run_migrations() {
    echo "Running database migrations..."

    # FreeBSD and NetBSD use similar commands for MySQL and PostgreSQL
    # OpenBSD may require different syntax or options for `psql` and `mysql`
    if [ "$(uname -s)" = "FreeBSD" ] || [ "$(uname -s)" = "NetBSD" ]; then
        mysql -u root -p < ./database/schema/mysql.sql
        psql -U postgres -d your_postgres_database -f ./database/schema/postgres.sql
        sqlite3 ./database/database.sqlite < ./database/schema/sqlite.sql
    elif [ "$(uname -s)" = "OpenBSD" ]; then
        mysql -u root -p < ./database/schema/mysql.sql
        psql -U postgres -d your_postgres_database -f ./database/schema/postgres.sql
        sqlite3 ./database/database.sqlite < ./database/schema/sqlite.sql
    fi

    echo "Database setup complete."
}

# Build Astro project
build_project() {
    echo "Building Astro project..."
    npm run build
    echo "Build complete."
}

# Main script execution
install_packages
run_migrations
build_project

echo "Setup complete. You can now start the server."
