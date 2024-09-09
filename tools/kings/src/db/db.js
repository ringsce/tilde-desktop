import mysql from 'mysql2/promise';
import { Client } from 'pg';
import sqlite3 from 'sqlite3';
import { open } from 'sqlite';

// MySQL connection
export async function connectMySQL() {
  return mysql.createPool({
    host: 'localhost',
    user: 'your_mysql_user',
    password: 'your_mysql_password',
    database: 'your_mysql_database',
  });
}

// PostgreSQL connection
export async function connectPostgres() {
  const client = new Client({
    host: 'localhost',
    port: 5432,
    user: 'your_postgres_user',
    password: 'your_postgres_password',
    database: 'your_postgres_database',
  });
  await client.connect();
  return client;
}

// SQLite connection
export async function connectSQLite() {
  return open({
    filename: './database.sqlite',
    driver: sqlite3.Database,
  });
}
