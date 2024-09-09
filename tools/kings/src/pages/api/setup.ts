import type { APIRoute } from 'astro';
import fs from 'fs';
import path from 'path';
import { connectMySQL, connectPostgres, connectSQLite } from '../db';

const configPath = path.resolve('./config.json'); // Path to the configuration file

export const post: APIRoute = async ({ request }) => {
  const formData = await request.formData();
  const dbType = formData.get('dbType') as string;
  const host = formData.get('host') as string;
  const port = formData.get('port') as string;
  const user = formData.get('user') as string;
  const password = formData.get('password') as string;
  const database = formData.get('database') as string;

  // Save configuration to a JSON file
  const config = {
    dbType,
    host,
    port,
    user,
    password,
    database,
  };

  try {
    fs.writeFileSync(configPath, JSON.stringify(config, null, 2));
    
    // Test database connection
    let db;
    switch (dbType) {
      case 'mysql':
        db = await connectMySQL();
        break;
      case 'postgres':
        db = await connectPostgres();
        break;
      case 'sqlite':
        db = await connectSQLite();
        break;
      default:
        throw new Error('Unsupported database type');
    }
    db.end(); // Close connection
    
    return new Response('CMS setup successfully!', { status: 200 });
  } catch (error) {
    return new Response(`Setup failed: ${error.message}`, { status: 400 });
  }
};
