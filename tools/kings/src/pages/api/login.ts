import { AstroError } from 'astro';
import type { APIRoute } from 'astro';
import { connectMySQL, connectPostgres, connectSQLite } from './db'; // Import database connections

// Function to validate CAPTCHA
const validateCaptcha = (generatedCaptcha: string, userCaptcha: string): boolean => {
  return generatedCaptcha === userCaptcha;
};

// Function to handle login attempt
async function handleLogin(dbType: string, username: string, password: string, captcha: string, generatedCaptcha: string): Promise<boolean> {
  let db;
  let isValid = false;

  if (!validateCaptcha(generatedCaptcha, captcha)) {
    return false;
  }

  try {
    switch (dbType) {
      case 'mysql':
        db = await connectMySQL();
        const [rows] = await db.execute('SELECT * FROM users WHERE username = ? AND password = ?', [username, password]);
        isValid = rows.length > 0;
        await db.execute('INSERT INTO login_attempts (username, success) VALUES (?, ?)', [username, isValid]);
        await db.end();
        break;

      case 'postgres':
        db = await connectPostgres();
        const { rows } = await db.query('SELECT * FROM users WHERE username = $1 AND password = $2', [username, password]);
        isValid = rows.length > 0;
        await db.query('INSERT INTO login_attempts (username, success) VALUES ($1, $2)', [username, isValid]);
        await db.end();
        break;

      case 'sqlite':
        db = await connectSQLite();
        const row = await db.get('SELECT * FROM users WHERE username = ? AND password = ?', [username, password]);
        isValid = row !== undefined;
        await db.run('INSERT INTO login_attempts (username, success) VALUES (?, ?)', [username, isValid]);
        await db.close();
        break;

      default:
        throw new AstroError('Unsupported database type');
    }
  } catch (error) {
    console.error('Database error:', error);
    throw new AstroError('Database error');
  }

  return isValid;
}

export const post: APIRoute = async ({ request }) => {
  const formData = await request.formData();
  const username = formData.get('username') as string;
  const password = formData.get('password') as string;
  const generatedCaptcha = formData.get('generatedCaptcha') as string;
  const userCaptcha = formData.get('captcha') as string;
  const dbType = formData.get('dbType') as string; // Database type from form

  const isAuthenticated = await handleLogin(dbType, username, password, userCaptcha, generatedCaptcha);

  if (!isAuthenticated) {
    return new Response(JSON.stringify({ error: 'Invalid username, password, or CAPTCHA' }), { status: 400 });
  }

  return new Response(null, {
    status: 302,
    headers: {
      Location: '/dashboard', // Redirect to the desired page
    },
  });
};
