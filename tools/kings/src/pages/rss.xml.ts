import type { APIRoute } from 'astro';
import { generateRSS, RSSItem } from '../utils/rss';

// Example: Get your pages or blog posts
const getPosts = (): RSSItem[] => {
  // Replace this with your actual data fetching logic
  return [
    {
      title: 'Post 1',
      description: 'This is the first post.',
      link: 'https://yoursite.com/posts/post-1',
      pubDate: new Date('2023-08-01'),
    },
    {
      title: 'Post 2',
      description: 'This is the second post.',
      link: 'https://yoursite.com/posts/post-2',
      pubDate: new Date('2023-08-15'),
    },
  ];
};

export const get: APIRoute = async () => {
  const posts = getPosts(); // Replace with actual data retrieval
  const baseUrl = 'https://yoursite.com'; // Replace with your site's base URL

  const rss = generateRSS(posts, baseUrl);

  return new Response(rss, {
    headers: {
      'Content-Type': 'application/rss+xml; charset=utf-8',
    },
  });
};
