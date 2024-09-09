export interface RSSItem {
  title: string;
  description: string;
  link: string;
  pubDate: Date;
}

export const generateRSS = (items: RSSItem[], baseUrl: string): string => {
  const feed = `
    <rss version="2.0">
      <channel>
        <title>Your Site Title</title>
        <description>Your site description here</description>
        <link>${baseUrl}</link>
        <lastBuildDate>${new Date().toUTCString()}</lastBuildDate>
        ${items
          .map(
            (item) => `
          <item>
            <title>${item.title}</title>
            <description>${item.description}</description>
            <link>${item.link}</link>
            <pubDate>${item.pubDate.toUTCString()}</pubDate>
          </item>
        `
          )
          .join('')}
      </channel>
    </rss>
  `;
  return feed.trim();
};
