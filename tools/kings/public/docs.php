<?php
// WordPress API endpoint
$url = "https://ringscejs.gleentech.com/wp-json/wp/v2/posts";

// Fetch posts from the WordPress API
$posts = file_get_contents($url);
if ($posts === false) {
    echo "Error fetching posts from WordPress API.";
    exit;
}

// Decode JSON response into an array
$postsData = json_decode($posts, true);

// If decoding failed, show an error
if ($postsData === null) {
    echo "Error decoding JSON response.";
    exit;
}

// Output posts as JSON to be used by Astro
header('Content-Type: application/json');
echo json_encode($postsData);
