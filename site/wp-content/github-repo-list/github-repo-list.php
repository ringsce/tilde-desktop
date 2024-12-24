<?php
/*
Plugin Name: GitHub Repo List
Description: Fetch and update GitHub repositories every 5 minutes and display them on a page.
Version: 1.0
Author: Your Name
*/

if (!defined('ABSPATH')) {
    exit; // Exit if accessed directly.
}

// Schedule cron job on activation
register_activation_hook(__FILE__, function () {
    if (!wp_next_scheduled('github_repo_update')) {
        wp_schedule_event(time(), 'five_minutes', 'github_repo_update');
    }
});

// Clear cron job on deactivation
register_deactivation_hook(__FILE__, function () {
    $timestamp = wp_next_scheduled('github_repo_update');
    if ($timestamp) {
        wp_unschedule_event($timestamp, 'github_repo_update');
    }
});

// Add custom interval of 5 minutes
add_filter('cron_schedules', function ($schedules) {
    $schedules['five_minutes'] = [
        'interval' => 300,
        'display'  => __('Every 5 Minutes'),
    ];
    return $schedules;
});

// Fetch GitHub repos
add_action('github_repo_update', function () {
    $username = 'your-github-username'; // Replace with your GitHub username
    $url = "https://api.github.com/users/$username/repos";

    $response = wp_remote_get($url, [
        'headers' => [
            'User-Agent' => 'WordPress GitHub Repo Fetcher',
        ],
    ]);

    if (is_wp_error($response)) {
        return;
    }

    $repos = json_decode(wp_remote_retrieve_body($response), true);

    if (is_array($repos)) {
        update_option('github_repo_list', $repos);
    }
});

// Shortcode to display the repos
add_shortcode('github_repo_list', function () {
    $repos = get_option('github_repo_list', []);

    if (empty($repos)) {
        return '<p>No repositories found.</p>';
    }

    $output = '<ul>';
    foreach ($repos as $repo) {
        $output .= sprintf(
            '<li><a href="%s" target="_blank">%s</a></li>',
            esc_url($repo['html_url']),
            esc_html($repo['name'])
        );
    }
    $output .= '</ul>';

    return $output;
});
