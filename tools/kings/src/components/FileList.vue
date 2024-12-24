<template>
  <div>
    <!-- Section for displaying available files -->
    <h1>Available Files for Download</h1>
    <ul>
      <li v-for="file in files" :key="file.path">
        <a :href="`irc://localhostd:6667/#channel?command=/dcc send ${file.path}`">
          Download {{ file.name }} via DCC
        </a>
        <a :href="`irc://localhostd:6667/#channel?command=/xdcc send ${file.packNumber}`">
          Download Pack #{{ file.packNumber }} via XDCC
        </a>
      </li>
    </ul>

    <!-- Section for displaying available covers -->
    <h2>Available Covers</h2>
    <ul>
      <li v-for="cover in paginatedCovers" :key="cover.name" @click="showChapters(cover)">
        <img :src="cover.path" :alt="cover.name" />
        <p>{{ cover.name }}</p>
      </li>
    </ul>

    <!-- Section for displaying chapters and stats -->
    <div v-if="selectedCover">
      <h2>Chapters for {{ selectedCover.name }}</h2>
      <ul>
        <li v-for="chapter in chapters" :key="chapter.name">
          <p>{{ chapter.name }} - Readers: {{ chapter.readers }}</p>
        </li>
      </ul>
    </div>

    <!-- Pagination controls -->
    <div class="pagination">
      <button @click="prevPage" :disabled="currentPage === 1">Previous</button>
      <span>Page {{ currentPage }} of {{ totalPages }}</span>
      <button @click="nextPage" :disabled="currentPage === totalPages">Next</button>
    </div>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue';

const currentPage = ref(1);
const coversPerPage = 5; // Number of covers per page

// props with default value
const props = defineProps({
  covers: {
    type: Array,
    required: false,
    default: () => [], // Default to an empty array if not provided
  },
});

// computed properties
const totalPages = computed(() => Math.ceil(props.covers.length / coversPerPage));

const paginatedCovers = computed(() => {
  if (!props.covers || props.covers.length === 0) {
    return []; // Return an empty array if no covers are available
  }
  const start = (currentPage.value - 1) * coversPerPage;
  const end = start + coversPerPage;
  return props.covers.slice(start, end);
});

const selectedCover = ref(null);
const chapters = ref([]);

// methods for pagination
const prevPage = () => {
  if (currentPage.value > 1) currentPage.value--;
};

const nextPage = () => {
  if (currentPage.value < totalPages.value) currentPage.value++;
};

// method to show chapters and stats
const showChapters = (cover) => {
  selectedCover.value = cover;
  // Fetch chapters and stats (replace with actual logic)
  chapters.value = cover.chapters || [
    { name: 'Chapter 1', readers: 120 },
    { name: 'Chapter 2', readers: 95 },
    // Add more chapters as needed
  ];
};
</script>

<style scoped>
/* Styling for file list */
ul {
  list-style-type: none;
  padding: 0;
}

li {
  background-color: #fff;
  margin: 10px 0;
  padding: 15px;
  border: 1px solid #ddd;
  border-radius: 5px;
  text-align: center;
  cursor: pointer;
}

a {
  text-decoration: none;
  color: #007bff;
  display: block;
  margin: 5px 0;
}

a:hover {
  text-decoration: underline;
}

/* Styling for cover images */
img {
  max-width: 100px;
  border-radius: 5px;
  margin-bottom: 5px;
}

p {
  margin: 0;
  color: #333;
  font-size: 14px;
}

/* Pagination controls */
.pagination {
  display: flex;
  justify-content: center;
  align-items: center;
  margin-top: 20px;
}

button {
  background-color: #007bff;
  color: white;
  border: none;
  padding: 10px;
  cursor: pointer;
  margin: 0 10px;
}

button:disabled {
  background-color: #ccc;
  cursor: not-allowed;
}
</style>
