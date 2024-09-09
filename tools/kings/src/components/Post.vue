<template>
    <div>
      <head>
        <title>{{ title }}</title>
      </head>
      <body>
        <div class="container">
          <div v-html="content"></div>
        </div>
      </body>
    </div>
  </template>
  
  <script>
  export default {
    props: {
      title: {
        type: String,
        required: true,
      },
      content: {
        type: String,
        required: true,
      },
    },
  };
  </script>
  
  <style scoped>
  /* Add your styles here */
  .container {
    padding: 20px;
  }
  </style>
  