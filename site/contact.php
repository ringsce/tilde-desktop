<!-- contact.php -->
<?php 
    include_once 'header.php'; 
?>

<div class="container mt-5">
    <h1>Contact Us</h1>
    <p>If you have any questions, suggestions, or feedback, feel free to contact us using the form below. We appreciate your input!</p>

    <!-- Contact Form -->
    <form action="submit_contact.php" method="POST" class="mb-5">
        <div class="mb-3">
            <label for="name" class="form-label">Name</label>
            <input type="text" class="form-control" id="name" name="name" required>
        </div>
        <div class="mb-3">
            <label for="email" class="form-label">Email address</label>
            <input type="email" class="form-control" id="email" name="email" required>
        </div>
        <div class="mb-3">
            <label for="message" class="form-label">Message</label>
            <textarea class="form-control" id="message" name="message" rows="4" required></textarea>
        </div>
        <button type="submit" class="btn btn-primary">Send Message</button>
    </form>

    <!-- Donate Section -->
    <h2>Support Us</h2>
    <p>If you would like to support Tilde Desktop and help us continue developing open-source software, please consider donating via PayPal. Your support is greatly appreciated!</p>

    <!-- PayPal Donate Button -->
    <div class="text-center mb-5">
        <a href="https://www.paypal.com/donate?hosted_button_id=YOUR_PAYPAL_BUTTON_ID" target="_blank">
            <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" alt="Donate with PayPal">
        </a>
    </div>
</div>

<?php 
    include_once 'footer.php'; 
?>

