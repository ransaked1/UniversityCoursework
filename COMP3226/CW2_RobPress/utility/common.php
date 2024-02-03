<?php

// Class to hold utility functions
class Common extends Controller
{
    public static $csrfEnabled = true;

    public static function cleanInput($input)
    {
        // Use the clean method provided by F3 for XSS protection
        return \Base::instance()->clean($input);
    }

    public static function cleanMarkdown($markdown) {
        // Define allowable tags
        $allowedTags = '<b><i><u><a><s><big><small><ul><li><ol><blockquote><h1><h2><h3><header><footer><section>';

        $cleanHtml = strip_tags($markdown, $allowedTags);

        return $cleanHtml;
    }

    public static function cleanFilename($filename)
    {
        // Use F3's clean method for filename sanitization
        return \Base::instance()->clean($filename, 'filename');
    }

    public static function validatePassword($password, $oldPassword = null)
    {
        // Check minimum length
        if (strlen($password) < 9) {
            StatusMessage::add('Password must be at least 9 characters long', 'danger');
            return false;
        }

        // Check if it contains both numeric and alphabetic characters
        if (!preg_match('/[A-Za-z]/', $password) || !preg_match('/[0-9]/', $password)) {
            StatusMessage::add('Password must contain both numeric and alphabetic characters', 'danger');
            return false;
        }

        // Check if it contains a special character
        if (!preg_match('/[!@$%^&*+#-]/', $password)) {
            StatusMessage::add('Password must contain at least one special character (!,@,$,%,^,&,*,+,#,-) ', 'danger');
            return false;
        }

        // Check different from old password (both hashed and plain for backwards compatibility)
        if (!is_null($oldPassword) && (password_verify($password, $oldPassword) || $password === $oldPassword)) {
            StatusMessage::add('New password must be different from the old password', 'danger');
            return false;
        }

        return true;
    }

    public static function validateCsrfToken($token, $debug)
    {
        // Skip validation in debug mode
        if ($debug == 1)
            return true;

        // Fail if no token supplied
        if ($token == null)
            return false;

        $f3 = Base::instance();

        // Get token
        $expectedToken = $f3->get('COOKIE.csrf_token');
        // Compare received and expected tokens
        return hash_equals($expectedToken, $token);
    }

    public static function validateAndSanitizeUrl($url, $allowedDomains = ['team3213.robpress.ecs.soton.ac.uk'])
    {
        // Sanitize the input
        Common::cleanInput($url);

        // Remove whitespaces
        $url = trim($url);

        // Check the value is a valid URL
        if (filter_var($url, FILTER_VALIDATE_URL) === false) {
            return false; // Not a valid URL
        }

        // Parse URL to extract domain
        $parsedUrl = parse_url($url);
        $domain = isset($parsedUrl['host']) ? $parsedUrl['host'] : '';

        if (!in_array($domain, $allowedDomains)) {
            return false; // Domain not allowed
        }

        return $url;
    }

    public static function validateAndSanitizePath($path, $allowedPaths = [])
    {
        // Sanitize the input
        Common::cleanInput($path);

        // Remove slashes
        $path = trim($path, '/');

        if (!in_array($path, $allowedPaths)) {
            return false; // Path not allowed
        }

        return $path;
    }
}

?>