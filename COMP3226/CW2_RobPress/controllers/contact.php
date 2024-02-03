<?php

class Contact extends Controller
{

	public function index($f3)
	{
		$csrf_token = null; // Initialize empty CSRF token
		
		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!Common::validateCsrfToken($csrf_token, $debug)) {
				StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/');
			}

			// Sanitize input
			$from = Common::cleanInput($from);
			$to = Common::cleanInput($to);
			$subject = Common::cleanInput($subject);
			$message = Common::cleanInput($message);

			$from = "From: $from";

			mail($to, $subject, $message, $from);

			StatusMessage::add('Thank you for contacting us');
			return $f3->reroute('/');
		}
	}

}

?>