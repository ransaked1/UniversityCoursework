<?php

#[AllowDynamicProperties]
class AuthHelper
{
	private $domain;
	private $secure;

	/** Construct a new Auth helper */
	public function __construct($controller)
	{
		$this->controller = $controller;

		$f3 = Base::instance();

		$environment = getenv('APP_ENV') ?: 'production';

		// Load DOMAIN and SECURE based on the environment
		$this->domain = $f3->get($environment . '.DOMAIN');
		$this->secure = $f3->get($environment . '.SECURE');
	}

	/** Attempt to resume a previously logged in session if one exists */
	public function resume()
	{
		$f3 = Base::instance();

		//Ignore if already running session	
		if ($f3->exists('SESSION.user.id'))
			return;

		//Log user back in from cookie
		if ($f3->exists('COOKIE.RobPress_User')) {
			$user = unserialize(base64_decode($f3->get('COOKIE.RobPress_User')));

			// Generate CSRF token
			if ($f3->exists('COOKIE.csrf_token')) {
				$this->setCookie('csrf_token', bin2hex(random_bytes(32)));
			}

			$this->forceLogin($user);
		}
	}

	private function setCookie($name, $value, $expires = 0, $path = '/')
	{
		setcookie($name, $value, [
			'expires' => $expires,
			'path' => $path,
			'domain' => '',
			'secure' => $this->secure, // Only send the cookie over HTTPS
			'httponly' => true, // Restrict access to JavaScript
			'samesite' => 'Lax' // Lax because of being a blog
		]);
	}

	/** Perform any checks before starting login */
	public function checkLogin($username, $password, $request, $debug)
	{
		//DO NOT check login when in debug mode
		if ($debug == 1) {
			return true;
		}

		return true;
	}

	/** Look up user by username and password and log them in */
	public function login($username, $password)
	{
		$f3 = Base::instance();
		$db = $this->controller->db;

		// use parametrized queries instead of directly appending user provided input
		$userData = $db->connection->exec("SELECT * FROM users WHERE username=:username", array(':username' => $username));

		// Get data associated with user including hashed password
		if (!empty($userData)) {
			$user = $userData[0];
			$hashedPasswordFromDatabase = $user['password'];

			// Check password input against hashed from db
			if (password_verify($password, $hashedPasswordFromDatabase)) {
				$this->setupSession($user);
				return $this->forceLogin($user);
			} elseif ($password === $hashedPasswordFromDatabase) {
				$this->setupSession($user);
				return $this->forceLogin($user);
			}
		}
		return false;
	}

	/** Log user out of system */
	public function logout()
	{
		$f3 = Base::instance();

		// Verify CSRF token
		if (!$f3->exists('COOKIE.csrf_token')) {
			// Kill CSRF token
			$this->setCookie('csrf_token', '');
		}

		//Kill the session
		session_destroy();

		//Kill the cookie
		setcookie('RobPress_User', '', time() - 3600, '/', $this->domain, $this->secure, true);
	}

	/** Set up the session for the current user */
	public function setupSession($user)
	{
		//Remove previous session
		session_destroy();

		// Generate random secure session id
		$sessionId = bin2hex(random_bytes(64));

		//Setup new session
		session_id($sessionId);

		// Setup CSRF token
		$this->setCookie('csrf_token', bin2hex(random_bytes(32)));

		//Setup cookie for storing user details and for relogging in
		setcookie('RobPress_User', base64_encode(serialize($user)), time() + 3600 * 24 * 30, '/', $this->domain, $this->secure, true);

		//And begin!
		new Session();
	}

	/** Not used anywhere in the code, for debugging only */
	public function specialLogin($username)
	{
		//YOU ARE NOT ALLOWED TO CHANGE THIS FUNCTION
		$f3 = Base::instance();
		$user = $this->controller->Model->Users->fetch(array('username' => $username));
		$array = $user->cast();
		return $this->forceLogin($array);
	}

	/** Not used anywhere in the code, for debugging only */
	public function debugLogin($username, $password = 'admin', $admin = true)
	{
		//YOU ARE NOT ALLOWED TO CHANGE THIS FUNCTION
		$user = $this->controller->Model->Users->fetch(array('username' => $username));

		//Create a new user if the user does not exist
		if (!$user) {
			$user = $this->controller->Model->Users;
			$user->username = $user->displayname = $username;
			$user->email = "$username@robpress.org";
			$user->setPassword($password);
			$user->created = mydate();
			$user->bio = '';
			if ($admin) {
				$user->level = 2;
			} else {
				$user->level = 1;
			}
			$user->save();
		}

		$user->setPassword($password);

		//Move user up to administrator
		if ($admin && $user->level < 2) {
			$user->level = 2;
			$user->save();
		}

		//Log in as new user
		return $this->forceLogin($user);
	}

	/** Force a user to log in and set up their details */
	public function forceLogin($user)
	{
		//YOU ARE NOT ALLOWED TO CHANGE THIS FUNCTION
		$f3 = Base::instance();

		if (is_object($user)) {
			$user = $user->cast();
		}

		$f3->set('SESSION.user', $user);
		return $user;
	}

	/** Get information about the current user */
	public function user($element = null)
	{
		$f3 = Base::instance();
		if (!$f3->exists('SESSION.user')) {
			return false;
		}
		if (empty($element)) {
			return $f3->get('SESSION.user');
		} else {
			return $f3->get('SESSION.user.' . $element);
		}
	}

}

?>