<?php
class User extends Controller
{

	public function view($f3)
	{
		$userid = $f3->get('PARAMS.3');

		// Sanitize user input before using it
		$useridClean = Common::cleanInput($userid);

		// Chnaged from fetch to fetchById
		$u = $this->Model->Users->fetchById($useridClean);

		// Check user exists
		if (!ctype_digit($useridClean) || !$u) {
			StatusMessage::add('Cannot find user to view.', 'danger');
			return $f3->reroute('/');
		}

		$articles = $this->Model->Posts->fetchAll(array('user_id' => $useridClean));
		$comments = $this->Model->Comments->fetchAll(array('user_id' => $useridClean));

		$f3->set('u', $u);
		$f3->set('articles', $articles);
		$f3->set('comments', $comments);
	}

	public function add($f3)
	{
		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			if (!Common::validateCsrfToken($csrf_token, $debug)) {
				StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return;
			}

			// Clean input during registration
			$username = Common::cleanInput($username);
			$password = Common::cleanInput($password);
			$password2 = Common::cleanInput($password2);

			$check = $this->Model->Users->fetch(array('username' => $username));
			if (!empty($check)) {
				StatusMessage::add('User already exists', 'danger');
			} else if ($password != $password2) {
				StatusMessage::add('Passwords must match', 'danger');
			} else {

				$oldUser = $this->Model->Users->fetch(array('username' => $username));
				$oldPasswordHash = (!empty($oldUser)) ? $oldUser[0]['password'] : null;

				// Validate new password before assignment
				if (Common::validatePassword($password, $oldPasswordHash)) {
					$user = $this->Model->Users;
					$user->copyfrom('POST');
					$user->created = mydate();
					$user->bio = '';
					$user->level = 1;

					$user->username = $username;

					if (empty($displayname)) {
						$user->displayname = $username;
					}

					// Set the hashed users password
					$user->setPassword(password_hash($password, PASSWORD_BCRYPT));

					$user->save();
					StatusMessage::add('Registration complete', 'success');
					return $f3->reroute('/user/login');
				}
			}
		}
	}

	public function login($f3)
	{
		/** YOU MAY NOT CHANGE THIS FUNCTION - Make any changes in Auth->checkLogin, Auth->login and afterLogin() (AuthHelper.php) */
		if ($this->request->is('post')) {

			//Check for debug mode
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			//Either allow log in with checked and approved login, or debug mode login
			list($username, $password) = array($this->request->data['username'], $this->request->data['password']);
			if (
				($this->Auth->checkLogin($username, $password, $this->request, $debug) && ($this->Auth->login($username, $password))) ||
				($debug && $this->Auth->debugLogin($username))
			) {

				$this->afterLogin($f3);

			} else {
				StatusMessage::add('Invalid username or password', 'danger');
			}
		}
	}

	/* Handle after logging in */
	private function afterLogin($f3)
	{
		StatusMessage::add('Logged in successfully', 'success');

		$destination = isset($_GET['from']) ? $_GET['from'] : '/';

		$allowedPaths = ['blog', 'blog/search', 'contact', 'page/display/rob'];
		$validatedDestination = Common::validateAndSanitizePath($destination, $allowedPaths);

		if ($validatedDestination !== false) {
			$f3->reroute($validatedDestination);
		} else {
			$f3->reroute('/');
		}
	}

	public function logout($f3)
	{
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			$f3->reroute('/');
			return ;
		}

		$this->Auth->logout();
		StatusMessage::add('Logged out successfully', 'success');
		$f3->reroute('/');
	}


	public function profile($f3)
	{
		$id = $this->Auth->user('id');

		// Redirect if user not logged in
		if (!$id) {
			return $f3->reroute('/');
		}

		// Sanitize id
		$id = Common::cleanInput($id);

		// Chnaged from fetch to fetchById
		$u = $this->Model->Users->fetchById($id);
		$oldpass = $u->password;

		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			if (!Common::validateCsrfToken($csrf_token, $debug)) {
				StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/user/profile/' . $id);
			}

			$u->copyfrom('POST');

			// Clean inputs for bio and displayname
			$u->displayname = Common::cleanInput($u->displayname);
			$u->bio = Common::cleanInput($u->bio);

			if (empty($u->password)) {
				$u->password = $oldpass;
			} else {
				// Validate password
				if (Common::validatePassword($password, $oldpass)) {
					$u->password = password_hash($u->password, PASSWORD_BCRYPT);
					$u->save();
					\StatusMessage::add('Profile updated successfully', 'success');
					return $this->logout($f3); // Logout user after changing password
				} else {
					$f3->reroute('/user/profile');
				}
			}

			//Handle avatar upload
			if (isset($_FILES['avatar']) && isset($_FILES['avatar']['tmp_name']) && !empty($_FILES['avatar']['tmp_name'])) {
				$url = File::Upload($_FILES['avatar']);

				if ($url === false)
					$f3->reroute('/user/profile');

				$u->avatar = $url;
			} else if (isset($reset)) {
				$u->avatar = '';
			}

			$u->save();
			\StatusMessage::add('Profile updated successfully', 'success');
			return $f3->reroute('/user/profile');
		}
		$_POST = $u->cast();
		$f3->set('u', $u);
	}

	public function promote($f3)
	{
		$id = $this->Auth->user('id');

		// Sanitize id
		$id = Common::cleanInput($id);

		// Chnaged from fetch to fetchById
		$u = $this->Model->Users->fetchById($id);
		$u->level = 2;
		$u->save();
		return $f3->reroute('/');
	}

}
?>