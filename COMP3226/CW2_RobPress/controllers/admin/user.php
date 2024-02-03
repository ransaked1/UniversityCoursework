<?php

namespace Admin;

class User extends AdminController
{

	public function index($f3)
	{
		$users = $this->Model->Users->fetchAll();
		$f3->set('users', $users);
	}

	public function export($f3)
	{
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!\Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			\StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			return $f3->reroute('/admin/user/');
		}

		$users = $this->Model->Users->fetchAll();

		// Create temp steam for output
		$output = fopen('php://temp', 'w+');

		//Add headings
		$headings = ['ID', 'Username', 'Display Name', 'Email', 'Password', 'Level', 'Created'];
		fputcsv($output, $headings);

		//Add users
		foreach ($users as $user) {
			$fields = [$user->id, $user->username, $user->displayname, $user->email, $user->password, $user->level, $user->created];
			fputcsv($output, $fields);
		}

		// Rewind stream for reading
		rewind($output);

		// Output CSV to the browser
		header('Content-Type: application/csv');
		header('Content-Disposition: attachment; filename=export.csv');
		fpassthru($output);

		fclose($output);
		exit();
	}

	public function edit($f3)
	{
		$id = $f3->get('PARAMS.3');

		// Chnaged from fetch to fetchById
		$u = $this->Model->Users->fetchById($id);

		$csrf_token = null; // Initialize empty CSRF token

		// Check numeric id supplied and is a valid user
		if (!ctype_digit($id) || !$u) {
			\StatusMessage::add('Cannot find user to edit.', 'danger');
			return $f3->reroute('/admin/user');
		}

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/user/');
			}

			$u->copyfrom('POST');

			$newPassword = $this->request->data['password'];
			$newPassword = \Common::cleanInput($newPassword);

			// Validate and set new password
			if (\Common::validatePassword($newPassword)) {
				$newPasswordHashed = password_hash($newPassword, PASSWORD_BCRYPT);
				$u->setPassword($newPasswordHashed);
			} else {
				return $f3->reroute('/admin/user');
			}

			$u->save();
			\StatusMessage::add('User updated successfully', 'success');
			return $f3->reroute('/admin/user');
		}
		$_POST = $u->cast();
		$f3->set('u', $u);
	}

	public function delete($f3)
	{
		// Get token parameter
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!\Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			\StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			return $f3->reroute('/admin/user');
		}

		$id = $f3->get('PARAMS.3');
		// Chnaged from fetch to fetchById
		$u = $this->Model->Users->fetchById($id);

		if ($id == $this->Auth->user('id')) {
			\StatusMessage::add('You cannot remove yourself', 'danger');
			return $f3->reroute('/admin/user');
		}

		//Remove all posts and comments
		$posts = $this->Model->Posts->fetchAll(array('user_id' => $id));
		foreach ($posts as $post) {
			$post_categories = $this->Model->Post_Categories->fetchAll(array('post_id' => $post->id));
			foreach ($post_categories as $cat) {
				$cat->erase();
			}
			$post->erase();
		}
		$comments = $this->Model->Comments->fetchAll(array('user_id' => $id));
		foreach ($comments as $comment) {
			$comment->erase();
		}
		$u->erase();

		\StatusMessage::add('User has been removed', 'success');
		return $f3->reroute('/admin/user');
	}


}

?>