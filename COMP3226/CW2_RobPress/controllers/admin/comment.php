<?php

namespace Admin;

class Comment extends AdminController
{

	public function index($f3)
	{
		//Unmoderated comments
		$un = $this->Model->Comments->fetchAll(array('moderated' => 0));
		$unmoderated = $this->Model->map($un, 'user_id', 'Users');
		$unmoderated = $this->Model->map($un, 'blog_id', 'Posts', true, $unmoderated);

		//Moderated comments
		$mod = $this->Model->Comments->fetchAll(array('moderated' => 1));
		$moderated = $this->Model->map($mod, 'user_id', 'Users');
		$moderated = $this->Model->map($mod, 'blog_id', 'Posts', true, $moderated);

		$f3->set('unmoderated', $unmoderated);
		$f3->set('moderated', $moderated);
	}

	public function edit($f3)
	{
		$id = $f3->get('PARAMS.3');
		$id = \Common::cleanInput($id); // Sanitize user input
		// Chnaged from fetch to fetchById
		$comment = $this->Model->Comments->fetchById($id);

		$csrf_token = null; // Initialize empty CSRF token

		// Check numeric id supplied and is a valid comment
		if (!ctype_digit($id) || !$comment) {
			\StatusMessage::add('Cannot find comment to edit.', 'danger');
			return $f3->reroute('/admin/comment');
		}

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/comment/');
			}

			// Validate and sanitize user inputs
			$subject = \Common::cleanInput($subject);
			$message = \Common::cleanInput($message);

			$comment->copyfrom('POST');

			// Overrite subject and message
			$comment->subject = $subject;
			$comment->message = $message;

			$comment->save();
			\StatusMessage::add('Comment updated successfully', 'success');
			return $f3->reroute('/admin/comment');
		}
		$_POST = $comment;
		$f3->set('comment', $comment);
	}

}

?>