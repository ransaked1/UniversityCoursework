<?php

namespace Admin;

class Blog extends AdminController
{

	public function index($f3)
	{
		$posts = $this->Model->Posts->fetchAll();
		$blogs = $this->Model->map($posts, 'user_id', 'Users');
		$blogs = $this->Model->map($posts, array('post_id', 'Post_Categories', 'category_id'), 'Categories', false, $blogs);
		$f3->set('blogs', $blogs);
	}

	public function delete($f3)
	{
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!\Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			\StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			$f3->reroute('/admin/blog');
			return ;
		}

		$postid = $f3->get('PARAMS.3');
		$post = $this->Model->Posts->fetchById($postid);
		$post->erase();

		//Remove from categories
		$cats = $this->Model->Post_Categories->fetchAll(array('post_id' => $postid));
		foreach ($cats as $cat) {
			$cat->erase();
		}

		\StatusMessage::add('Post deleted successfully', 'success');
		return $f3->reroute('/admin/blog');
	}

	public function add($f3)
	{
		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			$post = $this->Model->Posts;
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/blog/');
			}

			// Sanitize input
			$title = \Common::cleanInput($title);
			$content = \Common::cleanInput($content);
			$summary = \Common::cleanInput($summary);

			$post->title = $title;
			$post->content = $content;
			$post->summary = $summary;
			$post->user_id = $this->Auth->user('id');
			$post->created = $post->modified = mydate();

			// Check for errors
			$errors = false;
			if (empty($post->title)) {
				$errors = 'You did not specify a title';
			}

			if ($errors) {
				\StatusMessage::add($errors, 'danger');
			} else {
				// Determine whether to publish or draft
				if (!isset($Publish)) {
					$post->published = null;
				} else {
					$post->published = mydate($this->request->data['published']);
				}

				// Save post
				$post->save();
				$postid = $post->id;

				// Now assign categories
				$link = $this->Model->Post_Categories;
				if (!isset($categories)) {
					$categories = array();
				}
				foreach ($categories as $category) {
					$link->reset();
					$link->category_id = $category;
					$link->post_id = $postid;
					$link->save();
				}

				\StatusMessage::add('Post added successfully', 'success');
				return $f3->reroute('/admin/blog');
			}
		}
		$categories = $this->Model->Categories->fetchList();
		$f3->set('categories', $categories);
	}

	public function edit($f3)
	{
		$postid = $f3->get('PARAMS.3');
		$postid = \Common::cleanInput($postid); // Sanitize user input
		$post = $this->Model->Posts->fetchById($postid);
		$blog = $this->Model->map($post, array('post_id', 'Post_Categories', 'category_id'), 'Categories', false);
		$csrf_token = null; // Initialize empty CSRF token

		// Check numeric id supplied and is a valid post
		if (!ctype_digit($postid) || !$post) {
			\StatusMessage::add('Cannot find post to edit.', 'danger');
			return $f3->reroute('/admin/blog');
		}

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/blog');
			}

			$post->copyfrom('POST');
			$post->modified = mydate();
			$post->user_id = $this->Auth->user('id');

			$post->title = \Common::cleanInput($post->title);
			$post->summary = \Common::cleanInput($post->summary);
			$post->content = \Common::cleanMarkdown($post->content);


			//Determine whether to publish or draft
			if (!isset($Publish)) {
				$post->published = null;
			} else {
				$post->published = mydate($published);
			}

			//Save changes
			$post->save();

			$link = $this->Model->Post_Categories;
			//Remove previous categories
			$old = $link->fetchAll(array('post_id' => $postid));
			foreach ($old as $oldcategory) {
				$oldcategory->erase();
			}

			//Now assign new categories				
			if (!isset($categories)) {
				$categories = array();
			}
			foreach ($categories as $category) {
				$link->reset();
				$link->category_id = $category;
				$link->post_id = $postid;
				$link->save();
			}

			\StatusMessage::add('Post updated successfully', 'success');
			return $f3->reroute('/admin/blog');
		}

		$_POST = $post->cast();
		foreach ($blog['Categories'] as $cat) {
			if (!$cat)
				continue;
			$_POST['categories'][] = $cat->id;
		}

		$categories = $this->Model->Categories->fetchList();
		$f3->set('categories', $categories);
		$f3->set('post', $post);
	}

}

?>