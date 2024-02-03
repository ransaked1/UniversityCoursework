<?php

namespace Admin;

class Category extends AdminController
{

	public function index($f3)
	{
		$categories = $this->Model->Categories->fetchAll();
		$counts = array();
		foreach ($categories as $category) {
			$counts[$category->id] = $this->Model->Post_Categories->fetchCount(array('category_id' => $category->id));
		}
		$f3->set('categories', $categories);
		$f3->set('counts', $counts);
	}

	public function add($f3)
	{
		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/category/');
			}

			$category = $this->Model->Categories;
			$category->title = \Common::cleanInput($this->request->data['title']); // Sanitize user input
			$category->save();

			\StatusMessage::add('Category added successfully', 'success');
			return $f3->reroute('/admin/category');
		}
	}

	public function delete($f3)
	{
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!\Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			\StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			$f3->reroute('/admin/category');
			return ;
		}

		$categoryid = $f3->get('PARAMS.3');
		$categoryid = \Common::cleanInput($categoryid); // Sanitize user input
		$category = $this->Model->Categories->fetchById($categoryid);
		$category->erase();

		//Delete links		
		$links = $this->Model->Post_Categories->fetchAll(array('category_id' => $categoryid));
		foreach ($links as $link) {
			$link->erase();
		}

		\StatusMessage::add('Category deleted successfully', 'success');
		return $f3->reroute('/admin/category');
	}

	public function edit($f3)
	{
		$categoryid = $f3->get('PARAMS.3');
		$categoryid = \Common::cleanInput($categoryid); // Sanitize user input
		$category = $this->Model->Categories->fetchById($categoryid);
		$csrf_token = null; // Initialize empty CSRF token

		// Check numeric id supplied and is a valid category
		if (!ctype_digit($categoryid) || !$category) {
			\StatusMessage::add('Cannot find category to edit.', 'danger');
			return $f3->reroute('/admin/category');
		}

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/categoty/');
			}

			$category->title = $this->request->data['title'];
			$category->save();
			\StatusMessage::add('Category updated successfully', 'success');
			return $f3->reroute('/admin/category');
		}
		$f3->set('category', $category);
	}


}

?>