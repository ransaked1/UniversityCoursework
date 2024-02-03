<?php

namespace Admin;

class Page extends AdminController
{

	public function index($f3)
	{
		$pages = $this->Model->Pages->fetchAll();
		$f3->set('pages', $pages);
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
				return $f3->reroute('/admin/page');
			}

			$pagename = strtolower(str_replace(" ", "_", $this->request->data['title']));
			$pagename = \Common::cleanFilename($pagename); // Sanitize page name
			$this->Model->Pages->create($pagename);

			if ($pagename === '') {
				\StatusMessage::add('Cannot create page with no name', 'danger');
				return $f3->reroute('/admin/page');
			}

			\StatusMessage::add('Page created successfully', 'success');
			return $f3->reroute('/admin/page/edit/' . $pagename);
		}
	}

	public function edit($f3)
	{
		$pagename = $f3->get('PARAMS.3');
		$pagename = \Common::cleanFilename($pagename); // Sanitize page name

		// Placeholder page name to avoid issues with creating empty page names
		if ($pagename === '' | empty($pagename)) {
			$pagename = 'default';
		}

		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/admin/page/');
			}

			$pages = $this->Model->Pages;
			$pages->title = $pagename;
			$pages->content = $this->request->data['content'];
			$pages->save();

			\StatusMessage::add('Page updated successfully', 'success');
			return $f3->reroute('/admin/page');
		}

		$pagetitle = ucfirst(str_replace("_", " ", str_ireplace(".html", "", $pagename)));
		$page = $this->Model->Pages->fetch($pagename);
		$f3->set('pagetitle', $pagetitle);
		$f3->set('page', $page);
	}

	public function delete($f3)
	{
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!\Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			\StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			$f3->reroute('/admin/page');
			return ;
		}

		$pagename = $f3->get('PARAMS.3');
		$pagename = \Common::cleanFilename($pagename); // Sanitize page name
		$this->Model->Pages->delete($pagename);
		\StatusMessage::add('Page deleted successfully', 'success');
		return $f3->reroute('/admin/page');
	}

}

?>