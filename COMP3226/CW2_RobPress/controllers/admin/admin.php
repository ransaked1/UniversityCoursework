<?php

namespace Admin;

class Admin extends AdminController
{

	public function index($f3)
	{
		$users = $this->Model->Users->fetchCount();
		$posts = $this->Model->Posts->fetchCount();
		$comments = $this->Model->Comments->fetchCount();
		$f3->set('users', $users);
		$f3->set('posts', $posts);
		$f3->set('comments', $comments);
	}

}

?>