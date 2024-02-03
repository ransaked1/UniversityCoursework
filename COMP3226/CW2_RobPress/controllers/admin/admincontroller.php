<?php

namespace Admin;

class AdminController extends \Controller
{

	protected $level;
	protected $layout = 'admin';

	public function __construct()
	{
		parent::__construct();
		$this->layout = 'admin';
		$this->level = 2; //Set default admin level required
	}

	public function beforeRoute($f3)
	{
		parent::beforeRoute($f3);

		//Check access of user
		$access = $this->Auth->user('level');

		//No access if not logged in
		if (empty($access) || $access < 2) {
			\StatusMessage::add('Access Denied', 'danger');
			return $f3->reroute('/');
		}
	}

	public function adminMenu()
	{
		return array(
			array('label' => 'Home', 'link' => 'admin', 'icon' => 'home'),
			array('label' => 'Posts', 'link' => 'admin/blog', 'icon' => 'pencil'),
			array('label' => 'Categories', 'link' => 'admin/category', 'icon' => 'folder'),
			array('label' => 'Comments', 'link' => 'admin/comment', 'icon' => 'comments'),
			array('label' => 'Pages', 'link' => 'admin/page', 'icon' => 'file'),
			array('label' => 'Users', 'link' => 'admin/user', 'icon' => 'user'),
			array('label' => 'Settings', 'link' => 'admin/settings', 'icon' => 'cog'),
		);
	}

	public function afterRoute($f3)
	{
		$f3->set('adminmenu', $this->adminMenu());
		parent::afterRoute($f3);
	}

}

?>