<?php

class Errorer extends Controller
{

	public function __construct()
	{
		parent::__construct();
	}

	public function errorer($f3)
	{

		//Load settings
		$settings = Settings::getSettings();
		$settings['base'] = $f3->get('BASE');
		$settings['path'] = $f3->get('PATH');
		$this->Settings = $settings;
		$f3->set('site', $settings);
		$f3->set('title', 'Fatal Error');

		//Display error
		echo View::instance()->render('Errorer/errorer.htm');
	}

}

?>