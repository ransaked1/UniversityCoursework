<?php

namespace Admin;

class Settings extends AdminController
{

	public function index($f3)
	{
		$settings = $this->Model->Settings->fetchAll();

		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$modelSettings = $this->Model->Settings;
			$debug = $modelSettings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				$f3->set('settings', $modelSettings);
				return;
			}

			foreach ($settings as $setting) {
				if (isset($this->request->data[$setting->setting])) {
					$setting->value = $this->request->data[$setting->setting];
					$setting->save();
				} else {
					$setting->value = 0;
					$setting->save();
				}
			}
			\StatusMessage::add('Settings updated', 'success');
		}
		$f3->set('settings', $settings);
	}

	public function clearcache($f3)
	{
		$cache = isset($this->request->data['cache']) ? getcwd() . '/' . \Common::cleanInput($this->request->data['cache']) : getcwd() . '/tmp/cache';

		$cache = str_replace(".", "", $cache);
		$this->delTree($cache);
	}

	public function delTree($dir)
	{
		$files = array_diff(scandir($dir), array('.', '..'));
		foreach ($files as $file) {
			(is_dir("$dir/$file") && !is_link($dir)) ? $this->delTree("$dir/$file") : unlink("$dir/$file");
		}
		return rmdir($dir);
	}

}


?>