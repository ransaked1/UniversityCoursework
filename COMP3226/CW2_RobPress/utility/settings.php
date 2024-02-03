<?php

class Settings
{

	/** Get all the settings */
	public static function getSettings()
	{
		$model = new SettingsModel('Settings', new Database());
		$settings = $model->fetchList(array('setting', 'value'));
		return $settings;
	}

	/** Turn data object strings into data objects */
	public static function process($data)
	{
		return eval($data);
	}

}

?>