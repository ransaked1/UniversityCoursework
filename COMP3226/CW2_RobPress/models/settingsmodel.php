<?php

class SettingsModel extends GenericModel
{

	public function getSetting($key)
	{
		$setting = $this->fetch(array('setting' => $key));
		return $setting->value;
	}

	public function setSetting($key, $value)
	{
		$setting = $this->fetch(array('setting' => $key));
		$setting->value = $value;
		return $setting->save();
	}

}

?>