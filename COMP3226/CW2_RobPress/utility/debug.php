<?php

class Debug
{

	public static function pagelist()
	{
		$pagedir = getcwd() . "/../pages/";
		$pages = array();
		if ($handle = opendir($pagedir)) {
			while (false !== ($file = readdir($handle))) {
				var_dump($file);
			}
		}
	}
}

Debug::pagelist();

?>