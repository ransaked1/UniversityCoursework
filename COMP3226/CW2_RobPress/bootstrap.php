<?php

//Set up logging
ini_set("log_errors", 1);
ini_set("error_log", "error.log");

//Set up session
session_save_path(getcwd() . DIRECTORY_SEPARATOR . 'tmp');

//Check for database configuration
if (!$f3->get('db')) {
	die('Unable to read database configuration. Ensure your database configuration exists and is correct');
}

//Connect to database
$dbconfig = $f3->get('db');
$db = new DB\SQL(
	'mysql:host=' . $dbconfig['server'] . ';port=3306;dbname=' . $dbconfig['name'],
	$dbconfig['username'],
	$dbconfig['password']
);
$f3->set('DB', $db);

//Enable SQL Sessions
new \DB\SQL\Session($db);

//Check for settings 
$settings = Settings::getSettings();
if ($settings['debug'] == 1) {

	//Define DEBUG mode as 1 if debug mode is enabled
	define('DEBUG', 1);
}
