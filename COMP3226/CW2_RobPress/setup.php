<?php

error_reporting(E_ALL);
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);

echo '<p>Fixing folders</p>';
if (!file_exists("./tmp")) {
	mkdir("tmp");
}

echo '<p>Fixing permissions</p>';
`chmod 0755 ./`;
`chmod -R u+rwx ./`;
`find ./ -type d -exec chmod 0755 {} +`;
`find ./ -type f -exec chmod 0644 {} +`;
`chmod -R a+rwX ./tmp ./uploads ./pages`;

echo '<p>Permissions fixed</p>';

if (!file_exists('config/db.cfg')) {
	echo '<p>It appears you have lost your database configuration file and password. Please contact Oli</p>';
}

?>