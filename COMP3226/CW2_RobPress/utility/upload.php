<?php

//Move up a folder
chdir('..');

//Load framework
require 'vendor/autoload.php';
$f3 = \Base::instance();
$f3->config('config/config.cfg');
$f3->set('AUTOLOAD', 'controllers/; models/; helpers/; utility/;');

//Load configuration
$f3->config('config/db.cfg');

//Load global functions
include_once("bootstrap.php");
include_once("functions.php");

//Get editor details
$CKEditor = $_GET['CKEditor'];
$funcNum = $_GET['CKEditorFuncNum'];

//Initialise return values
$url = '';
$message = '';

//Process uploaded file
if (isset($_FILES['upload'])) {
	$url = File::Upload($_FILES['upload']);
	if (!$url) {
		$message = 'The upload failed';
	}
} else {
	$message = 'No file was uploaded';
}

//Return to CKEditor
echo "<script type='text/javascript'> window.parent.CKEDITOR.tools.callFunction($funcNum, '$url', '$message')</script>";

?>