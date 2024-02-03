<?php
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

//Define homepage 
$f3->route('GET /', 'Blog->index');

//Define admin routes
$f3->route('GET|POST /admin', 'Admin\Admin->index');
$f3->route('GET|POST /admin/@controller', 'Admin\@controller->index');
$f3->route('GET|POST /admin/@controller/@action', 'Admin\@controller->@action');
$f3->route('GET|POST /admin/@controller/@action/*', 'Admin\@controller->@action');

//Define default routes
$f3->route('GET|POST /@controller', '@controller->index');
$f3->route('GET|POST /@controller/@action', '@controller->@action');
$f3->route('GET|POST /@controller/@action/*', '@controller->@action');
$f3->route('GET /page/@pagename', 'Page->display');

//Define API 
$f3->route('GET|POST /api/@model', 'API->display');
$f3->route('GET|POST /api/@model/@id', 'API->display');

//Run!
$f3->run();

?>