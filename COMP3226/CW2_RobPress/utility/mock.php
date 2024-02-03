<?php

/** This class is used in testing and should not be modified */

/** DO NOT MODIFY THIS CLASS */
#[AllowDynamicProperties]
class Mock
{

	public $f3;
	public $request = array();
	public $messages = array();
	public $session = array();
	public $params = array();
	public $reroute = false;
	public $cookies;

	public function __construct($f3)
	{
		$this->f3 = $f3;
		$this->cookies = tempnam('/tmp', 'cookie.txt');
	}

	//Before mocking
	public function start()
	{
		//Back up session
		$this->session = $this->f3->get('SESSION');
		$this->params = $this->f3->get('PARAMS');
	}

	//After mocking
	public function done()
	{
		//Restore session
		$_SESSION = $this->session;
		$this->f3->set('SESSION', $this->session);
		$this->f3->set('PARAMS', $this->params);

		//Restore original login
		$controller = new User();
		// Supply a secure key for the helper
		$auth = new AuthHelper($controller);
		$auth->forceLogin($this->f3->get('SESSION.user'));
		$auth->setupSession($this->f3->get('SESSION.user'));

		//Restore session
		$_SESSION = $this->session;
		$this->f3->set('SESSION', $this->session);
		$this->f3->set('PARAMS', $this->params);
	}

	//Mock a route
	public function run($path, $data = array(), $get = array())
	{

		//Remove first /
		if (substr($path, 0, 1) == '/') {
			$path = substr($path, 1);
		}

		//Set default path
		if (empty($path)) {
			$path = 'blog/index';
		}

		$bits = explode("/", $path);
		$prefix = '';
		$controllerClass = $controller = array_shift($bits);
		if (strtolower($controller) == 'admin') {
			$prefix = 'admin';
			$controller = array_shift($bits);
			$controllerClass = $prefix . '\\' . $controller;
		}

		if (!empty($bits)) {
			$action = array_shift($bits);
		} else {
			$action = 'index';
		}

		$parameters = $bits;

		//Before Route
		$c = new $controllerClass;
		$c->beforeRoute($this->f3);

		//Setup data
		$c->request->data = $data;
		if (!empty($data)) {
			$c->request->type = 'POST';
			$_POST = $data;
		}
		$_GET = $get;

		//Setup route
		$old_params = $this->f3->get('PARAMS');
		$new_params = explode("/", $path);

		//Remove prefix
		if (!empty($prefix)) {
			array_shift($new_params);
		}

		//Build new path
		$new_path = strtolower($path);
		if (substr($new_path, 0, 1) != '/') {
			$new_path = "/$new_path";
		}

		array_unshift($new_params, $new_path);
		$new_params['controller'] = strtolower($controller);
		$new_params['action'] = strtolower($action);
		$this->f3->set('PARAMS', $new_params);

		//Run route function
		call_user_func(array($c, $action), $this);
		$messages = StatusMessage::peek();
		$this->messages = $messages;

		//Handle reroute
		if (!empty($this->reroute)) {
			$reroute = $this->reroute;
			$this->reroute = false;
			return $this->run($reroute);
		}

		//After Route
		ob_start();
		$c->afterRoute($this->f3);
		$output = ob_get_contents();
		ob_end_clean();

		//Restore F3
		$this->f3->set('PARAMS', $old_params);

		$this->output = $output;
		return $output;
	}

	//Disable reroute function
	public function reroute($parameters)
	{
		$this->reroute = $parameters;
		return true;
	}

	//Make a web request
	public function make_request($url)
	{
		//Get realm and remove port (due to Docker/other configs)
		$s = dirname($this->f3->get('REALM')) . '/' . $url;
		$s = preg_replace("!:[0-9]+/!", "/", $s);

		$ch = curl_init();
		curl_setopt($ch, CURLOPT_URL, $s);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		curl_setopt($ch, CURLOPT_COOKIEJAR, $this->cookies);
		curl_setopt($ch, CURLOPT_COOKIEFILE, $this->cookies);
		$output = curl_exec($ch);
		$response = curl_getinfo($ch);
		curl_close($ch);
		if ($response['http_code'] != 200) {
			return false;
		}
		return $output;
	}

	//Get a form
	public function get_form($subject)
	{
		$form = [];

		if (!preg_match('/<form/si', $subject)) {
			return false;
		}

		$lsubject = preg_replace('/^.*(<form[^>]*name=[\'"])(' . $form_name . ')([\'"])/si', '\1\2\3', $subject);
		$lsubject = preg_replace('/<\/form.*$/si', '', $lsubject);
		$lsubject = preg_replace('/^.*<form/si', '<form', $lsubject);

		preg_match("/<form([^>]*)>/", $lsubject, $matches);
		$formdata = $matches[1];
		preg_match('/action=[\'"]([^\'"]*)[\'"]/', $formdata, $matches);
		$forminfo = $matches;
		$form['_action'] = $matches[1];

		$lsubject = preg_replace('/^.*<form[^>]*>/si', '', $lsubject);
		preg_match_all('/<input[^>]*name[ ]*=[ ]*[\'"]([^\'"]*)[\'"][^>]*[ ]*value[ ]*=[ ]*[\'"]([^\'"]*)[\'"]/sim', $lsubject, $result, PREG_PATTERN_ORDER);
		for ($i = 0; $i < count($result[0]); $i++) {
			$name = $result[1][$i];
			$value = $result[2][$i];
			$form[$name] = $value;
		}
		preg_match_all('/<input[^>]*value=[\'"]([^\'"]*)[\'"][^>]*name=[\'"]([^\'"]*)[\'"]/sim', $lsubject, $result, PREG_PATTERN_ORDER);
		for ($i = 0; $i < count($result[0]); $i++) {
			$name = $result[1][$i];
			$value = $result[2][$i];
			$form[$name] = $value;
		}
		preg_match_all('/<textarea[^>]*name=[\'"]([^\'"]*)[\'"][^>]*>([^<]*)/sim', $lsubject, $result, PREG_PATTERN_ORDER);
		for ($i = 0; $i < count($result[0]); $i++) {
			$name = $result[1][$i];
			$value = $result[2][$i];
			$form[$name] = $value;
		}
		return $form;
	}

	//Use F3 normal functions for everything else
	public function __call($name, $arguments)
	{
		return call_user_func_array(array($this->f3, $name), $arguments);
	}

}
