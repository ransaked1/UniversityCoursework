<?php

class Api extends Controller
{

	public function display($f3)
	{
		extract($f3->get('PARAMS'));
		extract($f3->get('GET'));

		// Extra cleaning for api requests
		$token = filter_var(Common::cleanInput($token), FILTER_VALIDATE_INT);
		$id = filter_var(Common::cleanInput($id), FILTER_VALIDATE_INT);
		$model = filter_var(Common::cleanInput($model), FILTER_VALIDATE_INT);

		//Check for authentication token and fail without
		if (!isset($token) || $token != $f3->get('site.apikey')) {
			echo json_encode(array('error' => '403'));
			die();
		}

		//Provide API access
		if (!isset($id)) {
			$results = array();
			$result = $this->Model->$model->fetchAll();
			foreach ($result as $r) {
				$results[] = $r->cast();
			}
		} else {
			// Chnaged from fetch to fetchById
			$result = $this->Model->$model->fetchById($id);
			$results = $result->cast();
		}

		//File not found
		if (empty($results)) {
			// Sending generic error response
			http_response_code(404);
			echo json_encode(array('error' => 'Not Found'));
			die();
		}

		// Add json header
		header('Content-Type: application/json');
		echo json_encode($results);
		exit();
	}

}

?>