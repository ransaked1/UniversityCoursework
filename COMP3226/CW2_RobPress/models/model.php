<?php

#[AllowDynamicProperties]
class Model
{

	/** Provide interface to model */
	public function __construct($controller)
	{
		$this->controller = $controller;
		$this->db = $controller->db;
	}

	/** Provide access to models in the database */
	public function __get($name)
	{
		$modelclass = $name . "Model";
		if (class_exists($modelclass)) {
			$this->name = new $modelclass($name, $this->db);
		} else {
			$this->name = new GenericModel($name, $this->db);
		}
		return $this->name;
	}

	/** Provide mapping functionality to simulate joins */
	public function map($objects, $field, $model, $single = true, $previous = array())
	{
		$cache = array();
		$results = array();
		if (!is_array($objects)) {
			//Single object
			$object = $objects;
			if (isset($previous)) {
				$result = $previous;
				$result[ucfirst($object->name)] = $object;
			} else {
				$result = array(ucfirst($object->name) => $object);
			}
			if (is_array($field)) {
				list($key1, $link, $key2) = $field;
				$result[$link] = $this->$link->fetchAll(array($key1 => $object['id']));
				if (sizeof($result[$link]) == 0) {
					$result[$model] = array();
				}
				foreach ($result[$link] as $linked) {
					if (isset($cache[$linked->$key2])) {
						$result[$model][] = $cache[$linked->$key2];
					} else {
						$cache[$linked->$key2] = $result[$model][] = $this->$model->fetch($linked->$key2);
					}
				}
			} else {
				$result[$model] = $this->$model->fetch($object[$field]);
			}
			return $result;
		} else {
			//Multiple objects
			foreach ($objects as $i => $object) {
				if (isset($previous[$i])) {
					$result = $previous[$i];
					$result[ucfirst($object->name)] = $object;
				} else {
					$result = array(ucfirst($object->name) => $object);
				}

				//Link table
				if (is_array($field)) {
					list($key1, $link, $key2) = $field;
					$result[$link] = $this->$link->fetchAll(array($key1 => $object['id']));
					if (sizeof($result[$link]) == 0) {
						$result[$model] = array();
					}
					foreach ($result[$link] as $linked) {
						if (isset($cache[$linked->$key2])) {
							$result[$model][] = $cache[$linked->$key2];
						} else {
							$cache[$linked->$key2] = $result[$model][] = $this->$model->fetch($linked->$key2);
						}
					}
				}
				//Single
				else if ($single) {
					if (isset($cache[$object[$field]])) {
						$result[$model] = $cache[$object[$field]];
					} else {
						$cache[$object[$field]] = $result[$model] = $this->$model->fetch($object[$field]);
					}
					//Multiple
				} else {
					$result[$model] = $this->$model->fetchAll(array($field => $object['id']));
				}
				$results[] = $result;
			}
			return $results;
		}
	}

}

?>