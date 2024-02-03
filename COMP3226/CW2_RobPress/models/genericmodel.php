<?php

#[AllowDynamicProperties]
class GenericModel extends \DB\SQL\Mapper
{

	public $name;
	protected $database;

	/** Set up a new SQL mapped object */
	public function __construct($name, $db)
	{
		parent::__construct($db->connection, strtolower($name));
		$this->name = strtolower($name);
		$this->database = $db;
	}

	/** Fetch a list of all matching items */
	public function fetchList($field = 'title', $conditions = array(), $options = array())
	{
		$conditions = $this->prepare($conditions);

		//Break out query and conditions and remember PDO is numbered from 1, not 0
		if (!empty($conditions)) {
			$query = $conditions[0];
			unset($conditions[0]);
		} else {
			$query = "";
		}

		if (is_array($field)) {
			$field1 = $field[0];
			$field2 = $field[1];
		} else {
			$field1 = 'id';
			$field2 = $field;
		}
		$f1 = $this->db->quotekey($field1);
		$f2 = $this->db->quotekey($field2);

		//Handle empty conditions	
		if (empty($query)) {
			$query = "1=1";
		}

		$results = $this->db->exec("SELECT $f1,$f2 FROM $this->name WHERE " . $query, $conditions);
		$output = Hash::combine($results, '{n}.' . $field1, '{n}.' . $field2);
		return $output;
	}

	/** Fetch count matching given conditions */
	public function fetchCount($conditions = array(), $options = array())
	{
		$conditions = $this->prepare($conditions);
		return $this->count($conditions, $options);
	}

	/** Fetch all items as SQL mapped objects */
	public function fetchAll($conditions = array(), $options = array())
	{
		$conditions = $this->prepare($conditions);
		return $this->find($conditions, $options);
	}

	/** Fetch a single item as a SQL mapped object */
	public function fetch($conditions = array(), $options = array())
	{
		if (is_numeric($conditions)) {
			$conditions = array('id' => $conditions);
		}
		$conditions = $this->prepare($conditions);
		return $this->load($conditions, $options);
	}

	/** Fetch a single item as a SQL mapped object by ID */
	public function fetchById($id, $options = array())
	{
		return $this->fetch(array('id' => $id));
	}

	/** Convert conditions array into SQL query */
	protected function prepare($conditions)
	{
		if (is_array($conditions) && !empty($conditions)) {
			$result = array(0 => "");
			foreach ($conditions as $key => $value) {
				$join = "";
				if (!empty($result[0])) {
					$join = " AND ";
				}
				if (!empty($result[0])) {
					$result[0] .= $join;
				}

				//Change from IN to single if only one element
				if (is_array($value) && sizeof($value) == 1) {
					$value = array_values($value);
					$value = $value[0];
				}
				//Handle empty arrays
				if (is_array($value) && empty($value)) {
					continue;
				}

				//Handle special cases
				if (!is_array($value) && preg_match('!IS (NOT )?NULL!sim', $value)) {
					$result[0] .= "$key $value";
				}
				//Handle array of values
				else if (is_array($value)) {
					$instr = str_repeat('?,', count($value) - 1) . '?';
					$result[0] .= "$key IN ($instr)";
					foreach ($value as $v) {
						$result[] = $v;
					}
					//Handle single value
				} else {
					$result[0] .= "$key = ?";
					$result[] = $value;
				}
			}
			if (empty($result[0])) {
				return array();
			}
			return $result;
		}
		return $conditions;
	}

}

?>