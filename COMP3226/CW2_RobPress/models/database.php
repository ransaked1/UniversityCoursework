<?php

class Database
{

	public static $singleton;
	public $connection;

	/** Return the single database connection */
	public static function getConnection()
	{
		$f3 = Base::instance();
		return $f3->get('DB');
	}

	/** Create a new database object */
	public function __construct()
	{
		$this->connection = self::getConnection();
	}

	/** Perform a direct database query */
	public function query($sql)
	{
		$result = $this->connection->exec($sql);
		return $result;
	}

}

?>