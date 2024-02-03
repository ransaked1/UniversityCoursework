<?php

class UsersModel extends GenericModel
{

	/** Update the password for a user account */
	public function setPassword($password)
	{
		$this->password = $password;
	}

}

?>