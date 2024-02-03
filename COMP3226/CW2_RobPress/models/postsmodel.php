<?php

class PostsModel extends GenericModel
{

	public function fetchPublished()
	{
		return $this->fetchAll(array('published' => 'IS NOT NULL'), array('order' => 'published DESC'));
	}

}

?>