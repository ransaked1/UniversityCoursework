<?php

class StatusMessage
{

	/** Add a message to the queue of messages to be displayed to the user */
	public static function add($message, $type = 'info')
	{
		$f3 = Base::instance();
		$messages = $f3->get('SESSION.messages');
		$messages[] = array('type' => $type, 'message' => $message);
		$f3->set('SESSION.messages', $messages);
	}

	/** Get currently queued messages only, leave in queue */
	public static function peek()
	{
		$f3 = Base::instance();
		if (!$f3->exists('SESSION.messages')) {
			return array();
		}
		$messages = $f3->get('SESSION.messages');
		return $messages;
	}

	/** Get currently queued messages and reset message queue */
	public static function get()
	{
		$f3 = Base::instance();
		if (!$f3->exists('SESSION.messages')) {
			return array();
		}
		$messages = $f3->get('SESSION.messages');
		$f3->set('SESSION.messages', array());
		return $messages;
	}

}

?>