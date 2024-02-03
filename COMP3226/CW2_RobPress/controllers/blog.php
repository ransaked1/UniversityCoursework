<?php
class Blog extends Controller
{

	public function index($f3)
	{
		if ($f3->exists('PARAMS.3')) {
			$categoryid = $f3->get('PARAMS.3');

			// Sanitize input
			$categoryid = Common::cleanInput($categoryid);

			// Changed from fetch to fetchById
			$category = $this->Model->Categories->fetchById($categoryid);
			$postlist = array_values($this->Model->Post_Categories->fetchList(array('id', 'post_id'), array('category_id' => $categoryid)));
			$posts = $this->Model->Posts->fetchAll(array('id' => $postlist, 'published' => 'IS NOT NULL'), array('order' => 'published DESC'));
			$f3->set('category', $category);
		} else {
			$posts = $this->Model->Posts->fetchPublished();
		}

		$blogs = $this->Model->map($posts, 'user_id', 'Users');
		$blogs = $this->Model->map($posts, array('post_id', 'Post_Categories', 'category_id'), 'Categories', false, $blogs);
		$f3->set('blogs', $blogs);
	}

	public function view($f3)
	{
		$id = $f3->get('PARAMS.3');

		// Sanitize input
		$id = Common::cleanInput($id);

		if (empty($id)) {
			return $f3->reroute('/');
		}

		// Changed from fetch to fetchById
		$post = $this->Model->Posts->fetchById($id);
		if (empty($post)) {
			return $f3->reroute('/');
		}

		$blog = $this->Model->map($post, 'user_id', 'Users');
		$blog = $this->Model->map($post, array('post_id', 'Post_Categories', 'category_id'), 'Categories', false, $blog);

		$comments = $this->Model->Comments->fetchAll(array('blog_id' => $id));
		$allcomments = $this->Model->map($comments, 'user_id', 'Users');

		$f3->set('comments', $allcomments);
		$f3->set('blog', $blog);
	}

	public function reset($f3)
	{
		$allposts = $this->Model->Posts->fetchAll();
		$allcategories = $this->Model->Categories->fetchAll();
		$allcomments = $this->Model->Comments->fetchAll();
		$allmaps = $this->Model->Post_Categories->fetchAll();
		foreach ($allposts as $post)
			$post->erase(); foreach ($allcategories as $cat)
			$cat->erase(); foreach ($allcomments as $com)
			$com->erase(); foreach ($allmaps as $map)
			$map->erase();
		StatusMessage::add('Blog has been reset');
		return $f3->reroute('/');
	}

	public function comment($f3)
	{
		$id = $f3->get('PARAMS.3');

		// Sanitize input
		$id = Common::cleanInput($id);
		

		// Changed from fetch to fetchById
		$post = $this->Model->Posts->fetchById($id);

		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			if (!Common::validateCsrfToken($csrf_token, $debug)) {
				StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/blog/view/' . $id);
			}

			// Validate and sanitize user inputs
			$subject = Common::cleanInput($subject);
			$message = Common::cleanInput($message);

			$comment = $this->Model->Comments;
			$comment->copyfrom('POST');
			
			// Overrite subject and message
			$comment->subject = $subject;
			$comment->message = $message;

			$comment->blog_id = $id;
			$comment->created = mydate();

			//Moderation of comments
			if (!empty($this->Settings['moderate']) && $this->Auth->user('level') < 2) {
				$comment->moderated = 0;
			} else {
				$comment->moderated = 1;
			}

			//Default subject
			if (empty($this->request->data['subject'])) {
				$comment->subject = 'RE: ' . $post->title;
			}

			$comment->save();

			//Redirect
			if ($comment->moderated == 0) {
				StatusMessage::add('Your comment has been submitted for moderation and will appear once it has been approved', 'success');
			} else {
				StatusMessage::add('Your comment has been posted', 'success');
			}
			return $f3->reroute('/blog/view/' . $id);
		}
	}

	public function moderate($f3)
	{
		list($id, $option) = explode("/", $f3->get('PARAMS.3'));
		$comments = $this->Model->Comments;
		// Changed from fetch to fetchById
		$comment = $comments->fetchById($id);

		// Get csrf token
		$csrfTokenFromURL = $f3->get('GET.csrf_token');

		$settings = $this->Model->Settings;
		$debug = $settings->getSetting('debug');

		if (!Common::validateCsrfToken($csrfTokenFromURL, $debug)) {
			StatusMessage::add('CSRF token validation failed. Refresh the page and try again.', 'danger');
			$f3->reroute('/blog/view/' . $comment->blog_id);
			return ;
		}

		$post_id = $comment->blog_id;
		//Approve
		if ($option == 1) {
			$comment->moderated = 1;
			$comment->save();
		} else {
			//Deny
			$comment->erase();
		}
		StatusMessage::add('The comment has been moderated');
		$f3->reroute('/blog/view/' . $comment->blog_id);
	}

	public function search($f3)
	{
		$csrf_token = null; // Initialize empty CSRF token

		if ($this->request->is('post')) {
			extract($this->request->data);

			// Get debug value
			$settings = $this->Model->Settings;
			$debug = $settings->getSetting('debug');

			// Check for CSRF
			if (!\Common::validateCsrfToken($csrf_token, $debug)) {
				\StatusMessage::add('CSRF token validation failed. Try again.', 'danger');
				return $f3->reroute('/blog/search/');
			}

			// Sanitize input
			$search = Common::cleanInput($search);

			$f3->set('search', $search);

			//Get search results
			$search = str_replace("*", "%", $search); //Allow * as wildcard
			// Use parameterized queries insted of directly appending user provided input
			$ids = $this->db->connection->exec("SELECT id FROM `posts` WHERE `title` LIKE :search OR `content` LIKE :search", array(':search' => "%$search%"));
			$ids = Hash::extract($ids, '{n}.id');
			if (empty($ids)) {
				StatusMessage::add('No search results found for ' . $search);
				return $f3->reroute('/blog/search');
			}

			//Load associated data
			$posts = $this->Model->Posts->fetchAll(array('id' => $ids));
			$blogs = $this->Model->map($posts, 'user_id', 'Users');
			$blogs = $this->Model->map($posts, array('post_id', 'Post_Categories', 'category_id'), 'Categories', false, $blogs);

			$f3->set('blogs', $blogs);
			$this->action = 'results';
		}
	}
}
?>