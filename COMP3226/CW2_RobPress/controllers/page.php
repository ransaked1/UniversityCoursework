<?php

class Page extends Controller
{

    function display($f3)
    {
        // Get user input
        $userInput = $f3->get('PARAMS.3');

        // Sanitize user input before using it
        $pagename = $f3->clean($userInput);

        // Construct absolute file path
        $basePath = $f3->get('BASE');
        $filePath = $basePath . 'pages/' . $pagename . '.html';

        if (!ctype_alnum($pagename) || !file_exists($filePath)) {
            $f3->error('404');
            return;
        }

        $page = $this->Model->Pages->fetch($pagename);
        $pagetitle = ucfirst(str_replace("_", " ", str_replace(".html", "", $pagename)));

        $f3->set('pagetitle', $pagetitle);
        $f3->set('page', $page);
    }

}

?>