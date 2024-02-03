<?php

class File
{

	public static function Upload($array, $local = false)
	{
		$f3 = Base::instance();
		extract($array);

		// Validate files
		if (!self::validateFileUpload($array)) {
			return false;
		}

		$directory = getcwd() . '/uploads';

		// Generate a unique file name
		$name = self::generateUniqueFileName($name);

		$destination = $directory . '/' . $name;
		$webdest = '/uploads/' . $name;

		// Local files get moved
		if ($local) {
			if (copy($tmp_name, $destination)) {
				chmod($destination, 0666);
				return $webdest;
			} else {
				return false;
			}
		} else {
			// POSTed files are done with move_uploaded_file
			if (move_uploaded_file($tmp_name, $destination)) {
				chmod($destination, 0666);
				return $webdest;
			} else {
				return false;
			}
		}
	}

	public static function validateFileUpload($file)
	{
		// Validate file type, mime and extension
		$allowedTypes = ['image/jpeg', 'image/png', 'image/gif'];
		$allowedExtensions = ['jpg', 'jpeg', 'png', 'gif'];

		// Check finfo can be used too
		if (function_exists('finfo_open')) {
			$finfo = finfo_open(FILEINFO_MIME_TYPE);
			$actualMimeType = finfo_file($finfo, $file['tmp_name']);
			finfo_close($finfo);

			// Check file type
			if (!in_array($actualMimeType, $allowedTypes)) {
				\StatusMessage::add('Invalid file type! Please upload a JPEG, PNG, or GIF image.', 'error');
				return false;
			}
		}

		// Check mime type
		if (!in_array($file['type'], $allowedTypes)) {
			\StatusMessage::add('Invalid file type! Please upload a JPEG, PNG, or GIF image.', 'error');
			return false;
		}

		// Check extension
		$fileExtension = strtolower(pathinfo($file['name'], PATHINFO_EXTENSION));
		if (!in_array($fileExtension, $allowedExtensions)) {
			\StatusMessage::add('Invalid file extension! Please upload an image with a valid extension.', 'error');
			return false;
		}

		// Validate file size
		$maxFileSize = 10 * 1024 * 1024; // 10 MB
		if ($file['size'] > $maxFileSize) {
			\StatusMessage::add('File size exceeds the 10 MB limit.', 'error');
			return false;
		}

		return true;
	}

	private static function generateUniqueFileName($filename)
	{
		// Sanitize filename
		$sanitizedFilename = Common::cleanFilename($filename);

		// Limit the length of the generated filename
		$limitedFilename = substr($sanitizedFilename, 0, 50);

		// Append a timestamp to the original file name
		$timestamp = time();

		$uniqueName = $timestamp . '_' . $limitedFilename;

		return $uniqueName;
	}

}

?>