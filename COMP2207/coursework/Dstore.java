import java.io.*;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class Dstore {

	static String currentFileName;
	static int currentFileSize;
	static String fileFolder;
	static String fileToSend;

	private static DLogger logger;

	public static void main(String[] args) {

		final int port;
		final int controllerPort;
		final int timeout;
		final String folder;

		try {
			port = Integer.parseInt(args[0]);
			controllerPort = Integer.parseInt(args[1]);
			timeout = Integer.parseInt(args[2]);
			folder = args[3];
		} catch (NumberFormatException e) {
			System.err.println("Invalid input format. Please provide integer values for arguments.");
			return;
		} catch (ArrayIndexOutOfBoundsException e) {
			System.err.println("Invalid number of arguments. Please provide 4 integer values for arguments.");
			return;
		}

		// Initialize logger
		logger = new DLogger(Logger.LoggingType.TERMINAL, port);

		// Set data store folder
		setFolder(folder);

		// Initialize data store folder if necessary
		initFolder(folder);

		try (ServerSocket serverSocket = new ServerSocket(port);
				 Socket controller = new Socket(InetAddress.getLoopbackAddress(), controllerPort);
				 PrintWriter controllerPrintWriter = new PrintWriter(controller.getOutputStream(), true);) {

			// Log data store is listening on the specified port and send a JOIN message to controller
			logger.log("Data Store setup on port " + port);
			controllerPrintWriter.println(String.format("%s %d", Protocol.JOIN_TOKEN, port));

			//System.out.println(controller);
			//System.out.println(serverSocket);

			// Wait for client connections and start a new thread to handle each connection
			while (true) {
				Socket client = serverSocket.accept();
				//logger.log(String.valueOf(client.getPort()));
				new Thread(() -> handleClient(client, controllerPrintWriter, timeout)).start();
			}
		} catch (Exception e) {
			logger.log("Error: " + e.getMessage());
		}
	}

	/**
	 * Handles client connections and processes incoming messages.
	 *
	 * @param client                the client socket connection
	 * @param controllerPrintWriter the PrintWriter to send messages to the controller
	 * @param timeout               the timeout for the socket connection
	 */
	private static void handleClient(Socket client, PrintWriter controllerPrintWriter, int timeout) {
		while (true) {
			try {
				var dataStoreOutput = client.getOutputStream();
				var printWriter = new PrintWriter(dataStoreOutput, true);
				var in = new BufferedReader(new InputStreamReader(client.getInputStream()));

				String line;
				while ((line = in.readLine()) != null) {
					String[] contents = line.split(" ");
					String command = contents[0];

					logger.messageReceived(client, line);
					switch (command) {
						case Protocol.STORE_TOKEN -> {
							// Parse port number and receive file metadata from client
							var port = Integer.parseInt(contents[2]);
							receiveStore(client, printWriter, contents[1], port);
							// Set socket timeout and receive file content from client
							client.setSoTimeout(timeout);
							receiveFileContent(client, controllerPrintWriter, client.getInputStream(), currentFileSize, true);
						}
						case Protocol.LIST_TOKEN -> listFiles(client, controllerPrintWriter);
						case Protocol.LOAD_DATA_TOKEN -> {
							client.setSoTimeout(timeout);
							load(contents[1], dataStoreOutput);
						}
						case Protocol.REMOVE_TOKEN -> removeFile(client, contents[1], controllerPrintWriter);
						case Protocol.REBALANCE_TOKEN -> {
							// Extract remaining rebalance from command
							var remainingRebalance = line.split(" ", 2)[1];
							handleRebalance(client, controllerPrintWriter, remainingRebalance);
						}
						case Protocol.REBALANCE_STORE_TOKEN -> {
							var port = Integer.parseInt(contents[2]);
							receiveStore(client, printWriter, contents[1], port);
							receiveFileContent(client, printWriter, client.getInputStream(), currentFileSize, false);
						}
						case Protocol.ACK_TOKEN -> sendFileContent(dataStoreOutput);
						default -> logger.log("Unidentified message: " + line);
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	// Initializes the folder at the given file path by creating it if it doesn't exist, and clearing its contents if it already exists.
	private static void initFolder(String dir) {
		// Create a Path object for the directory
		Path directoryPath = Paths.get(dir);

		// Attempt to create the directory if it doesn't exist
		try {
			Files.createDirectories(directoryPath);
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Check if the directory already contains files or subdirectories.
		try (Stream<Path> stream = Files.list(directoryPath)) {
			// If the directory is not empty, clear its contents.
			if (stream.findAny().isPresent()) {
				clearDirectory(directoryPath.toFile());
				logger.log("Directory reset!");
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Recursively deletes all files in the given directory.
	 *
	 * @param dir the directory to clear
	 * @throws IllegalArgumentException if the given file is not a directory
	 * @throws RuntimeException         if failed to list the files in the directory
	 */
	private static void clearDirectory(File dir) {
		// check if the given file is a directory
		if (!dir.isDirectory()) {
			throw new IllegalArgumentException("Not a directory: " + dir);
		}

		// list all files in the directory
		File[] files = dir.listFiles();
		if (files == null) {
			throw new RuntimeException("Failed to list files in directory: " + dir);
		}

		// delete each file in the directory
		for (File file : files) {
			try {
				Files.delete(file.toPath());
			} catch (IOException e) {
				logger.log("Failed to delete file: " + file + ": " + e.getMessage());
			}
		}
	}

	/**
	 * Receives a store request and sends an ACK back to the client.
	 *
	 * @param printWriter the PrintWriter used to send the ACK
	 * @param fileName    the name of the file being stored
	 * @param fileSize    the size of the file being stored
	 */
	private static synchronized void receiveStore(Socket client, PrintWriter printWriter, String fileName, int fileSize) {
		try {
			// Save the current file name and size
			currentFileName = fileName;
			currentFileSize = fileSize;

			Path filePath = Paths.get(fileFolder, currentFileName);

//			System.out.println(filePath);
//			if (Files.exists(filePath)) {
//				logger.log("File already exists, don't write: " + currentFileName);
//				return;
//			}

			// Send an ACK back to the client
			printWriter.println(Protocol.ACK_TOKEN);
			logger.messageSent(client, Protocol.ACK_TOKEN);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Receives the contents of a file from an input stream and stores it in a file
	 * with the current file name.
	 *
	 * @param printWriter  the PrintWriter used to send messages back to the client
	 * @param inputStream  the InputStream used to read the file content
	 * @param fileSize     the size of the file content in bytes
	 * @param storeAckFlag a flag indicating whether to send a store acknowledgement back to the client
	 */
	private static synchronized void receiveFileContent(Socket client, PrintWriter printWriter, InputStream inputStream, int fileSize, boolean storeAckFlag) {
		try {
			// Read the file content from the input stream into a byte array
			byte[] data = new byte[fileSize];
			inputStream.readNBytes(data, 0, fileSize);

			// Write the file content to a file with the current file name
			Path filePath = Paths.get(fileFolder, currentFileName);

//			System.out.println(filePath);
//			if (Files.exists(filePath)) {
//				logger.log("File already exists, don't write: " + currentFileName);
//				return;
//			}

			Files.createDirectories(filePath.getParent());
			Files.write(filePath, data);

			// Send a store acknowledgement back to the client if requested
			if (storeAckFlag) {
				printWriter.println(Protocol.STORE_ACK_TOKEN + " " + currentFileName);
				logger.messageSent(client, Protocol.STORE_ACK_TOKEN + " " + currentFileName);
			}

			logger.log("File successfully stored: " + currentFileName);
		} catch (IOException e) {
			System.err.println("Error storing file: " + e.getMessage());
		}
	}

	/**
	 * Load the contents of a file with the given name from the file system and write them to the given output stream.
	 *
	 * @param fileName the name of the file to load
	 * @param out      the output stream to write the file contents to
	 * @throws IllegalArgumentException if the fileName or out parameter is null
	 */
	private static synchronized void load(String fileName, OutputStream out) {
		try {
			// Check for invalid arguments
			if (fileName == null || out == null) {
				throw new IllegalArgumentException("Invalid arguments");
			}

			// Load the file
			logger.log("Loading file " + fileName);
			Path path = Paths.get(fileFolder, fileName);
			byte[] data = Files.readAllBytes(path);
			out.write(data);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	// Listing files in a directory and sending the list to a PrintWriter
	private static synchronized void listFiles(Socket client, PrintWriter printWriter) {
		// Create a StringBuilder to store the list of file names
		StringBuilder files = new StringBuilder();

		// Get a Path object representing the directory to list files from
		Path folderPath = Paths.get(fileFolder);

		try (DirectoryStream<Path> stream = Files.newDirectoryStream(folderPath)) {
			for (Path path : stream) {
				files.append(" ").append(path.getFileName().toString());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Send the list of file names to the PrintWriter
		printWriter.println(Protocol.LIST_TOKEN + files);
		logger.messageSent(client, Protocol.LIST_TOKEN + files);
	}

	/**
	 * Handles the rebalance operation by sending files to the appropriate nodes and removing files from nodes that are no
	 * longer responsible for them.
	 *
	 * @param controllerPrintWriter the PrintWriter for the controller node
	 * @param rebalanceInfo         a String containing information about the files to send and remove
	 */
	private static synchronized void handleRebalance(Socket client, PrintWriter controllerPrintWriter, String rebalanceInfo) {
		String[] contents = rebalanceInfo.split(" ");

		int filesToSendCounter = 0;
		int sendIndex = 1;

		// Send each file to the appropriate nodes
		while (filesToSendCounter < Integer.parseInt(contents[0])) {
			String fileName = contents[sendIndex];
			int portsCount = Integer.parseInt(contents[sendIndex + 1]);

			// Send the file to each node responsible for it
			for (int i = 1; i <= portsCount; i++) {
				int port = Integer.parseInt(contents[sendIndex + 1 + i]);
				sendFile(port, fileName);
			}

			// Increment counters and update indexes for the next file
			filesToSendCounter++;
			sendIndex = sendIndex + portsCount + 2;
		}

		// Initialize counters and index for removing files
		int removeIndex = sendIndex;
		int filesToRemoveCounter = Integer.parseInt(contents[removeIndex]);

		// Remove each file that is no longer being handled by this node
		for (int i = 1; i <= filesToRemoveCounter; i++) {
			removeFile(client, contents[removeIndex + i], null);
		}

		controllerPrintWriter.println(Protocol.REBALANCE_COMPLETE_TOKEN);
		logger.messageSent(client, Protocol.REBALANCE_COMPLETE_TOKEN);
	}

	/**
	 * Sends a file to a specified data store over a socket connection.
	 *
	 * @param port     The port number to connect to.
	 * @param fileName The name of the file to send.
	 */
	private static synchronized void sendFile(Integer port, String fileName) {
		try (Socket dataStore = new Socket(InetAddress.getLoopbackAddress(), port);
				 OutputStream out = dataStore.getOutputStream();
				 PrintWriter dataStorePrintWriter = new PrintWriter(out, true)) {

			// Get the path to the file and its size
			Path filePath = Path.of(fileFolder).resolve(fileName);
			long fileSize = Files.size(filePath);

			// Send a command and file info to the data store
			dataStorePrintWriter.println(Protocol.REBALANCE_STORE_TOKEN + " " + fileName + " " + fileSize);
			logger.messageSent(dataStore, Protocol.REBALANCE_STORE_TOKEN + " " + fileName + " " + fileSize);

			// Set the file name to send and send the file contents
			fileToSend = fileName;
			sendFileContent(out);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static synchronized void sendFileContent(OutputStream out) {
		Path filePath = Paths.get(fileFolder, fileToSend);

		try (InputStream in = Files.newInputStream(filePath); out) {
			// Read all bytes of the input stream and store them in a byte array
			byte[] data = in.readAllBytes();

			// Write the byte array to the output stream
			out.write(data);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Removes a file with the given name from the file system.
	 *
	 * @param fileName    the name of the file to remove
	 * @param printWriter a PrintWriter to write the REMOVE_ACK message to
	 */
	private static void removeFile(Socket client, String fileName, PrintWriter printWriter) {
		Path filePath = Path.of(fileFolder, fileName);

		// If the file exists, attempt to delete it
		if (Files.exists(filePath)) {
			try {
				// Delete the file
				Files.delete(filePath);

				// If a PrintWriter is provided, write a REMOVE_ACK message to it
				if (printWriter != null) {
					printWriter.println(Protocol.REMOVE_ACK_TOKEN + " " + fileName);
					logger.messageSent(client, Protocol.REMOVE_ACK_TOKEN + " " + fileName);
				}
			} catch (IOException e) {
				logger.log("Failed to delete file " + fileName + ": " + e.getMessage());
			}
		} else {
			printWriter.println(Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
			logger.messageSent(client, Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
		}
	}

	public static void setFolder(String fileFolder) {
		Dstore.fileFolder = fileFolder;
	}
}