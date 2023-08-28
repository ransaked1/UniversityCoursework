import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

class Controller {

	static Index index;
	static Map<Integer, Socket> dataStoreSocketList;
	static Map<String, ArrayList<Integer>> fileList;
	static Map<String, PrintWriter> fileStorePrintWriter;
	static Map<String, PrintWriter> fileRemovePrintWriter;

	static int rebalanceCounter;
	static int rebalanceCompleteCounter;
	static int replicationFactor;
	static int completedStores;
	static int loadPortCounter;

	private static CLogger logger;

	private static void init() {
		index = new Index(new ArrayList<>(), new ConcurrentHashMap<>());
		fileList = new ConcurrentHashMap<>();
		dataStoreSocketList = new ConcurrentHashMap<>();
		fileStorePrintWriter = new ConcurrentHashMap<>();
		fileRemovePrintWriter = new ConcurrentHashMap<>();

		rebalanceCounter = 0;
		rebalanceCompleteCounter = 0;
		replicationFactor = 0;
		completedStores = 0;
		loadPortCounter = 0;
	}

	public static void main(String[] args) {

		Controller controller = new Controller();

		final int controllerPort;
		final int replicationFactor;
		int timeout;
		int rebalancePeriod;

		// Initialize method variables
		init();

		// Initialize logger
		logger = new CLogger(Logger.LoggingType.TERMINAL);

		// Get values for user input arguments
		try {
			controllerPort = Integer.parseInt(args[0]);
			replicationFactor = Integer.parseInt(args[1]);
			timeout = Integer.parseInt(args[2]);
			rebalancePeriod = Integer.parseInt(args[3]);
		} catch (NumberFormatException e) {
			logger.log("Invalid input format. Please provide integer values for arguments.");
			return;
		} catch (ArrayIndexOutOfBoundsException e) {
			logger.log("Invalid number of arguments. Please provide 4 integer values for arguments.");
			return;
		}

		// Set the replication factor and clear data store list
		setReplicationFactor(replicationFactor);
		index.reset();

		// Create thread pool with fixed number of threads
		ExecutorService pool = Executors.newFixedThreadPool(10);

		// Start rebalance period thread
		controller.startRebalanceTimer(rebalancePeriod * 1000);

		try {
			var serverSocket = new ServerSocket(controllerPort);
			logger.log("Controller setup on port " + controllerPort);

			// Loop indefinitely to handle incoming client connections
			while (true) {
				try {
					// Accept the client and create a new thread to handle the request
					Socket client = serverSocket.accept();
					pool.submit(() -> controller.handleClient(client, timeout));
				} catch (Exception e) {
					logger.log("Error handling client connection: " + e.getMessage());
				}
			}
		} catch (Exception e) {
			logger.log("Error starting server socket: " + e.getMessage());
		}
	}

	private void handleClient(Socket client, int timeout) {
		int currentDataStorePort = 0;
		boolean isDataStore = false; // flag indicating if the client is a datastore
		String line;

		try (client;
				 var out = new PrintWriter(client.getOutputStream(), true);
				 var in = new BufferedReader(new InputStreamReader(client.getInputStream()))) {

			// read lines from the client
			while ((line = in.readLine()) != null) {
				String[] contents = line.split(" ");
				String command = contents[0]; // the first element is the command
				logger.messageReceived(client, line);

				switch (command) {
					case Protocol.JOIN_TOKEN -> {
						isDataStore = true;
						currentDataStorePort = Integer.parseInt(contents[1]);
						addDataStore(client, Integer.parseInt(contents[1]));
						rebalance();
					}
					case Protocol.LIST_TOKEN -> {
						if (isDataStore) { // if the client is a datastore
							handleDataStoreList(currentDataStorePort, contents);
						} else { // if the client is not a datastore
							if (checkEnoughDataStores(client, out)) {
								handleClientList(client, out);
							}
						}
					}
					case Protocol.STORE_TOKEN -> {
						if (checkEnoughDataStores(client, out)) {
							CountDownLatch storeLatch = new CountDownLatch(1);
							handleStore(client, out, contents, storeLatch);
							boolean storeAckReceived = storeLatch.await(timeout, TimeUnit.MILLISECONDS);
							if (!storeAckReceived) {
								logger.log("STORE_ACK not received within timeout");
							}
						}
					}
					case Protocol.STORE_ACK_TOKEN -> handleStoreAck(client, currentDataStorePort, contents);
					case Protocol.LOAD_TOKEN -> {
						if (checkEnoughDataStores(client, out)) {
							handleLoad(client, contents, 1, out);
						}
					}
					case Protocol.RELOAD_TOKEN -> handleReload(client, out, contents);
					case Protocol.REMOVE_TOKEN -> {
						if (checkEnoughDataStores(client, out)) {
							CountDownLatch removeLatch = new CountDownLatch(1);
							handleRemove(client, out, contents, removeLatch);
							boolean storeAckReceived = removeLatch.await(timeout, TimeUnit.MILLISECONDS);
							if (!storeAckReceived) {
								logger.log("REMOVE_ACK not received within timeout");
							}
						}
					}
					case Protocol.REMOVE_ACK_TOKEN -> {
						if (checkEnoughDataStores(client, out)) {
							handleRemoveAck(client, currentDataStorePort, contents);
						}
					}
					case Protocol.REBALANCE_COMPLETE_TOKEN -> handleRebalanceComplete(client, out);
					default -> logger.log("Unidentified message: " + line);
				}
			}
		} catch (SocketException e) {
			logger.log("SocketException occurred. Failed Data Store will be fixed: " + e.getMessage());
			fixFailedDataStore(currentDataStorePort);
		} catch (IOException | InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void handleClientList(Socket client, PrintWriter out) {
		listFiles(client, out);
	}

	private void handleRebalanceComplete(Socket client, PrintWriter out) {
		out.println(Protocol.LIST_TOKEN);
		logger.messageSent(client, Protocol.LIST_TOKEN);

		setRebalanceCompleteCounter(getRebalanceCompleteCounter() - 1);
		if (getRebalanceCompleteCounter() == 0) {
			updateIndexAfterRebalance();
		}
	}

	private void handleRemoveAck(Socket client, int currentPort, String[] contents) {
		if (index.fileStatus.get(contents[1]) == Index.STATUS_REMOVING) {
			removeComplete(client, currentPort, fileRemovePrintWriter.get(contents[1]), contents[1]);
		}
	}

	private void handleRemove(Socket client, PrintWriter out, String[] contents, CountDownLatch removeLatch) {
		try {
			if (index.fileNames.contains(contents[1])) {
				fileRemovePrintWriter.put(contents[1], out);
			}
			remove(client, out, contents[1]);
			removeLatch.countDown();
		} catch (Exception e) {
			logger.log("Remove handling failed");
		}
	}

	private void handleReload(Socket client, PrintWriter out, String[] contents) {
		// Check node has been processed
		// If yes, don't send request and return an error
		if (getLoadPortCounter() == fileList.get(contents[1]).size() - 1) {
			out.println(Protocol.ERROR_LOAD_TOKEN);
		} else if (checkEnoughDataStores(client, out)) { // If no, send request to next node
			handleLoad(client, contents, getLoadPortCounter() + 1, out);
		}
	}

	private void handleLoad(Socket client, String[] contents, int loadPortCounter, PrintWriter out) {
		if (index.fileNames.contains(contents[1])) {
			setLoadPortCounter(loadPortCounter);
			load(out, contents[1], getLoadPortCounter());
		} else {
			out.println(Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
			logger.messageSent(client, Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
		}
	}

	private void handleStoreAck(Socket client, int currentPort, String[] contents) {
		if (index.fileStatus.get(contents[1]) == Index.STATUS_STORING) {
			storeComplete(client, currentPort, fileStorePrintWriter.get(contents[1]), getCompletedStores() + 1, contents[1]);
		}
	}

	private synchronized void storeComplete(Socket client, int dataStorePort, PrintWriter printWriter, int completedStores, String fileName) {
		// Get the list of data stores that have this file
		List<Integer> fileList = Controller.fileList.computeIfAbsent(fileName, k -> new ArrayList<>());

		// Add the data store port to the file list if it isn't already there
		if (!fileList.contains(dataStorePort)) {
			fileList.add(dataStorePort);
		}

		// If number of completed stores less than the replication factor, increment the number of completed stores and return
		if (!(completedStores == getReplicationFactor())) {
			setCompletedStores(getCompletedStores() + 1);
			return;
		}

		// Add the file to the index
		index.fileNames.add(fileName);

		// Reset the number of completed stores to 0
		setCompletedStores(0);

		// Set the file status to stored and send a STORE_COMPLETE message to the data store
		index.fileStatus.put(fileName, Index.STATUS_STORED);
		printWriter.println(Protocol.STORE_COMPLETE_TOKEN);
		logger.messageSent(client, Protocol.STORE_COMPLETE_TOKEN);
	}

	private void handleStore(Socket client, PrintWriter out, String[] contents, CountDownLatch storeLatch) {
		try {
			if (!index.fileStatus.containsKey(contents[1])) {
				fileStorePrintWriter.put(contents[1], out);
			}
			store(client, out, replicationFactor, contents[1], Integer.parseInt(contents[2]));
			storeLatch.countDown();
		} catch (Exception e) {
			logger.log("Store handling failed");
		}
	}

	private void handleDataStoreList(int port, String[] contents) {
		rebalanceCounter++;
		logger.log("Data Store LIST");
		ArrayList<String> fileNames = new ArrayList<>(Arrays.asList(contents).subList(1, contents.length));
		updateHashMap(port, fileNames);
	}

	private synchronized void updateHashMap(Integer port, ArrayList<String> fileNames) {
		if (!fileList.isEmpty()) {
			for (String fileName : fileNames) {
				if (fileList.containsKey(fileName)) {
					fileList.get(fileName).add(port);
				}
			}
		}
		// Check and calculate rebalances
		if (rebalanceCounter >= dataStoreSocketList.size()) {
			//System.out.println(getDataStoreHashMap());
			rebalanceCounter = 0;
			calculateRebalances();
		}
	}

	private synchronized boolean checkEnoughDataStores(Socket client, PrintWriter printWriter) {
		if (dataStoreSocketList.size() >= replicationFactor) {
			return true;
		}
		printWriter.println(Protocol.ERROR_NOT_ENOUGH_DSTORES_TOKEN);
		logger.messageSent(client, Protocol.ERROR_NOT_ENOUGH_DSTORES_TOKEN);
		return false;
	}

	private synchronized void addDataStore(Socket dataStore, Integer port) throws IOException {
		//System.out.println(dataStore);
		dataStoreSocketList.put(port, new Socket(InetAddress.getLoopbackAddress(), port));
		logger.dstoreJoined(dataStore, port.toString());
	}

	private synchronized void listFiles(Socket client, PrintWriter printWriter) {
		StringBuilder fileNames = new StringBuilder();
		if (index.getFileNames().size() > 0) {
			for (String file : index.getFileNames()) {
				if (index.fileStatus.get(file) == Index.STATUS_STORED)
					fileNames.append(" ").append(file);
			}
		}
		printWriter.println(Protocol.LIST_TOKEN + fileNames);
		logger.messageSent(client, Protocol.LIST_TOKEN + fileNames);
	}

	private synchronized void store(Socket clientSocket, PrintWriter printWriter, int replicationFactor, String fileName, int fileSize) {
		// Check if the file is already being stored
		if (!index.fileStatus.containsKey(fileName)) {
			// If not, add it to the file list and set its status to storing
			ArrayList<Integer> tmp = new ArrayList<>();
			tmp.add(fileSize);
			fileList.put(fileName, tmp);
			index.fileStatus.put(fileName, Index.STATUS_STORING);

			// Create a string of data store ports to send to the client
			StringBuilder portsBuilder = new StringBuilder();
			int currentReplicationFactor = 0;
			for (Integer key : dataStoreSocketList.keySet()) {
				if (currentReplicationFactor < replicationFactor) {
					portsBuilder.append(" ").append(key);
					currentReplicationFactor++;
				}
			}
			String ports = portsBuilder.toString();

			// Send the list of data store ports to the client
			printWriter.println(Protocol.STORE_TO_TOKEN + ports);
			logger.messageSent(clientSocket, Protocol.STORE_TO_TOKEN + ports);
		} else {
			if (index.fileStatus.get(fileName) == Index.STATUS_REMOVED) {
				printWriter.println(Protocol.STORE_TO_TOKEN);
				logger.messageSent(clientSocket, Protocol.STORE_TO_TOKEN);
			}

			printWriter.println(Protocol.ERROR_FILE_ALREADY_EXISTS_TOKEN);
			logger.messageSent(clientSocket, Protocol.ERROR_FILE_ALREADY_EXISTS_TOKEN);
		}
	}

	private synchronized void load(PrintWriter printWriter, String fileName, int loadPortCounter) {
		for (String key : fileList.keySet()) {
			// Check if the key is equal to the given file name and the index contains the file name
			if (key.equals(fileName) && index.fileNames.contains(key)) {
				int fileSize = fileList.get(fileName).get(0);
				int port = fileList.get(fileName).get(loadPortCounter);
				// Send response to print writer with load port and file size
				printWriter.println(Protocol.LOAD_FROM_TOKEN + " " + port + " " + fileSize);
				logger.log("Loading from " + port + " with size " + fileSize);
			}
		}
	}

	private synchronized void remove(Socket client, PrintWriter printWriter, String fileName) {
		// Check if the file exists in the index
		if (!index.fileNames.contains(fileName)) {
			printWriter.println(Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
			logger.messageSent(client, Protocol.ERROR_FILE_DOES_NOT_EXIST_TOKEN);
			return;
		}

		// Update the file status in the index to removing
		index.fileStatus.remove(fileName);
		index.fileStatus.put(fileName, Index.STATUS_REMOVING);
		logger.log("Remove in progress");

		// If the file is stored in the file list send a remove message to other data stores
		if (fileList.containsKey(fileName)) {
			ArrayList<Integer> dataStores = fileList.get(fileName);
			for (int i = 1; i < dataStores.size(); i++) {
				sendRemove(dataStores.get(i), fileName);
			}
		}
	}

	private synchronized void sendRemove(Integer port, String fileName) {
		try {
			Socket dataStore = dataStoreSocketList.get(port);
			PrintWriter dataStorePrintWriter = new PrintWriter(dataStore.getOutputStream(), true);

			dataStorePrintWriter.println(Protocol.REMOVE_TOKEN + " " + fileName);
			logger.messageSent(dataStore, Protocol.REMOVE_TOKEN + " " + fileName);
		} catch (IOException e) {
			logger.log("Failed to send remove request to port " + port + ": " + e.getMessage());
		}
	}

	private synchronized void removeComplete(Socket client, Integer port, PrintWriter printWriter, String fileName) {
		logger.log(String.valueOf(fileList.get(fileName).size() - 1));

		// Check if there are remaining ports that have the file
		if (fileList.get(fileName).size() - 1 > 1) {
			// If there are remaining ports remove the given port from the list and return
			fileList.get(fileName).remove(port);
			return;
		}

		// If all ports have been removed, remove the file from the index
		index.fileNames.remove(fileName);
		//setCompletedDeletes(0);
		fileList.remove(fileName);

		// Update the file status in the index to removed
		index.fileStatus.remove(fileName);
		index.fileStatus.put(fileName, Index.STATUS_REMOVED);

		// Notify client remove has completed successfully
		printWriter.println(Protocol.REMOVE_COMPLETE_TOKEN);
		logger.messageSent(client, Protocol.REMOVE_COMPLETE_TOKEN);
	}

	private synchronized void startRebalanceTimer(int rebalanceTime) {
		Timer timer = new Timer();
		timer.scheduleAtFixedRate(new TimerTask() {
			@Override
			public void run() {
				rebalance();
			}
		}, rebalanceTime, rebalanceTime);
	}

	private synchronized void rebalance() {
		// Make ensure that the hash map is up-to-date
		updateHashMap();

		// Iterate over the dataStoreSocketList and send LIST message to each data store
		for (Integer dataStorePort : dataStoreSocketList.keySet()) {
			try {
				Socket dataStore = dataStoreSocketList.get(dataStorePort);
				//System.out.println(dataStore);
				PrintWriter dataStorePrintWriter = new PrintWriter(dataStore.getOutputStream(), true);
				dataStorePrintWriter.println(Protocol.LIST_TOKEN);
				logger.messageSent(dataStore, Protocol.LIST_TOKEN);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private synchronized void updateHashMap() {
		for (Map.Entry<String, ArrayList<Integer>> entry : fileList.entrySet()) {
			int size = entry.getValue().get(0);
			entry.getValue().set(0, size);
		}
	}

	private synchronized void calculateRebalances() {
		// Get the HashMap of data stores and their corresponding files
		HashMap<Integer, ArrayList<String>> dataStoreHashMap = getDataStoreHashMap();

		// Calculate the minimum and maximum replication factor
		double minReplication = Math.floor(replicationFactor * fileList.size() / (double) dataStoreSocketList.size());
		double maxReplication = Math.ceil(replicationFactor * fileList.size() / (double) dataStoreSocketList.size());

		// Initialize HashMaps to store files to be stored, removed, and sent to other data stores
		HashMap<Integer, ArrayList<String>> filesToStoreHashMap = new HashMap<>();
		HashMap<Integer, ArrayList<String>> filesToRemoveHashMap = removeFiles(index.fileStatus);
		HashMap<String, ArrayList<Integer>> filesToSendHashMap = new HashMap<>();

		// Loop until replication is complete
		while (!isReplicationComplete(dataStoreHashMap, replicationFactor, minReplication, maxReplication)) {
			// Loop until data store with maximum replication factor reaches the maximum required
			reachMaxReplicationStep(dataStoreHashMap, maxReplication, filesToStoreHashMap, filesToSendHashMap);

			// If replication is complete exit the loop
			if (isReplicationComplete(dataStoreHashMap, replicationFactor, minReplication, maxReplication)) {
				break;
			}

			// Loop until files with minimum replication factor reach the required replication factor
			reachMinReplicationStep(dataStoreHashMap, filesToRemoveHashMap);
		}

		// Prepare and send rebalance requests to the relevant data stores
		prepareRebalanceRequests(filesToStoreHashMap, filesToRemoveHashMap, filesToSendHashMap);
	}

	private void reachMaxReplicationStep(HashMap<Integer, ArrayList<String>> dataStoreHashMap, double maxReplication, HashMap<Integer, ArrayList<String>> filesToStoreHashMap, HashMap<String, ArrayList<Integer>> filesToSendHashMap) {
		Integer minDataStore;
		String minFile;

		while (!checkDataStoreMaxReplication(maxReplication, dataStoreHashMap)) {
			Integer skip = 0;
			// Get data store with minimum files
			minDataStore = getDataStore(dataStoreHashMap, false);
			// Get file with minimum replication factor
			minFile = getMinFile(skip, dataStoreHashMap);

			while (dataStoreHashMap.get(minDataStore).contains(minFile)) {
				skip++;
				minFile = getMinFile(skip, dataStoreHashMap);
			}

			// Add the file to the data store and update the hashmaps
			dataStoreHashMap.get(minDataStore).add(minFile);
			putFilesDataStoreHashMap(minFile, minDataStore, filesToStoreHashMap);
			putFilesDataStoreAndListHashMap(minFile, minDataStore, filesToSendHashMap);
		}
	}

	private void reachMinReplicationStep(HashMap<Integer, ArrayList<String>> dataStoreHashMap, HashMap<Integer, ArrayList<String>> filesToRemoveHashMap) {
		String maxFile;
		Integer maxDataStore;


		while (!checkFilesMinReplication(replicationFactor, dataStoreHashMap)) {
			// Get data store with maximum files
			maxDataStore = getDataStore(dataStoreHashMap, true);
			// Get file with maximum replication factor
			maxFile = getMaxFile(dataStoreHashMap, maxDataStore);

			// Remove the file from the data store and update the hashmap
			putFilesDataStoreHashMap(maxFile, maxDataStore, filesToRemoveHashMap);
			dataStoreHashMap.get(maxDataStore).remove(maxFile);
		}
	}

	private void putFilesDataStoreAndListHashMap(String minFile, Integer minDataStore, HashMap<String, ArrayList<Integer>> filesToSendHashMap) {
		if (filesToSendHashMap.containsKey(minFile)) {
			if (!filesToSendHashMap.get(minFile).contains(minDataStore)) {
				filesToSendHashMap.get(minFile).add(minDataStore);
			}
		} else {
			if (fileList.get(minFile) != null) {
				filesToSendHashMap.put(minFile, new ArrayList<>(Arrays.asList(fileList.get(minFile).get(1), minDataStore)));
			}
		}
	}

	private void putFilesDataStoreHashMap(String file, Integer dataStore, HashMap<Integer, ArrayList<String>> filesHashMap) {
		if (filesHashMap.containsKey(dataStore)) {
			if (!filesHashMap.get(dataStore).contains(file)) {
				filesHashMap.get(dataStore).add(file);
			}
		} else {
			filesHashMap.put(dataStore, new ArrayList<>(Collections.singletonList(file)));
		}
	}

	private synchronized HashMap<Integer, ArrayList<String>> removeFiles(ConcurrentHashMap<String, Integer> statusHashMap) {
		HashMap<Integer, ArrayList<String>> filesToRemove = new HashMap<>();
		for (String key : statusHashMap.keySet()) {
			if (statusHashMap.get(key) == Index.STATUS_REMOVING) {
				ArrayList<Integer> ports = fileList.get(key);
				ports.remove(0);
				for (Integer port : ports) {
					putFilesDataStoreHashMap(key, port, filesToRemove);
				}
			}
		}
		return filesToRemove;
	}

	private synchronized static HashMap<Integer, ArrayList<String>> getDataStoreHashMap() {
		for (String key : index.fileNames) {
			if (index.fileStatus.get(key) == Index.STATUS_REMOVED) {
				fileList.remove(key);
			}
		}
		HashMap<Integer, ArrayList<String>> dataStoreHashMap = new HashMap<>();

		for (String key : fileList.keySet()) {
			ArrayList<Integer> values = fileList.get(key);
			for (int i = 1; i < values.size(); i++) {
				if (dataStoreHashMap.containsKey(values.get(i))) {
					if (!dataStoreHashMap.get(values.get(i)).contains(key)) {
						dataStoreHashMap.get(values.get(i)).add(key);
					}
				} else {
					ArrayList<String> tempValue = new ArrayList<>();
					tempValue.add(key);
					dataStoreHashMap.put(values.get(i), tempValue);
				}
			}
		}

		for (Integer port : dataStoreSocketList.keySet()) {
			if (!dataStoreHashMap.containsKey(port)) {
				dataStoreHashMap.put(port, new ArrayList<>());
			}
		}
		return dataStoreHashMap;
	}

	private synchronized boolean checkDataStoreMaxReplication(Double maxReplication, HashMap<Integer, ArrayList<String>> hashMap) {
		for (Integer key : hashMap.keySet()) {
			if (hashMap.get(key).size() < maxReplication) {
				return false;
			}
		}
		return true;
	}

	private Integer getDataStore(HashMap<Integer, ArrayList<String>> dataStoreHashMap, boolean findMax) {
		// Initialize resultDataStoreSize to either the minimum or maximum integer value
		Integer resultDataStore = 1;
		int resultDataStoreSize = findMax ? Integer.MIN_VALUE : Integer.MAX_VALUE;

		for (Integer dataStore : dataStoreHashMap.keySet()) {
			int dataStoreSize = dataStoreHashMap.get(dataStore).size();

			// If looking for the maximum data store size check the current data store is bigger than the previous biggest data store
			if (findMax) {
				if (dataStoreSize > resultDataStoreSize) {
					resultDataStore = dataStore;
					resultDataStoreSize = dataStoreSize;
				}
			} else { // If looking for the minimum data store size check if the current data store is smaller than the previous smallest data store
				if (dataStoreSize < resultDataStoreSize) {
					resultDataStore = dataStore;
					resultDataStoreSize = dataStoreSize;
				}
			}
		}
		return resultDataStore;
	}

	private String getMinFile(Integer skip, HashMap<Integer, ArrayList<String>> hashMap) {
		HashSet<String> filesSet = new HashSet<>();
		for (ArrayList<String> fileNames : hashMap.values()) {
			filesSet.addAll(fileNames);
		}
		String minFile = getMinFileReplications(revertToKeyFileName(hashMap), filesSet);
		while (skip > 0) {
			skip--;
			filesSet.remove(minFile);
			minFile = getMinFileReplications(revertToKeyFileName(hashMap), filesSet);
		}
		return minFile;
	}

	private String getMaxFile(HashMap<Integer, ArrayList<String>> hashMap, Integer maxDataStore) {
		String maxFileName = "";
		int tempMax = Integer.MIN_VALUE;
		ArrayList<String> fileArray = hashMap.get(maxDataStore);
		HashMap<String, ArrayList<Integer>> invertedHashMap = revertToKeyFileName(hashMap);

		for (String key : fileArray) {
			if (invertedHashMap.get(key).size() > tempMax) {
				tempMax = invertedHashMap.get(key).size();
				maxFileName = key;
			}
		}
		return maxFileName;
	}

	private String getMinFileReplications(HashMap<String, ArrayList<Integer>> hashMap, Set<String> stringSet) {
		String minFileName = "";
		int minFileReplications = Integer.MAX_VALUE;

		for (String key : stringSet) {
			if (hashMap.get(key).size() < minFileReplications) {
				minFileName = key;
				minFileReplications = hashMap.get(key).size();
			}
		}
		return minFileName;
	}

	private HashMap<String, ArrayList<Integer>> revertToKeyFileName(HashMap<Integer, ArrayList<String>> hashMap) {
		HashMap<String, ArrayList<Integer>> reverted = new HashMap<>();
		for (int key : hashMap.keySet()) {
			for (String fileName : hashMap.get(key)) {
				reverted.computeIfAbsent(fileName, k -> new ArrayList<>()).add(key);
			}
		}
		return reverted;
	}

	/**
	 * Checks if the replication of the data store is complete.
	 *
	 * @param dataStoreHashMap  the HashMap containing the data store
	 * @param replicationFactor the number of copies to be made
	 * @param minReplication    the minimum number of replicas that must exist
	 * @param maxReplication    the maximum number of replicas that must exist
	 * @return true if the replication of the data store is complete, false otherwise
	 */
	private boolean isReplicationComplete(HashMap<Integer, ArrayList<String>> dataStoreHashMap, int replicationFactor, double minReplication, double maxReplication) {
		final var fileHashMap = revertToKeyFileName(dataStoreHashMap);

		for (var entry : fileHashMap.entrySet()) {
			if (!(entry.getValue().size() == replicationFactor)) {
				return false;
			}
		}

		for (var entry : dataStoreHashMap.entrySet()) {
			if (!(entry.getValue().size() >= minReplication && entry.getValue().size() <= maxReplication)) {
				return false;
			}
		}

		return true; // return true otherwise
	}

	private boolean checkFilesMinReplication(Integer replicationFactor, HashMap<Integer, ArrayList<String>> hashMap) {
		HashMap<String, ArrayList<Integer>> fileHashMap = revertToKeyFileName(hashMap);
		for (String key : fileHashMap.keySet()) {
			if (fileHashMap.get(key).size() > replicationFactor) {
				return false;
			}
		}
		return true;
	}

	private synchronized void prepareRebalanceRequests(HashMap<Integer, ArrayList<String>> fileToStoreHashMap, HashMap<Integer, ArrayList<String>> fileToRemoveHashMap, HashMap<String, ArrayList<Integer>> fileToSendHashMap) {
		// If no rebalance is needed return
		if (fileToStoreHashMap.isEmpty() && fileToRemoveHashMap.isEmpty() && fileToSendHashMap.isEmpty()) {
			logger.log("No rebalancing required.");
			return;
		}

		// Prepare final send map
		var finalSend = new HashMap<HashMap<Integer, String>, ArrayList<Integer>>();
		fileToSendHashMap.forEach((file, portsToSend) -> {
			var linkHashMap = new HashMap<Integer, String>();
			linkHashMap.put(portsToSend.get(0), file);
			finalSend.put(linkHashMap, new ArrayList<>(portsToSend.subList(1, portsToSend.size())));
		});

		// Set rebalance complete counter to zero
		setRebalanceCompleteCounter(0);

		// Loop through all data stores and prepare the files to send and remove for each one
		prepareFilesSendRemove(fileToRemoveHashMap, finalSend);
	}

	private void prepareFilesSendRemove(HashMap<Integer, ArrayList<String>> fileToRemoveHashMap, HashMap<HashMap<Integer, String>, ArrayList<Integer>> finalSend) {
		for (int port : dataStoreSocketList.keySet()) {
			StringBuilder filesToSend = new StringBuilder();
			StringBuilder filesToRemove = new StringBuilder();
			AtomicInteger filesToSendCounter = new AtomicInteger();

			finalSend.keySet().stream()
					.filter(map -> map.containsKey(port))
					.forEach(map -> {
						filesToSendCounter.getAndIncrement();
						var file = map.get(port);
						var ports = finalSend.get(map);
						filesToSend.append(" ").append(file).append(" ").append(ports.size()).append(" ").append(getPortsToMessage(ports));
					});

			// Insert the file send counter at the beginning of the filesToSend string builder
			filesToSend.insert(0, filesToSendCounter.get());
			var removeList = fileToRemoveHashMap.get(port);

			// If there are files to remove, append them to the filesToRemove string builder
			filesToRemove.append(removeList == null ? "0" : (removeList.size() + " " + String.join(" ", removeList)));

			// Send the rebalance request to the data store
			sendRebalanceRequest(port, filesToSend.toString(), filesToRemove.toString());
		}
	}

	private String getPortsToMessage(ArrayList<Integer> portsList) {
		return portsList.stream()
				.map(Object::toString)
				.collect(Collectors.joining(" "));
	}

	// This method sends a rebalance request to a data store on the specified port, with the files to send and remove
	private void sendRebalanceRequest(Integer port, String filesToSend, String filesToRemove) {
		try {
			var dataStore = dataStoreSocketList.get(port);
			var dataStorePrintWriter = new PrintWriter(dataStore.getOutputStream(), true);

			dataStorePrintWriter.println(Protocol.REBALANCE_TOKEN + " " + filesToSend + " " + filesToRemove);
			logger.messageSent(dataStore, Protocol.REBALANCE_TOKEN + " " + filesToSend + " " + filesToRemove);

			// Increment the rebalance complete counter
			setRebalanceCompleteCounter(getRebalanceCompleteCounter() + 1);
		} catch (IOException e) {
			logger.log("Failed to send rebalance request: " + e.getMessage());
		}
	}

	private synchronized void updateIndexAfterRebalance() {
		for (String key : index.fileStatus.keySet()) {
			if (index.fileStatus.get(key) == Index.STATUS_REMOVING) {
				index.fileStatus.remove(key);
				index.fileNames.remove(key);
			}
		}
	}

	private static synchronized void fixFailedDataStore(Integer port) {
		if (port == 0)
			return;

		// Remove the failed data store from the data store list
		dataStoreSocketList.remove(port);

		// For each file in file list that was stored on the failed data store - remove the data store from the list of data stores that store the file.
		// If there is only one data store left that stores the file - remove the file from the file list and the index.
		fileList.entrySet().stream()
				.filter(entry -> entry.getValue().contains(port))
				.forEach(entry -> {
					entry.getValue().remove(port);

					if (entry.getValue().size() == 1) {
						String key = entry.getKey();

						fileList.remove(key);
						index.fileNames.remove(key);
						index.fileStatus.remove(key);
					}
				});
	}

	public static int getReplicationFactor() {
		return replicationFactor;
	}

	public static void setReplicationFactor(int replicationFactor) {
		Controller.replicationFactor = replicationFactor;
	}

	public static int getCompletedStores() {
		return completedStores;
	}

	public static void setCompletedStores(int completedStores) {
		Controller.completedStores = completedStores;
	}

	public static int getLoadPortCounter() {
		return loadPortCounter;
	}

	public static void setLoadPortCounter(int loadPortCounter) {
		Controller.loadPortCounter = loadPortCounter;
	}

	public static int getRebalanceCompleteCounter() {
		return rebalanceCompleteCounter;
	}

	public static void setRebalanceCompleteCounter(int rebalanceCompleteCounter) {
		Controller.rebalanceCompleteCounter = rebalanceCompleteCounter;
	}
}