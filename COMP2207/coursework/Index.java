import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

public class Index {

	public final static int STATUS_STORED = 1001;
	public final static int STATUS_REMOVED = 2001;
	public final static int STATUS_STORING = 1000;
	public final static int STATUS_REMOVING = 2000;

	ArrayList<String> fileNames;
	ConcurrentHashMap<String, Integer> fileStatus;

	public Index(ArrayList<String> fileNames, ConcurrentHashMap<String, Integer> fileStatus) {
		this.fileNames = fileNames;
		this.fileStatus = fileStatus;
	}

	public ArrayList<String> getFileNames() {
		return fileNames;
	}

	public void reset() {
		this.fileNames.clear();
		this.fileStatus.clear();
	}
}