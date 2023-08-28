import java.net.Socket;

public class CLogger extends Logger {

	private static final String LOG_SUFFIX = "controller";

	public CLogger(LoggingType loggingType) {
		super(loggingType);
	}

	@Override
	protected String getLogFileSuffix() {
		return LOG_SUFFIX;
	}

	public void dstoreJoined(Socket socket, String dataStorePort) {
		log(String.format("[Dstore Joined %s %d <- %d]", dataStorePort, socket.getLocalPort(), socket.getPort()));
	}
}