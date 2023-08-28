import java.io.IOException;
import java.io.PrintStream;
import java.net.Socket;

public abstract class Logger implements AutoCloseable {

	public enum LoggingType {
		NONE,
		TERMINAL,
		FILE,
		BOTH
	}

	protected final LoggingType loggingType;
	protected PrintStream printStream;

	protected Logger(LoggingType loggingType) {
		this.loggingType = loggingType;
	}

	protected abstract String getLogFileSuffix();

	protected synchronized PrintStream getPrintStream() throws IOException {
		if (printStream == null) {
			printStream = new PrintStream(getLogFileSuffix() + "_" + System.currentTimeMillis() + ".log");
		}
		return printStream;
	}

	protected void log(String message) {
		switch (loggingType) {
			case BOTH -> {
				System.out.println(message);
				try {
					getPrintStream().println(message);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			case FILE -> {
				try {
					getPrintStream().println(message);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			case TERMINAL -> {
				System.out.println(message);
			}
			default -> { break; }
		}
	}

	public void messageSent(Socket socket, String message) {
		log("[%d->%d] %s".formatted(socket.getLocalPort(), socket.getPort(), message));
	}

	public void messageReceived(Socket socket, String message) {
		log("[%d<-%d] %s".formatted(socket.getLocalPort(), socket.getPort(), message));
	}

	@Override
	public void close() throws Exception {
		if (printStream != null) {
			printStream.close();
		}
	}
}
