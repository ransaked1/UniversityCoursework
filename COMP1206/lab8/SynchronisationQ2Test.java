/**
 *
 */

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.lang.Thread.State;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.joor.Reflect;
import org.joor.ReflectException;
import org.junit.jupiter.api.Test;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.ConstructorDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.expr.BooleanLiteralExpr;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.FieldAccessExpr;
import com.github.javaparser.ast.expr.SimpleName;
import com.github.javaparser.ast.expr.ThisExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.CatchClause;
import com.github.javaparser.ast.stmt.Statement;
import com.github.javaparser.ast.stmt.SynchronizedStmt;
import com.github.javaparser.ast.stmt.TryStmt;
import com.github.javaparser.ast.stmt.WhileStmt;
import com.github.javaparser.ast.type.PrimitiveType;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.NumberQueue;
import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.Tuple;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.CanSleepForJUnitTest;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.NumberFactory;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.UncaughtExceptionCollector;
import uk.ac.soton.ecs.comp1206.labtestlibrary.util.Single;

import factory.*;

/**
 * Tests for the Lab on threading, exceptions and synchronisation. Question 2:
 * Cyclic queue and belt.
 *
 * @author jan
 *
 */
class SynchronisationQ2Test {

	private static long DOUBLE_LOCK_WAITING_TIME = 1000;
	private static int QUEUE_SIZE = 5;
	private static int NUMBER_WRITING_THREADS = 5000;
	private static String SYNCHRONISED_QUEUE_CLASS_NAME = "Belt";
	private static String SYNCHRONISED_QUEUE_NEW_NAME_FOR_TEST = "BeltForJUnitTest";
	private static String SYNCHRONISED_QUEUE_SLEEP_PARAMETER_NAME = "isSleepingForJUnitTest";

	/**
	 * Test that an empty queue returns true on empty method and throws an exception
	 * on dequeue.
	 */
	@Test
	void queueEmptyExceptionTest() {
		NumberQueue queue = new CyclicQueue(QUEUE_SIZE);
		assertEquals(true, queue.isEmpty(), "Queue shaould be empty.");
		assertThrows(IndexOutOfBoundsException.class, () -> {
			queue.dequeue();
		});
	}

	/**
	 * Test whether a full queue throws an exception on enqueue.
	 */
	@Test
	void queueFullExceptionTest() {
		NumberQueue queue = fullQueue(CyclicQueue.class);
		assertThrows(IndexOutOfBoundsException.class, () -> {
			queue.enqueue(0);
		});
	}

	/**
	 * Tests that the right elements are returned from the belt.
	 */
	@Test
	void queueRightValueTest() {
		NumberQueue queue = fullQueue(Belt.class);
		queue.dequeue();
		assertEquals(1, queue.dequeue(), "Wrong element from queue.");
		queue.dequeue();
		assertEquals(3, queue.dequeue(), "Wrong element from queue.");
		queue.enqueue(5);
		queue.enqueue(6);
		queue.enqueue(7);
		queue.dequeue();
		queue.dequeue();
		queue.dequeue();
		assertEquals(7, queue.dequeue(), "Wrong element from queue.");
		assertEquals(true, queue.isEmpty(), "Queue should be empty.");
	}

	/**
	 * Test that the queue works when enough elements are added and removes to reach
	 * the boundary of the array. Inserts and removes a number of
	 */
	@Test
	void queueRunTest() {
		NumberQueue queue = emptyQueue();
		double elemetsFactor = 5.5;
		int loopBoundary = (int) Math.floor(elemetsFactor * QUEUE_SIZE);
		queue.enqueue(-1);
		for (int i = 0; i < loopBoundary; i++) {
			queue.enqueue(i);
			queue.dequeue();
		}
		assertEquals(false, queue.isEmpty(), "Queue should contain one element.");
		int lastNumber = loopBoundary - 1;
		assertEquals(lastNumber, queue.dequeue(), "Wrong element in queue.");
		assertEquals(true, queue.isEmpty(), "Queue should be empty.");
	}

	/**
	 * Tests that consumption and production threads can add to and take from the
	 * queue, respectively.
	 */
	@Test
	void beltWorkerTest() {
		for (int i = 0; i < 10; i++) {
			NumberQueue belt = new Belt(50);
			int numberProducer = 10;
			int numberConsumer = 10;
			NumberFactory factory = null;
			try {
				factory = new NumberFactory(numberProducer, Producer.class, numberConsumer, Consumer.class, belt);
			} catch (Exception e) {
				fail("The factory workers could not be initialised. Have you implemented the constructor as described in the instructions? ("
						+ e.getMessage() + ")");
			}
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			factory.endShift();
			assertEquals(0, factory.getNumberIndexOutOfBoundsException(),
					"Some threads had uncontrolled acceess to the belt.");
			checkForUncaughtExceptions(factory);
		}
	}

	/**
	 * Tests that production threads cannot overfill the queue.
	 */
	@Test
	void productionQueueTest() {
		for (int i = 0; i < 10; i++) {
			NumberQueue belt = new Belt(50);
			int numberProducer = 10;
			NumberFactory factory = null;
			try {
				factory = new NumberFactory(numberProducer, Producer.class, 0, null, belt);
			} catch (Exception e) {
				fail("The factory workers could not be initialised. Have you implemented the constructor as described in the instructions? ("
						+ e.getMessage() + ")");
			}
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			factory.endShift();
			assertEquals(0, factory.getNumberIndexOutOfBoundsException(),
					"Some threads had uncontrolled acceess to the belt.");
			checkForUncaughtExceptions(factory);
		}
	}

	/**
	 * Tests that consumption threads cannot take from an empty queue.
	 */
	@Test
	void consumptionQueueTest() {
		for (int i = 0; i < 10; i++) {
			NumberQueue belt = new Belt(50);
			int numberConsumer = 10;
			NumberFactory factory = null;
			try {
				factory = new NumberFactory(0, null, numberConsumer, Consumer.class, belt);
			} catch (Exception e) {
				fail("The factory workers could not be initialised. Have you implemented the constructor as described in the instructions? ("
						+ e.getMessage() + ")");
			}
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			factory.endShift();
			assertEquals(0, factory.getNumberIndexOutOfBoundsException(),
					"Some threads had uncontrolled acceess to the belt.");
			checkForUncaughtExceptions(factory);
		}
	}

	/**
	 * Test that no two threads can write at the same time to the {@link Belt}.
	 *
	 * @throws InterruptedException
	 */
	@Test
	void concurrentAccessTest() throws InterruptedException {
		NumberQueue belt = new Belt(NUMBER_WRITING_THREADS);
		List<Thread> threads = new ArrayList<>();
		for (int i = 0; i < NUMBER_WRITING_THREADS; i++) {
			threads.add(createEnqueueThread(belt, i));
		}
		for (int i = 0; i < NUMBER_WRITING_THREADS; i++) {
			threads.get(i).start();
		}
		for (int i = 0; i < NUMBER_WRITING_THREADS; i++) {
			threads.get(i).join();
		}
		List<Integer> numbersOnBelt = new ArrayList<>();
		while (!belt.isEmpty()) {
			numbersOnBelt.add(belt.dequeue());
		}
		boolean containsAllNumbers = true;
		int i = 0;
		while (i < NUMBER_WRITING_THREADS && containsAllNumbers) {
			if (!numbersOnBelt.contains(i)) {
				containsAllNumbers = false;
			}
			i++;
		}
		assertTrue(containsAllNumbers,
				"Some threads had non-synchronised acceess to the belt. Two or more threads have written to the same index.");
	}

	/**
	 *
	 * Tests whether a thread that dequeues an empty queue will block correctly
	 * until there is a number available.
	 *
	 * @throws FileNotFoundException If the source file for the queue cannot be
	 *                               found.
	 */
	@Test
	void dequeueThenEnqueueTest() throws FileNotFoundException {
		Belt belt = new Belt(10);
		try {
			final List<Integer> number = new ArrayList<Integer>();
			final Single<Boolean> outOfBoundsException = new Single<Boolean>(false);

			Thread dequeueThread = new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						number.add(belt.dequeue());
					} catch (IndexOutOfBoundsException iobEx) {
						outOfBoundsException.setValue(true);
					}
				}
			});

			dequeueThread.start();

			// Wait to make sure we're blocking
			Thread.sleep(500);

			int magicNumber = 999;

			Thread enqueueThread = startEnqueueThread(belt, magicNumber);

			enqueueThread.join(DOUBLE_LOCK_WAITING_TIME);
			dequeueThread.join(DOUBLE_LOCK_WAITING_TIME);

			assertEquals(false, outOfBoundsException.getValue(),
					"An IndexOutOfBoundsException occured. Is your dequeue method waiting if numbers are available?");
			assertEquals(1, number.size(),
					"Did not find a number on queue. Is your enqueue method waiting if no numbers are available?");
			assertEquals(magicNumber, (int) number.get(0),
					"Did not find a number on queue. Is your enqueue method waiting if no numbers are available?");

		} catch (IllegalMonitorStateException iMEx) {
			fail("Synchronisation exception. Have you used synchronized, wait and notify correctly?");
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/**
	 *
	 * Tests whether a thread that enqueues a full queue will block correctly until
	 * there is space available.
	 *
	 * @throws FileNotFoundException If the source file for the queue cannot be
	 *                               found.
	 */
	@Test
	void enqueueThenDequeueTest() throws FileNotFoundException {
		Belt belt = new Belt(1);
		final Single<Boolean> outOfBoundsException = new Single<Boolean>(false);
		try {

			belt.enqueue(1);

			Thread enqueueThread = new Thread(new Runnable() {

				@Override
				public void run() {
					try {
						belt.enqueue(2);
					} catch (IndexOutOfBoundsException iobEx) {
						outOfBoundsException.setValue(true);
					}
				}
			});

			enqueueThread.start();

			// Wait to make sure we're blocking
			Thread.sleep(500);

			assertEquals(false, outOfBoundsException.getValue(),
					"An IndexOutOfBoundsException occured. Is your enqueue method waiting if space is available?");
			assertEquals(Thread.State.WAITING, enqueueThread.getState(),
					"Was expecting thread trying to enqueue integer on full belt to be waiting.");

			Tuple<Thread, DequeueRunner> dequeueThreadStartPair = startDequeueThread(belt);
			Thread dequeueThread = dequeueThreadStartPair.getFirstValue();
			DequeueRunner dequeueRunner = dequeueThreadStartPair.getSecondValue();
			dequeueThread.join(500);
			assertEquals(true, dequeueRunner.hasDequeuedValue(), "A thread is waiting. Have you implemented the locks correctly?");
			assertEquals(1, dequeueRunner.getDequeuedValue(), "Wrong number on belt");

			enqueueThread.join(DOUBLE_LOCK_WAITING_TIME);

			assertEquals(Thread.State.TERMINATED, enqueueThread.getState(), "Was expecting thread to be terminated.");
			dequeueThreadStartPair = startDequeueThread(belt);
			dequeueThread = dequeueThreadStartPair.getFirstValue();
			dequeueRunner = dequeueThreadStartPair.getSecondValue();
			dequeueThread.join(500);
			assertEquals(true, dequeueRunner.hasDequeuedValue(), "A thread is waiting. Have you implemented the locks correctly?");
			assertEquals(2, dequeueRunner.getDequeuedValue(), "Wrong number on belt");

		} catch (IllegalMonitorStateException iMEx) {
			fail("Synchronisation exception. Have you used synchronized, wait and notify correctly?");
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/**
	 * Checks for uncaught exceptions and produce a fail if an uncaught exception
	 * exists.
	 *
	 * @param exceptionCollector The exception collector to check for uncaught
	 *                           exceptions.
	 */
	private void checkForUncaughtExceptions(UncaughtExceptionCollector exceptionCollector) {
		List<Exception> uncaughtExceptions = exceptionCollector.getUncaughtExceptions();
		if (uncaughtExceptions.size() > 0) {
			Exception firstException = uncaughtExceptions.get(0);
			String exceptionType = firstException.getClass().getName();
			String firstExceptionMessage = firstException.getMessage();
			if (firstException.getCause() != null) {
				firstExceptionMessage = firstException.getCause().getMessage();
				exceptionType = firstException.getCause().getClass().getName();
			}
			assertEquals(0, uncaughtExceptions.size(), "Some threads had uncaught exceptions. For example: ("
					+ exceptionType + ") " + firstExceptionMessage);
		}
	}

	/**
	 * Creates and starts a thread that enqueues the given number.
	 *
	 * @param queue  The queue to add the number to.
	 * @param number The number to add to the queue.
	 * @return The created and started thread.
	 */
	private Thread startEnqueueThread(NumberQueue queue, int number) {
		Thread enqueueThreat = createEnqueueThread(queue, number);
		enqueueThreat.start();
		return enqueueThreat;
	}

	/**
	 * Creates a thread that enqueues the given number.
	 *
	 * @param queue  The queue to add the number to.
	 * @param number The number to add to the queue.
	 * @return The created thread.
	 */
	private Thread createEnqueueThread(NumberQueue queue, int number) {
		Thread enqueueThreat = new Thread(new Runnable() {

			@Override
			public void run() {
				queue.enqueue(number);
			}
		});
		return enqueueThreat;
	}

	/**
	 * Creates and starts a thread that dequeues a number.
	 *
	 * @param queue The queue to get the number from.
	 * @return The created and started thread.
	 */
	private Tuple<Thread, DequeueRunner> startDequeueThread(NumberQueue queue) {
		DequeueRunner runner = new DequeueRunner(queue);
		Thread dequeueThreat = new Thread(runner);
		dequeueThreat.start();
		return new Tuple<Thread, SynchronisationQ2Test.DequeueRunner>(dequeueThreat, runner);
	}

	/**
	 * A runner that dequeues a value from a queue.
	 * @author jan
	 *
	 */
	private class DequeueRunner implements Runnable{

		private NumberQueue queue;
		private Integer dequeuedValue = null;

		/**
		 * Creates a new runner given the queue to dequeue from.
		 * @param queue
		 */
		public DequeueRunner(NumberQueue queue) {
			this.queue = queue;
		}

		/**
		 * The dequeued value. This might be a wrong value if the thread hasn't dequeued yet.
		 * @return the value from the queue.
		 */
		public int getDequeuedValue() {
			return dequeuedValue;
		}

		/**
		 * Indicated if the thread has dequeued a value yet.
		 * @return true if a value was dequeued and false otherwise.
		 */
		public boolean hasDequeuedValue() {
			return (dequeuedValue == null) ? false : true;
		}

		/**
		 * Get the next value from the queue.
		 */
		@Override
		public void run() {
			dequeuedValue = queue.dequeue();
		}
	}

	/**
	 * Parses the synchronisation queue java file.
	 *
	 * @return The parsed queue class.
	 * @throws FileNotFoundException If the source file for the queue cannot be
	 *                               found.
	 */
	private CompilationUnit getSynchronisedQueueClass() throws FileNotFoundException {
		File currentAbsolutePath = Paths.get("").toAbsolutePath().toFile();
		Collection<File> files = FileSystemAccess.findJavaFile(currentAbsolutePath, SYNCHRONISED_QUEUE_CLASS_NAME);
		if (files.isEmpty()) {
			fail("Source file not found. Ensure that the test is run upwards in the directory tree of the source file of NumberQueue.");
		}
		String filePath = files.iterator().next().getAbsolutePath();
		CompilationUnit cu = StaticJavaParser.parse(new FileInputStream(filePath));
		return cu;
	}

	/**
	 * Insert a sleep forever block into the enqueue method that can be turned of
	 * and on.
	 *
	 * @param queueClass The class to insert the block into.
	 * @return false if no synchronized block/method could be found otherwise true.
	 */
	private boolean modifyClassForTest(ClassOrInterfaceDeclaration queueClass) {
		renameClassForTest(queueClass);
		addSleepVariableAndMethod(queueClass);
		boolean sleepBlockAdded = addSleepLoop(queueClass);
		return sleepBlockAdded;
	}

	/**
	 * Renames the queue class to prevent clash with the original class.
	 *
	 * @param queueClass The class to rename.
	 */
	private void renameClassForTest(ClassOrInterfaceDeclaration queueClass) {
		SimpleName newName = new SimpleName(SYNCHRONISED_QUEUE_NEW_NAME_FOR_TEST);
		queueClass.setName(newName);
		queueClass.findAll(ConstructorDeclaration.class).stream().forEach(constructor -> {
			constructor.setName(newName);
		});
	}

	/**
	 *
	 * Inserts a boolean parameter and a method to control the sleep forever block.
	 * The parameter is initially false and the method will toggle this to true.
	 *
	 * @param queueClass The class to insert the block into.
	 */
	private void addSleepVariableAndMethod(ClassOrInterfaceDeclaration queueClass) {
		queueClass.addImplementedType(CanSleepForJUnitTest.class);
		String boolParameterMethodName = CanSleepForJUnitTest.class.getMethods()[0].getName();
		queueClass.addFieldWithInitializer(new PrimitiveType(PrimitiveType.Primitive.BOOLEAN),
				SYNCHRONISED_QUEUE_SLEEP_PARAMETER_NAME, new BooleanLiteralExpr(false));
		MethodDeclaration sleepMethod = queueClass.addMethod(boolParameterMethodName);
		sleepMethod.addModifier(Modifier.Keyword.PUBLIC);
		BlockStmt sleepSetterBody = sleepMethod.createBody();
		Statement setSleepTrueStatement = StaticJavaParser
				.parseStatement("this." + SYNCHRONISED_QUEUE_SLEEP_PARAMETER_NAME + " = true;");
		sleepSetterBody.addStatement(0, setSleepTrueStatement);
	}

	/**
	 * Insert a sleep forever block into the enqueue method.
	 *
	 * @param queueClass The class to insert the block into.
	 * @return false if no synchronized block/method could be found otherwise true.
	 */
	private boolean addSleepLoop(ClassOrInterfaceDeclaration queueClass) {
		List<MethodDeclaration> foundEnqueueMethods = queueClass.getMethodsByName("enqueue");
		boolean synchroniseInEnqueueFound = foundEnqueueMethods.size() > 0 ? true : false;
		for (MethodDeclaration method : foundEnqueueMethods) {
			boolean methodIsSynchronized = method.getDeclarationAsString().contains("synchronized");
			WhileStmt whileTrueSleep = createSleepLoop();
			BlockStmt synchronizedBlock = null;
			if (methodIsSynchronized) {
				synchronizedBlock = method.getBody().get();
			} else {
				BlockStmt methodBlock = method.getBody().get();
				List<SynchronizedStmt> foundSynchronizedStmts = methodBlock.findAll(SynchronizedStmt.class);
				if (foundSynchronizedStmts.size() > 0) {
					SynchronizedStmt firstSynchronizedStmt = foundSynchronizedStmts.get(0);
					synchronizedBlock = firstSynchronizedStmt.getBody();
				}
			}
			if (synchronizedBlock == null) {
				synchroniseInEnqueueFound = false;
			} else {
				synchronizedBlock.addStatement(whileTrueSleep);
			}
		}
		return synchroniseInEnqueueFound;
	}

	/**
	 * Creates a sleep loop block. The block consists of a while loop that depends
	 * on the sleep variable. Any {@link InterruptedException} is caught and
	 * ignored.
	 *
	 * @return The sleep loop block.
	 */
	private WhileStmt createSleepLoop() {
		Statement sleepStatement = StaticJavaParser.parseStatement("Thread.sleep(Integer.MAX_VALUE);");
		NodeList<Statement> sleepStatementInList = new NodeList<>();
		sleepStatementInList.add(sleepStatement);
		BlockStmt sleepBlock = new BlockStmt(sleepStatementInList);
		// BooleanLiteralExpr trueExpression = new BooleanLiteralExpr(true);
		TryStmt tryStatement = new TryStmt();
		CatchClause catchStatement = new CatchClause(new NodeList<Modifier>(), new NodeList<AnnotationExpr>(),
				StaticJavaParser.parseClassOrInterfaceType("InterruptedException"), new SimpleName("e"),
				StaticJavaParser.parseBlock("{}"));
		NodeList<CatchClause> catchClauses = new NodeList<>();
		catchClauses.add(catchStatement);
		tryStatement.setTryBlock(sleepBlock);
		tryStatement.setCatchClauses(catchClauses);
		NodeList<Statement> wholeBlock = new NodeList<>();
		wholeBlock.add(tryStatement);
		BlockStmt tryStatementBlock = new BlockStmt(wholeBlock);
		Expression boolParameterAsExpression = new FieldAccessExpr(new ThisExpr(),
				SYNCHRONISED_QUEUE_SLEEP_PARAMETER_NAME);
		WhileStmt whileTrueSleep = new WhileStmt(boolParameterAsExpression, tryStatementBlock);
		return whileTrueSleep;
	}

	/**
	 * Compiles the queue test class and instantiates it with a given capacity.
	 *
	 * @param cu        The class to compile and instantiate.
	 * @param queueSize
	 * @return
	 */
	private NumberQueue compileAndInstantiateQueueTestClass(CompilationUnit cu, int queueSize) {
		Reflect r = null;
		try {
			String completeNewClass = SYNCHRONISED_QUEUE_NEW_NAME_FOR_TEST;
			r = Reflect.compile(completeNewClass, cu.toString());
			r = r.create(queueSize);
		} catch (ReflectException re) {
			if (re.getCause() != null && re.getCause().getClass().isInstance(NoSuchMethodException.class)) {
				fail("Could not create queue. Ensure that the requested constructor exists.");
			} else {
				if (re.getCause() != null) {
					fail(re.getCause().getMessage().toString());
				} else {
					fail(re.getMessage().toString());
				}
			}
		}
		NumberQueue theNewQueue = r.get();
		return theNewQueue;
	}

	/**
	 * Fills a queue with enough consecutive numbers to be full
	 *
	 * @param queueClass The {@link NumberQueue} to instantiate.
	 * @return The queue with consecutive numbers.
	 */
	private NumberQueue fullQueue(Class<? extends NumberQueue> queueClass) {
		NumberQueue queue = null;
		try {
			queue = queueClass.getConstructor(int.class).newInstance(QUEUE_SIZE);
			for (int i = 0; i < QUEUE_SIZE; i++) {
				queue.enqueue(i);
			}
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
				| NoSuchMethodException | SecurityException e) {
			fail("The queue class could not be instantiated. Make sure you set up the project correctly and you have implemented the queue classes described in lab 7 and lab 8.");
		}
		return queue;
	}

	/**
	 * Creates an empty queue.
	 *
	 * @return An empty queue.
	 */
	private NumberQueue emptyQueue() {
		Belt queue = new Belt(QUEUE_SIZE);
		return queue;
	}
}