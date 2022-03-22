/**
 *
 */

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.Expression;
import com.github.javaparser.ast.expr.MethodCallExpr;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;

import uk.ac.soton.ecs.comp1206.labtestlibrary.interfaces.threading.Seat;
import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;
import uk.ac.soton.ecs.comp1206.labtestlibrary.recursion.Tuple;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.philosopher.PhilosopherEager;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.philosopher.PhilosopherSleepyhead;
import uk.ac.soton.ecs.comp1206.labtestlibrary.threading.philosopher.Table;

/**
 * Tests for the Lab on threading, exceptions and synchronisation. Question 3:
 * Philosopher.
 *
 * @author jan
 *
 */
class SynchronisationQ3Test {

	private static boolean setupError = false;
	private static List<PhilosopherSleepyhead> allTheSleepyheads = new ArrayList<PhilosopherSleepyhead>();
	private static Table table;

	/**
	 * Create a table with all the forks and seats and lets four lazy philosophers
	 * pick up a fork. Sets a fail flag if the setup failed.
	 */
	@BeforeAll
	public static void setup() {
		try {
			table = new Table(new Factory());
			for (int i = 0; i < table.getNumberSeats() - 1; i++) {
				PhilosopherSleepyhead sleepyhead = new PhilosopherSleepyhead(table.getSeat(i), table);
				allTheSleepyheads.add(sleepyhead);
				Thread sleepyheadThread = new Thread(sleepyhead);
				sleepyheadThread.start();
			}
			table.waitForFourForks();
		} catch (Exception e) {
			e.printStackTrace();
			setupError = true;
		}
	}

	/**
	 * Lets the threads philosopher threads finish.
	 */
	@AfterAll
	public static void tearDown() {
		for (PhilosopherSleepyhead sleepyhead : allTheSleepyheads) {
			sleepyhead.wakeUp();
		}
	}

	/**
	 * Tests if the right number of locks/forks is locked.
	 */
	@Test
	public void UsedForksTest() {
		String failMessage = "Four threads have attempted to pick up a fork and should have aquired "
				+ "a lock. Have you used the locks from 'assignForks' and used 'askFork1' " + "to lock a lock?";
		assertEquals(4, table.getForksInUse(), failMessage);
	}

	/**
	 * Checks that the last philosopher is not producing a deadlock.
	 *
	 * @throws InterruptedException
	 */
	@Test
	public void PhilosopherTest() throws InterruptedException {
		if (setupError) {
			fail("Error in setup.");
		}
		PhilosopherEager eagerBeaver = new PhilosopherEager(table.getLastSeat(), table);
		Thread eagerBeaverThread = new Thread(eagerBeaver);
		eagerBeaverThread.start();
		boolean eagerBeaverResult = table.waitForForkOrWaitFifthOrWaitingFork(eagerBeaverThread);
		if (eagerBeaverResult) {
			String failMessage = "Five threads have attempted to pick up a fork and should have aquired "
					+ "a lock. Have you used the locks from 'assignForks' and used 'askFork1' " + "to lock a lock?";
			assertEquals(table.getForksInUse(), 5, failMessage);
			fail("All philosopher have taken a fork. You have produced a deadlock.");
		}
	}

	/**
	 * Tests that only the appropriate method calls are used in the seats.
	 * @throws FileNotFoundException
	 */
	@Test
	public void testLockingMethods() throws FileNotFoundException {
		Tuple<Class<? extends Seat>, Class<? extends Seat>> seats = new Factory().getSeats();
		CompilationUnit firstSeatClassUnit = getSeatClass(seats.getFirstValue());
		ClassOrInterfaceDeclaration firstSeatClass = firstSeatClassUnit.getClassByName(seats.getFirstValue().getSimpleName()).get();
		CompilationUnit secondSeatClassUnit = getSeatClass(seats.getSecondValue());
		ClassOrInterfaceDeclaration secondSeatClass = secondSeatClassUnit.getClassByName(seats.getSecondValue().getSimpleName()).get();
		List<MethodDeclaration> askFork1SearchResults = firstSeatClass.getMethodsByName("askFork1");
		List<MethodDeclaration> askFork2SearchResults = secondSeatClass.getMethodsByName("askFork2");
		assertTrue(askFork1SearchResults.size() > 0, "Method askFork1 missing. Have you implemented the method?");
		assertTrue(askFork2SearchResults.size() > 0, "Method askFork2 missing. Have you implemented the method?");
		Optional<BlockStmt> askFork1BodyOptional = askFork1SearchResults.get(0).getBody();
		Optional<BlockStmt> askFork2BodyOptional = askFork2SearchResults.get(0).getBody();
		assertFalse(askFork1BodyOptional.isEmpty(), "Method askFork1 appears to have no body. Have you implemented the method?");
		assertFalse(askFork2BodyOptional.isEmpty(), "Method askFork2 appears to have no body. Have you implemented the method?");
		BlockStmt askFork1Body = askFork1BodyOptional.get();
		BlockStmt askFork2Body = askFork2BodyOptional.get();
		NodeList<Statement> askFork1Statements = askFork1Body.getStatements();
		NodeList<Statement> askFork2Statements = askFork2Body.getStatements();
		assertTrue(askFork1Statements.size() > 0, "Your implementation of askFork1 appears to have an empty body. Have you implemented the method?");
		assertTrue(askFork2Statements.size() > 0, "Your implementation of askFork2 appears to have an empty body. Have you implemented the method?");
		assertEquals(1, askFork1Statements.size(), "Your implementation of askFork1 has more lines than necessary. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		assertEquals(1, askFork2Statements.size(), "Your implementation of askFork2 has more lines than necessary. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		Statement askFork1OnlyStatement = askFork1Statements.get(0);
		Statement askFork2OnlyStatement = askFork2Statements.get(0);
		assertTrue(askFork1OnlyStatement.isExpressionStmt(), "Method askFork1 appears to contain the wrong statement. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		assertTrue(askFork2OnlyStatement.isExpressionStmt(), "Method askFork2 appears to contain the wrong statement. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		Expression askFork1Expression = askFork1OnlyStatement.asExpressionStmt().getExpression();
		Expression askFork2Expression = askFork2OnlyStatement.asExpressionStmt().getExpression();
		assertTrue(askFork1Expression.isMethodCallExpr(), "Method askFork1 appears to contain the wrong statement. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		assertTrue(askFork2Expression.isMethodCallExpr(), "Method askFork2 appears to contain the wrong statement. If you are debugging that might be OK, otherwise the method should only lock a fork.");
		MethodCallExpr askFork1MethodCallExpr = askFork1Expression.asMethodCallExpr();
		MethodCallExpr askFork2MethodCallExpr = askFork2Expression.asMethodCallExpr();
		assertEquals("lock", askFork1MethodCallExpr.getName().toString(), "It appears that you are calling the wrong method in askFork1. Make sure you lock the fork.");
		assertEquals("lock", askFork2MethodCallExpr.getName().toString(), "It appears that you are calling the wrong method in askFork2. Make sure you lock the fork.");
	}

	/**
	 * Parses a seat java file.
	 *
	 * @return The parsed seat class.
	 * @throws FileNotFoundException If the source file for the seat cannot be
	 *                               found.
	 */
	private CompilationUnit getSeatClass(Class<? extends Seat> seatClass) throws FileNotFoundException {
		File currentAbsolutePath = Paths.get("").toAbsolutePath().toFile();
		Collection<File> files = FileSystemAccess.findJavaFile(currentAbsolutePath, seatClass.getSimpleName());
		if (files.isEmpty()) {
			fail("Source file not found. Ensure that the test is run upwards in the directory tree of the source file of NumberQueue.");
		}
		String filePath = files.iterator().next().getAbsolutePath();
		CompilationUnit cu = StaticJavaParser.parse(new FileInputStream(filePath));
		return cu;
	}
}