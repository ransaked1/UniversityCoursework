import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import uk.ac.soton.ecs.comp1206.labtestlibrary.body.AccessModifier;
import uk.ac.soton.ecs.comp1206.labtestlibrary.body.Class;
import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;
import uk.ac.soton.ecs.comp1206.labtestlibrary.parse.JavaFile;

/**
 * Test for lab 5 on files and IO.
 *
 * The java classes are provided as static fields at the bottom of the file.
 *
 * @author jan
 *
 */
class FilesIOQ2Test {

	static final String TEST_DIRECTORY_PATH = "tmpDir";
	static final String ALL_FILES_CONTENT_FILE_NAME = "AllFiles.txt";

	/**
	 * Creates the random numbers and the java files for the file writing and the java file concatenation tasks, respectively.
	 * @throws Exception
	 */
	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		createJavaFiles();
	}

	/**
	 * Removes the test folder and test files.
	 * @throws IOException
	 */
	@AfterAll
	public static void cleanUpAfterClass() throws IOException {
		removeTestFolder();
	}

	/**
	 * Checks that there is a suitable exception for the case of not passing a directory.
	 * @throws IOException
	 */
	@Test
	public void checkParameterNotDirectoryException() throws IOException {
		JavaFileUtil fileConcatenator = new JavaFileUtil();
		assertThrows(IllegalArgumentException.class, () -> {
			fileConcatenator.concatenateJavaFiles(Paths.get(TEST_DIRECTORY_PATH, "Main.java").toString(), ALL_FILES_CONTENT_FILE_NAME);
		});
	}

	/**
	 * Checks if three java files are concatenated correctly by parsing the output
	 * file and checking it for the java constructs in the source files.
	 *
	 * @throws IOException
	 */
	@Test
	public void checkJavaFileConcatenationNoError() throws IOException {
		JavaFileUtil fileConcatenator = new JavaFileUtil();
		try{
			fileConcatenator.concatenateJavaFiles(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		}
		catch(Exception e){
			fail("File concatentaion should not have thrown any exception.");
		}
	}

	/**
	 * Checks if three java files are concatenated correctly by parsing the output
	 * file and checking it for the java constructs of the file Main.java.
	 * {@link uk.ac.soton.ecs.comp1206.controlflow.filesIO.FilesIOQ2Test#MAIN}
	 *
	 * @throws IOException
	 */
	@Test
	public void checkJavaFileConcatenationCorrectMain() throws IOException {
		JavaFileUtil fileConcatenator = new JavaFileUtil();
		fileConcatenator.concatenateJavaFiles(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		Path pathToAllFile = Paths.get(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		JavaFile javaFile = new JavaFile(pathToAllFile);
		ArrayList<String> mainMethodsNames = new ArrayList<String>();
		ArrayList<String> empty = new ArrayList<String>();
		mainMethodsNames.add("main");
		checkClassExists("Main", mainMethodsNames, empty, javaFile);
	}

	/**
	 * Checks if three java files are concatenated correctly by parsing the output
	 * file and checking it for the java constructs of the file Car.java.
	 * {@link uk.ac.soton.ecs.comp1206.controlflow.filesIO.FilesIOQ2Test#CAR}
	 *
	 * @throws IOException
	 */
	@Test
	public void checkJavaFileConcatenationCorrectCar() throws IOException {
		JavaFileUtil fileConcatenator = new JavaFileUtil();
		fileConcatenator.concatenateJavaFiles(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		Path pathToAllFile = Paths.get(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		JavaFile javaFile = new JavaFile(pathToAllFile);
		ArrayList<String> empty = new ArrayList<String>();
		ArrayList<String> carMethodsNames = new ArrayList<String>();
		carMethodsNames.add("toString");
		checkClassExists("Car", carMethodsNames, empty, javaFile);
	}

	/**
	 * Checks if three java files are concatenated correctly by parsing the output
	 * file and checking it for the java constructs of the file Engine.java.
	 * {@link uk.ac.soton.ecs.comp1206.controlflow.filesIO.FilesIOQ2Test#ENGINE}
	 *
	 * @throws IOException
	 */
	@Test
	public void checkJavaFileConcatenationCorrectEngine() throws IOException {
		JavaFileUtil fileConcatenator = new JavaFileUtil();
		fileConcatenator.concatenateJavaFiles(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		Path pathToAllFile = Paths.get(TEST_DIRECTORY_PATH, ALL_FILES_CONTENT_FILE_NAME);
		JavaFile javaFile = new JavaFile(pathToAllFile);
		ArrayList<String> engineMethodsNames = new ArrayList<String>();
		engineMethodsNames.add("getFuel");
		ArrayList<String> engineFieldNames = new ArrayList<String>();
		engineFieldNames.add("fuel");
		checkClassExists("Engine", engineMethodsNames, engineFieldNames, javaFile);
	}

	/**
	 * Asserts that the class exists and has the specified methods and all the
	 * specified fields are private.
	 *
	 * @param className   The name of the class to check.
	 * @param methodNames
	 * @param fieldNames
	 * @param javaFile
	 */
	private void checkClassExists(String className, List<String> methodNames, List<String> fieldNames,
																JavaFile javaFile) {
		Optional<Class> classOptional = javaFile.getClassByName(className);
		assertTrue(classOptional.isPresent(), "Class " + className + " could not be found in file.");
		Class classObj = classOptional.get();
		checkMethodExists(methodNames, classObj);
		for (String fieldName : fieldNames) {
			checkFieldIsPrivate(fieldName, classObj);
		}
	}

	/**
	 * Asserts that the class has all of the specified methods.
	 *
	 * @param methodNames
	 * @param classObj
	 */
	private void checkMethodExists(List<String> methodNames, Class classObj) {
		List<String> namesClassMethods = classObj.getMethodNames();
		for (String methodName : methodNames) {
			assertTrue(namesClassMethods.contains(methodName),
					"Method " + methodName + " in class " + classObj.getName() + " wasn't found.");
		}

	}

	/**
	 * Asserts that the specified field exist and is private.
	 *
	 * @param fieldName
	 * @param classObj
	 */
	private void checkFieldIsPrivate(String fieldName, Class javaClass) {
		Optional<AccessModifier> fieldAccessOptional = javaClass.getFieldAccessModifier(fieldName);
		assertTrue(fieldAccessOptional.isPresent(),
				"Field " + fieldName + " in class " + javaClass.getName() + " could not be found in file.");
		boolean fieldIsPrivate = fieldAccessOptional.get().equals(AccessModifier.PRIVATE);
		assertTrue(fieldIsPrivate,
				"Field " + fieldName + " in class " + javaClass.getName() + " is not private.");
	}

	/**
	 * Creates a test folder with three java files for the concatenate test.
	 *
	 * @throws IOException
	 */
	private static void createJavaFiles() throws IOException {
		File testDirectory = new File(TEST_DIRECTORY_PATH);
		testDirectory.mkdirs();
		String[] files = { "Main.java", "Car.java", "Engine.java" };
		String[] contents = { MAIN, CAR, ENGINE };
		for (int i = 0; i < files.length; i++) {
			Path pathToFile = Paths.get(TEST_DIRECTORY_PATH, files[i]);
			FileWriter fw = new FileWriter(pathToFile.toFile());
			BufferedWriter bw = new BufferedWriter(fw);
			bw.write(contents[i]);
			bw.flush();
			bw.close();
			fw.close();
		}
	}

	/**
	 * Removes the created test folder with java files.
	 *
	 * @throws IOException
	 */
	private static void removeTestFolder() throws IOException {
		File testDirectory = new File(TEST_DIRECTORY_PATH);
		FileSystemAccess.deleteDirectory(testDirectory);
	}

	private static final String CAR = "public class Car {\n" + "	\n" + "	private Engine engine;\n" + "\n"
			+ "	/**\n" + "	 * @param engine\n" + "	 */\n" + "	public Car(Engine engine) {\n"
			+ "		this.engine = engine;\n" + "	}\n" + "\n" + "	@Override\n" + "	public String toString() {\n"
			+ "		return \"A car with a \" + this.engine.getFuel() + \" engine.\";\n" + "	}\n" + "}";

	private static final String ENGINE = "public class Engine {\n" + "	private FuelType fuel;\n" + "\n" + "	/**\n"
			+ "	 * @param fuel\n" + "	 */\n" + "	public Engine(FuelType fuel) {\n" + "		super();\n"
			+ "		this.fuel = fuel;\n" + "	}\n" + "\n" + "	/**\n" + "	 * @return the fuel\n" + "	 */\n"
			+ "	public FuelType getFuel() {\n" + "		return fuel;\n" + "	}\n" + "}";

	private static final String MAIN = "public class Main {\n" + "\n" + "	public static void main(String[] args) {\n"
			+ "		Car car = new Car(new Engine(FuelType.Diesel));\n" + "		System.out.println(car);\n" + "	}\n"
			+ "}";
}
