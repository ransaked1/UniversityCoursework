import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import uk.ac.soton.ecs.comp1206.labtestlibrary.io.FileSystemAccess;

/**
 * Test for lab 5 on files and IO.
 *
 * @author jan, mjg1a10
 *
 */
class FilesIOQ1Test {

    static final String CHAR_FILE_PATH = "randomNumbersCharByteFile.txt";
    static final String BYTE_FILE_PATH = "randomNumbersByteFile.out";
    static long seed = 1;
    static ArrayList<Integer> randomNumbers;

    /**
     * Creates the random numbers and the java files for the file writing and the java file concatenation tasks, respectively.
     * @throws Exception
     */
    @BeforeAll
    public static void setUpBeforeClass() throws Exception {
        initialiseRandomNumbers();
    }

    /**
     * Removes the test folder and test files.
     * @throws IOException
     */
    @AfterAll
    public static void cleanUpAfterClass() throws IOException {
        removeTestFiles();
    }

    /**
     * Test that the file written using a char stream was created.
     */
    @Test
    public void checkCharFileExists() {
        File file = new File(CHAR_FILE_PATH);
        boolean doseFileExist = file.isFile();
        assertTrue(doseFileExist, "Char file does not exist.");
    }

    /**
     * Test that the file written using a char stream contains data.
     */
    @Test
    public void checkCharFileContainsData() {
        File file = new File(CHAR_FILE_PATH);
        boolean isFileSizeGreaterZero = (file.length() > 0);
        assertTrue(isFileSizeGreaterZero, "Char file has no content.");
    }

    /**
     * Test that the file written using a char stream contains the right data.
     */
    @Test
    public void checkCharFileRightData() throws IOException {
        File file = new File(CHAR_FILE_PATH);
        FileReader fr = new FileReader(file);
        BufferedReader br = new BufferedReader(fr);
        ArrayList<Integer> readNumbers = new ArrayList<>();
        String currLine = "";
        while ((currLine = br.readLine()) != null) {
            readNumbers.add(Integer.parseInt(currLine));
        }
        fr.close();
        assertEquals(randomNumbers, readNumbers, "Read integers are different to expected ones.");
    }

    /**
     * Test that the file written using a byte stream exists.
     */
    @Test
    public void checkByteFileExists() {
        File file = new File(BYTE_FILE_PATH);
        boolean doseFileExist = file.isFile();
        assertTrue(doseFileExist, "Byte file does not exist.");
    }

    /**
     * Test that the file written using a byte steam contains data.
     */
    @Test
    public void checkByteFileContainsData() {
        File file = new File(BYTE_FILE_PATH);
        boolean isFileSizeGreaterZero = (file.length() > 0);
        assertTrue(isFileSizeGreaterZero, "Byte file has no content.");
    }

    /**
     * Test that the file written using a byte stream contains the right data.
     */
    @Test
    public void checkByteFileRightData() throws IOException {
        File file = new File(BYTE_FILE_PATH);
        FileInputStream fos = new FileInputStream(file);
        DataInputStream dos = new DataInputStream(fos);
        ArrayList<Integer> readNumbers = new ArrayList<>();
        while (dos.available() > 0) {
            readNumbers.add(dos.readInt());
        }
        dos.close();
        fos.close();
        assertEquals(randomNumbers, readNumbers, "Read integers are different to expected ones.");
    }

    /**
     * Uses the seed to calculate all the random number to compare them to the file
     * content of the write random number to file exercise.
     *
     * @throws IOException
     */
    private static void initialiseRandomNumbers() throws IOException {
        seed = new Random().nextLong();
        RandomNumberWriter randomByteWriter = new RandomNumberWriter(seed);
        RandomNumberWriter randomCharWriter = new RandomNumberWriter(seed);
        randomByteWriter.writeRandomByte(BYTE_FILE_PATH);
        randomCharWriter.writeRandomChars(CHAR_FILE_PATH);
        Random random = new Random(seed);
        randomNumbers = new ArrayList<Integer>();
        for (int i = 0; i < 10000; i++) {
            Integer n = Integer.valueOf(random.nextInt(100000));
            randomNumbers.add(n);
        }
    }

    /**
     * Removes the created test files.
     *
     * @throws IOException
     */
    private static void removeTestFiles() {
        FileSystemAccess.deleteFile(new File(CHAR_FILE_PATH));
        FileSystemAccess.deleteFile(new File(BYTE_FILE_PATH));
    }
}