package uk.ac.soton.comp1206;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Hello world! A java doc.
 *
 */
public class App 
{
	public static final Logger logger = LogManager.getLogger("HelloWorld");
	
    public static void main( String[] args )
    {
        logger.error("Hello,World!");
    }
}
