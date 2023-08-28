package DataManagers;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;

class CampaignManagerTest {

	static CSVtoSQLReader reader = new CSVtoSQLReader("input_db_test");

	static CampaignManager campaignManager = new CampaignManager("input_db_test");

	@Test
	@DisplayName("Checking data integrity after column read")
	void getColumnContents() {
		reader.writeLogToTable("input/server_log.csv", "server_log");
		ArrayList<Object> result = campaignManager.getColumnContents("server_log", "id");
		assertEquals("8895519749317550080", result.get(0));
		assertEquals("4323349762979258368", result.get(4613));
		assertEquals("6066646639206401024", result.get(12717));
		assertEquals("7951651794438804480", result.get(23921));
		assertEquals("6752478673601913856", result.get(23922));
	}

	@Test
	@DisplayName("Checking correct campaign table creation")
	void addCampaign() throws SQLException {
		campaignManager.setupCampaignTable("campaigns", "campaign_name, impression_table_name, click_table_name, server_table_name");
		var expected = new ArrayList<String>();
		campaignManager.addCampaign("Test Campaign 1"); expected.add("Test Campaign 1");
		campaignManager.addCampaign("Test Campaign 2"); expected.add("Test Campaign 2");
		campaignManager.addCampaign("Test Campaign 3"); expected.add("Test Campaign 3");
		campaignManager.addCampaign("Test Campaign 4"); expected.add("Test Campaign 4");
		assertTrue(reader.checkTableExists("campaigns"));
		assertEquals(expected, campaignManager.getColumnContents("campaigns", "campaign_name"));
	}

	@Test
	@DisplayName("Checking database integrity after campaign removal")
	void dropCampaign() throws SQLException {
		campaignManager.setupCampaignTable("campaigns", "campaign_name, impression_table_name, click_table_name, server_table_name");
		campaignManager.addCampaign("Test Campaign");
		reader.setupInputTable("test_campaign_impression_log", "dummy");
		reader.setupInputTable("test_campaign_click_log", "dummy");
		reader.setupInputTable("test_campaign_server_log", "dummy");

		campaignManager.dropCampaign("Test Campaign");
		assertFalse(reader.checkTableExists("test_campaign_impression_log"));
		assertFalse(reader.checkTableExists("test_campaign_click_log"));
		assertFalse(reader.checkTableExists("test_campaign_server_log"));
		assertTrue(reader.checkTableExists("campaigns"));
	}

	@AfterAll
	static void afterAll() throws SQLException {
		reader.dropTable("test_campaign_impression_log");
		reader.dropTable("test_campaign_click_log");
		reader.dropTable("test_campaign_server_log");
		reader.dropTable("impression_log");
		reader.dropTable("click_log");
		reader.dropTable("server_log");
		reader.dropTable("campaigns");
	}
}