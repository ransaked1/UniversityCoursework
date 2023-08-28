package Graphics;

import DataManagers.CampaignManager;
import Services.CSVtoSQLReaderService;
import Services.CSVtoSQLReaderTask;
import com.jfoenix.controls.JFXButton;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.net.URL;
import java.sql.SQLException;
import java.util.Objects;
import java.util.ResourceBundle;

/**
 * Class manages interactions for the new Campaign Creator window
 */
@SuppressWarnings("ALL")
public class CreateCampaignController implements Initializable {
	public static final String INPUT_DB_NAME = "input_db";
	@FXML
	private TextField campaignNameField;
	@FXML
	private JFXButton cancelCampaignBtn;
	@FXML
	private JFXButton clickChooser;
	@FXML
	private Label clickLogFile;
	@FXML
	private JFXButton createCampaignBtn;
	@FXML
	private JFXButton impressionChooser;
	@FXML
	private Label impressionLogFile;
	@FXML
	private JFXButton serverChooser;
	@FXML
	private Label serverLogFile;
	@FXML
	private ProgressBar clickProgress;
	@FXML
	private ProgressBar impressionProgress;
	@FXML
	private ProgressBar serverProgress;

	// Declaring variables for paths and counts
	private String impressionPath;
	private Long impressionCount;

	private String clickPath;
	private Long clickCount;

	private String serverPath;
	private Long serverCount;

	FileChooser fileChooser;
	private MainSceneController mainSceneController;
	private CampaignManager campaignManager = new CampaignManager(INPUT_DB_NAME);

	private CSVtoSQLReaderService impressionService;
	private CSVtoSQLReaderService clickService;
	private CSVtoSQLReaderService serverService;

	public void setMainSceneController(MainSceneController mainSceneController) {
		this.mainSceneController = mainSceneController;
	}

	/**
	 * Initialize the creator with a default unique untitled name not in the database
	 *
	 * @param url
	 * @param resourceBundle
	 */
	@Override
	public void initialize(URL url, ResourceBundle resourceBundle) {
		// Getting the names already present
		//CampaignManager campaignManager = new CampaignManager(INPUT_DB_NAME);
		var campaignNames = campaignManager.getColumnContents("campaigns", "campaign_name");

		// Generating a name and checking it is not present
		for (int i = 1; i <= 10000; i++) {
			if (!campaignNames.contains("Untitled" + i)) {
				campaignNameField.setPromptText("Untitled" + i);
				break;
			}
		}

		// Initializig the file chooser
		fileChooser = new FileChooser();
		fileChooser.setTitle("Open Resource File");
		fileChooser.getExtensionFilters().add(
				new FileChooser.ExtensionFilter("CSV Files", "*.csv")
		);
	}

	/**
	 * Count the number of rows in given file
	 *
	 * @param fileName Name of file to count rows of
	 * @return long representing the rows counted
	 */
	public long countLineNumberReader(String fileName) {

		File file = new File(fileName);
		long lines = 0;

		try (LineNumberReader lnr = new LineNumberReader(new FileReader(file))) {

			while (lnr.readLine() != null) {} // Skip over lines until the last one

			lines = lnr.getLineNumber(); // Get the line number of that last row

		} catch (IOException e) {
			e.printStackTrace();
		}

		return lines;
	}

	/**
	 * Close the window
	 */
	@FXML
	void cancelCampaign() throws SQLException {
		//impressionService.cancel();
		//clickService.cancel();
		//serverService.cancel();

		//campaignManager.dropCampaign(campaignNameField.getText());

		Stage stage = (Stage) cancelCampaignBtn.getScene().getWindow();
		stage.close();
	}

	/**
	 * Chooser for the click log file
	 */
	@FXML
	void chooseClickFile() {
		File selectedFile = fileChooser.showOpenDialog(mainSceneController.stage);
		if (selectedFile != null) {
			setClickPath(selectedFile.getAbsolutePath());
			clickLogFile.setText(selectedFile.getName());
			fileChooser.setInitialDirectory(new File(selectedFile.getParent()));

			new Thread(() -> {
				clickCount = countLineNumberReader(getClickPath());
				//System.out.println(clickCount);
			}).start();
		}

		if (impressionPath != null && clickPath != null && serverPath != null)
			createCampaignBtn.setDisable(false);
	}

	/**
	 * Chooser for the impression log file
	 */
	@FXML
	void chooseImpressionFile() {
		File selectedFile = fileChooser.showOpenDialog(mainSceneController.stage);
		if (selectedFile != null) {
			setImpressionPath(selectedFile.getAbsolutePath());
			impressionLogFile.setText(selectedFile.getName());
			fileChooser.setInitialDirectory(new File(selectedFile.getParent()));

			new Thread(() -> {
				impressionCount = countLineNumberReader(getImpressionPath());
				//System.out.println(impressionCount);
			}).start();
		}

		if (impressionPath != null && clickPath != null && serverPath != null)
			createCampaignBtn.setDisable(false);
	}

	/**
	 * Chooser for the server log file
	 */
	@FXML
	void chooseServerFile() {
		File selectedFile = fileChooser.showOpenDialog(mainSceneController.stage);
		if (selectedFile != null) {
			setServerPath(selectedFile.getAbsolutePath());
			serverLogFile.setText(selectedFile.getName());
			fileChooser.setInitialDirectory(new File(selectedFile.getParent()));

			new Thread(() -> {
				serverCount = countLineNumberReader(getServerPath());
				//System.out.println(serverCount);
			}).start();
		}

		if (impressionPath != null && clickPath != null && serverPath != null)
			createCampaignBtn.setDisable(false);
	}

	/**
	 * Creating a new campaign on button press
	 * @throws SQLException Throw exception if failed to connect to database
	 */
	@FXML
	void createCampaign() throws SQLException {
		// Use the untitled name from the field prompt if none other specified by the user
		if (Objects.equals(campaignNameField.getText(), ""))
			campaignNameField.setText(campaignNameField.getPromptText());

		// Make progress bards visible
		impressionProgress.setVisible(true);
		clickProgress.setVisible(true);
		serverProgress.setVisible(true);

		// Add the campaign to the campaign table
		//CampaignManager campaignManager = new CampaignManager("input_db");
		campaignManager.addCampaign(campaignNameField.getText());

		// Counting the lines for all files (non threaded, fast enough)
		if (impressionCount == null)
			impressionCount = countLineNumberReader(getImpressionPath());

		if (clickCount == null)
			clickCount = countLineNumberReader(getClickPath());

		if (serverCount == null)
			serverCount = countLineNumberReader(getServerPath());

		//System.out.println(impressionCount);
		//System.out.println(clickCount);
		//System.out.println(serverCount);

		// Set the campaign name added as the one to work on in the main controller
		var campaignName = campaignNameField.getText().toLowerCase().replaceAll(" ", "_");

		try {
			// Setting up the threaded tasks to process the input files and bind their progress to the progress bars
			impressionService = new CSVtoSQLReaderService(INPUT_DB_NAME, impressionCount, impressionPath, campaignName + "_impression_log", campaignName);
			impressionProgress.progressProperty().bind(impressionService.progressProperty());

			clickService = new CSVtoSQLReaderService(INPUT_DB_NAME, clickCount, clickPath, campaignName + "_click_log", campaignName);
			clickProgress.progressProperty().bind(clickService.progressProperty());

			serverService = new CSVtoSQLReaderService(INPUT_DB_NAME, serverCount, serverPath, campaignName + "_server_log", campaignName);
			serverProgress.progressProperty().bind(serverService.progressProperty());

			// Impression service completion triggers the click log processing service
			impressionService.setOnSucceeded(e -> clickService.start());

			impressionService.setOnFailed(e -> {
				showAlert("Impression log file couldn't be read. Please check its format and data integrity.");
				impressionProgress.setVisible(false);
				clickProgress.setVisible(false);
				serverProgress.setVisible(false);
				try {
					campaignManager.dropCampaign(campaignNameField.getText());
				} catch (SQLException ex) {
					throw new RuntimeException(ex);
				}
			});

			// Click service completion triggers the server log processing service
			clickService.setOnSucceeded(e -> serverService.start());

			clickService.setOnFailed(e -> {
				showAlert("Click log file couldn't be read. Please check its format and data integrity.");
				impressionProgress.setVisible(false);
				clickProgress.setVisible(false);
				serverProgress.setVisible(false);
				try {
					campaignManager.dropCampaign(campaignNameField.getText());
				} catch (SQLException ex) {
					throw new RuntimeException(ex);
				}
			});

			// When server log processing completes, the window is automatically closed
			serverService.setOnSucceeded(e -> {
				mainSceneController.setCampaign(campaignNameField.getText());
				serverProgress.getScene().getWindow().hide();
			});

			serverService.setOnFailed(e -> {
				showAlert("Server log file couldn't be read. Please check its format and data integrity.");
				impressionProgress.setVisible(false);
				clickProgress.setVisible(false);
				serverProgress.setVisible(false);
				try {
					campaignManager.dropCampaign(campaignNameField.getText());
				} catch (SQLException ex) {
					throw new RuntimeException(ex);
				}
			});

			impressionService.start();

		}catch(Exception e){
			//e.printStackTrace();
			showAlert("Log file couldn't be read. Please check its format and data integrity.");
		}
	}

	void showAlert(String message) {
		Alert alert = new Alert(Alert.AlertType.ERROR, message, ButtonType.CANCEL);
		alert.showAndWait();
	}

	void setImpressionPath(String s){
		impressionPath = s;
	}

	void setClickPath(String s){
		clickPath = s;
	}

	void setServerPath(String s){
		serverPath = s;
	}

	public String getImpressionPath() {
		return impressionPath;
	}

	public String getClickPath() {
		return clickPath;
	}

	public String getServerPath() {
		return serverPath;
	}
}
