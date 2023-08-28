package Graphics;

import DataManagers.CampaignManager;
import DataManagers.DataCalculateManager;
import DataManagers.DataGraphManager;
import Services.FetchDatesTask;
import Services.FetchGraphDataTask;
import Services.MetricCalculatorTask;
import com.jfoenix.controls.*;
import com.jfoenix.transitions.hamburger.HamburgerBackArrowBasicTransition;
import javafx.application.Application;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.geometry.Point2D;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.chart.*;
import javafx.scene.control.*;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.ScrollEvent;
import javafx.scene.input.ZoomEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.shape.Box;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Path;
import javafx.scene.text.Text;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.util.Duration;
import javafx.util.Pair;

import java.awt.*;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.sql.SQLException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.*;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;


@SuppressWarnings("SwitchLabeledRuleCanBeCodeBlock")
public class MainSceneController extends Application implements Initializable {
	// Constants for the database to use and operation options used when threading the fetching and calculations
	private static final String INPUT_DB_NAME = "input_db";
	private static final int FETCH_IMPRESSION = 0;
	private static final int FETCH_CLICKS = 1;
	private static final int FETCH_UNIQUES = 2;
	private static final int FETCH_BOUNCES = 3;
	private static final int FETCH_CONVERSIONS = 4;
	private static final int FETCH_COST = 5;
	private static final int CALCULATE_CTR = 6;
	private static final int CALCULATE_CPM = 7;
	private static final int CALCULATE_CPC = 8;
	private static final int CALCULATE_CPA = 9;
	private static final int CALCULATE_BOUNCE_RATE = 10;
	private static final int CALCULATE_DISTRIBUTION = 11;

	private static final int GRANULARITY_HOUR = 0;
	private static final int GRANULARITY_DAY = 1;
	private static final int GRANULARITY_WEEK = 2;

	private static final int GENDER_BOTH = 0;
	private static final int GENDER_FEMALE = 1;
	private static final int GENDER_MALE = 2;

	private static final int AGE_UNDER_25 = 0;
	private static final int AGE_25_TO_34 = 1;
	private static final int AGE_35_TO_44 = 2;
	private static final int AGE_45_TO_54 = 3;
	private static final int AGE_ABOVE_54 = 4;

	private static final int INCOME_LOW = 0;
	private static final int INCOME_MEDIUM = 1;
	private static final int INCOME_HIGH = 2;

	private static final int CONTEXT_SM = 0;
	private static final int CONTEXT_SHOPPING = 1;
	private static final int CONTEXT_BLOG = 2;
	private static final int CONTEXT_NEWS = 3;

	private static final int BOUNCE_TIME = 0;
	private static final int BOUNCE_SINGLE = 1;

	@FXML
	private ToggleButton granularityButtonHour;
	@FXML
	private ToggleButton granularityButtonDay;
	@FXML
	private ToggleButton granularityButtonWeek;
	@FXML
	private ToggleGroup granularityButtons;

	@FXML
	private ToggleButton genderButtonBoth;
	@FXML
	private ToggleButton genderButtonFemale;
	@FXML
	private ToggleButton genderButtonMale;
	@FXML
	private ToggleGroup genderButtons;

	@FXML
	private JFXRadioButton button25to34;
	@FXML
	private JFXRadioButton button35to44;
	@FXML
	private JFXRadioButton button45to54;
	@FXML
	private JFXRadioButton buttonAbove54;
	@FXML
	private JFXRadioButton buttonUnder25;

	@FXML
	private JFXRadioButton buttonIncomeHigh;
	@FXML
	private JFXRadioButton buttonIncomeMedium;
	@FXML
	private JFXRadioButton buttonIncomeLow;

	@FXML
	private JFXRadioButton buttonContextBlog;
	@FXML
	private JFXRadioButton buttonContextNews;
	@FXML
	private JFXRadioButton buttonContextSM;
	@FXML
	private JFXRadioButton buttonContextShopping;

	@FXML
	private ToggleButton bounceButtonTime;
	@FXML
	private ToggleButton bounceButtonSingle;

	@FXML
	private JFXButton applyFiltersButton;
	@FXML
	private Label graphNameLabel;
	@FXML
	private TextField graphNameField;
	@FXML
	private Button saveGraphButton;
	@FXML
	private Button deleteGraphButton;

	@FXML
	private JFXDrawer campaignDrawer;
	@FXML
	private JFXHamburger campaignHamburger;
	@FXML
	private Label lblTotalCost;
	@FXML
	private Label lblImpressions;
	@FXML
	private Label lblClicks;
	@FXML
	private Label lblCtr;
	@FXML
	private Label lblBounces;
	@FXML
	private Label lblConversions;
	@FXML
	private Label lblCpc;
	@FXML
	private Label lblUniques;
	@FXML
	private Label lblCpm;
	@FXML
	private Label lblBounceRate;
	@FXML
	private Label lblCpa;
	@FXML
	private LineChart lineChart;
	@FXML
	private BarChart histogram;
	@FXML
	private ComboBox<String> selectTypeComBox;
	@FXML
	private DatePicker timePickerEnd;
	@FXML
	private DatePicker timePickerStart;
	@FXML
	public JFXComboBox<String> campaignPicker;
	@FXML
	private Label cursorCoords;
	private Boolean isHistoActive = false;

	// The names of the tables
	private String tableNameImpressions;
	private String tableNameClick;
	private String tableNameServer;

	Stage stage;
	private final DataCalculateManager calcManager = new DataCalculateManager(INPUT_DB_NAME);
	private final DataGraphManager graphManager = new DataGraphManager(INPUT_DB_NAME);

	private String campaign = ""; // Current campaign worked on in the main window
	private int granularity = GRANULARITY_WEEK;
	private String graphType = "Impressions";
	private XYChart.Series<String, Number> series;
	private XYChart.Series<String, Number> seriesHour;
	private XYChart.Series<String, Number> seriesDay;
	private XYChart.Series<String, Number> seriesWeek;
	private XYChart.Series<String, Number> histoSeries;



	private ArrayList<String> graphNames = new ArrayList<>();
	private ArrayList<String> histoNames = new ArrayList<>();
	private ArrayList<XYChart.Series<String, Number>> savedGraphDataHour = new ArrayList<>();
	private ArrayList<XYChart.Series<String, Number>> savedGraphDataDay = new ArrayList<>();
	private ArrayList<XYChart.Series<String, Number>> savedGraphDataWeek = new ArrayList<>();
	private ArrayList<XYChart.Series<String, Number>> savedHistoData = new ArrayList<>();

	int finalI = 0;


	// Initializing executor to manage the threaded operations
	private final Executor exec = Executors.newCachedThreadPool(runnable -> {
		Thread t = new Thread(runnable);
		t.setDaemon(true);
		return t;
	});

	private final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	/**
	 * Initializing the main window from FXML
	 *
	 * @param stage Stage for the main scene
	 * @throws IOException Throw exception if failed to connect to database
	 */
	@Override
	public void start(Stage stage) throws IOException {
		this.stage = stage;
		this.stage.setTitle("Ad Auction Dashboard");
		Parent root = FXMLLoader.load(Objects.requireNonNull(getClass().getClassLoader().getResource("dashboard.fxml")));
		Scene scene = new Scene(root, 1024, 768);
		stage.setScene(scene);
		stage.show();
	}

	@Override
	public void initialize(URL loation, ResourceBundle resources) {
		// Initializing the main screen texts when application starts
		lblTotalCost.setText("NaN");
		//lblTotalCost.setGraphic(new ProgressIndicator());
		lblImpressions.setText("NaN");
		lblClicks.setText("NaN");
		lblCtr.setText("NaN");
		lblBounces.setText("NaN");
		lblConversions.setText("NaN");
		lblCpc.setText("NaN");
		lblUniques.setText("NaN");
		lblCpm.setText("NaN");
		lblBounceRate.setText("NaN");
		lblCpa.setText("NaN");

		ObservableList<String> options =
				FXCollections.observableArrayList(
						"Impressions",
						"Clicks",
						"Uniques",
						"Bounces",
						"Conversions",
						"Bounce Rate",
						"Cost",
						"CTR",
						"CPM",
						"CPC",
						"CPA",
						"Click Distribution"
				);
		selectTypeComBox.setItems(options);
		selectTypeComBox.setValue("Impressions");

		saveGraphButton.setDisable(true);
		deleteGraphButton.setDisable(true);

		lineChart.setCreateSymbols(false);
		lineChart.getXAxis().setAnimated(false);
		lineChart.getYAxis().setAnimated(false);
		lineChart.setLegendVisible(false);

		histogram.setCategoryGap(0);
		histogram.setLegendVisible(true);
		histogram.setAnimated(false);
		histogram.setLegendSide(Side.RIGHT);

		// Load the drawer FXML into the Anchor Pane that will hold it
		try {
			FXMLLoader loader = new FXMLLoader(getClass().getClassLoader().getResource("campaign_drawer.fxml"));
			AnchorPane box = loader.load();
			CampaignDrawerController controller = loader.getController();
			controller.setMainSceneController(this);
			campaignDrawer.setSidePane(box);

			//Changing offset so the buttons below the drawer are clickable when it's closed
			//https://stackoverflow.com/questions/52172841/javafx-jfoenix-drawer-is-blocking-the-nodes-behind-the-overlay
			campaignDrawer.setOnDrawerOpening(event ->
			{
				AnchorPane.setRightAnchor(campaignDrawer, 0.0);
				AnchorPane.setLeftAnchor(campaignDrawer, 0.0);
				AnchorPane.setTopAnchor(campaignDrawer, 62.0);
			});
			campaignDrawer.setOnDrawerClosed(event ->
			{
				AnchorPane.clearConstraints(campaignDrawer);
				AnchorPane.setLeftAnchor(campaignDrawer, -255.0);
				AnchorPane.setTopAnchor(campaignDrawer, 62.0);
				AnchorPane.setBottomAnchor(campaignDrawer, 0.0);
			});
		} catch (IOException ex) {
			Logger.getLogger(MainSceneController.class.getName()).log(Level.SEVERE, null, ex);
		}

		// The Hamburger icon that controls the drawer opening and closing
		campaignHamburger.addEventHandler(MouseEvent.MOUSE_PRESSED, (e) -> {
			campaignDrawer.toggle(); // Toggle the drawer when hamburger pressed
		});

		fetchCampaignsIntoPicker();
	}

	//https://stackoverflow.com/questions/16337218/open-pdf-file-located-in-a-ressource-folder
	@FXML
	void openManual() {
		if (Desktop.isDesktopSupported())
		{
			InputStream jarPdf = getClass().getClassLoader().getResourceAsStream("manual.pdf");

			try {
				File pdfTemp = new File("manual.pdf");
				// Extraction du PDF qui se situe dans l'archive
				FileOutputStream fos = new FileOutputStream(pdfTemp);
				while (jarPdf.available() > 0) {
					fos.write(jarPdf.read());
				}   // while (pdfInJar.available() > 0)
				fos.close();
				// Ouverture du PDF
				Desktop.getDesktop().open(pdfTemp);
			}   // try

			catch (IOException e) {
				System.out.println("erreur : " + e);
			}   // catch (IOException e)
		}
	}

	public void fetchCampaignsIntoPicker() {
		CampaignManager campaignManager = new CampaignManager("input_db");
		var out = campaignManager.getColumnContents("campaigns", "campaign_name");

		// Store fetched names into an array list of cells
		List<String> list = new ArrayList<>();
		for (Object s : out) {
			list.add((String) s);
		}

		ObservableList<String> campaignObservableList = FXCollections.observableArrayList(list);

		// Muting setOnAction so it's not triggered when setting the items
		EventHandler<ActionEvent> handler = campaignPicker.getOnAction();
		campaignPicker.setOnAction(null);
		campaignPicker.setItems(campaignObservableList);
		campaignPicker.setOnAction(handler);
	}

	public void focusCampaign() throws SQLException {
		if (Objects.equals(campaign, "")) {
			lblTotalCost.setText("NaN");
			lblImpressions.setText("NaN");
			lblClicks.setText("NaN");
			lblCtr.setText("NaN");
			lblBounces.setText("NaN");
			lblConversions.setText("NaN");
			lblCpc.setText("NaN");
			lblUniques.setText("NaN");
			lblCpm.setText("NaN");
			lblBounceRate.setText("NaN");
			lblCpa.setText("NaN");

			lineChart.getData().clear();
			histogram.getData().clear();
		} else {
			// Resetting filters before getting the new data
			calcManager.resetFilters();

			// Deriving the names for tables from the campaign name
			// This process is uniformized across the application so we can make this assumptions
			var campaignNameProcessed = campaign.toLowerCase().replaceAll(" ", "_");
			tableNameImpressions = campaignNameProcessed + "_impression_log";
			tableNameClick = campaignNameProcessed + "_click_log";
			tableNameServer = campaignNameProcessed + "_server_log";

			// Method that gets the initial dates for the date pickers
			launchFetchDatesTask();

			// Method starts the tasks for fetching and calculating metrics
			fetchAndCalculateTasks();
		}
	}

	@FXML
	void selectGraphType() {
		graphType = selectTypeComBox.getValue();
		if (graphType.equals("Click Distribution")){
			graphNameField.setPromptText(setHistoName());
		}else{
			graphNameField.setPromptText(setGraphName());
		}
		if (campaign != "")
			drawGraph();
	}

	@FXML
	void switchFocusedCampaign() throws SQLException {
		if (campaignPicker.getValue() != null)
			campaign = campaignPicker.getValue();

		saveGraphButton.setDisable(false);
		deleteGraphButton.setDisable(false);
		toggleSelectorEnabling();

		calcManager.resetFilters();

		focusCampaign();

		saveGraphButton.setDisable(false);
		deleteGraphButton.setDisable(false);
		toggleSelectorEnabling();
	}


	@FXML
	void toggleGranularity(ActionEvent event) throws ParseException {
		if (event.getSource() == granularityButtonHour) {
			granularity = GRANULARITY_HOUR;
		}
		if (event.getSource() == granularityButtonDay) {
			granularity = GRANULARITY_DAY;
		}
		if (event.getSource() == granularityButtonWeek) {
			granularity = GRANULARITY_WEEK;
		}
		if (!Objects.equals(campaign, "")) {
			drawGraph();
		}
	}

	@FXML
	void toggleGenderFilter(ActionEvent event) throws SQLException {
		if (event.getSource() == genderButtonBoth) {
			calcManager.setGenderFilter(GENDER_BOTH);
		}
		if (event.getSource() == genderButtonFemale) {
			calcManager.setGenderFilter(GENDER_FEMALE);
		}
		if (event.getSource()== genderButtonMale) {
			calcManager.setGenderFilter(GENDER_MALE);
		}
	}

	@FXML
	void addAgeFilter(ActionEvent event) throws SQLException {
		boolean allUnselected = true;
		if(button25to34.isSelected() || button35to44.isSelected() || button45to54.isSelected() || buttonAbove54.isSelected() || buttonUnder25.isSelected()){
			allUnselected = false;
		}

		if (event.getSource() == buttonUnder25) {
			if (allUnselected){buttonUnder25.setSelected(true); return;}
			calcManager.setAgeFilter(AGE_UNDER_25, !calcManager.getAgeFilter(AGE_UNDER_25));
		}
		if (event.getSource() == button25to34) {
			if (allUnselected){button25to34.setSelected(true); return;}
			calcManager.setAgeFilter(AGE_25_TO_34, !calcManager.getAgeFilter(AGE_25_TO_34));
		}
		if (event.getSource() == button35to44) {
			if (allUnselected){button35to44.setSelected(true); return;}
			calcManager.setAgeFilter(AGE_35_TO_44, !calcManager.getAgeFilter(AGE_35_TO_44));
		}
		if (event.getSource() == button45to54) {
			if (allUnselected){button45to54.setSelected(true); return;}
			calcManager.setAgeFilter(AGE_45_TO_54, !calcManager.getAgeFilter(AGE_45_TO_54));
		}
		if (event.getSource() == buttonAbove54) {
			if (allUnselected){buttonAbove54.setSelected(true); return;}
			calcManager.setAgeFilter(AGE_ABOVE_54, !calcManager.getAgeFilter(AGE_ABOVE_54));
		}
	}

	@FXML
	void addIncomeFilter(ActionEvent event) throws SQLException {
		boolean allUnselected = true;
		if(buttonIncomeLow.isSelected() || buttonIncomeMedium.isSelected() || buttonIncomeHigh.isSelected()){
			allUnselected = false;
		}

		if (event.getSource() == buttonIncomeLow) {
			if (allUnselected){buttonIncomeLow.setSelected(true); return;}
			calcManager.setIncomeFilter(INCOME_LOW, !calcManager.getIncomeFilter(INCOME_LOW));
		}
		if (event.getSource() == buttonIncomeMedium) {
			if (allUnselected){buttonIncomeMedium.setSelected(true); return;}
			calcManager.setIncomeFilter(INCOME_MEDIUM, !calcManager.getIncomeFilter(INCOME_MEDIUM));
		}
		if (event.getSource() == buttonIncomeHigh) {
			if (allUnselected){buttonIncomeHigh.setSelected(true); return;}
			calcManager.setIncomeFilter(INCOME_HIGH, !calcManager.getIncomeFilter(INCOME_HIGH));
		}
	}

	@FXML
	void addContextFilter(ActionEvent event) throws SQLException {
		boolean allUnselected = true;
		if(buttonContextSM.isSelected() || buttonContextShopping.isSelected() || buttonContextBlog.isSelected() || buttonContextNews.isSelected()){
			allUnselected = false;
		}

		if (event.getSource() == buttonContextSM) {
			if (allUnselected){buttonContextSM.setSelected(true); return;}
			calcManager.setContextFilter(CONTEXT_SM, !calcManager.getContextFilter(CONTEXT_SM));
		}
		if (event.getSource() == buttonContextShopping) {
			if (allUnselected){buttonContextShopping.setSelected(true); return;}
			calcManager.setContextFilter(CONTEXT_SHOPPING, !calcManager.getContextFilter(CONTEXT_SHOPPING));
		}
		if (event.getSource() == buttonContextBlog) {
			if (allUnselected){buttonContextBlog.setSelected(true); return;}
			calcManager.setContextFilter(CONTEXT_BLOG, !calcManager.getContextFilter(CONTEXT_BLOG));
		}
		if (event.getSource() == buttonContextNews) {
			if (allUnselected){buttonContextNews.setSelected(true); return;}
			calcManager.setContextFilter(CONTEXT_NEWS, !calcManager.getContextFilter(CONTEXT_NEWS));
		}
	}

	@FXML
	void toggleBounceFilter(ActionEvent event) throws SQLException {
		if (event.getSource() == bounceButtonTime) {
			calcManager.setBounceFilter(BOUNCE_TIME);
		}
		if (event.getSource() == bounceButtonSingle) {
			calcManager.setBounceFilter(BOUNCE_SINGLE);
		}

	}

	@FXML
	void applyFilters(ActionEvent event) throws SQLException {
		if (!Objects.equals(campaign, "")) {
			fetchAndCalculateTasks();
		}
	}

	@FXML
	void setTimeFilterEnd() throws SQLException, ParseException {
		calcManager.setTimeFilterStart(timePickerStart.getValue() + " 00:00:00");
		calcManager.setTimeFilterEnd(timePickerEnd.getValue() + " 23:59:59");
		var campaignNameProcessed = campaign.toLowerCase().replaceAll(" ", "_");
		tableNameImpressions = campaignNameProcessed + "_impression_log";
		tableNameClick = campaignNameProcessed + "_click_log";
		tableNameServer = campaignNameProcessed + "_server_log";
		if (!sdf.parse(timePickerStart.getValue() + " 00:00:00").before(sdf.parse(timePickerEnd.getValue() + " 23:59:59")))
			showAlert("Start date is set after the end date!");
		fetchAndCalculateTasks();
	}

	@FXML
	void setTimeFilterStart() throws SQLException, ParseException {
		calcManager.setTimeFilterStart(timePickerStart.getValue() + " 00:00:00");
		calcManager.setTimeFilterEnd(timePickerEnd.getValue() + " 23:59:59");
		var campaignNameProcessed = campaign.toLowerCase().replaceAll(" ", "_");
		tableNameImpressions = campaignNameProcessed + "_impression_log";
		tableNameClick = campaignNameProcessed + "_click_log";
		tableNameServer = campaignNameProcessed + "_server_log";
		if (!sdf.parse(timePickerStart.getValue() + " 00:00:00").before(sdf.parse(timePickerEnd.getValue() + " 23:59:59")))
			showAlert("Start date is set after the end date!");
		fetchAndCalculateTasks();
	}

	void showAlert(String message) {
		Alert alert = new Alert(Alert.AlertType.ERROR, message, ButtonType.CANCEL);
		alert.showAndWait();
	}

	/**
	 * Triggered when the campaign creator is closed and handles the actions after a new campaign is added
	 */
	@FXML
	public void handleAddCampaign() {
		// Close the campaign drawer automatically
		campaignDrawer.toggle();

		var temp = campaign;
		fetchCampaignsIntoPicker();
		campaign = temp;

		campaignPicker.setValue(campaign);
	}

	public void setFocusedCampaign(String campaignName) {
		campaignPicker.setValue(campaignName);
	}

	private void fetchAndCalculateTasks() throws SQLException {
		// Disable switching while loading to avoid data overwrites with multiple tasks running in parallel

		toggleSelectorEnabling();
		//System.out.println("Update started!");

		startProgressIndicators();

		// Refactored methods that launch tasks for metric fetching and calculations
		launchFiveTasks(FETCH_IMPRESSION, FETCH_CLICKS, FETCH_UNIQUES, FETCH_BOUNCES, FETCH_CONVERSIONS);
		launchFiveTasks(FETCH_COST, CALCULATE_CTR, CALCULATE_BOUNCE_RATE, CALCULATE_CPC, CALCULATE_CPA);
		launchTask(CALCULATE_CPM, tableNameImpressions, tableNameClick, tableNameServer);

		// Calculate the graph
		System.out.println("Switching graph");
		fetchGraphsData(tableNameImpressions, tableNameClick, tableNameServer);
	}

	private void fetchGraphsData(String impressionTableName, String clickTableName, String serverTableName) {

		launchFiveTasksGraph(FETCH_IMPRESSION, FETCH_CLICKS, FETCH_UNIQUES, FETCH_BOUNCES, FETCH_CONVERSIONS);
		var task1 = new FetchGraphDataTask(FETCH_COST, impressionTableName, clickTableName, serverTableName);
		task1.setOnSucceeded(e -> {
			System.out.println("Finished task1");
			launchFourTasksGraph(CALCULATE_CTR, CALCULATE_CPC, CALCULATE_BOUNCE_RATE, CALCULATE_CPA);
			var task2 = new FetchGraphDataTask(CALCULATE_CPM, impressionTableName, clickTableName, serverTableName);
			task2.setOnSucceeded(event -> {
				System.out.println("Finished task2");
				var task3 = new FetchGraphDataTask(CALCULATE_DISTRIBUTION, impressionTableName, clickTableName, serverTableName);
				task3.setOnSucceeded(event2 -> { System.out.println("Finished task3"); drawGraph();});
				task3.setOnFailed(event2 -> System.out.println("Failed graph task: " + CALCULATE_DISTRIBUTION));
				System.out.println("Started task3");
				exec.execute(task3);
			});
			task2.setOnFailed(event -> System.out.println("Failed graph task: " + CALCULATE_CPM));
			System.out.println("Started task2");
			exec.execute(task2);
		});
		task1.setOnFailed(e -> System.out.println("Failed graph task: " + CALCULATE_CPM));
		System.out.println("Started task1");
		exec.execute(task1);
	}

	// https://stackoverflow.com/questions/25892695/tooltip-on-line-chart-showing-date
	private void drawGraph() {
			System.out.println("DrawingGraph");

			int DEFAULT_LOWER_BOUND = 0;
			int DEFAULT_UPPER_BOUND = 275000;
			int DEFAULT_TICK_UNIT = 1;

		//boolean titled = false;
			saveGraphButton.setDisable(true);
			deleteGraphButton.setDisable(true);
			series = new XYChart.Series<>();

			seriesHour = new XYChart.Series<>();
			seriesDay = new XYChart.Series<>();
			seriesWeek = new XYChart.Series<>();

			histogram.getData().clear();
			lineChart.setVisible(true);
			lineChart.getData().clear();
			histogram.setVisible(false);

			NumberAxis xAxis = new NumberAxis(DEFAULT_LOWER_BOUND, DEFAULT_UPPER_BOUND, DEFAULT_TICK_UNIT);
			NumberAxis yAxis = new NumberAxis(DEFAULT_LOWER_BOUND, DEFAULT_UPPER_BOUND, DEFAULT_TICK_UNIT);
			//lineChart = new LineChart<>(xAxis, yAxis);

			//System.out.println(graphManager.getConversionsPoints(GRANULARITY_HOUR));
			//System.out.println(graphManager.getConversionsPoints(GRANULARITY_DAY));
			//System.out.println(graphManager.getConversionsPoints(GRANULARITY_WEEK));

			//ArrayList<String> dates = new ArrayList<>();\

			ArrayList<Pair<String, Integer>> pointsInteger = new ArrayList<>();
			ArrayList<Pair<String, Double>> pointsDouble = new ArrayList<>();
			boolean disableSelectorsForHistogram = false;

			for (int i = 0; i < 3; i++) {
				//titled = false;
				switch (graphType) {
					case "Impressions" -> pointsInteger = graphManager.getImpressionsPoints(i);
					case "Clicks" -> pointsInteger = graphManager.getClicksPoints(i);
					case "Uniques" -> pointsInteger = graphManager.getUniquesPoints(i);
					case "Bounces" -> pointsInteger = graphManager.getBouncesPoints(i);
					case "Conversions" -> pointsInteger = graphManager.getConversionsPoints(i);
					case "Bounce Rate" -> pointsDouble = graphManager.getBRatePoints(i);
					case "Cost" -> pointsDouble = graphManager.getCostsPoints(i);
					case "CTR" -> pointsDouble = graphManager.getCtrPoints(i);
					case "CPM" -> pointsDouble = graphManager.getCpmPoints(i);
					case "CPC" -> pointsDouble = graphManager.getCpcPoints(i);
					case "CPA" -> pointsDouble = graphManager.getCpaPoints(i);
					case "Click Distribution" -> {
						disableSelectorsForHistogram = true;
						drawHistogram();
						return ;
					}
				}



				if (pointsInteger.size() != 0) {
					for (Pair point : pointsInteger) {
						XYChart.Data<String, Number> data = new XYChart.Data(point.getKey(),point.getValue());
						Circle label = new Circle(2);

						data.setNode(label);

						if (graphNameField.getText().equals(""))
							data.setExtraValue(graphNameField.getPromptText());
						else
							data.setExtraValue(graphNameField.getText());

						data.getNode().setOnMouseEntered(mouseEvent -> {
							label.setRadius(label.getRadius()*3);
							cursorCoords.setText(data.getExtraValue() + ": " + data.getXValue() + " , " + data.getYValue());
						});

						data.getNode().setOnMouseExited(mouseEvent -> {
							label.setRadius(label.getRadius()/3);
							cursorCoords.setText("");
						});

						switch (i) {
							case GRANULARITY_HOUR -> seriesHour.getData().add(data);
							case GRANULARITY_DAY -> seriesDay.getData().add(data);
							case GRANULARITY_WEEK -> seriesWeek.getData().add(data);
						}
					}

				} else {
					for (Pair point : pointsDouble) {
						XYChart.Data<String, Number> data = new XYChart.Data(point.getKey(),point.getValue());
						Circle label = new Circle(2);
						data.setNode(label);

						if (graphNameField.getText().equals(""))
							data.setExtraValue(graphNameField.getPromptText());
						else
							data.setExtraValue(graphNameField.getText());

						data.getNode().setOnMouseEntered(mouseEvent -> {
							label.setRadius(label.getRadius()*3);
							cursorCoords.setText(data.getExtraValue() + ": " + data.getXValue() + " , " + data.getYValue());
						});

						data.getNode().setOnMouseExited(mouseEvent -> {
							label.setRadius(label.getRadius()/3);
							cursorCoords.setText("");
						});

						switch (i) {
							case GRANULARITY_HOUR -> seriesHour.getData().add(data);
							case GRANULARITY_DAY -> seriesDay.getData().add(data);
							case GRANULARITY_WEEK -> seriesWeek.getData().add(data);
						}
					}
				}
			}

			switch(granularity){
				case GRANULARITY_HOUR -> series = seriesHour;
				case GRANULARITY_DAY -> series = seriesDay;
				case GRANULARITY_WEEK -> series = seriesWeek;
			}

			if (graphNameField.getText().equals(""))
				series.setName(graphNameField.getPromptText());
			else
				series.setName(graphNameField.getText());

			lineChart.getData().addAll(series);

			if (graphNames.size() > 0){
				if (granularity == GRANULARITY_DAY) {
					for (int i = 0; i < savedGraphDataDay.size(); i++) {
						savedGraphDataDay.get(i).setName(graphNames.get(i));
						lineChart.getData().addAll(savedGraphDataDay.get(i));
					}
				}
				if (granularity == GRANULARITY_HOUR) {
					for (int i = 0; i < savedGraphDataHour.size(); i++) {
						savedGraphDataHour.get(i).setName(graphNames.get(i));
						lineChart.getData().addAll(savedGraphDataHour.get(i));
					}
				}
				if (granularity == GRANULARITY_WEEK) {
					for (int i = 0; i < savedGraphDataWeek.size(); i++) {
						savedGraphDataWeek.get(i).setName(graphNames.get(i));
						lineChart.getData().addAll(savedGraphDataWeek.get(i));
					}
				}
			}

			enableSelectors();

			if (disableSelectorsForHistogram) {
				granularityButtonHour.setDisable(true);
				granularityButtonDay.setDisable(true);
				granularityButtonWeek.setDisable(true);
			}

		graphNameField.clear();
		graphNameField.setPromptText(setGraphName());
	}


	private void drawHistogram() {
		System.out.println("DrawingHistogram");
		lineChart.getData().clear();
		lineChart.setVisible(false);
		histogram.setVisible(true);
		histogram.getData().clear();
		histoSeries = new XYChart.Series<>();
		ArrayList<Pair<String, Integer>> clicksPerCost = graphManager.getDistributionPoints();

		Object prev = "HELD";
		Object value = 0;
		for (Pair point : clicksPerCost) {
			if (prev != "HELD") {
				histoSeries.getData().add(new XYChart.Data(prev + " - " + point.getKey(), value));
			}
			prev = point.getKey();
			value = point.getValue();
		}
		histoSeries.getData().add(new XYChart.Data(prev + " +", value));

		if (graphNameField.getText().equals(""))
			histoSeries.setName(graphNameField.getPromptText());
		else
			histoSeries.setName(graphNameField.getText());


		if (histoNames.size() > 0){
			for (int i = 0; i < savedHistoData.size(); i++) {
				savedHistoData.get(i).setName(histoNames.get(i));
				histogram.getData().addAll(savedHistoData.get(i));
			}
		}

		histogram.getData().addAll(histoSeries);

		graphNameField.clear();
		graphNameField.setPromptText(setHistoName());

		enableSelectors();
		granularityButtonDay.setDisable(true);
		granularityButtonWeek.setDisable(true);
		granularityButtonHour.setDisable(true);
	}

	@FXML
	private void saveGraph(){
		if(graphType.equals("Click Distribution")){
			if (graphNameField.getText().equals(""))
				histoNames.add(graphNameField.getPromptText());
			else
				histoNames.add(graphNameField.getText());

			graphNameField.clear();
			graphNameField.setPromptText(setHistoName());

			savedHistoData.add(histoSeries);
		}else {
			if (graphNameField.getText().equals(""))
				graphNames.add(graphNameField.getPromptText());
			else
				graphNames.add(graphNameField.getText());

			graphNameField.clear();
			graphNameField.setPromptText(setGraphName());

			savedGraphDataHour.add(seriesHour);
			savedGraphDataDay.add(seriesDay);
			savedGraphDataWeek.add(seriesWeek);
		}

		saveGraphButton.setText("Snapshot Saved");
		saveGraphButton.setDisable(true);

		finalI += 1;

	}


	private String setGraphName() {
		int counter = 1;

		Iterator itr = graphNames.iterator();
		while(itr.hasNext())
		{
			String nextName = (String) itr.next();
			if (nextName.equals("Snapshot-" + counter))
				counter += 1;
		}
		return "Snapshot-" + counter;
	}

	private String setHistoName() {
		int counter = 1;

		Iterator itr = histoNames.iterator();
		while(itr.hasNext())
		{
			String nextName = (String) itr.next();
			if (nextName.equals("Snapshot-" + counter))
				counter += 1;
		}
		return "Snapshot-" + counter;
	}



	@FXML
	private void deleteGraph(){
		savedGraphDataHour = new ArrayList<>();
		savedGraphDataDay = new ArrayList<>();
		savedGraphDataWeek = new ArrayList<>();
		graphNames = new ArrayList<>();

		histoNames = new ArrayList<>();
		savedHistoData = new ArrayList<>();
		graphNameField.setPromptText("Snapshot-1");

		drawGraph();
	}


	private void enableSelectors() {
		campaignPicker.setDisable(false);

		applyFiltersButton.setDisable(false);
		saveGraphButton.setDisable(false);
		deleteGraphButton.setDisable(false);
		saveGraphButton.setText("Take Snapshot");

//		timePickerStart.setDisable(!timePickerStart.isDisabled());
//		timePickerEnd.setDisable(!timePickerEnd.isDisabled());

		genderButtonBoth.setDisable(false);
		genderButtonFemale.setDisable(false);
		genderButtonMale.setDisable(false);

		bounceButtonSingle.setDisable(false);
		bounceButtonTime.setDisable(false);

		granularityButtonHour.setDisable(false);
		granularityButtonDay.setDisable(false);
		granularityButtonWeek.setDisable(false);
	}

	private void toggleSelectorEnabling() {
		campaignPicker.setDisable(!campaignPicker.isDisabled());
		if(saveGraphButton.getText().equals("Take Snapshot")) {
			saveGraphButton.setDisable(!saveGraphButton.isDisabled());
		}
		deleteGraphButton.setDisable(!deleteGraphButton.isDisabled());
		applyFiltersButton.setDisable(!applyFiltersButton.isDisabled());

//		timePickerStart.setDisable(!timePickerStart.isDisabled());
//		timePickerEnd.setDisable(!timePickerEnd.isDisabled());

		genderButtonBoth.setDisable(!genderButtonBoth.isDisabled());
		genderButtonFemale.setDisable(!genderButtonFemale.isDisabled());
		genderButtonMale.setDisable(!genderButtonMale.isDisabled());

		bounceButtonSingle.setDisable(!bounceButtonSingle.isDisabled());
		bounceButtonTime.setDisable(!bounceButtonTime.isDisabled());

		if(graphType != "Click Distribution") {
			granularityButtonHour.setDisable(!granularityButtonHour.isDisabled());
			granularityButtonDay.setDisable(!granularityButtonDay.isDisabled());
			granularityButtonWeek.setDisable(!granularityButtonWeek.isDisabled());
		}
	}

	private void startProgressIndicators() {
		lblTotalCost.setText("");
		lblImpressions.setText("");
		lblClicks.setText("");
		lblCtr.setText("");
		lblBounces.setText("");
		lblConversions.setText("");
		lblCpc.setText("");
		lblUniques.setText("");
		lblCpm.setText("");
		lblBounceRate.setText("");
		lblCpa.setText("");

		lblTotalCost.setGraphic(setupProgressIndicatorSpinner());
		lblImpressions.setGraphic(setupProgressIndicatorSpinner());
		lblClicks.setGraphic(setupProgressIndicatorSpinner());
		lblCtr.setGraphic(setupProgressIndicatorSpinner());
		lblBounces.setGraphic(setupProgressIndicatorSpinner());
		lblConversions.setGraphic(setupProgressIndicatorSpinner());
		lblCpc.setGraphic(setupProgressIndicatorSpinner());
		lblUniques.setGraphic(setupProgressIndicatorSpinner());
		lblCpm.setGraphic(setupProgressIndicatorSpinner());
		lblBounceRate.setGraphic(setupProgressIndicatorSpinner());
		lblCpa.setGraphic(setupProgressIndicatorSpinner());
	}

	private ProgressIndicator setupProgressIndicatorSpinner() {
		ProgressIndicator progressIndicator = new ProgressIndicator();
		progressIndicator.setMinSize(26,26);
		return progressIndicator;
	}

	private void launchFetchDatesTask() {
		// Fetching dates for date pickers
		// source: https://stackoverflow.com/questions/35907325/how-to-set-minimum-and-maximum-date-in-datepicker-calander-in-javafx8
		var task = new FetchDatesTask(tableNameClick);
		task.setOnSucceeded(e -> {
			EventHandler<ActionEvent> handlerStart = timePickerStart.getOnAction();
			EventHandler<ActionEvent> handlerEnd = timePickerEnd.getOnAction();
			timePickerStart.setOnAction(null);
			timePickerEnd.setOnAction(null);

			DatePicker maxDate = new DatePicker();
			DatePicker minDate = new DatePicker();

			// Fetch the start and end dates and remove the time part
			var start = calcManager.getStart().split(" ")[0];
			var end = calcManager.getEnd().split(" ")[0];

			maxDate.setValue(LocalDate.parse(end));
			minDate.setValue(LocalDate.parse(start));

			// Make cells before start and after end red
			final Callback<DatePicker, DateCell> cellFactory;
			cellFactory = getPickerDateCellCallback(minDate, maxDate);

			// Set unavailable cells to both pickers
			timePickerStart.setDayCellFactory(cellFactory);
			timePickerEnd.setDayCellFactory(cellFactory);

			// Set the dates on pickers
			timePickerStart.setValue(LocalDate.parse(start));
			timePickerEnd.setValue(LocalDate.parse(end));

			timePickerStart.setOnAction(handlerStart);
			timePickerEnd.setOnAction(handlerEnd);

			// Enable the pickers
			timePickerStart.setDisable(false);
			timePickerEnd.setDisable(false);
		});
		exec.execute(task);
	}

	private static Callback<DatePicker, DateCell> getPickerDateCellCallback(DatePicker minDate, DatePicker maxDate) {
		final Callback<DatePicker, DateCell> cellFactory;
		cellFactory = (final DatePicker datePicker) -> new DateCell() {
			@Override
			public void updateItem(LocalDate item, boolean empty) {
				super.updateItem(item, empty);
				if (item.isBefore(minDate.getValue())) { // Disable all dates before start
					setDisable(true);
					setStyle("-fx-background-color: #ffc0cb;"); // Red background
				}
				if (item.isAfter(maxDate.getValue())) { //Disable all dates after end
					setDisable(true);
					setStyle("-fx-background-color: #ffc0cb;"); // Red background
				}
			}
		};
		return cellFactory;
	}

	/**
	 * Refactored method for launching threaded tasks for fetching and calculation metrics
	 *
	 * @param firstOperation Operation code between 0 and 10 for task 1
	 * @param secondOperation Operation code between 0 and 10 for task 2
	 * @param thirdOperation Operation code between 0 and 10 for task 3
	 * @param fourthOperation Operation code between 0 and 10 for task 4
	 * @param fifthOperation Operation code between 0 and 10 for task 5
	 */
	private void launchFiveTasks(int firstOperation, int secondOperation, int thirdOperation, int fourthOperation, int fifthOperation) {
		launchTask(firstOperation, tableNameImpressions, tableNameClick, tableNameServer);
		launchTask(secondOperation, tableNameImpressions, tableNameClick, tableNameServer);
		launchTask(thirdOperation, tableNameImpressions, tableNameClick, tableNameServer);
		launchTask(fourthOperation, tableNameImpressions, tableNameClick, tableNameServer);
		launchTask(fifthOperation,tableNameImpressions, tableNameClick, tableNameServer);
	}

	private void launchFiveTasksGraph(int firstOperation, int secondOperation, int thirdOperation, int fourthOperation, int fifthOperation) {
		var task1 = new FetchGraphDataTask(firstOperation, tableNameImpressions, tableNameClick, tableNameServer);
		task1.setOnSucceeded(event1 -> {
			var task2 = new FetchGraphDataTask(secondOperation, tableNameImpressions, tableNameClick, tableNameServer);
			task2.setOnSucceeded(event2 -> {
				var task3 = new FetchGraphDataTask(thirdOperation, tableNameImpressions, tableNameClick, tableNameServer);
				task3.setOnSucceeded(event3 -> {
					var task4 = new FetchGraphDataTask(fourthOperation, tableNameImpressions, tableNameClick, tableNameServer);
					task4.setOnSucceeded(event4 -> {
						var task5 = new FetchGraphDataTask(fifthOperation, tableNameImpressions, tableNameClick, tableNameServer);
						task5.setOnFailed(e -> System.out.println("Failed graph task number: " + fifthOperation));
						exec.execute(task5); //Giving the task to the executor for scheduling
					});
					task4.setOnFailed(e -> System.out.println("Failed graph task number: " + fourthOperation));
					exec.execute(task4); //Giving the task to the executor for scheduling
				});
				task3.setOnFailed(e -> System.out.println("Failed graph task number: " + thirdOperation));
				exec.execute(task3); //Giving the task to the executor for scheduling
			});
			task2.setOnFailed(e -> System.out.println("Failed graph task number: " + secondOperation));
			exec.execute(task2); //Giving the task to the executor for scheduling
		});
		task1.setOnFailed(e -> System.out.println("Failed graph task number: " + firstOperation));
		exec.execute(task1); //Giving the task to the executor for scheduling
	}

	private void launchFourTasksGraph(int firstOperation, int secondOperation, int thirdOperation, int fourthOperation) {
		var task1 = new FetchGraphDataTask(firstOperation, tableNameImpressions, tableNameClick, tableNameServer);
		task1.setOnSucceeded(event1 -> {
			var task2 = new FetchGraphDataTask(secondOperation, tableNameImpressions, tableNameClick, tableNameServer);
			task2.setOnSucceeded(event2 -> {
				var task3 = new FetchGraphDataTask(thirdOperation, tableNameImpressions, tableNameClick, tableNameServer);
				task3.setOnSucceeded(event3 -> {
					var task4 = new FetchGraphDataTask(fourthOperation, tableNameImpressions, tableNameClick, tableNameServer);
					task4.setOnFailed(e -> System.out.println("Failed graph task number: " + fourthOperation));
					exec.execute(task4); //Giving the task to the executor for scheduling
				});
				task3.setOnFailed(e -> System.out.println("Failed graph task number: " + thirdOperation));
				exec.execute(task3); //Giving the task to the executor for scheduling
			});
			task2.setOnFailed(e -> System.out.println("Failed graph task number: " + secondOperation));
			exec.execute(task2); //Giving the task to the executor for scheduling
		});
		task1.setOnFailed(e -> System.out.println("Failed graph task number: " + firstOperation));
		exec.execute(task1); //Giving the task to the executor for scheduling
	}

	/**
	 * Method given the operation to execute creates a task for it and sets up the correct label to update on completion
	 *
	 * @param operation Operation to execute (check the constants for values)
	 * @param impressionTableName The impression table to work on
	 * @param clickTableName The click table to work on
	 * @param serverTableName The server table to work on
	 */
	private void launchTask(int operation, String impressionTableName, String clickTableName, String serverTableName) {
		var task = new MetricCalculatorTask(operation, impressionTableName, clickTableName, serverTableName);

		switch (operation) {
			case FETCH_IMPRESSION -> task.setOnSucceeded(e -> postMetricCalculationInteger(lblImpressions, calcManager.getImpressions()));
			case FETCH_CLICKS -> task.setOnSucceeded(e -> postMetricCalculationInteger(lblClicks, calcManager.getClicks()));
			case FETCH_UNIQUES -> task.setOnSucceeded(e -> postMetricCalculationInteger(lblUniques, calcManager.getUniques()));
			case FETCH_BOUNCES -> task.setOnSucceeded(e -> postMetricCalculationInteger(lblBounces, calcManager.getBounces()));
			case FETCH_CONVERSIONS -> task.setOnSucceeded(e -> postMetricCalculationInteger(lblConversions, calcManager.getConversions()));
			case FETCH_COST -> task.setOnSucceeded(e -> postMetricCalculationDouble(lblTotalCost,calcManager.getCost()));
			case CALCULATE_CTR -> task.setOnSucceeded(e -> postMetricCalculationDouble(lblCtr, calcManager.getCtr()));
			case CALCULATE_CPC -> task.setOnSucceeded(e -> postMetricCalculationDouble(lblCpc, calcManager.getCpc()));
			case CALCULATE_CPA -> task.setOnSucceeded(e -> postMetricCalculationDouble(lblCpa, calcManager.getCpa()));
			case CALCULATE_BOUNCE_RATE -> task.setOnSucceeded(e -> postMetricCalculationDouble(lblBounceRate, calcManager.getBounceRate()));
			case CALCULATE_CPM -> task.setOnSucceeded(e -> {
				lblCpm.setGraphic(null);
				lblCpm.setText(Double.toString(calcManager.getCpm()));
			});
		}
		task.setOnFailed(e -> System.out.println("Failed task: " + operation));
		exec.execute(task); //Giving the task to the executor for scheduling
	}

	private void launchTaskGraph(int operation, String impressionTableName, String clickTableName, String serverTableName)   {
		var task = new FetchGraphDataTask(operation, impressionTableName, clickTableName, serverTableName);


		task.setOnFailed(e -> System.out.println("Failed graph task Number: " + operation));
		exec.execute(task); //Giving the task to the executor for scheduling
	}

	private void postMetricCalculationInteger(Label label, Integer value) {
		label.setGraphic(null);
		label.setText(Integer.toString(value));
	}

	private void postMetricCalculationDouble(Label label, Double value) {
		label.setGraphic(null);
		label.setText(Double.toString(value));
	}

	public void Launch() {
		launch();
	}

	public void setCampaign(String campaign) {
		this.campaign = campaign;
	}

	public String getCampaign() {
		return campaign;
	}
}
