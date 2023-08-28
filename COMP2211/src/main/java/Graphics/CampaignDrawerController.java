package Graphics;

import DataManagers.CampaignManager;
import Services.CampaignDropTask;
import com.jfoenix.controls.JFXListView;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * Class manages interactions for the Side Drawer UI component
 */
@SuppressWarnings("ALL")
public class CampaignDrawerController implements Initializable {
	private static final String INPUT_DB_NAME = "input_db";

	@FXML
	private JFXListView<HBoxCell> campaignList; // List view for the campaign names loaded

	private MainSceneController mainSceneController; // MainSceneController associated to this drawer
	private CampaignManager campaignManager = new CampaignManager("input_db");

	// Initializing executor to manage the threaded operations
	private final Executor exec = Executors.newCachedThreadPool(runnable -> {
		Thread t = new Thread(runnable);
		t.setDaemon(true);
		return t ;
	});

	/**
	 * Initializing the campaign manager for the component, initializing the campaign manager table and fetching from it
	 *
	 * @param url
	 * @param resourceBundle
	 */
	@Override
	public void initialize(URL url, ResourceBundle resourceBundle) {
		try {
			campaignManager.setupCampaignTable("campaigns", "campaign_name, impression_table_name, click_table_name, server_table_name");
			fetchCampaigns();
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Class defines a cell for the campaign name and buttons for each campaign
	 */
	public class HBoxCell extends HBox {
		Label label = new Label();
		Button delButton = new Button( "", new ImageView("icons/delete.png"));
		ProgressIndicator progressIndicator = new ProgressIndicator();

		/**
		 * Initializing a small spinner
		 * @return Spinner UI component
		 */
		private ProgressIndicator setupProgressIndicatorSpinner() {
			progressIndicator.setMinSize(16,16);
			return progressIndicator;
		}

		/**
		 * Initializing the cell with the name of the campaign it represents
		 *
		 * @param campaignName Text to assign to the cell
		 */
		HBoxCell(String campaignName) {
			super();

			// Setting up the label with the campaign name
			label.setText(campaignName);
			label.setMaxWidth(Double.MAX_VALUE);
			HBox.setHgrow(label, Priority.ALWAYS);

			// Setting up the red delete button for the campaign
			delButton.setMaxSize(20,20);
			delButton.setStyle("-fx-background-color:#dc4d3c");

			//Start a drop campaign threaded operation and update the UI when task completes
			delButton.setOnAction(e -> {
				try {
					// Replace button icon with spinner while task deletes the campaign
					delButton.setGraphic(setupProgressIndicatorSpinner());

					// Setting up the parallel task to drop the campaign
					final var task = new CampaignDropTask(INPUT_DB_NAME, campaignName);
					task.setOnSucceeded(event -> {
						fetchCampaigns();
						var temp = mainSceneController.getCampaign();
						mainSceneController.fetchCampaignsIntoPicker();
						mainSceneController.setCampaign(temp);

						if (campaignName.equals(mainSceneController.getCampaign())) {
							var out = campaignManager.getColumnContents("campaigns", "campaign_name");
							if (out.size() > 0) {
								mainSceneController.setFocusedCampaign((String) out.get(0));
							} else {
								mainSceneController.setFocusedCampaign("");
							}
							try {
								mainSceneController.focusCampaign();
							} catch (SQLException ex) {
								throw new RuntimeException(ex);
							}
						}
					});

					task.setOnCancelled(event -> {
						fetchCampaigns();
						var temp = mainSceneController.getCampaign();
						mainSceneController.fetchCampaignsIntoPicker();
						mainSceneController.setCampaign(temp);

						if (campaignName.equals(mainSceneController.getCampaign())) {
							var out = campaignManager.getColumnContents("campaigns", "campaign_name");
							if (out.size() > 0) {
								mainSceneController.setFocusedCampaign((String) out.get(0));
							} else {
								mainSceneController.setFocusedCampaign("");
							}
							try {
								mainSceneController.focusCampaign();
							} catch (SQLException ex) {
								throw new RuntimeException(ex);
							}
						}
					});

					task.setOnFailed(event -> {
						fetchCampaigns();
						var temp = mainSceneController.getCampaign();
						mainSceneController.fetchCampaignsIntoPicker();
						mainSceneController.setCampaign(temp);

						if (campaignName.equals(mainSceneController.getCampaign())) {
							var out = campaignManager.getColumnContents("campaigns", "campaign_name");
							if (out.size() > 0) {
								mainSceneController.setFocusedCampaign((String) out.get(0));
							} else {
								mainSceneController.setFocusedCampaign("");
							}
							try {
								mainSceneController.focusCampaign();
							} catch (SQLException ex) {
								throw new RuntimeException(ex);
							}
						}
					});
					exec.execute(task);
				} catch (Exception ex) {
					throw new RuntimeException(ex);
				}
			});

			this.getChildren().addAll(label, delButton);
		}
	}

	/**
	 * Fetching the campaigns present in the database and updating the UI
	 */
	private void fetchCampaigns() {
		var out = campaignManager.getColumnContents("campaigns", "campaign_name");

		// Store fetched names into an array list of cells
		List<HBoxCell> list = new ArrayList<>();
		for (Object s : out) {
			list.add(new HBoxCell((String) s));
		}

		// Update the list and UI component associated with it
		ObservableList<HBoxCell> campaignObservableList = FXCollections.observableList(list);
		campaignList.setItems(campaignObservableList);
	}

	/**
	 * Pressing the add campaign button sets up a new instance of the CreateCampaignController class
	 *
	 * @throws IOException Throw exception if failed to connect to database
	 */
	@FXML
	public void addCampaign() throws IOException {
		// Loading the FXML for the window UI
		FXMLLoader loader = new FXMLLoader(getClass().getClassLoader().getResource("campaign_creator.fxml"));
		Parent root = loader.load();
		CreateCampaignController controller = loader.getController();
		controller.setMainSceneController(mainSceneController);

		Scene scene = new Scene(root);
		Stage newAddCampaignStage = new Stage();
		newAddCampaignStage.setTitle("Add Campaign Source");
		newAddCampaignStage.setScene(scene);

		// When the window closes (when add campaign is cancelled or it completes and automatically closes)
		newAddCampaignStage.setOnHidden(event -> {
			fetchCampaigns();
			mainSceneController.handleAddCampaign();
		});
		newAddCampaignStage.initModality(Modality.APPLICATION_MODAL); // Lock other windows while
		newAddCampaignStage.show();
	}

	public void setMainSceneController(MainSceneController mainSceneController) {
		this.mainSceneController = mainSceneController;
	}
}
