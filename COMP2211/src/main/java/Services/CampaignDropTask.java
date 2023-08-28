package Services;

import DataManagers.CampaignManager;
import javafx.concurrent.Task;

/**
 * Class sets up a campaign manager and runs the dropCampaign method to remove a campaign
 */
public final class CampaignDropTask extends Task {

	CampaignManager campaignManager;
	String campaignName;

	public CampaignDropTask(String databaseName, String campaignName) {
		this.campaignName = campaignName;
		campaignManager = new CampaignManager(databaseName);
	}
	@Override
	protected Object call() throws Exception {
		campaignManager.dropCampaign(campaignName);
		return null;
	}
}
