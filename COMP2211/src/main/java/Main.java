import Graphics.MainSceneController;

//TO DO:
// When deleting a campaign and automatically switching to another one, buttons are not blocked
// Buttons become active if you switch metric while graph data is still loading

public class Main {
  public static void main(String[] args) {
    MainSceneController mainScene = new MainSceneController();
    mainScene.Launch();
  }
}
