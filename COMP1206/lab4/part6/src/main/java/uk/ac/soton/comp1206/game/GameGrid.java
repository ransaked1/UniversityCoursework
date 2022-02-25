package uk.ac.soton.comp1206.game;

import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import java.util.ArrayList;

public class GameGrid extends GridPane {

	private final ArrayList<BlockClickedListener> listeners = new ArrayList<>();

	public GameGrid(Color[] colors, Grid grid, int width, int height) {
		var cols = grid.getCols();
		var rows = grid.getRows();

		setMaxWidth(width);
		setMaxHeight(height);
		setGridLinesVisible(true);

		for (var y = 0; y < rows; y++) {
			for (var x = 0; x < cols; x++) {
				var block = new GameBlock(colors, x, y, width/cols, height/rows);
				add(block, x, y);

				block.valueProperty().bind(grid.getGridProperty(x, y));

				block.setOnMouseClicked((e) -> blockClicked(block));
			}
		}
	}

	public void addListener(BlockClickedListener listener) {
		this.listeners.add(listener);
	}

	public void blockClicked(GameBlock block) {
		for(BlockClickedListener listener : listeners) {
			listener.blockClicked(block);
		}
	}
}
