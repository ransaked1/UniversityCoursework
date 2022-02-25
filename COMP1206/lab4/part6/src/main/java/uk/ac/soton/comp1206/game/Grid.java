package uk.ac.soton.comp1206.game;

import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleIntegerProperty;

public class Grid {

	private final int cols;
	private final int rows;
	final SimpleIntegerProperty[][] grid;

	public Grid(int cols, int rows) {
		this.cols = cols;
		this.rows = rows;

		grid = new SimpleIntegerProperty[rows][cols];

		for (var y = 0; y < rows; y++) {
			for (var x = 0; x < cols; x++) {
				grid[x][y] = new SimpleIntegerProperty(0);
			}
		}
	}

	public IntegerProperty getGridProperty(int x, int y) {
		return grid[x][y];
	}

	public void set(int x, int y, int value) {
		grid[x][y].set(value);
	}

	public int get(int x, int y) {
		return grid[x][y].get();
	}

	public int getCols() {
		return cols;
	}

	public int getRows() {
		return rows;
	}
}
